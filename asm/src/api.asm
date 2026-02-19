; api.asm — 6 API endpoint handlers
; Each handler: (query_string, &resp_ptr, &resp_len)
default rel

extern snprintf, malloc, memcpy, strlen, free, strdup, strcmp
extern atoll, atoi
extern parse_query_param
extern db_get_readings, db_get_readings_downsampled, db_get_latest, db_get_devices
extern db_get_filtered_count
extern clock_gettime, fprintf, fopen, fread, fclose
extern stderr

extern pthread_rwlock_rdlock, pthread_rwlock_unlock

extern g_port, g_poll_interval_ms, g_fetch_timeout_ms, g_max_api_rows
extern g_ds_bucket_count, g_ds_bucket_keys, g_ds_bucket_values
extern g_devices, g_device_count
extern g_requests_served, g_active_conns, g_poll_successes, g_poll_failures
extern g_started_at, g_health, g_health_lock

extern http_200_json_hdr, http_400_response, http_400_response_len
extern http_500_response, http_500_response_len
extern fmt_stats_json, fmt_config_json, fmt_config_device
extern fmt_health_entry
extern str_null_json, str_param_from, str_param_to, str_param_device, str_param_limit
extern str_param_downsample
extern fmt_ds_bucket_first, fmt_ds_bucket_rest
extern fmt_config_devices_hdr
extern str_proc_statm

section .rodata
read_mode: db "r", 0
fmt_lld: db `%lld`, 0
fmt_quoted: db `"%s"`, 0

section .text

; ── build_json_response(json_body, &resp_ptr, &resp_len) ─────────────────────
; Takes ownership of json_body string (will free it)
global build_json_response
build_json_response:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 520           ; header buffer (512) + padding

    mov r12, rdi            ; json body
    mov r13, rsi            ; &resp_ptr
    mov r14, rdx            ; &resp_len

    ; Get body length
    mov rdi, r12
    call strlen wrt ..plt
    mov r15, rax            ; body_len (callee-saved)

    ; Build header
    lea rdi, [rsp]
    mov esi, 512
    lea rdx, [http_200_json_hdr]
    mov rcx, r15            ; Content-Length
    xor eax, eax
    call snprintf wrt ..plt
    mov ebx, eax            ; header_len (callee-saved)

    ; Allocate: header + body
    movsx rdi, ebx
    add rdi, r15
    add rdi, 1
    call malloc wrt ..plt
    test rax, rax
    jz .bjr_fail

    mov [r13], rax          ; resp_ptr
    mov r13, rax            ; reuse r13 for buffer ptr (don't need &resp_ptr anymore)

    ; Copy header
    mov rdi, rax
    lea rsi, [rsp]
    movsx rdx, ebx          ; rbx = header_len, survives calls
    call memcpy wrt ..plt

    ; Copy body after header
    movsx rdi, ebx
    add rdi, r13             ; buffer + header_len
    mov rsi, r12
    mov rdx, r15             ; body_len (r15 survives calls)
    call memcpy wrt ..plt

    ; Set total length
    movsx rax, ebx
    add rax, r15
    mov [r14], rax

    ; Free json body
    mov rdi, r12
    call free wrt ..plt
    jmp .bjr_done

.bjr_fail:
    mov rdi, r12
    call free wrt ..plt
    mov qword [r13], 0
    mov qword [r14], 0

.bjr_done:
    add rsp, 520
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

; ── handle_readings(query, &resp_ptr, &resp_len) ─────────────────────────────
global handle_readings
handle_readings:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 344           ; param buffers + saved query ptr + downsample buf

    ; Stack layout:
    ;   [rsp+0..63]    from buffer
    ;   [rsp+64..127]  to buffer
    ;   [rsp+128..191] device buffer
    ;   [rsp+192..255] limit buffer
    ;   [rsp+256..319] downsample buffer
    ;   [rsp+320]      saved query pointer (8 bytes)
    ;   [rsp+328]      bucket_ms (8 bytes)

    mov [rsp + 320], rdi    ; save original query pointer
    mov r13, rsi            ; &resp_ptr
    mov r14, rdx            ; &resp_len
    mov qword [rsp + 328], 0  ; bucket_ms = 0 (no downsample)

    ; Parse 'from'
    mov rdi, [rsp + 320]
    lea rsi, [str_param_from]
    lea rdx, [rsp]
    mov ecx, 63
    call parse_query_param
    test eax, eax
    jnz .hr_no_from
    lea rdi, [rsp]
    call atoll wrt ..plt
    mov r15, rax
    jmp .hr_parse_to
.hr_no_from:
    xor r15d, r15d

.hr_parse_to:
    mov rdi, [rsp + 320]
    lea rsi, [str_param_to]
    lea rdx, [rsp + 64]
    mov ecx, 63
    call parse_query_param
    test eax, eax
    jnz .hr_no_to
    lea rdi, [rsp + 64]
    call atoll wrt ..plt
    mov rbx, rax
    jmp .hr_parse_device
.hr_no_to:
    mov rbx, 0x7FFFFFFFFFFFFFFF

.hr_parse_device:
    mov rdi, [rsp + 320]
    lea rsi, [str_param_device]
    lea rdx, [rsp + 128]
    mov ecx, 63
    call parse_query_param
    test eax, eax
    jnz .hr_no_device
    lea r12, [rsp + 128]     ; device ptr
    jmp .hr_parse_limit
.hr_no_device:
    xor r12d, r12d           ; NULL = all

.hr_parse_limit:
    mov rdi, [rsp + 320]
    lea rsi, [str_param_limit]
    lea rdx, [rsp + 192]
    mov ecx, 63
    call parse_query_param
    test eax, eax
    jnz .hr_default_limit
    lea rdi, [rsp + 192]
    call atoi wrt ..plt
    test eax, eax
    jle .hr_default_limit
    cmp eax, [g_max_api_rows]
    jle .hr_limit_ok
    mov eax, [g_max_api_rows]
.hr_limit_ok:
    mov [rsp + 336], eax    ; save limit temporarily
    jmp .hr_parse_downsample
.hr_default_limit:
    mov eax, [g_max_api_rows]
    mov [rsp + 336], eax

.hr_parse_downsample:
    mov rdi, [rsp + 320]
    lea rsi, [str_param_downsample]
    lea rdx, [rsp + 256]
    mov ecx, 63
    call parse_query_param
    test eax, eax
    jnz .hr_query           ; no downsample param — proceed normally

    ; Validate downsample value — loop over config bucket keys
    xor ecx, ecx              ; i = 0
.ds_loop:
    cmp ecx, [g_ds_bucket_count]
    jge .hr_bad_request        ; not found — invalid

    push rcx
    lea rdi, [rsp + 256 + 8]  ; downsample buffer (adjust for pushed rcx)
    mov rsi, [g_ds_bucket_keys + rcx*8]
    call strcmp wrt ..plt
    pop rcx
    test eax, eax
    jz .ds_found

    inc ecx
    jmp .ds_loop

.ds_found:
    mov rax, [g_ds_bucket_values + rcx*8]
    mov [rsp + 328], rax       ; bucket_ms
    jmp .hr_query

.hr_query:
    mov ecx, [rsp + 336]    ; limit
    cmp qword [rsp + 328], 0
    jne .hr_downsample_query

    ; Normal query (no downsample)
    mov rdi, r15            ; from
    mov rsi, rbx            ; to
    mov rdx, r12            ; device (NULL or ptr)
                             ; ecx = limit
    call db_get_readings
    jmp .hr_check_result

.hr_downsample_query:
    mov rdi, r15            ; from
    mov rsi, rbx            ; to
    mov rdx, r12            ; device (NULL or ptr)
                             ; ecx = limit
    mov r8, [rsp + 328]     ; bucket_ms
    call db_get_readings_downsampled

.hr_check_result:
    test rax, rax
    jz .hr_error

    mov rdi, rax
    mov rsi, r13
    mov rdx, r14
    call build_json_response
    jmp .hr_done

.hr_bad_request:
    ; Return 400 Bad Request
    mov rdi, http_400_response_len + 1
    call malloc wrt ..plt
    test rax, rax
    jz .hr_error
    mov [r13], rax
    mov rdi, rax
    lea rsi, [http_400_response]
    mov rdx, http_400_response_len
    call memcpy wrt ..plt
    mov qword [r14], http_400_response_len
    jmp .hr_done

.hr_error:
    mov qword [r13], 0
    mov qword [r14], 0

.hr_done:
    add rsp, 344
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

; ── handle_readings_count(query, &resp_ptr, &resp_len) ────────────────────────
global handle_readings_count
handle_readings_count:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 280           ; param buffers

    ; Stack layout:
    ;   [rsp+0..63]    from buffer
    ;   [rsp+64..127]  to buffer
    ;   [rsp+128..191] device buffer
    ;   [rsp+256]      saved query pointer (8 bytes)

    mov [rsp + 256], rdi    ; save original query pointer
    mov r13, rsi            ; &resp_ptr
    mov r14, rdx            ; &resp_len

    ; Parse 'from'
    mov rdi, [rsp + 256]
    lea rsi, [str_param_from]
    lea rdx, [rsp]
    mov ecx, 63
    call parse_query_param
    test eax, eax
    jnz .hrc_no_from
    lea rdi, [rsp]
    call atoll wrt ..plt
    mov r15, rax
    jmp .hrc_parse_to
.hrc_no_from:
    xor r15d, r15d

.hrc_parse_to:
    mov rdi, [rsp + 256]
    lea rsi, [str_param_to]
    lea rdx, [rsp + 64]
    mov ecx, 63
    call parse_query_param
    test eax, eax
    jnz .hrc_no_to
    lea rdi, [rsp + 64]
    call atoll wrt ..plt
    mov rbx, rax
    jmp .hrc_parse_device
.hrc_no_to:
    mov rbx, 0x7FFFFFFFFFFFFFFF

.hrc_parse_device:
    mov rdi, [rsp + 256]
    lea rsi, [str_param_device]
    lea rdx, [rsp + 128]
    mov ecx, 63
    call parse_query_param
    test eax, eax
    jnz .hrc_no_device
    lea r12, [rsp + 128]     ; device ptr
    jmp .hrc_query
.hrc_no_device:
    xor r12d, r12d           ; NULL = all

.hrc_query:
    mov rdi, r15            ; from
    mov rsi, rbx            ; to
    mov rdx, r12            ; device (NULL or ptr)
    call db_get_filtered_count

    test rax, rax
    jz .hrc_error

    mov rdi, rax
    mov rsi, r13
    mov rdx, r14
    call build_json_response
    jmp .hrc_done

.hrc_error:
    mov qword [r13], 0
    mov qword [r14], 0

.hrc_done:
    add rsp, 280
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

; ── handle_readings_latest(query, &resp_ptr, &resp_len) ──────────────────────
global handle_readings_latest
handle_readings_latest:
    push rbp
    mov rbp, rsp
    push r12
    push r13

    mov r12, rsi
    mov r13, rdx

    call db_get_latest
    test rax, rax
    jz .hrl_error

    mov rdi, rax
    mov rsi, r12
    mov rdx, r13
    call build_json_response
    jmp .hrl_done

.hrl_error:
    mov qword [r12], 0
    mov qword [r13], 0

.hrl_done:
    pop r13
    pop r12
    pop rbp
    ret

; ── handle_devices(query, &resp_ptr, &resp_len) ──────────────────────────────
global handle_devices
handle_devices:
    push rbp
    mov rbp, rsp
    push r12
    push r13

    mov r12, rsi
    mov r13, rdx

    call db_get_devices
    test rax, rax
    jz .hd_error

    mov rdi, rax
    mov rsi, r12
    mov rdx, r13
    call build_json_response
    jmp .hd_done

.hd_error:
    mov qword [r12], 0
    mov qword [r13], 0

.hd_done:
    pop r13
    pop r12
    pop rbp
    ret

; ── handle_health(query, &resp_ptr, &resp_len) ───────────────────────────────
global handle_health
handle_health:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8200           ; JSON output buffer

    mov r13, rsi            ; &resp_ptr
    mov r14, rdx            ; &resp_len

    ; Read-lock health data
    lea rdi, [g_health_lock]
    call pthread_rwlock_rdlock wrt ..plt

    ; Build JSON array into stack buffer
    mov byte [rsp], '['
    mov ebx, 1              ; write position

    mov r12d, [g_device_count]
    xor r15d, r15d          ; device index

; Macro-like pattern: recalc health entry ptr from r15d into rcx before each snprintf
; r15d = device index (callee-saved), ebx = write pos (callee-saved)
; r12d = device count (callee-saved)

; Clamp snprintf return to [0, size-1] then advance ebx (write position)
; %1 = size argument passed to the preceding snprintf call
%macro CLAMP_ADV 1
    test eax, eax
    jle %%skip
    cmp eax, %1
    jl %%adv
    mov eax, %1 - 1
%%adv:
    add ebx, eax
%%skip:
%endmacro

%macro HEALTH_PTR 1
    mov eax, r15d
    imul eax, 296
    lea rcx, [g_health + rax + %1]
%endmacro

.hh_loop:
    cmp r15d, r12d
    jge .hh_close
    ; Safety: stop if buffer nearly full (each entry needs ~1024 bytes max)
    cmp ebx, 7168
    jge .hh_close

    cmp r15d, 0
    je .hh_no_comma
    mov byte [rsp + rbx], ','
    inc ebx
.hh_no_comma:
    mov byte [rsp + rbx], '{'
    inc ebx

    ; "ip":"value"
    HEALTH_PTR 0
    lea rdi, [rsp + rbx]
    mov esi, 200
    lea rdx, [.fmt_ip]
    ; rcx already set by HEALTH_PTR
    xor eax, eax
    call snprintf wrt ..plt
    CLAMP_ADV 200

    ; ,"label":"value"
    HEALTH_PTR 64
    lea rdi, [rsp + rbx]
    mov esi, 200
    lea rdx, [.fmt_label]
    xor eax, eax
    call snprintf wrt ..plt
    CLAMP_ADV 200

    ; ,"status":"value"
    HEALTH_PTR 128
    lea rdi, [rsp + rbx]
    mov esi, 200
    lea rdx, [.fmt_status]
    xor eax, eax
    call snprintf wrt ..plt
    CLAMP_ADV 200

    ; ,"lastSuccess": value_or_null
    mov eax, r15d
    imul eax, 296
    cmp qword [g_health + rax + 192], 0
    je .hh_ls_null
    mov rcx, [g_health + rax + 192]
    lea rdi, [rsp + rbx]
    mov esi, 100
    lea rdx, [.fmt_ls_num]
    xor eax, eax
    call snprintf wrt ..plt
    CLAMP_ADV 100
    jmp .hh_le
.hh_ls_null:
    lea rdi, [rsp + rbx]
    mov esi, 100
    lea rdx, [.fmt_ls_null]
    xor eax, eax
    call snprintf wrt ..plt
    CLAMP_ADV 100

.hh_le:
    mov eax, r15d
    imul eax, 296
    cmp qword [g_health + rax + 200], 0
    je .hh_le_null
    mov rcx, [g_health + rax + 200]
    lea rdi, [rsp + rbx]
    mov esi, 100
    lea rdx, [.fmt_le_num]
    xor eax, eax
    call snprintf wrt ..plt
    CLAMP_ADV 100
    jmp .hh_lem
.hh_le_null:
    lea rdi, [rsp + rbx]
    mov esi, 100
    lea rdx, [.fmt_le_null]
    xor eax, eax
    call snprintf wrt ..plt
    CLAMP_ADV 100

.hh_lem:
    mov eax, r15d
    imul eax, 296
    cmp byte [g_health + rax + 208], 0
    je .hh_lem_null
    lea rcx, [g_health + rax + 208]
    lea rdi, [rsp + rbx]
    mov esi, 200
    lea rdx, [.fmt_lem_str]
    xor eax, eax
    call snprintf wrt ..plt
    CLAMP_ADV 200
    jmp .hh_cf
.hh_lem_null:
    lea rdi, [rsp + rbx]
    mov esi, 100
    lea rdx, [.fmt_lem_null]
    xor eax, eax
    call snprintf wrt ..plt
    CLAMP_ADV 100

.hh_cf:
    mov eax, r15d
    imul eax, 296
    mov ecx, [g_health + rax + 272]
    lea rdi, [rsp + rbx]
    mov esi, 100
    lea rdx, [.fmt_cf]
    xor eax, eax
    call snprintf wrt ..plt
    CLAMP_ADV 100

    inc r15d
    jmp .hh_loop

.hh_close:
    mov byte [rsp + rbx], ']'
    mov byte [rsp + rbx + 1], 0

    ; Unlock
    lea rdi, [g_health_lock]
    call pthread_rwlock_unlock wrt ..plt

    ; strdup and build response
    lea rdi, [rsp]
    call strdup wrt ..plt
    test rax, rax
    jz .hh_fail

    mov rdi, rax
    mov rsi, r13
    mov rdx, r14
    call build_json_response
    jmp .hh_done

.hh_fail:
    mov qword [r13], 0
    mov qword [r14], 0

.hh_done:
    add rsp, 8200
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

section .rodata
; Format fragments for health entries
.fmt_ip:       db `"ip":"%s"`, 0
.fmt_label:    db `,"label":"%s"`, 0
.fmt_status:   db `,"status":"%s"`, 0
.fmt_ls_num:   db `,"lastSuccess":%lld`, 0
.fmt_ls_null:  db `,"lastSuccess":null`, 0
.fmt_le_num:   db `,"lastError":%lld`, 0
.fmt_le_null:  db `,"lastError":null`, 0
.fmt_lem_str:  db `,"lastErrorMessage":"%s"`, 0
.fmt_lem_null: db `,"lastErrorMessage":null`, 0
.fmt_cf:       db `,"consecutiveFailures":%d}`, 0

section .text

; ── handle_config(query, &resp_ptr, &resp_len) ───────────────────────────────
global handle_config
handle_config:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 4104

    mov r13, rsi
    mov r14, rdx

    ; Build config JSON: {"pollIntervalMs":%d,"downsampleBuckets":{
    lea rdi, [rsp]
    mov esi, 4096
    lea rdx, [fmt_config_json]
    mov ecx, [g_poll_interval_ms]
    xor eax, eax
    xor ebx, ebx
    call snprintf wrt ..plt
    CLAMP_ADV 4096

    ; Add downsampleBuckets entries: "key":value, ...
    mov r12d, [g_ds_bucket_count]
    xor r15d, r15d

.hc_bucket_loop:
    cmp r15d, r12d
    jge .hc_bucket_done
    cmp ebx, 3600
    jge .hc_bucket_done

    ; Use fmt_ds_bucket_first for first entry, fmt_ds_bucket_rest for rest
    cmp r15d, 0
    jne .hc_bucket_rest
    lea rdx, [fmt_ds_bucket_first]
    jmp .hc_bucket_fmt
.hc_bucket_rest:
    lea rdx, [fmt_ds_bucket_rest]
.hc_bucket_fmt:
    ; snprintf(buf+pos, remaining, fmt, key_str, value_i64)
    mov eax, r15d
    mov rcx, [g_ds_bucket_keys + rax*8]    ; key string
    mov r8, [g_ds_bucket_values + rax*8]   ; value i64
    lea rdi, [rsp + rbx]
    mov esi, 4096
    sub esi, ebx
    xor eax, eax
    call snprintf wrt ..plt
    ; Clamp advance
    mov ecx, 4090
    sub ecx, ebx
    test ecx, ecx
    jle .hc_bucket_done
    cmp eax, ecx
    cmovg eax, ecx
    test eax, eax
    jle .hc_bucket_no_adv
    add ebx, eax
.hc_bucket_no_adv:
    inc r15d
    jmp .hc_bucket_loop

.hc_bucket_done:
    ; Close buckets object and start devices: },"devices":[
    lea rdi, [rsp + rbx]
    mov esi, 4096
    sub esi, ebx
    lea rdx, [fmt_config_devices_hdr]
    xor eax, eax
    call snprintf wrt ..plt
    CLAMP_ADV 4096

    ; Add devices
    mov r12d, [g_device_count]
    xor r15d, r15d

.hc_dev_loop:
    cmp r15d, r12d
    jge .hc_dev_done
    cmp ebx, 3800
    jge .hc_dev_done

    cmp r15d, 0
    je .hc_no_comma
    mov byte [rsp + rbx], ','
    inc ebx
.hc_no_comma:
    ; Get device pointers
    mov eax, r15d
    shl eax, 4
    lea rcx, [g_devices]
    mov r8, [rcx + rax]      ; ip ptr
    mov r9, [rcx + rax + 8]  ; label ptr
    ; snprintf(buf, size, fmt, ip, label)
    lea rdi, [rsp + rbx]
    mov esi, 4096
    sub esi, ebx
    lea rdx, [fmt_config_device]
    mov rcx, r8              ; ip
    mov r8, r9               ; label
    xor eax, eax
    call snprintf wrt ..plt
    ; Clamp advance to remaining buffer
    mov ecx, 4090
    sub ecx, ebx
    test ecx, ecx
    jle .hc_dev_done
    cmp eax, ecx
    cmovg eax, ecx
    test eax, eax
    jle .hc_no_adv
    add ebx, eax
.hc_no_adv:

    inc r15d
    jmp .hc_dev_loop

.hc_dev_done:
    mov byte [rsp + rbx], ']'
    mov byte [rsp + rbx + 1], '}'
    mov byte [rsp + rbx + 2], 0

    lea rdi, [rsp]
    call strdup wrt ..plt
    test rax, rax
    jz .hc_fail

    mov rdi, rax
    mov rsi, r13
    mov rdx, r14
    call build_json_response
    jmp .hc_done

.hc_fail:
    mov qword [r13], 0
    mov qword [r14], 0

.hc_done:
    add rsp, 4104
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

; ── handle_stats(query, &resp_ptr, &resp_len) ────────────────────────────────
global handle_stats
handle_stats:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 4120

    mov r13, rsi
    mov r14, rdx

    ; Get current wall time in ms
    sub rsp, 16
    xor edi, edi            ; CLOCK_REALTIME
    mov rsi, rsp
    call clock_gettime wrt ..plt
    mov rax, [rsp]
    imul rax, 1000
    mov rdx, [rsp + 8]
    mov rcx, rdx
    shr rcx, 20
    add rax, rcx
    mov r12, rax            ; now_ms
    add rsp, 16

    ; uptime_ms
    mov rbx, r12
    sub rbx, [g_started_at]

    ; Get RSS from /proc/self/statm
    xor r15d, r15d          ; default RSS = 0
    lea rdi, [str_proc_statm]
    lea rsi, [read_mode]
    call fopen wrt ..plt
    test rax, rax
    jz .hs_no_rss

    mov r12, rax            ; FILE* (reuse r12, don't need now_ms)
    sub rsp, 256
    mov rdi, rsp
    mov esi, 1
    mov edx, 200
    mov rcx, r12
    call fread wrt ..plt
    mov byte [rsp + rax], 0

    mov rdi, r12
    call fclose wrt ..plt

    ; Parse: skip first number, get second (RSS pages)
    lea rdi, [rsp]
    ; Skip whitespace
.hs_skip1:
    cmp byte [rdi], ' '
    jne .hs_num1
    inc rdi
    jmp .hs_skip1
.hs_num1:
    ; Skip first number
    cmp byte [rdi], ' '
    je .hs_space1
    cmp byte [rdi], 0
    je .hs_no_rss_cleanup
    inc rdi
    jmp .hs_num1
.hs_space1:
    inc rdi                 ; skip space
    call atoll wrt ..plt
    imul rax, 4096          ; pages → bytes
    mov r15, rax
.hs_no_rss_cleanup:
    add rsp, 256
    jmp .hs_build
.hs_no_rss:

.hs_build:
    ; Build stats JSON
    ; Push stack args (right-to-left): poll_failures, poll_successes, active_conns, requests_served
    mov rax, [g_poll_failures]
    push rax
    mov rax, [g_poll_successes]
    push rax
    mov rax, [g_active_conns]
    push rax
    mov rax, [g_requests_served]
    push rax

    lea rdi, [rsp + 32]     ; buffer (4 pushes = 32 bytes)
    mov esi, 4096
    lea rdx, [fmt_stats_json]
    mov rcx, rbx            ; uptime_ms
    mov r8, r15              ; rss_bytes
    mov r9, [g_started_at]   ; started_at
    xor eax, eax
    call snprintf wrt ..plt

    add rsp, 32

    lea rdi, [rsp]
    call strdup wrt ..plt
    test rax, rax
    jz .hs_fail

    mov rdi, rax
    mov rsi, r13
    mov rdx, r14
    call build_json_response
    jmp .hs_done

.hs_fail:
    mov qword [r13], 0
    mov qword [r14], 0

.hs_done:
    add rsp, 4120
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret
