; poller.asm — Background pthread: poll devices, update health, insert readings
default rel

extern http_get, db_insert_reading
extern pthread_rwlock_wrlock, pthread_rwlock_unlock
extern nanosleep, clock_gettime, snprintf, strstr, sscanf, fprintf
extern strlen, strdup, free
extern stderr
extern log_ts

extern g_devices, g_device_count, g_health, g_health_lock
extern g_shutdown, g_poll_successes, g_poll_failures, g_started_at
extern g_poll_interval_ms

extern fmt_str
extern str_ag_pm01, str_ag_pm02, str_ag_pm10, str_ag_pm02comp
extern str_ag_rco2, str_ag_atmp, str_ag_atmpcomp
extern str_ag_rhum, str_ag_rhumcomp, str_ag_tvoc, str_ag_nox, str_ag_wifi
extern str_ag_serial, str_ag_model
extern str_status_ok, str_status_error
extern str_null_json
extern log_poll_success, log_poll_error

extern fmt_extract_int, fmt_extract_float

section .text

; ── get_now_ms() → milliseconds since epoch in rax ───────────────────────────
get_now_ms:
    push rbp
    mov rbp, rsp
    sub rsp, 16
    mov edi, 0              ; CLOCK_REALTIME
    mov rsi, rsp
    call clock_gettime wrt ..plt
    mov rax, [rsp]
    imul rax, 1000
    mov rdx, [rsp + 8]
    mov rcx, rdx
    shr rcx, 20             ; approx /1000000
    add rax, rcx
    add rsp, 16
    pop rbp
    ret

; ── extract_json_num(response, key, outbuf, outlen) → 0 found, -1 not ───────
; Finds "key": value in JSON and writes the number as a string to outbuf
; rdi = response, rsi = key (including quotes), rdx = outbuf, rcx = outlen
extract_json_num:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    sub rsp, 8

    mov r12, rdx            ; outbuf
    mov r13, rcx            ; outlen

    ; strstr(response, key)
    call strstr wrt ..plt
    test rax, rax
    jz .ejn_not_found

    ; Skip past key and find ':'
    mov rdi, rax
.ejn_find_colon:
    cmp byte [rdi], ':'
    je .ejn_found_colon
    cmp byte [rdi], 0
    je .ejn_not_found
    inc rdi
    jmp .ejn_find_colon

.ejn_found_colon:
    inc rdi                  ; skip ':'
    ; Skip whitespace
.ejn_skip_ws:
    cmp byte [rdi], ' '
    je .ejn_ws_next
    cmp byte [rdi], 9       ; tab
    je .ejn_ws_next
    jmp .ejn_copy_value
.ejn_ws_next:
    inc rdi
    jmp .ejn_skip_ws

.ejn_copy_value:
    ; Check for null
    cmp byte [rdi], 'n'
    je .ejn_not_found

    ; Copy number value — only accept [0-9.eE+-] to prevent injection
    xor ecx, ecx
.ejn_copy:
    cmp ecx, r13d
    jge .ejn_copy_done
    movzx eax, byte [rdi + rcx]
    cmp al, ','
    je .ejn_copy_done
    cmp al, '}'
    je .ejn_copy_done
    cmp al, ']'
    je .ejn_copy_done
    cmp al, ' '
    je .ejn_copy_done
    cmp al, 0
    je .ejn_copy_done
    ; Validate: only accept numeric characters
    cmp al, '0'
    jb .ejn_check_special
    cmp al, '9'
    jbe .ejn_valid
.ejn_check_special:
    cmp al, '.'
    je .ejn_valid
    cmp al, 'e'
    je .ejn_valid
    cmp al, 'E'
    je .ejn_valid
    cmp al, '+'
    je .ejn_valid
    cmp al, '-'
    je .ejn_valid
    jmp .ejn_copy_done          ; reject invalid character
.ejn_valid:
    mov [r12 + rcx], al
    inc ecx
    jmp .ejn_copy

.ejn_copy_done:
    mov byte [r12 + rcx], 0
    xor eax, eax
    jmp .ejn_done

.ejn_not_found:
    mov eax, -1

.ejn_done:
    add rsp, 8
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

; ── determine_device_type(response) → "indoor" or "outdoor" ptr ──────────────
; Simple heuristic: if has rco2 field → indoor, else outdoor
determine_device_type:
    push rbp
    mov rbp, rsp

    mov rdi, rdi            ; response already in rdi
    lea rsi, [str_ag_rco2]
    call strstr wrt ..plt
    test rax, rax
    jz .ddt_outdoor

    lea rax, [str_indoor]
    jmp .ddt_done
.ddt_outdoor:
    lea rax, [str_outdoor]
.ddt_done:
    pop rbp
    ret

extern str_indoor, str_outdoor

; ── extract_serial(response, outbuf, outlen) ─────────────────────────────────
; Extract "serialno":"value" from response
extract_serial:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    sub rsp, 8

    mov r12, rsi            ; outbuf
    mov r13, rdx            ; outlen

    ; Find "serialno"
    lea rsi, [str_ag_serial]
    call strstr wrt ..plt
    test rax, rax
    jz .es_not_found

    ; Find the value string
    mov rdi, rax
.es_find_colon:
    cmp byte [rdi], ':'
    je .es_got_colon
    cmp byte [rdi], 0
    je .es_not_found
    inc rdi
    jmp .es_find_colon

.es_got_colon:
    inc rdi
    ; Skip whitespace and opening quote
.es_skip_ws:
    cmp byte [rdi], ' '
    je .es_next_ws
    cmp byte [rdi], '"'
    je .es_got_quote
    jmp .es_not_found
.es_next_ws:
    inc rdi
    jmp .es_skip_ws
.es_got_quote:
    inc rdi                  ; skip opening quote
    xor ecx, ecx
.es_copy:
    cmp ecx, r13d
    jge .es_copy_done
    movzx eax, byte [rdi + rcx]
    cmp al, '"'
    je .es_copy_done
    cmp al, 0
    je .es_copy_done
    mov [r12 + rcx], al
    inc ecx
    jmp .es_copy

.es_copy_done:
    mov byte [r12 + rcx], 0
    xor eax, eax
    jmp .es_done

.es_not_found:
    ; Default to IP address (caller handles this)
    mov eax, -1

.es_done:
    add rsp, 8
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

; ── poll_device(index) — poll one device and update health/DB ────────────────
poll_device:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8600           ; response buf (4096) + SQL buf (4096) + field bufs

    mov ebx, edi            ; device index

    ; Get device IP and label
    mov eax, ebx
    shl eax, 4
    lea rcx, [g_devices]
    mov r12, [rcx + rax]    ; ip
    mov r13, [rcx + rax + 8] ; label

    ; HTTP GET
    sub rsp, 4096           ; response buffer
    mov rdi, r12             ; ip
    mov rsi, rsp             ; response buffer
    mov rdx, 4095            ; max size
    call http_get
    mov r14, rax             ; bytes read or -1

    cmp r14, -1
    je .pd_error
    cmp r14, 10             ; too short to be valid JSON
    jl .pd_error

    ; Find HTTP body (after \r\n\r\n) — check bounds to prevent OOB read
    mov rdi, rsp
    xor ecx, ecx
    sub r14d, 3              ; need 4 bytes from each scan position
    cmp r14d, 0
    jle .pd_error            ; response too short for headers
.pd_find_body:
    cmp ecx, r14d
    jge .pd_error
    cmp byte [rdi + rcx], 0x0D
    jne .pd_fb_next
    cmp byte [rdi + rcx + 1], 0x0A
    jne .pd_fb_next
    cmp byte [rdi + rcx + 2], 0x0D
    jne .pd_fb_next
    cmp byte [rdi + rcx + 3], 0x0A
    jne .pd_fb_next
    lea r15, [rdi + rcx + 4]  ; body start
    jmp .pd_got_body
.pd_fb_next:
    inc ecx
    jmp .pd_find_body

.pd_got_body:
    ; Extract fields from JSON response
    ; Each field goes into a scratch buffer; we'll use "NULL" for missing ones

    ; Use offsets into the 8600-byte scratch area (after the 4096 response buffer sub)
    ; Field buffers at [rsp + 4096 + N*32], 32 bytes each, 12 fields
    %define FIELD_BUF(n) rsp + 4096 + (n * 32)

    ; Extract each field
    mov rdi, r15
    lea rsi, [str_ag_pm01]
    lea rdx, [FIELD_BUF(0)]
    mov ecx, 31
    call extract_json_num
    test eax, eax
    jnz .pd_null_0
    jmp .pd_field_1
.pd_null_0:
    lea rdi, [FIELD_BUF(0)]
    mov dword [rdi], 'NULL'
    mov byte [rdi + 4], 0

.pd_field_1:
    mov rdi, r15
    lea rsi, [str_ag_pm02]
    lea rdx, [FIELD_BUF(1)]
    mov ecx, 31
    call extract_json_num
    test eax, eax
    jnz .pd_null_1
    jmp .pd_field_2
.pd_null_1:
    lea rdi, [FIELD_BUF(1)]
    mov dword [rdi], 'NULL'
    mov byte [rdi + 4], 0

.pd_field_2:
    mov rdi, r15
    lea rsi, [str_ag_pm10]
    lea rdx, [FIELD_BUF(2)]
    mov ecx, 31
    call extract_json_num
    test eax, eax
    jnz .pd_null_2
    jmp .pd_field_3
.pd_null_2:
    lea rdi, [FIELD_BUF(2)]
    mov dword [rdi], 'NULL'
    mov byte [rdi + 4], 0

.pd_field_3:
    mov rdi, r15
    lea rsi, [str_ag_pm02comp]
    lea rdx, [FIELD_BUF(3)]
    mov ecx, 31
    call extract_json_num
    test eax, eax
    jnz .pd_null_3
    jmp .pd_field_4
.pd_null_3:
    lea rdi, [FIELD_BUF(3)]
    mov dword [rdi], 'NULL'
    mov byte [rdi + 4], 0

.pd_field_4:
    mov rdi, r15
    lea rsi, [str_ag_rco2]
    lea rdx, [FIELD_BUF(4)]
    mov ecx, 31
    call extract_json_num
    test eax, eax
    jnz .pd_null_4
    jmp .pd_field_5
.pd_null_4:
    lea rdi, [FIELD_BUF(4)]
    mov dword [rdi], 'NULL'
    mov byte [rdi + 4], 0

.pd_field_5:
    mov rdi, r15
    lea rsi, [str_ag_atmp]
    lea rdx, [FIELD_BUF(5)]
    mov ecx, 31
    call extract_json_num
    test eax, eax
    jnz .pd_null_5
    jmp .pd_field_6
.pd_null_5:
    lea rdi, [FIELD_BUF(5)]
    mov dword [rdi], 'NULL'
    mov byte [rdi + 4], 0

.pd_field_6:
    mov rdi, r15
    lea rsi, [str_ag_atmpcomp]
    lea rdx, [FIELD_BUF(6)]
    mov ecx, 31
    call extract_json_num
    test eax, eax
    jnz .pd_null_6
    jmp .pd_field_7
.pd_null_6:
    lea rdi, [FIELD_BUF(6)]
    mov dword [rdi], 'NULL'
    mov byte [rdi + 4], 0

.pd_field_7:
    mov rdi, r15
    lea rsi, [str_ag_rhum]
    lea rdx, [FIELD_BUF(7)]
    mov ecx, 31
    call extract_json_num
    test eax, eax
    jnz .pd_null_7
    jmp .pd_field_8
.pd_null_7:
    lea rdi, [FIELD_BUF(7)]
    mov dword [rdi], 'NULL'
    mov byte [rdi + 4], 0

.pd_field_8:
    mov rdi, r15
    lea rsi, [str_ag_rhumcomp]
    lea rdx, [FIELD_BUF(8)]
    mov ecx, 31
    call extract_json_num
    test eax, eax
    jnz .pd_null_8
    jmp .pd_field_9
.pd_null_8:
    lea rdi, [FIELD_BUF(8)]
    mov dword [rdi], 'NULL'
    mov byte [rdi + 4], 0

.pd_field_9:
    mov rdi, r15
    lea rsi, [str_ag_tvoc]
    lea rdx, [FIELD_BUF(9)]
    mov ecx, 31
    call extract_json_num
    test eax, eax
    jnz .pd_null_9
    jmp .pd_field_10
.pd_null_9:
    lea rdi, [FIELD_BUF(9)]
    mov dword [rdi], 'NULL'
    mov byte [rdi + 4], 0

.pd_field_10:
    mov rdi, r15
    lea rsi, [str_ag_nox]
    lea rdx, [FIELD_BUF(10)]
    mov ecx, 31
    call extract_json_num
    test eax, eax
    jnz .pd_null_10
    jmp .pd_field_11
.pd_null_10:
    lea rdi, [FIELD_BUF(10)]
    mov dword [rdi], 'NULL'
    mov byte [rdi + 4], 0

.pd_field_11:
    mov rdi, r15
    lea rsi, [str_ag_wifi]
    lea rdx, [FIELD_BUF(11)]
    mov ecx, 31
    call extract_json_num
    test eax, eax
    jnz .pd_null_11
    jmp .pd_fields_done
.pd_null_11:
    lea rdi, [FIELD_BUF(11)]
    mov dword [rdi], 'NULL'
    mov byte [rdi + 4], 0

.pd_fields_done:
    ; Frame layout (rbp-relative, stable across push/pop):
    ;   rbp - 8640 = base of 8600-byte scratch area
    ;   FIELD_BUF(n) = rbp - 8640 + n*32
    ;   device_type ptr: rbp - 8256  (offset 384)
    ;   device_id ptr:   rbp - 8248  (offset 392)
    ;   serial buf:      rbp - 8240  (offset 400)
    ;   timestamp:       rbp - 8168  (offset 472)
    ;   SQL buffer:      rbp - 8160  (offset 480)

    ; Determine device type
    mov rdi, r15
    call determine_device_type
    mov [rbp - 8256], rax    ; device_type ptr

    ; Get serial number for device_id
    mov rdi, r15
    lea rsi, [rbp - 8240]   ; serial buf
    mov edx, 63
    call extract_serial
    test eax, eax
    jnz .pd_use_ip_as_id
    lea rax, [rbp - 8240]
    jmp .pd_got_device_id
.pd_use_ip_as_id:
    mov rax, r12             ; use IP as device_id
.pd_got_device_id:
    mov [rbp - 8248], rax    ; device_id ptr

    ; Get current timestamp
    call get_now_ms
    mov [rbp - 8168], rax

    ; Build params array for parameterized db_insert_reading at [rbp - 8160]
    ; Array layout: 16 string pointers
    ;   [0]=device_id, [1]=device_type, [2]=device_ip,
    ;   [3..14]=pm01..wifi (FIELD_BUF(0)..FIELD_BUF(11)),
    ;   [15]=raw_json

    mov rax, [rbp - 8248]            ; device_id
    mov [rbp - 8160], rax
    mov rax, [rbp - 8256]            ; device_type
    mov [rbp - 8152], rax
    mov [rbp - 8144], r12            ; device_ip

    ; Field buffers: FIELD_BUF(n) = rbp - 8640 + n*32
%assign _fi 0
%rep 12
    lea rax, [rbp - 8640 + _fi*32]
    mov [rbp - 8136 + _fi*8], rax
%assign _fi _fi+1
%endrep

    mov [rbp - 8040], r15            ; raw_json (body)

    ; Call db_insert_reading(timestamp, params_array)
    mov rdi, [rbp - 8168]            ; timestamp (int64)
    lea rsi, [rbp - 8160]            ; params array
    call db_insert_reading

    ; Update health: success
    call get_now_ms
    mov r14, rax             ; now_ms

    lea rdi, [g_health_lock]
    call pthread_rwlock_wrlock wrt ..plt

    mov eax, ebx
    imul eax, 296
    lea rdi, [g_health + rax + 128]  ; dest = status field
    mov esi, 64
    lea rdx, [fmt_str]
    lea rcx, [str_status_ok]
    xor eax, eax
    call snprintf wrt ..plt

    mov eax, ebx
    imul eax, 296
    lea rcx, [g_health + rax]
    mov [rcx + 192], r14     ; lastSuccess
    mov dword [rcx + 272], 0 ; consecutiveFailures = 0
    ; Clear error message
    mov byte [rcx + 208], 0

    lea rdi, [g_health_lock]
    call pthread_rwlock_unlock wrt ..plt

    lock inc qword [g_poll_successes]

    ; Log success
    call log_ts
    mov rdi, [rel stderr wrt ..got]
    mov rdi, [rdi]
    lea rsi, [log_poll_success]
    mov rdx, r12             ; ip
    mov rcx, r13             ; label
    xor eax, eax
    call fprintf wrt ..plt

    add rsp, 4096            ; pop response buffer
    jmp .pd_done

.pd_error:
    add rsp, 4096            ; pop response buffer

    ; Update health: error
    call get_now_ms
    mov r14, rax

    lea rdi, [g_health_lock]
    call pthread_rwlock_wrlock wrt ..plt

    mov eax, ebx
    imul eax, 296
    lea rdi, [g_health + rax + 128]  ; dest = status field
    mov esi, 64
    lea rdx, [fmt_str]
    lea rcx, [str_status_error]
    xor eax, eax
    call snprintf wrt ..plt

    mov eax, ebx
    imul eax, 296
    lea rcx, [g_health + rax]
    mov [rcx + 200], r14     ; lastError
    inc dword [rcx + 272]    ; consecutiveFailures++
    ; Set error message (bounded)
    lea rdi, [rcx + 208]
    mov esi, 64
    lea rdx, [fmt_str]
    lea rcx, [.err_conn_failed]
    xor eax, eax
    call snprintf wrt ..plt

    lea rdi, [g_health_lock]
    call pthread_rwlock_unlock wrt ..plt

    lock inc qword [g_poll_failures]

    ; Log error
    call log_ts
    mov rdi, [rel stderr wrt ..got]
    mov rdi, [rdi]
    lea rsi, [log_poll_error]
    mov rdx, r12             ; ip
    mov rcx, r13             ; label
    lea r8, [.err_conn_failed]
    xor eax, eax
    call fprintf wrt ..plt

.pd_done:
    add rsp, 8600
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

section .rodata
.err_conn_failed: db `connection failed`, 0

section .text

; ── poller_thread_main(arg) — thread entry point ─────────────────────────────
global poller_thread_main
poller_thread_main:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    sub rsp, 32

.ptm_loop:
    cmp qword [g_shutdown], 0
    jne .ptm_exit

    ; Poll each device
    mov r12d, [g_device_count]
    xor ebx, ebx

.ptm_device_loop:
    cmp ebx, r12d
    jge .ptm_sleep

    cmp qword [g_shutdown], 0
    jne .ptm_exit

    mov edi, ebx
    call poll_device

    inc ebx
    jmp .ptm_device_loop

.ptm_sleep:
    ; Sleep for poll interval
    ; Convert milliseconds to timespec {seconds, nanoseconds}
    mov eax, [g_poll_interval_ms]
    xor edx, edx
    mov ecx, 1000
    div ecx                  ; eax = seconds, edx = remaining ms
    mov [rsp], rax           ; tv_sec (zero-extended)
    imul edx, 1000000        ; ms to ns
    mov [rsp + 8], rdx       ; tv_nsec

    lea rdi, [rsp]
    xor esi, esi             ; remaining = NULL
    call nanosleep wrt ..plt

    jmp .ptm_loop

.ptm_exit:
    xor eax, eax
    add rsp, 32
    pop r12
    pop rbx
    pop rbp
    ret
