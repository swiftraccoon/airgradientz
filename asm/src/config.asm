; config.asm — Config file loading and env var overrides
; Priority: env vars > airgradientz.json (mandatory)
default rel

extern getenv, fopen, fread, fclose, strstr, sscanf, atoi, atoll, strlen
extern malloc, free, strdup, snprintf, fprintf, memset, strchr
extern stderr, exit

extern str_port_env, str_db_path_env, str_config_path_env
extern str_default_db, str_config_local, str_config_parent
extern str_key_poll_interval, str_key_fetch_timeout, str_key_max_api_rows
extern str_key_downsample_buckets
extern str_key_devices, str_key_ip, str_key_label, str_key_ports, str_key_asm
extern log_config_loaded, log_ts
extern MAX_DEVICES, MAX_CONFIG_SIZE

; ── Exported globals ─────────────────────────────────────────────────────────

section .bss

global g_port
g_port: resd 1

global g_poll_interval_ms
g_poll_interval_ms: resd 1

global g_fetch_timeout_ms
g_fetch_timeout_ms: resd 1

global g_max_api_rows
g_max_api_rows: resd 1

global g_ds_bucket_count
g_ds_bucket_count: resd 1

global g_ds_bucket_keys
g_ds_bucket_keys: resq 16     ; up to 16 string pointers

global g_ds_bucket_values
g_ds_bucket_values: resq 16   ; up to 16 i64 values

global g_db_path
g_db_path: resq 1

global g_device_count
g_device_count: resd 1

; Device array: ip_ptr (8) + label_ptr (8) = 16 bytes each
global g_devices
g_devices: resb 16 * 8    ; MAX_DEVICES=8

section .rodata

fatal_no_config:
    db `fatal: config file not found`, 10, 0

fatal_missing_keys:
    db `fatal: missing required config keys:`, 0

fatal_key_port:
    db ` port`, 0

fatal_key_poll_interval:
    db ` pollIntervalMs`, 0

fatal_key_fetch_timeout:
    db ` fetchTimeoutMs`, 0

fatal_key_max_api_rows:
    db ` maxApiRows`, 0

fatal_key_downsample_buckets:
    db ` downsampleBuckets`, 0

fatal_key_devices:
    db ` devices`, 0

fatal_newline:
    db 10, 0

section .data

str_rb: db "rb", 0
fmt_sscanf_int: db "%*[^:]:%d", 0
fmt_sscanf_str: db `%*[^\\"]\\"%63[^\\"]"`, 0

section .text

; ── extract_int(buf, key) → int value or -1 ──────────────────────────────────
; Find key in buf, extract integer value after the colon
; rdi = buf, rsi = key
extract_int:
    push rbp
    mov rbp, rsp
    push rbx
    push r12

    mov r12, rdi            ; save buf

    ; strstr(buf, key)
    call strstr wrt ..plt
    test rax, rax
    jz .not_found

    ; Move past the key to find the colon and value
    mov rdi, rax
    lea rsi, [fmt_sscanf_int]
    sub rsp, 16
    lea rdx, [rsp]
    mov dword [rsp], -1
    call sscanf wrt ..plt
    mov eax, [rsp]
    add rsp, 16
    jmp .done

.not_found:
    mov eax, -1

.done:
    pop r12
    pop rbx
    pop rbp
    ret

; ── extract_string(buf, key, outbuf, outlen) → 0 ok, -1 not found ───────────
; rdi = buf, rsi = key, rdx = outbuf, rcx = outlen
extract_string:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14

    mov r12, rdx            ; outbuf
    mov r13, rcx            ; outlen

    ; strstr(buf, key)
    call strstr wrt ..plt
    test rax, rax
    jz .es_not_found

    ; Find the value string: skip to next quote after colon
    ; Find ':' first
    mov rdi, rax
    mov sil, ':'
    call strchr wrt ..plt
    test rax, rax
    jz .es_not_found

    ; Now find opening quote
    mov rdi, rax
    mov sil, '"'
    call strchr wrt ..plt
    test rax, rax
    jz .es_not_found

    ; rax points to opening quote, skip it
    inc rax
    ; Copy until closing quote
    mov rdi, r12
    xor ecx, ecx
.copy_loop:
    cmp ecx, r13d
    jge .copy_done
    mov dl, [rax + rcx]
    cmp dl, '"'
    je .copy_done
    cmp dl, 0
    je .copy_done
    mov [rdi + rcx], dl
    inc ecx
    jmp .copy_loop
.copy_done:
    mov byte [rdi + rcx], 0
    xor eax, eax
    jmp .es_done

.es_not_found:
    mov eax, -1

.es_done:
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

; ── parse_devices(buf) — parse devices array from JSON ───────────────────────
; Parses {"devices":[{"ip":"...","label":"..."},...]
; rdi = buf
global parse_devices
parse_devices:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 136            ; 64 for ip + 64 for label + 8 padding

    mov r12, rdi            ; buf

    ; Find "devices"
    lea rsi, [str_key_devices]
    call strstr wrt ..plt
    test rax, rax
    jz .pd_done

    ; Find the opening '['
    mov rdi, rax
    mov sil, '['
    call strchr wrt ..plt
    test rax, rax
    jz .pd_done

    mov r13, rax            ; current position in devices array
    xor r14d, r14d          ; device count

.pd_device_loop:
    cmp r14d, 8             ; MAX_DEVICES
    jge .pd_done

    ; Find next '{'
    mov rdi, r13
    mov sil, '{'
    call strchr wrt ..plt
    test rax, rax
    jz .pd_done
    mov r13, rax

    ; Find matching '}'
    mov rdi, r13
    mov sil, '}'
    call strchr wrt ..plt
    test rax, rax
    jz .pd_done
    mov r15, rax            ; end of this device object

    ; Extract ip from this device object
    mov rdi, r13
    lea rsi, [str_key_ip]
    lea rdx, [rsp]          ; ip buffer
    mov ecx, 63
    call extract_string
    test eax, eax
    jnz .pd_next

    ; Extract label
    mov rdi, r13
    lea rsi, [str_key_label]
    lea rdx, [rsp + 64]     ; label buffer
    mov ecx, 63
    call extract_string
    test eax, eax
    jnz .pd_next

    ; strdup both strings
    lea rdi, [rsp]
    call strdup wrt ..plt
    test rax, rax
    jz .pd_next
    mov rbx, rax            ; ip_ptr

    lea rdi, [rsp + 64]
    call strdup wrt ..plt
    test rax, rax
    jz .pd_next

    ; Store in g_devices[r14d]
    mov r8d, r14d
    shl r8, 4               ; * 16
    lea r9, [g_devices]
    mov [r9 + r8], rbx      ; ip pointer
    mov [r9 + r8 + 8], rax  ; label pointer
    inc r14d

.pd_next:
    lea r13, [r15 + 1]      ; move past '}'
    jmp .pd_device_loop

.pd_done:
    mov [g_device_count], r14d

    add rsp, 136
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

; ── load_config() — main config entry point ──────────────────────────────────
global load_config
load_config:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8200           ; config file buffer + scratch

    ; Zero all config values (no hardcoded defaults — config file is mandatory)
    mov dword [g_port], 0
    mov dword [g_poll_interval_ms], 0
    mov dword [g_fetch_timeout_ms], 0
    mov dword [g_max_api_rows], 0
    mov dword [g_ds_bucket_count], 0
    mov dword [g_device_count], 0

    ; Default DB path
    lea rdi, [str_default_db]
    call strdup wrt ..plt
    mov [g_db_path], rax

    ; Try to load config file
    ; Check CONFIG_PATH env first
    lea rdi, [str_config_path_env]
    call getenv wrt ..plt
    test rax, rax
    jnz .try_config_file

    ; Try ./airgradientz.json
    lea rax, [str_config_local]
.try_config_file:
    mov r12, rax            ; config path to try
    mov rdi, rax
    lea rsi, [str_rb]
    call fopen wrt ..plt
    test rax, rax
    jnz .config_opened

    ; Try ../airgradientz.json
    lea rdi, [str_config_parent]
    lea rsi, [str_rb]
    call fopen wrt ..plt
    test rax, rax
    jz .no_config_file

.config_opened:
    mov r12, rax            ; FILE*
    lea rdi, [rsp]          ; buffer
    mov esi, 1              ; size
    mov edx, 8191           ; nmemb
    mov rcx, r12            ; stream
    call fread wrt ..plt
    mov r13, rax            ; bytes read
    mov byte [rsp + r13], 0 ; null terminate

    mov rdi, r12
    call fclose wrt ..plt

    ; Parse config values from buffer
    lea rdi, [rsp]
    lea rsi, [str_key_poll_interval]
    call extract_int
    cmp eax, -1
    je .skip_poll
    mov [g_poll_interval_ms], eax
.skip_poll:

    lea rdi, [rsp]
    lea rsi, [str_key_fetch_timeout]
    call extract_int
    cmp eax, -1
    je .skip_timeout
    mov [g_fetch_timeout_ms], eax
.skip_timeout:

    lea rdi, [rsp]
    lea rsi, [str_key_max_api_rows]
    call extract_int
    cmp eax, -1
    je .skip_max_rows
    mov [g_max_api_rows], eax
.skip_max_rows:

    ; Parse downsampleBuckets: {"5m": 300000, "10m": 600000, ...}
    lea rdi, [rsp]
    lea rsi, [str_key_downsample_buckets]
    call strstr wrt ..plt
    test rax, rax
    jz .skip_ds_buckets

    ; Find the opening '{' after "downsampleBuckets"
    mov rdi, rax
    mov sil, '{'
    call strchr wrt ..plt
    test rax, rax
    jz .skip_ds_buckets

    mov r12, rax                ; r12 = cursor inside the buckets object
    xor r14d, r14d              ; r14d = bucket count

.dsb_loop:
    cmp r14d, 16
    jge .dsb_done

    ; Find next '"' (start of key)
    mov rdi, r12
    mov sil, '"'
    call strchr wrt ..plt
    test rax, rax
    jz .dsb_done

    ; Check if we've gone past the closing '}'
    ; First find '}' from current position
    push rax                    ; save key quote ptr
    mov rdi, r12
    mov sil, '}'
    call strchr wrt ..plt
    mov rbx, rax                ; rbx = closing brace ptr (or NULL)
    pop rax                     ; restore key quote ptr
    test rbx, rbx
    jz .dsb_done
    cmp rax, rbx                ; if quote is past closing brace, we're done
    jge .dsb_done

    ; rax points to opening quote of key — skip it
    inc rax
    mov r12, rax                ; cursor = start of key text

    ; Find closing quote for key
    mov rdi, r12
    mov sil, '"'
    call strchr wrt ..plt
    test rax, rax
    jz .dsb_done

    ; Null-terminate the key temporarily for strdup
    mov byte [rax], 0
    mov rbx, rax                ; save position of closing quote

    ; strdup the key
    mov rdi, r12
    call strdup wrt ..plt
    test rax, rax
    jz .dsb_next_restore

    ; Store key pointer
    mov ecx, r14d
    mov [g_ds_bucket_keys + rcx*8], rax

    ; Restore the quote character
    mov byte [rbx], '"'

    ; Move cursor past the closing quote
    lea r12, [rbx + 1]

    ; Find ':' after the key
    mov rdi, r12
    mov sil, ':'
    call strchr wrt ..plt
    test rax, rax
    jz .dsb_done

    ; Skip ':' and any whitespace to find the number
    inc rax
.dsb_skip_ws:
    cmp byte [rax], ' '
    je .dsb_ws_next
    cmp byte [rax], 9           ; tab
    je .dsb_ws_next
    cmp byte [rax], 10          ; newline
    je .dsb_ws_next
    cmp byte [rax], 13          ; carriage return
    je .dsb_ws_next
    jmp .dsb_parse_val
.dsb_ws_next:
    inc rax
    jmp .dsb_skip_ws

.dsb_parse_val:
    mov r12, rax                ; cursor at start of number
    ; Use atoll to parse the integer value
    mov rdi, rax
    call atoll wrt ..plt

    ; Store value
    mov ecx, r14d
    mov [g_ds_bucket_values + rcx*8], rax

    ; Advance cursor past the number (find next ',' or '}')
    mov rdi, r12
    mov sil, ','
    call strchr wrt ..plt
    test rax, rax
    jz .dsb_try_close
    lea r12, [rax + 1]          ; move past ','
    inc r14d
    jmp .dsb_loop

.dsb_try_close:
    ; No more commas — find closing '}'
    mov rdi, r12
    mov sil, '}'
    call strchr wrt ..plt
    test rax, rax
    jz .dsb_done_inc
    lea r12, [rax + 1]
.dsb_done_inc:
    inc r14d
    jmp .dsb_done

.dsb_next_restore:
    ; strdup failed — restore quote and skip this entry
    mov byte [rbx], '"'
    lea r12, [rbx + 1]
    jmp .dsb_loop

.dsb_done:
    mov [g_ds_bucket_count], r14d

.skip_ds_buckets:

    ; Parse port from ports.asm key
    lea rdi, [rsp]
    lea rsi, [str_key_asm]
    call strstr wrt ..plt
    test rax, rax
    jz .skip_port_json
    ; Check it's inside "ports" section
    lea rdi, [rsp]
    lea rsi, [str_key_ports]
    call strstr wrt ..plt
    test rax, rax
    jz .skip_port_json
    ; Extract port value after "asm":
    mov rdi, rax            ; from "ports" onward
    lea rsi, [str_key_asm]
    call extract_int
    cmp eax, -1
    je .skip_port_json
    mov [g_port], eax
.skip_port_json:

    ; Parse devices
    lea rdi, [rsp]
    call parse_devices

    jmp .config_parsed

.no_config_file:
    ; Config file is mandatory — fatal error if not found
    call log_ts
    mov rdi, [rel stderr wrt ..got]
    mov rdi, [rdi]
    lea rsi, [fatal_no_config]
    xor eax, eax
    call fprintf wrt ..plt
    mov edi, 1
    call exit wrt ..plt

.config_parsed:
    ; ── Env var overrides ────────────────────────────────────────────────────

    ; PORT
    lea rdi, [str_port_env]
    call getenv wrt ..plt
    test rax, rax
    jz .no_port_env
    mov rdi, rax
    call atoi wrt ..plt
    test eax, eax
    jz .no_port_env
    mov [g_port], eax
.no_port_env:

    ; DB_PATH
    lea rdi, [str_db_path_env]
    call getenv wrt ..plt
    test rax, rax
    jz .no_db_env
    mov rdi, rax
    call strdup wrt ..plt
    ; Free old
    mov rdi, [g_db_path]
    push rax
    call free wrt ..plt
    pop rax
    mov [g_db_path], rax
.no_db_env:

    ; ── Validate required config values ───────────────────────────────────────
    ; Use r14 as a flag: 0 = all ok, 1 = at least one missing
    xor r14d, r14d

    ; Get stderr once for all validation prints
    mov r15, [rel stderr wrt ..got]
    mov r15, [r15]

    cmp dword [g_port], 0
    jg .valid_port
    ; First missing key — print the header
    test r14d, r14d
    jnz .skip_hdr_port
    mov rdi, r15
    lea rsi, [fatal_missing_keys]
    xor eax, eax
    call fprintf wrt ..plt
.skip_hdr_port:
    mov r14d, 1
    mov rdi, r15
    lea rsi, [fatal_key_port]
    xor eax, eax
    call fprintf wrt ..plt
.valid_port:

    cmp dword [g_poll_interval_ms], 0
    jg .valid_poll
    test r14d, r14d
    jnz .skip_hdr_poll
    mov rdi, r15
    lea rsi, [fatal_missing_keys]
    xor eax, eax
    call fprintf wrt ..plt
.skip_hdr_poll:
    mov r14d, 1
    mov rdi, r15
    lea rsi, [fatal_key_poll_interval]
    xor eax, eax
    call fprintf wrt ..plt
.valid_poll:

    cmp dword [g_fetch_timeout_ms], 0
    jg .valid_timeout
    test r14d, r14d
    jnz .skip_hdr_timeout
    mov rdi, r15
    lea rsi, [fatal_missing_keys]
    xor eax, eax
    call fprintf wrt ..plt
.skip_hdr_timeout:
    mov r14d, 1
    mov rdi, r15
    lea rsi, [fatal_key_fetch_timeout]
    xor eax, eax
    call fprintf wrt ..plt
.valid_timeout:

    cmp dword [g_max_api_rows], 0
    jg .valid_max_rows
    test r14d, r14d
    jnz .skip_hdr_max_rows
    mov rdi, r15
    lea rsi, [fatal_missing_keys]
    xor eax, eax
    call fprintf wrt ..plt
.skip_hdr_max_rows:
    mov r14d, 1
    mov rdi, r15
    lea rsi, [fatal_key_max_api_rows]
    xor eax, eax
    call fprintf wrt ..plt
.valid_max_rows:

    cmp dword [g_ds_bucket_count], 0
    jg .valid_ds_buckets
    test r14d, r14d
    jnz .skip_hdr_ds
    mov rdi, r15
    lea rsi, [fatal_missing_keys]
    xor eax, eax
    call fprintf wrt ..plt
.skip_hdr_ds:
    mov r14d, 1
    mov rdi, r15
    lea rsi, [fatal_key_downsample_buckets]
    xor eax, eax
    call fprintf wrt ..plt
.valid_ds_buckets:

    cmp dword [g_device_count], 0
    jg .valid_devices
    test r14d, r14d
    jnz .skip_hdr_dev
    mov rdi, r15
    lea rsi, [fatal_missing_keys]
    xor eax, eax
    call fprintf wrt ..plt
.skip_hdr_dev:
    mov r14d, 1
    mov rdi, r15
    lea rsi, [fatal_key_devices]
    xor eax, eax
    call fprintf wrt ..plt
.valid_devices:

    ; If any validation failed, print newline and exit
    test r14d, r14d
    jz .validation_ok
    mov rdi, r15
    lea rsi, [fatal_newline]
    xor eax, eax
    call fprintf wrt ..plt
    mov edi, 1
    call exit wrt ..plt

.validation_ok:

    ; Log config
    call log_ts
    mov rdi, [rel stderr wrt ..got]
    mov rdi, [rdi]
    lea rsi, [log_config_loaded]
    mov edx, [g_device_count]
    mov ecx, [g_poll_interval_ms]
    mov r8d, [g_fetch_timeout_ms]
    mov r9d, [g_max_api_rows]
    xor eax, eax
    call fprintf wrt ..plt

    add rsp, 8200
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret
