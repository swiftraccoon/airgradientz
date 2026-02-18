; config.asm — Config file loading and env var overrides
; Priority: env vars > airgradientz.json > hardcoded defaults
default rel

extern getenv, fopen, fread, fclose, strstr, sscanf, atoi, strlen
extern malloc, free, strdup, snprintf, fprintf, memset, strchr
extern stderr

extern str_port_env, str_db_path_env, str_config_path_env
extern str_default_db, str_config_local, str_config_parent
extern str_key_poll_interval, str_key_fetch_timeout, str_key_max_api_rows
extern str_key_downsample_threshold
extern str_key_devices, str_key_ip, str_key_label, str_key_ports, str_key_asm
extern log_config_loaded
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

global g_downsample_threshold
g_downsample_threshold: resd 1

global g_db_path
g_db_path: resq 1

global g_device_count
g_device_count: resd 1

; Device array: ip_ptr (8) + label_ptr (8) = 16 bytes each
global g_devices
g_devices: resb 16 * 8    ; MAX_DEVICES=8

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

    ; Set defaults
    mov dword [g_port], 3018
    mov dword [g_poll_interval_ms], 15000
    mov dword [g_fetch_timeout_ms], 5000
    mov dword [g_max_api_rows], 10000
    mov dword [g_downsample_threshold], 10000
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

    lea rdi, [rsp]
    lea rsi, [str_key_downsample_threshold]
    call extract_int
    cmp eax, -1
    je .skip_ds_threshold
    mov [g_downsample_threshold], eax
.skip_ds_threshold:

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

.no_config_file:
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

    ; Log config
    lea rdi, [log_config_loaded]
    mov esi, [g_device_count]
    mov edx, [g_poll_interval_ms]
    mov ecx, [g_fetch_timeout_ms]
    mov r8d, [g_max_api_rows]
    xor eax, eax
    mov r9, [rel stderr wrt ..got]
    mov r9, [r9]
    push r9
    push r8
    mov rdi, r9
    lea rsi, [log_config_loaded]
    mov edx, [g_device_count]
    mov ecx, [g_poll_interval_ms]
    mov r8d, [g_fetch_timeout_ms]
    mov r9d, [g_max_api_rows]
    xor eax, eax
    call fprintf wrt ..plt
    add rsp, 16

    add rsp, 8200
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret
