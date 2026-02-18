; router.asm — HTTP request line parsing, path routing, dispatch
default rel

extern strcmp, strlen, strncmp, snprintf, malloc, memcpy, free
extern url_decode

extern handle_readings, handle_readings_count, handle_readings_latest
extern handle_devices, handle_health, handle_config, handle_stats
extern serve_static_file

extern route_api_readings_latest, route_api_readings_count, route_api_readings
extern route_api_devices, route_api_health, route_api_config, route_api_stats
extern str_get, str_slash

extern http_405_response, http_405_response_len
extern http_404_response, http_404_response_len
extern http_500_response, http_500_response_len

section .text

; ── route_request(buf, &resp_ptr, &resp_len) ─────────────────────────────────
; rdi = request buffer, rsi = &resp_ptr, rdx = &resp_len
global route_request
route_request:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 1048           ; path(512) + query(512) + 24 padding for alignment

    mov r12, rdi            ; request buffer
    mov r13, rsi            ; &resp_ptr
    mov r14, rdx            ; &resp_len

    ; Parse: "METHOD PATH HTTP/x.x\r\n"
    ; Find first space → end of method
    xor ecx, ecx
.find_space1:
    cmp byte [r12 + rcx], ' '
    je .got_method
    cmp byte [r12 + rcx], 0
    je .bad_request
    inc ecx
    cmp ecx, 16
    jge .bad_request
    jmp .find_space1

.got_method:
    mov ebx, ecx            ; save method_len in callee-saved reg
    mov byte [r12 + rbx], 0 ; null-terminate method
    lea r15, [r12 + rbx + 1] ; start of path

    ; Check method is GET
    mov rdi, r12
    lea rsi, [str_get]
    call strcmp wrt ..plt
    mov byte [r12 + rbx], ' ' ; restore space
    test eax, eax
    jnz .method_not_allowed

    ; Find end of path
    mov rdi, r15
    xor ecx, ecx
.find_space2:
    movzx eax, byte [rdi + rcx]
    cmp al, ' '
    je .got_path_end
    cmp al, 0
    je .got_path_end
    cmp al, 0x0D
    je .got_path_end
    inc ecx
    cmp ecx, 511
    jge .bad_request
    jmp .find_space2

.got_path_end:
    mov ebx, ecx            ; save path_len in rbx (callee-saved)
    ; Copy path to stack buffer
    lea rdi, [rsp]
    mov rsi, r15
    mov edx, ebx
    call memcpy wrt ..plt
    mov byte [rsp + rbx], 0 ; null-terminate (rbx survives call)

    ; URL-decode the path
    lea rdi, [rsp]
    lea rsi, [rsp]
    mov edx, 511
    call url_decode

    ; Split path and query at '?'
    mov byte [rsp + 512], 0  ; default empty query
    lea rdi, [rsp]
    xor ecx, ecx
.find_query:
    cmp byte [rdi + rcx], 0
    je .do_route
    cmp byte [rdi + rcx], '?'
    je .got_query
    inc ecx
    jmp .find_query

.got_query:
    mov byte [rdi + rcx], 0  ; terminate path at '?'
    lea rsi, [rdi + rcx + 1]
    lea rdi, [rsp + 512]
    mov edx, 511
    call copy_str

    ; ── Route to handler ─────────────────────────────────────────────────────

.do_route:
    ; /api/readings/latest (exact match, check first — longer path)
    lea rdi, [rsp]
    lea rsi, [route_api_readings_latest]
    call strcmp wrt ..plt
    test eax, eax
    jz .route_readings_latest

    ; /api/readings/count (exact match, check before prefix /api/readings)
    lea rdi, [rsp]
    lea rsi, [route_api_readings_count]
    call strcmp wrt ..plt
    test eax, eax
    jz .route_readings_count

    ; /api/readings (prefix match)
    lea rdi, [rsp]
    lea rsi, [route_api_readings]
    mov edx, 13
    call strncmp wrt ..plt
    test eax, eax
    jz .route_readings

    ; /api/devices
    lea rdi, [rsp]
    lea rsi, [route_api_devices]
    call strcmp wrt ..plt
    test eax, eax
    jz .route_devices

    ; /api/health
    lea rdi, [rsp]
    lea rsi, [route_api_health]
    call strcmp wrt ..plt
    test eax, eax
    jz .route_health

    ; /api/config
    lea rdi, [rsp]
    lea rsi, [route_api_config]
    call strcmp wrt ..plt
    test eax, eax
    jz .route_config

    ; /api/stats
    lea rdi, [rsp]
    lea rsi, [route_api_stats]
    call strcmp wrt ..plt
    test eax, eax
    jz .route_stats

    ; Static file
    lea rdi, [rsp]
    mov rsi, r13
    mov rdx, r14
    call serve_static_file
    jmp .rr_done

.route_readings_latest:
    lea rdi, [rsp + 512]
    mov rsi, r13
    mov rdx, r14
    call handle_readings_latest
    jmp .rr_done

.route_readings_count:
    lea rdi, [rsp + 512]
    mov rsi, r13
    mov rdx, r14
    call handle_readings_count
    jmp .rr_done

.route_readings:
    lea rdi, [rsp + 512]
    mov rsi, r13
    mov rdx, r14
    call handle_readings
    jmp .rr_done

.route_devices:
    lea rdi, [rsp + 512]
    mov rsi, r13
    mov rdx, r14
    call handle_devices
    jmp .rr_done

.route_health:
    lea rdi, [rsp + 512]
    mov rsi, r13
    mov rdx, r14
    call handle_health
    jmp .rr_done

.route_config:
    lea rdi, [rsp + 512]
    mov rsi, r13
    mov rdx, r14
    call handle_config
    jmp .rr_done

.route_stats:
    lea rdi, [rsp + 512]
    mov rsi, r13
    mov rdx, r14
    call handle_stats
    jmp .rr_done

.method_not_allowed:
    lea rbx, [http_405_response]
    mov r15, http_405_response_len
    jmp .set_static_response

.bad_request:
    lea rbx, [http_404_response]
    mov r15, http_404_response_len

.set_static_response:
    ; Allocate and copy static response
    lea rdi, [r15 + 1]
    call malloc wrt ..plt
    test rax, rax
    jz .rr_done
    mov rdi, rax
    mov rsi, rbx
    mov rdx, r15
    mov [r13], rax           ; save resp_ptr before memcpy
    mov [r14], r15           ; save resp_len
    call memcpy wrt ..plt

.rr_done:
    ; If handler returned NULL response, send 500
    cmp qword [r13], 0
    jne .rr_exit
    mov rdi, http_500_response_len + 1
    call malloc wrt ..plt
    test rax, rax
    jz .rr_exit
    mov [r13], rax
    mov rdi, rax
    lea rsi, [http_500_response]
    mov rdx, http_500_response_len
    call memcpy wrt ..plt
    mov qword [r14], http_500_response_len

.rr_exit:
    add rsp, 1048
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

; ── copy_str(dst, src, maxlen) ───────────────────────────────────────────────
copy_str:
    xor ecx, ecx
.cs_loop:
    cmp ecx, edx
    jge .cs_done
    movzx eax, byte [rsi + rcx]
    mov [rdi + rcx], al
    test al, al
    jz .cs_ret
    inc ecx
    jmp .cs_loop
.cs_done:
    mov byte [rdi + rcx], 0
.cs_ret:
    ret
