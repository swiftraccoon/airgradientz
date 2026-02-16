; http_client.asm — Blocking HTTP GET client for polling AirGradient devices
default rel

extern socket, connect, close, write, read
extern setsockopt, htons, inet_pton
extern snprintf, strlen, malloc, free, memset
extern fprintf, stderr

extern fmt_http_get
extern AF_INET, SOCK_STREAM

section .data

; Timeout: 5 seconds (struct timeval)
recv_timeout:
    dq 5                    ; tv_sec
    dq 0                    ; tv_usec

section .text

; ── http_get(ip_str, response_buf, buf_size) → bytes read or -1 ──────────────
; rdi = IP address string (e.g., "192.168.88.6")
; rsi = response buffer
; rdx = buffer size
; Returns bytes read in rax, or -1 on error
global http_get
http_get:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 280           ; request buffer (256) + sockaddr_in (16) + padding

    mov r12, rdi            ; ip string
    mov r13, rsi            ; response buffer
    mov r14, rdx            ; buffer size

    ; Build HTTP request
    lea rdi, [rsp]
    mov esi, 255
    lea rdx, [fmt_http_get]
    mov rcx, r12            ; host = ip
    xor eax, eax
    call snprintf wrt ..plt
    mov r15d, eax           ; request length

    ; Create socket
    mov edi, 2              ; AF_INET
    mov esi, 1              ; SOCK_STREAM
    xor edx, edx
    call socket wrt ..plt
    test eax, eax
    js .hg_fail
    mov ebx, eax            ; fd

    ; Set receive timeout
    mov edi, ebx
    mov esi, 1              ; SOL_SOCKET
    mov edx, 20             ; SO_RCVTIMEO
    lea rcx, [recv_timeout]
    mov r8d, 16
    call setsockopt wrt ..plt

    ; Set send timeout
    mov edi, ebx
    mov esi, 1              ; SOL_SOCKET
    mov edx, 21             ; SO_SNDTIMEO
    lea rcx, [recv_timeout]
    mov r8d, 16
    call setsockopt wrt ..plt

    ; Setup sockaddr_in
    lea rdi, [rsp + 256]    ; sockaddr_in
    xor esi, esi
    mov edx, 16
    call memset wrt ..plt

    mov word [rsp + 256], 2  ; AF_INET
    mov edi, 80              ; port
    call htons wrt ..plt
    mov [rsp + 258], ax

    ; inet_pton(AF_INET, ip, &sin_addr)
    mov edi, 2
    mov rsi, r12
    lea rdx, [rsp + 260]
    call inet_pton wrt ..plt
    cmp eax, 1
    jne .hg_close

    ; Connect
    mov edi, ebx
    lea rsi, [rsp + 256]
    mov edx, 16
    call connect wrt ..plt
    test eax, eax
    jnz .hg_close

    ; Send request
    mov edi, ebx
    lea rsi, [rsp]
    movsx rdx, r15d
    call write wrt ..plt
    cmp rax, 0
    jle .hg_close

    ; Read response
    xor r15d, r15d          ; total bytes read
.hg_read_loop:
    mov edi, ebx
    lea rsi, [r13 + r15]
    mov rdx, r14
    sub rdx, r15
    cmp rdx, 0
    jle .hg_read_done
    call read wrt ..plt
    cmp rax, 0
    jle .hg_read_done
    add r15, rax
    jmp .hg_read_loop

.hg_read_done:
    ; Null-terminate
    cmp r15, r14
    jge .hg_truncate
    mov byte [r13 + r15], 0
    jmp .hg_close_ok
.hg_truncate:
    dec r15
    mov byte [r13 + r15], 0

.hg_close_ok:
    mov edi, ebx
    call close wrt ..plt
    mov rax, r15
    jmp .hg_done

.hg_close:
    mov edi, ebx
    call close wrt ..plt

.hg_fail:
    mov rax, -1

.hg_done:
    add rsp, 280
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret
