; http_client.asm — Blocking HTTP GET client for polling AirGradient devices
default rel

extern socket, connect, close, write, read
extern setsockopt, htons, inet_pton
extern snprintf, strlen, malloc, free, memset
extern fprintf, stderr
extern strchr, atoi

extern fmt_http_get
extern AF_INET, SOCK_STREAM

section .data

; Timeout: 5 seconds (struct timeval)
recv_timeout:
    dq 5                    ; tv_sec
    dq 0                    ; tv_usec

str_port_80:
    db `80`, 0

section .text

; ── http_get(ip_str, response_buf, buf_size) → bytes read or -1 ──────────────
; rdi = IP address string (e.g., "192.168.88.6" or "192.168.88.6:8080")
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
    ; Stack layout (552 bytes total; 56 pushed + 552 = 608 = 38*16, 16-byte aligned):
    ;   [rsp+0..255]   — request buffer (256 bytes)
    ;   [rsp+256..271] — sockaddr_in (16 bytes)
    ;   [rsp+272..279] — hostname_ptr (qword)
    ;   [rsp+280..287] — port_str_ptr (qword)
    ;   [rsp+288..543] — hostname copy buffer (256 bytes)
    ;   [rsp+544..551] — padding (8 bytes)
    sub rsp, 552

    mov r12, rdi            ; original ip string
    mov r13, rsi            ; response buffer
    mov r14, rdx            ; buffer size

    ; ── Parse host:port from ip string ───────────────────────────────────────
    ; strchr(ip_str, ':')
    mov rdi, r12
    mov esi, ':'
    call strchr wrt ..plt
    test rax, rax
    jz .hg_no_colon

    ; Found a colon: split into hostname copy and port string.
    ; rax = pointer to ':' in the original ip string.
    mov r15, rax            ; r15 = pointer to ':'

    ; Compute length of hostname part (colon_ptr - ip_str)
    sub r15, r12            ; r15 = offset of ':' from start of ip_str

    ; Cap hostname length at 255 bytes to stay within copy buffer
    cmp r15, 255
    jle .hg_len_ok
    mov r15, 255
.hg_len_ok:

    ; Copy hostname into [rsp+288..], null-terminate
    ; Use snprintf-style manual copy: memcpy(dst, src, len) then null-terminate
    lea rdi, [rsp + 288]    ; dst = hostname copy buffer
    mov rsi, r12            ; src = original ip string
    mov rdx, r15            ; length (no null byte)
    ; Simple byte copy loop (length is small; no need for memcpy call overhead)
.hg_copy_host:
    test rdx, rdx
    jz .hg_copy_host_done
    movzx ecx, byte [rsi]
    mov [rdi], cl
    inc rsi
    inc rdi
    dec rdx
    jmp .hg_copy_host
.hg_copy_host_done:
    mov byte [rdi], 0       ; null-terminate

    ; hostname_ptr = &hostname_copy_buf
    lea rax, [rsp + 288]
    mov [rsp + 272], rax

    ; port_str_ptr = original ':' position + 1
    ; Recompute: strchr returned pointer to ':', so port_str = that pointer + 1
    ; We saved the colon offset in r15 (after sub r15, r12 — now r15 is the length).
    ; We need the original rax from strchr. Let's get it from r12 + r15 (length saved).
    ; Actually we capped r15 at 255 but the real colon could be beyond that.
    ; Better to re-do: rerun strchr or use r12 + original_offset.
    ; We already did sub r15, r12 so r15 = offset. But we may have capped it.
    ; The port string is always at the real colon+1 in the ORIGINAL string.
    ; Recompute: scan from r12 for ':', then +1.
    ; Use strchr again (cheap; string is tiny):
    mov rdi, r12
    mov esi, ':'
    call strchr wrt ..plt
    ; rax = pointer to ':' (cannot be NULL since we found it above)
    inc rax                 ; skip ':'
    mov [rsp + 280], rax    ; port_str_ptr

    jmp .hg_parsed

.hg_no_colon:
    ; No colon: use ip string as-is for hostname, "80" as port string
    mov [rsp + 272], r12            ; hostname_ptr = ip string
    lea rax, [str_port_80]
    mov [rsp + 280], rax            ; port_str_ptr = "80"

.hg_parsed:
    ; ── Build HTTP request ────────────────────────────────────────────────────
    ; snprintf(request_buf, 255, fmt_http_get, hostname_ptr)
    lea rdi, [rsp]
    mov esi, 255
    lea rdx, [fmt_http_get]
    mov rcx, [rsp + 272]    ; hostname (without port)
    xor eax, eax
    call snprintf wrt ..plt
    mov r15d, eax           ; request length

    ; ── Create socket ─────────────────────────────────────────────────────────
    mov edi, 2              ; AF_INET
    mov esi, 1              ; SOCK_STREAM
    xor edx, edx
    call socket wrt ..plt
    test eax, eax
    js .hg_fail
    mov ebx, eax            ; fd

    ; ── Set receive timeout ───────────────────────────────────────────────────
    mov edi, ebx
    mov esi, 1              ; SOL_SOCKET
    mov edx, 20             ; SO_RCVTIMEO
    lea rcx, [recv_timeout]
    mov r8d, 16
    call setsockopt wrt ..plt

    ; ── Set send timeout ─────────────────────────────────────────────────────
    mov edi, ebx
    mov esi, 1              ; SOL_SOCKET
    mov edx, 21             ; SO_SNDTIMEO
    lea rcx, [recv_timeout]
    mov r8d, 16
    call setsockopt wrt ..plt

    ; ── Setup sockaddr_in ─────────────────────────────────────────────────────
    lea rdi, [rsp + 256]    ; sockaddr_in
    xor esi, esi
    mov edx, 16
    call memset wrt ..plt

    mov word [rsp + 256], 2  ; AF_INET

    ; Convert port string to integer and byte-swap
    mov rdi, [rsp + 280]    ; port_str_ptr
    call atoi wrt ..plt     ; eax = port number
    movzx edi, ax           ; zero-extend to 32-bit for htons
    call htons wrt ..plt
    mov [rsp + 258], ax

    ; ── inet_pton(AF_INET, hostname, &sin_addr) ───────────────────────────────
    mov edi, 2
    mov rsi, [rsp + 272]    ; hostname (without port)
    lea rdx, [rsp + 260]
    call inet_pton wrt ..plt
    cmp eax, 1
    jne .hg_close

    ; ── Connect ──────────────────────────────────────────────────────────────
    mov edi, ebx
    lea rsi, [rsp + 256]
    mov edx, 16
    call connect wrt ..plt
    test eax, eax
    jnz .hg_close

    ; ── Send request ─────────────────────────────────────────────────────────
    mov edi, ebx
    lea rsi, [rsp]
    movsx rdx, r15d
    call write wrt ..plt
    cmp rax, 0
    jle .hg_close

    ; ── Read response ─────────────────────────────────────────────────────────
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
    add rsp, 552
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret
