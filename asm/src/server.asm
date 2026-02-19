; server.asm — Socket setup, epoll event loop, connection state machine
default rel

extern socket, bind, listen, accept, close, read, write
extern setsockopt, fcntl, epoll_create1, epoll_ctl, epoll_wait
extern htons, htonl, memset, fprintf, free, clock_gettime, time
extern __errno_location
extern stderr

extern route_request
extern log_ts

extern g_port, g_shutdown, g_requests_served, g_active_conns
extern log_server_start

; Connection entry layout: 8240 bytes per fd
CONN_ENTRY_SIZE equ 8240
CONN_PHASE      equ 0
CONN_READ_POS   equ 4
CONN_READ_BUF   equ 8
CONN_RESP_PTR   equ 8200
CONN_RESP_LEN   equ 8208
CONN_WRITE_POS  equ 8216
CONN_LAST_ACT   equ 8224

section .bss

conn_table: resb CONN_ENTRY_SIZE * 4096
listen_fd:  resd 1
epoll_fd:   resd 1

section .text

; ── set_nonblocking(fd) ──────────────────────────────────────────────────────
set_nonblocking:
    push rbp
    mov rbp, rsp
    push rbx
    sub rsp, 8
    mov ebx, edi
    mov edi, ebx
    mov esi, 3              ; F_GETFL
    xor edx, edx
    call fcntl wrt ..plt
    or eax, 2048            ; O_NONBLOCK
    mov edi, ebx
    mov esi, 4              ; F_SETFL
    mov edx, eax
    call fcntl wrt ..plt
    add rsp, 8
    pop rbx
    pop rbp
    ret

; ── get_conn(fd) → pointer to conn entry ─────────────────────────────────────
get_conn:
    mov eax, edi
    imul rax, CONN_ENTRY_SIZE
    lea rax, [conn_table + rax]
    ret

; ── close_conn(fd) — cleanup and close ───────────────────────────────────────
close_conn:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    mov ebx, edi

    mov edi, [epoll_fd]
    mov esi, 2              ; EPOLL_CTL_DEL
    mov edx, ebx
    xor ecx, ecx
    call epoll_ctl wrt ..plt

    mov edi, ebx
    call get_conn
    mov r12, rax

    mov rdi, [r12 + CONN_RESP_PTR]
    test rdi, rdi
    jz .cc_no_free
    call free wrt ..plt
.cc_no_free:
    mov dword [r12 + CONN_PHASE], 0
    mov dword [r12 + CONN_READ_POS], 0
    mov qword [r12 + CONN_RESP_PTR], 0
    mov qword [r12 + CONN_RESP_LEN], 0
    mov qword [r12 + CONN_WRITE_POS], 0

    lock dec qword [g_active_conns]

    mov edi, ebx
    call close wrt ..plt

    pop r12
    pop rbx
    pop rbp
    ret

; ── get_epoch_time() → seconds in rax ────────────────────────────────────────
get_epoch_time:
    push rbp
    mov rbp, rsp
    sub rsp, 16
    xor edi, edi
    call time wrt ..plt
    add rsp, 16
    pop rbp
    ret

; ── srv_handle_read — handle EPOLLIN on client fd ────────────────────────────
; Uses r12d (fd) and r13d (events) from caller's registers
srv_handle_read:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    sub rsp, 8

    mov ebx, r12d           ; fd from server_run's r12d

    mov edi, ebx
    call get_conn
    mov r13, rax

    cmp dword [r13 + CONN_PHASE], 1  ; CONN_READING
    jne .shr_done

.shr_read_loop:
    mov edi, ebx
    mov eax, [r13 + CONN_READ_POS]
    lea rsi, [r13 + CONN_READ_BUF + rax]
    mov edx, 8192
    sub edx, [r13 + CONN_READ_POS]
    cmp edx, 0
    jle .shr_buf_full
    call read wrt ..plt

    cmp rax, -1
    jne .shr_check_eof
    ; Check errno — only retry on EAGAIN/EINTR
    call __errno_location wrt ..plt
    cmp dword [rax], 11     ; EAGAIN
    je .shr_done
    cmp dword [rax], 4      ; EINTR
    je .shr_read_loop
    ; Fatal read error — close connection
    mov edi, ebx
    call close_conn
    jmp .shr_done
.shr_check_eof:
    cmp rax, 0
    je .shr_close            ; EOF

    add [r13 + CONN_READ_POS], eax

    call get_epoch_time
    mov [r13 + CONN_LAST_ACT], rax

    ; Scan for \r\n\r\n
    mov ecx, [r13 + CONN_READ_POS]
    cmp ecx, 4
    jl .shr_read_loop

    lea rdi, [r13 + CONN_READ_BUF]
    xor eax, eax
.shr_scan:
    lea edx, [ecx - 3]     ; need 4 bytes from current pos
    cmp eax, edx
    jge .shr_read_loop
    cmp byte [rdi + rax], 0x0D
    jne .shr_scan_next
    cmp byte [rdi + rax + 1], 0x0A
    jne .shr_scan_next
    cmp byte [rdi + rax + 2], 0x0D
    jne .shr_scan_next
    cmp byte [rdi + rax + 3], 0x0A
    jne .shr_scan_next

    ; Found end of headers — null-terminate if room
    lea edx, [eax + 4]
    cmp edx, 8192           ; CONN_BUF_SIZE
    jge .shr_buf_full       ; no room for null terminator
    mov byte [rdi + rax + 4], 0

    lea rdi, [r13 + CONN_READ_BUF]
    lea rsi, [r13 + CONN_RESP_PTR]
    lea rdx, [r13 + CONN_RESP_LEN]
    call route_request

    lock inc qword [g_requests_served]

    mov dword [r13 + CONN_PHASE], 2  ; CONN_WRITING
    mov qword [r13 + CONN_WRITE_POS], 0

    ; Switch to EPOLLOUT
    sub rsp, 16
    mov dword [rsp], 0x80000004  ; EPOLLOUT | EPOLLET
    mov [rsp + 4], ebx
    mov edi, [epoll_fd]
    mov esi, 3               ; EPOLL_CTL_MOD
    mov edx, ebx
    mov rcx, rsp
    call epoll_ctl wrt ..plt
    add rsp, 16

    jmp .shr_done

.shr_scan_next:
    inc eax
    jmp .shr_scan

.shr_buf_full:
    mov edi, ebx
    call close_conn
    jmp .shr_done

.shr_close:
    mov edi, ebx
    call close_conn

.shr_done:
    add rsp, 8
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

; ── srv_handle_write — handle EPOLLOUT on client fd ──────────────────────────
srv_handle_write:
    push rbp
    mov rbp, rsp
    push rbx
    push r13

    mov ebx, r12d           ; fd

    mov edi, ebx
    call get_conn
    mov r13, rax

    cmp dword [r13 + CONN_PHASE], 2  ; CONN_WRITING
    jne .shw_done

.shw_write_loop:
    mov edi, ebx
    mov rsi, [r13 + CONN_RESP_PTR]
    add rsi, [r13 + CONN_WRITE_POS]
    mov rdx, [r13 + CONN_RESP_LEN]
    sub rdx, [r13 + CONN_WRITE_POS]
    cmp rdx, 0
    jle .shw_complete
    call write wrt ..plt

    cmp rax, -1
    jne .shw_check_eof
    ; Check errno — only retry on EAGAIN/EINTR
    call __errno_location wrt ..plt
    cmp dword [rax], 11     ; EAGAIN
    je .shw_done
    cmp dword [rax], 4      ; EINTR
    je .shw_write_loop
    ; Fatal write error — close
    mov edi, ebx
    call close_conn
    jmp .shw_done
.shw_check_eof:
    cmp rax, 0
    je .shw_complete

    add [r13 + CONN_WRITE_POS], rax

    mov rcx, [r13 + CONN_WRITE_POS]
    cmp rcx, [r13 + CONN_RESP_LEN]
    jl .shw_write_loop

.shw_complete:
    mov edi, ebx
    call close_conn

.shw_done:
    pop r13
    pop rbx
    pop rbp
    ret

; ── server_run() — main event loop ───────────────────────────────────────────
global server_run
server_run:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 4120

    ; Zero connection table
    lea rdi, [conn_table]
    xor esi, esi
    mov edx, CONN_ENTRY_SIZE * 4096
    call memset wrt ..plt

    ; Create socket
    mov edi, 2              ; AF_INET
    mov esi, 1              ; SOCK_STREAM
    xor edx, edx
    call socket wrt ..plt
    test eax, eax
    js .sr_fail
    mov [listen_fd], eax
    mov ebx, eax

    ; SO_REUSEADDR + SO_REUSEPORT
    sub rsp, 16
    mov dword [rsp], 1
    mov edi, ebx
    mov esi, 1
    mov edx, 2              ; SO_REUSEADDR
    mov rcx, rsp
    mov r8d, 4
    call setsockopt wrt ..plt
    mov edi, ebx
    mov esi, 1
    mov edx, 15             ; SO_REUSEPORT
    mov rcx, rsp
    mov r8d, 4
    call setsockopt wrt ..plt
    add rsp, 16

    ; Bind
    sub rsp, 16
    mov word [rsp], 2       ; AF_INET
    mov edi, [g_port]
    call htons wrt ..plt
    mov [rsp + 2], ax
    xor edi, edi
    call htonl wrt ..plt
    mov [rsp + 4], eax
    mov qword [rsp + 8], 0
    mov edi, ebx
    mov rsi, rsp
    mov edx, 16
    call bind wrt ..plt
    add rsp, 16
    test eax, eax
    jnz .sr_fail

    ; Listen
    mov edi, ebx
    mov esi, 512
    call listen wrt ..plt
    test eax, eax
    jnz .sr_fail

    ; Set non-blocking
    mov edi, ebx
    call set_nonblocking

    ; Create epoll
    xor edi, edi
    call epoll_create1 wrt ..plt
    test eax, eax
    js .sr_fail
    mov [epoll_fd], eax

    ; Add listen socket to epoll
    sub rsp, 16
    mov dword [rsp], 0x80000001  ; EPOLLIN | EPOLLET
    mov [rsp + 4], ebx
    mov edi, [epoll_fd]
    mov esi, 1
    mov edx, ebx
    mov rcx, rsp
    call epoll_ctl wrt ..plt
    add rsp, 16

    ; Log listening
    call log_ts
    mov rdi, [rel stderr wrt ..got]
    mov rdi, [rdi]
    lea rsi, [log_server_start]
    mov edx, [g_port]
    xor eax, eax
    call fprintf wrt ..plt

    ; ── Event loop ───────────────────────────────────────────────────────────

.event_loop:
    cmp qword [g_shutdown], 0
    jne .sr_shutdown

    mov edi, [epoll_fd]
    lea rsi, [rsp]
    mov edx, 256
    mov ecx, 500
    call epoll_wait wrt ..plt

    cmp eax, -1
    je .event_loop
    test eax, eax
    jz .event_loop

    mov r14d, eax
    xor r15d, r15d

.process_event:
    cmp r15d, r14d
    jge .sweep_stale

    mov eax, r15d
    imul eax, 12
    mov r12d, [rsp + rax + 4]  ; fd
    mov r13d, [rsp + rax]      ; events

    cmp r12d, [listen_fd]
    je .accept_loop

    ; Check errors first (EPOLLERR=0x008, EPOLLHUP=0x010)
    test r13d, 0x018
    jnz .event_error

    test r13d, 0x001        ; EPOLLIN
    jz .check_write
    call srv_handle_read
    jmp .next_event

.check_write:
    test r13d, 0x004        ; EPOLLOUT
    jz .next_event          ; no relevant events
    call srv_handle_write
    jmp .next_event

.event_error:
    mov edi, r12d
    call close_conn

.next_event:
    inc r15d
    jmp .process_event

.accept_loop:
    mov edi, [listen_fd]
    xor esi, esi
    xor edx, edx
    call accept wrt ..plt
    cmp eax, -1
    je .next_event

    mov r12d, eax

    cmp r12d, 4096
    jge .accept_close

    ; Connection limit — prevent resource exhaustion
    cmp qword [g_active_conns], 1024
    jge .accept_close

    mov edi, r12d
    call set_nonblocking

    mov edi, r12d
    call get_conn
    mov rbx, rax

    ; Free any leftover response from previous use of this FD
    mov rdi, [rbx + CONN_RESP_PTR]
    test rdi, rdi
    jz .accept_no_old_resp
    call free wrt ..plt
.accept_no_old_resp:

    mov dword [rbx + CONN_PHASE], 1
    mov dword [rbx + CONN_READ_POS], 0
    mov qword [rbx + CONN_RESP_PTR], 0
    mov qword [rbx + CONN_RESP_LEN], 0
    mov qword [rbx + CONN_WRITE_POS], 0

    call get_epoch_time
    mov [rbx + CONN_LAST_ACT], rax

    lock inc qword [g_active_conns]

    sub rsp, 16
    mov dword [rsp], 0x80000001
    mov [rsp + 4], r12d
    mov edi, [epoll_fd]
    mov esi, 1
    mov edx, r12d
    mov rcx, rsp
    call epoll_ctl wrt ..plt
    add rsp, 16

    jmp .accept_loop

.accept_close:
    mov edi, r12d
    call close wrt ..plt
    jmp .accept_loop

.sweep_stale:
    call get_epoch_time
    mov r13, rax
    xor r12d, r12d

.sweep_loop:
    cmp r12d, 4096
    jge .event_loop

    mov edi, r12d
    call get_conn
    cmp dword [rax + CONN_PHASE], 0
    je .sweep_next

    mov rcx, r13
    sub rcx, [rax + CONN_LAST_ACT]
    cmp rcx, 10             ; 10s timeout (prevents slowloris)
    jl .sweep_next

    mov edi, r12d
    call close_conn

.sweep_next:
    inc r12d
    jmp .sweep_loop

.sr_shutdown:
    mov edi, [listen_fd]
    call close wrt ..plt
    mov edi, [epoll_fd]
    call close wrt ..plt

    xor r12d, r12d
.shutdown_loop:
    cmp r12d, 4096
    jge .sr_exit

    mov edi, r12d
    call get_conn
    cmp dword [rax + CONN_PHASE], 0
    je .shutdown_next

    mov edi, r12d
    call close_conn

.shutdown_next:
    inc r12d
    jmp .shutdown_loop

.sr_exit:
    add rsp, 4120
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

.sr_fail:
    call log_ts
    mov rdi, [rel stderr wrt ..got]
    mov rdi, [rdi]
    lea rsi, [sr_err_fmt]
    xor eax, eax
    call fprintf wrt ..plt
    jmp .sr_exit

section .rodata
sr_err_fmt: db `[server] Failed to start\n`, 0
