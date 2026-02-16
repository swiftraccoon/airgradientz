; main.asm — Entry point: config, DB init, signal handling, poller thread, server
default rel

extern load_config
extern db_init, db_close
extern server_run
extern poller_thread_main

extern pthread_create, pthread_join, pthread_mutex_init, pthread_rwlock_init
extern sigaction, fprintf, clock_gettime, exit
extern memset, snprintf

extern stderr

extern g_port, g_db_path, g_device_count
extern log_server_start, log_server_stop, log_signal
extern SIGINT, SIGTERM, SIGPIPE, SIG_IGN

; ── Globals ──────────────────────────────────────────────────────────────────

section .bss

global g_db
g_db: resq 1               ; sqlite3*

global g_db_mutex
g_db_mutex: resb 40        ; pthread_mutex_t

global g_health_lock
g_health_lock: resb 56     ; pthread_rwlock_t

; Health data: per device
; 0-63:   ip (64 bytes)
; 64-127: label (64 bytes)
; 128-191: status string (64 bytes)
; 192-199: lastSuccess (int64)
; 200-207: lastError (int64)
; 208-271: lastErrorMessage (64 bytes)
; 272-275: consecutiveFailures (int32)
; 276-295: padding
; = 296 bytes per entry
global g_health
g_health: resb 296 * 8     ; MAX_DEVICES

; Atomic counters
global g_requests_served
g_requests_served: resq 1

global g_active_conns
g_active_conns: resq 1

global g_poll_successes
g_poll_successes: resq 1

global g_poll_failures
g_poll_failures: resq 1

global g_shutdown
g_shutdown: resq 1

global g_started_at
g_started_at: resq 1

; Poller thread handle
poller_tid: resq 1

section .data

; sigaction struct: sa_handler(8) + sa_flags(8) + sa_restorer(8) + sa_mask(128) = 152
sigact_buf: times 152 db 0

section .text

; ── Signal handler ───────────────────────────────────────────────────────────

signal_handler:
    ; Only async-signal-safe operations (no fprintf/PLT calls)
    mov qword [g_shutdown], 1
    ; Raw syscall: write(STDERR_FILENO, msg, len) — async-signal-safe
    mov eax, 1              ; SYS_write
    mov edi, 2              ; STDERR_FILENO
    lea rsi, [.sig_msg]
    mov edx, .sig_msg_len
    syscall
    ret

section .rodata
.sig_msg: db `[server] Signal received, shutting down`, 10
.sig_msg_len equ $ - .sig_msg

section .text

; ── Main entry ───────────────────────────────────────────────────────────────

global main
main:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 24

    ; Zero all BSS globals
    mov qword [g_shutdown], 0
    mov qword [g_requests_served], 0
    mov qword [g_active_conns], 0
    mov qword [g_poll_successes], 0
    mov qword [g_poll_failures], 0

    ; Record start time (milliseconds)
    sub rsp, 16
    mov edi, 1              ; CLOCK_MONOTONIC
    mov rsi, rsp
    call clock_gettime wrt ..plt
    mov rax, [rsp]          ; seconds
    imul rax, 1000
    mov rdx, [rsp + 8]     ; nanoseconds
    mov rcx, rdx
    shr rcx, 20             ; approximate /1000000
    add rax, rcx
    mov [g_started_at], rax

    ; Also get wall clock time for started_at epoch ms
    mov edi, 0              ; CLOCK_REALTIME
    mov rsi, rsp
    call clock_gettime wrt ..plt
    mov rax, [rsp]
    imul rax, 1000
    mov rdx, [rsp + 8]
    mov rcx, rdx
    shr rcx, 20
    add rax, rcx
    mov [g_started_at], rax
    add rsp, 16

    ; Load config
    call load_config

    ; Init pthread_mutex for DB
    lea rdi, [g_db_mutex]
    xor esi, esi            ; NULL attrs = default
    call pthread_mutex_init wrt ..plt

    ; Init pthread_rwlock for health
    lea rdi, [g_health_lock]
    xor esi, esi
    call pthread_rwlock_init wrt ..plt

    ; Initialize health entries from config
    call init_health_from_config

    ; Init database
    mov rdi, [g_db_path]
    call db_init
    test eax, eax
    jnz .db_fail

    ; Setup signal handlers
    ; SIGPIPE → SIG_IGN
    lea rdi, [sigact_buf]
    mov ecx, 152
    xor esi, esi
    push rdi
    call memset wrt ..plt
    pop rdi
    mov qword [rdi], 1      ; SIG_IGN
    mov edi, 13              ; SIGPIPE
    lea rsi, [sigact_buf]
    xor edx, edx
    call sigaction wrt ..plt

    ; SIGINT → signal_handler
    lea rdi, [sigact_buf]
    mov ecx, 152
    xor esi, esi
    push rdi
    call memset wrt ..plt
    pop rdi
    lea rax, [signal_handler]
    mov [rdi], rax
    ; SA_RESTART = 0x10000000
    mov qword [rdi + 8], 0x10000000
    mov edi, 2               ; SIGINT
    lea rsi, [sigact_buf]
    xor edx, edx
    call sigaction wrt ..plt

    ; SIGTERM → signal_handler
    mov edi, 15              ; SIGTERM
    lea rsi, [sigact_buf]
    xor edx, edx
    call sigaction wrt ..plt

    ; Start poller thread
    lea rdi, [poller_tid]
    xor esi, esi            ; NULL attrs
    lea rdx, [poller_thread_main]
    xor ecx, ecx            ; NULL arg
    call pthread_create wrt ..plt

    ; Run server (blocks until shutdown)
    call server_run

    ; Signal shutdown and join poller
    mov qword [g_shutdown], 1

    mov rdi, [poller_tid]
    xor esi, esi
    call pthread_join wrt ..plt

    ; Close database
    call db_close

    ; Log shutdown
    mov rdi, [rel stderr wrt ..got]
    mov rdi, [rdi]
    lea rsi, [log_server_stop]
    xor eax, eax
    call fprintf wrt ..plt

    xor eax, eax
    jmp .exit

.db_fail:
    mov eax, 1

.exit:
    add rsp, 24
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

; ── init_health_from_config — set up initial health entries ──────────────────

extern g_devices, g_device_count
extern str_status_unknown
extern fmt_str

init_health_from_config:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    sub rsp, 8

    mov r12d, [g_device_count]
    xor ebx, ebx            ; index

.ihfc_loop:
    cmp ebx, r12d
    jge .ihfc_done

    ; Calculate health entry offset: ebx * 296
    mov eax, ebx
    imul eax, 296
    lea r13, [g_health]
    add r13, rax             ; r13 = &g_health[ebx]

    ; Zero the entry
    mov rdi, r13
    xor esi, esi
    mov edx, 296
    call memset wrt ..plt

    ; Copy IP (bounded to 64 bytes)
    mov eax, ebx
    shl eax, 4              ; * 16
    lea rcx, [g_devices]
    mov rcx, [rcx + rax]    ; src = devices[i].ip
    mov rdi, r13             ; dest = health.ip
    mov esi, 64
    lea rdx, [fmt_str]
    xor eax, eax
    call snprintf wrt ..plt

    ; Copy label (bounded to 64 bytes)
    mov eax, ebx
    shl eax, 4
    lea rcx, [g_devices]
    mov rcx, [rcx + rax + 8] ; src = devices[i].label
    lea rdi, [r13 + 64]     ; dest = health.label
    mov esi, 64
    lea rdx, [fmt_str]
    xor eax, eax
    call snprintf wrt ..plt

    ; Set status to "unknown" (bounded to 64 bytes)
    lea rdi, [r13 + 128]
    mov esi, 64
    lea rdx, [fmt_str]
    lea rcx, [str_status_unknown]
    xor eax, eax
    call snprintf wrt ..plt

    ; Zero timestamps
    mov qword [r13 + 192], 0  ; lastSuccess
    mov qword [r13 + 200], 0  ; lastError
    mov dword [r13 + 272], 0  ; consecutiveFailures

    inc ebx
    jmp .ihfc_loop

.ihfc_done:
    add rsp, 8
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret
