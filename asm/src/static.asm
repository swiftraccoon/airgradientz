; static.asm — Static file serving with path traversal protection
default rel

extern open, read, close, malloc, free, snprintf, strlen, strcmp
extern realpath, strstr, memcpy, stat
extern content_type_for

extern str_public_dir, str_index_html, str_dotdot
extern http_200_file_hdr, http_404_response, http_404_response_len
extern http_403_response, http_403_response_len
extern fmt_path_join
extern O_RDONLY

section .data
max_file_size equ 1048576   ; 1MB max file

section .text

; ── serve_static_file(path, &resp_ptr, &resp_len) ────────────────────────────
; rdi = URL path (e.g., "/" or "/style.css"), rsi = &resp_ptr, rdx = &resp_len
global serve_static_file
serve_static_file:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 4712           ; path buffers + stat buf + header buf

    mov r12, rdi            ; path
    mov r13, rsi            ; &resp_ptr
    mov r14, rdx            ; &resp_len

    ; Check for path traversal: reject if ".." anywhere in decoded path
    mov rdi, r12
    lea rsi, [str_dotdot]
    call strstr wrt ..plt
    test rax, rax
    jnz .ssf_403

    ; Build filesystem path: "public" + url_path
    ; If path is "/" → "public/index.html"
    ; Otherwise → "public" + path
    cmp byte [r12], '/'
    jne .ssf_404
    cmp byte [r12 + 1], 0
    je .ssf_index

    ; "public" + path (path already starts with /)
    lea rdi, [rsp]          ; path buffer
    mov esi, 512
    lea rdx, [.path_fmt]
    lea rcx, [str_public_dir]
    mov r8, r12              ; url path (starts with /)
    xor eax, eax
    call snprintf wrt ..plt
    jmp .ssf_resolve

.ssf_index:
    lea rdi, [rsp]
    mov esi, 512
    lea rdx, [fmt_path_join]
    lea rcx, [str_public_dir]
    lea r8, [str_index_html]
    xor eax, eax
    call snprintf wrt ..plt

.ssf_resolve:
    ; Resolve realpath
    lea rdi, [rsp]          ; path
    lea rsi, [rsp + 512]    ; resolved buffer
    call realpath wrt ..plt
    test rax, rax
    jz .ssf_404

    ; Verify resolved path starts with our public dir (realpath check)
    ; Get realpath of "public" directory
    lea rdi, [str_public_dir]
    lea rsi, [rsp + 1024]   ; resolved public dir
    call realpath wrt ..plt
    test rax, rax
    jz .ssf_404

    ; Check that resolved file path starts with resolved public dir
    lea rdi, [rsp + 1024]
    call strlen wrt ..plt
    mov ebx, eax            ; public_dir_len

    lea rdi, [rsp + 512]    ; resolved file path
    lea rsi, [rsp + 1024]   ; resolved public dir
    mov edx, ebx            ; compare length
    extern strncmp
    call strncmp wrt ..plt
    test eax, eax
    jnz .ssf_403            ; path escapes public dir

    ; stat the file to get size
    lea rdi, [rsp + 512]    ; resolved path
    lea rsi, [rsp + 1536]   ; stat buffer (144 bytes)
    call stat wrt ..plt
    test eax, eax
    jnz .ssf_404

    ; Get file size from stat buf (st_size at offset 48 on x86_64 Linux)
    mov r15, [rsp + 1536 + 48]  ; file_size

    ; Check file size limit
    cmp r15, max_file_size
    jg .ssf_404

    ; Check it's a regular file (st_mode at offset 24, S_IFREG = 0x8000)
    mov eax, [rsp + 1536 + 24]
    and eax, 0xF000
    cmp eax, 0x8000
    jne .ssf_404

    ; Get content type
    lea rdi, [rsp + 512]    ; resolved path
    call content_type_for
    mov r12, rax            ; content_type

    ; Build header
    lea rdi, [rsp + 1680]   ; header buffer
    mov esi, 512
    lea rdx, [http_200_file_hdr]
    mov rcx, r12            ; content type
    mov r8, r15             ; content length
    xor eax, eax
    call snprintf wrt ..plt
    mov ebx, eax            ; header_len

    ; Open and read file
    lea rdi, [rsp + 512]
    xor esi, esi            ; O_RDONLY
    call open wrt ..plt
    cmp eax, -1
    je .ssf_404
    mov r12d, eax           ; fd

    ; Allocate response: header + file content + null terminator
    movsx rdi, ebx          ; header_len (sign-extend)
    add rdi, r15            ; + file_size
    inc rdi                 ; + 1
    call malloc wrt ..plt
    test rax, rax
    jz .ssf_close_fail

    mov [r13], rax          ; save resp_ptr via caller's pointer
    mov r13, rax            ; reuse r13 as buffer base

    ; Copy header into buffer
    mov rdi, r13
    lea rsi, [rsp + 1680]
    movsx rdx, ebx
    call memcpy wrt ..plt

    ; Read file content into buffer after header
    mov edi, r12d           ; arg1: fd
    movsx rsi, ebx
    add rsi, r13            ; arg2: buf = buffer + header_len
    mov rdx, r15            ; arg3: count = file_size
    call read wrt ..plt

    ; Set total length = header_len + file_size
    movsx rax, ebx
    add rax, r15
    mov [r14], rax

    ; Close fd
    mov edi, r12d
    call close wrt ..plt
    jmp .ssf_done

.ssf_close_fail:
    mov edi, r12d
    call close wrt ..plt

.ssf_403:
    ; Return 403 (path traversal)
    mov rdi, http_403_response_len + 1
    call malloc wrt ..plt
    test rax, rax
    jz .ssf_null
    mov [r13], rax
    mov rdi, rax
    lea rsi, [http_403_response]
    mov rdx, http_403_response_len
    call memcpy wrt ..plt
    mov qword [r14], http_403_response_len
    jmp .ssf_done

.ssf_404:
    ; Return 404
    mov rdi, http_404_response_len + 1
    call malloc wrt ..plt
    test rax, rax
    jz .ssf_null
    mov [r13], rax
    mov rdi, rax
    lea rsi, [http_404_response]
    mov rdx, http_404_response_len
    call memcpy wrt ..plt
    mov qword [r14], http_404_response_len
    jmp .ssf_done

.ssf_null:
    mov qword [r13], 0
    mov qword [r14], 0

.ssf_done:
    add rsp, 4712
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

section .rodata
.path_fmt: db `%s%s`, 0
