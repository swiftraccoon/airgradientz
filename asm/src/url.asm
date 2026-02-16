; url.asm — URL decode, query param parsing, content type detection
default rel

extern strlen, strcmp, strstr

extern ct_html, ct_css, ct_js, ct_json, ct_png, ct_jpeg, ct_svg, ct_ico, ct_octet
extern ext_html, ext_css, ext_js, ext_json, ext_png, ext_jpg, ext_jpeg, ext_svg, ext_ico

section .text

; ── hex_digit(c) → value or -1 ───────────────────────────────────────────────
; dil = character
; returns value in eax
hex_digit:
    movzx eax, dil
    cmp al, '0'
    jb .hd_fail
    cmp al, '9'
    jbe .hd_num
    cmp al, 'A'
    jb .hd_fail
    cmp al, 'F'
    jbe .hd_upper
    cmp al, 'a'
    jb .hd_fail
    cmp al, 'f'
    jbe .hd_lower
.hd_fail:
    mov eax, -1
    ret
.hd_num:
    sub eax, '0'
    ret
.hd_upper:
    sub eax, 'A'
    add eax, 10
    ret
.hd_lower:
    sub eax, 'a'
    add eax, 10
    ret

; ── url_decode(src, dst, maxlen) → decoded length ────────────────────────────
; rdi = src, rsi = dst, rdx = maxlen
; Decodes %XX sequences and '+' → space, in-place safe (dst can == src)
global url_decode
url_decode:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14

    mov r12, rdi            ; src
    mov r13, rsi            ; dst
    mov r14, rdx            ; maxlen
    xor ebx, ebx            ; output position

.ud_loop:
    movzx eax, byte [r12]
    test al, al
    jz .ud_done
    cmp rbx, r14
    jge .ud_done

    cmp al, '%'
    je .ud_percent
    cmp al, '+'
    je .ud_plus

    mov [r13 + rbx], al
    inc rbx
    inc r12
    jmp .ud_loop

.ud_plus:
    mov byte [r13 + rbx], ' '
    inc rbx
    inc r12
    jmp .ud_loop

.ud_percent:
    ; Need 2 hex digits
    movzx edi, byte [r12 + 1]
    test dil, dil
    jz .ud_literal
    call hex_digit
    cmp eax, -1
    je .ud_literal
    mov ecx, eax
    shl ecx, 4

    movzx edi, byte [r12 + 2]
    test dil, dil
    jz .ud_literal
    call hex_digit
    cmp eax, -1
    je .ud_literal
    or ecx, eax

    ; Reject null byte (%00) — prevents C string truncation attacks
    test cl, cl
    jz .ud_skip_pct

    mov [r13 + rbx], cl
    inc rbx
    add r12, 3
    jmp .ud_loop

.ud_skip_pct:
    add r12, 3              ; skip %00 entirely
    jmp .ud_loop

.ud_literal:
    mov byte [r13 + rbx], '%'
    inc rbx
    inc r12
    jmp .ud_loop

.ud_done:
    mov byte [r13 + rbx], 0
    mov rax, rbx

    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

; ── parse_query_param(query, key, out, outlen) → 0 found, -1 not found ──────
; rdi = query string (after ?), rsi = key name, rdx = output buffer, rcx = max length
global parse_query_param
parse_query_param:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8

    mov r12, rdi            ; query
    mov r13, rsi            ; key
    mov r14, rdx            ; outbuf
    mov r15, rcx            ; maxlen

    test r12, r12
    jz .pq_not_found
    cmp byte [r12], 0
    je .pq_not_found

    ; Get key length
    mov rdi, r13
    call strlen wrt ..plt
    mov ebx, eax            ; keylen

    ; Scan query string for key=value
    mov rdi, r12

.pq_scan:
    cmp byte [rdi], 0
    je .pq_not_found

    ; Check if current position matches key followed by '='
    xor ecx, ecx
.pq_match:
    cmp ecx, ebx
    jge .pq_check_eq
    movzx eax, byte [rdi + rcx]
    cmp al, [r13 + rcx]
    jne .pq_advance
    inc ecx
    jmp .pq_match

.pq_check_eq:
    cmp byte [rdi + rcx], '='
    jne .pq_advance

    ; Found! Copy value
    lea rdi, [rdi + rcx + 1]  ; skip past key=
    xor ecx, ecx
.pq_copy:
    cmp rcx, r15
    jge .pq_copy_done
    movzx eax, byte [rdi + rcx]
    cmp al, '&'
    je .pq_copy_done
    cmp al, 0
    je .pq_copy_done
    mov [r14 + rcx], al
    inc ecx
    jmp .pq_copy

.pq_copy_done:
    mov byte [r14 + rcx], 0
    xor eax, eax
    jmp .pq_done

.pq_advance:
    ; Skip to next '&' or end
    cmp byte [rdi], 0
    je .pq_not_found
    cmp byte [rdi], '&'
    je .pq_next_param
    inc rdi
    jmp .pq_advance

.pq_next_param:
    inc rdi                  ; skip '&'
    jmp .pq_scan

.pq_not_found:
    mov eax, -1

.pq_done:
    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

; ── content_type_for(path) → pointer to content type string ──────────────────
; rdi = path string
global content_type_for
content_type_for:
    push rbp
    mov rbp, rsp
    push rbx
    push r12

    mov r12, rdi

    ; Find last '.' in path
    mov rdi, r12
    call strlen wrt ..plt
    mov ecx, eax
    lea rdi, [r12 + rcx]

.ctf_find_dot:
    cmp rdi, r12
    jle .ctf_default
    dec rdi
    cmp byte [rdi], '.'
    jne .ctf_find_dot

    ; rdi points to the dot — compare extensions
    mov rbx, rdi

    ; .html
    mov rdi, rbx
    lea rsi, [ext_html]
    call strcmp wrt ..plt
    test eax, eax
    jz .ctf_html

    ; .css
    mov rdi, rbx
    lea rsi, [ext_css]
    call strcmp wrt ..plt
    test eax, eax
    jz .ctf_css

    ; .js
    mov rdi, rbx
    lea rsi, [ext_js]
    call strcmp wrt ..plt
    test eax, eax
    jz .ctf_js

    ; .json
    mov rdi, rbx
    lea rsi, [ext_json]
    call strcmp wrt ..plt
    test eax, eax
    jz .ctf_json

    ; .png
    mov rdi, rbx
    lea rsi, [ext_png]
    call strcmp wrt ..plt
    test eax, eax
    jz .ctf_png

    ; .jpg
    mov rdi, rbx
    lea rsi, [ext_jpg]
    call strcmp wrt ..plt
    test eax, eax
    jz .ctf_jpeg

    ; .jpeg
    mov rdi, rbx
    lea rsi, [ext_jpeg]
    call strcmp wrt ..plt
    test eax, eax
    jz .ctf_jpeg

    ; .svg
    mov rdi, rbx
    lea rsi, [ext_svg]
    call strcmp wrt ..plt
    test eax, eax
    jz .ctf_svg

    ; .ico
    mov rdi, rbx
    lea rsi, [ext_ico]
    call strcmp wrt ..plt
    test eax, eax
    jz .ctf_ico

.ctf_default:
    lea rax, [ct_octet]
    jmp .ctf_done

.ctf_html:
    lea rax, [ct_html]
    jmp .ctf_done
.ctf_css:
    lea rax, [ct_css]
    jmp .ctf_done
.ctf_js:
    lea rax, [ct_js]
    jmp .ctf_done
.ctf_json:
    lea rax, [ct_json]
    jmp .ctf_done
.ctf_png:
    lea rax, [ct_png]
    jmp .ctf_done
.ctf_jpeg:
    lea rax, [ct_jpeg]
    jmp .ctf_done
.ctf_svg:
    lea rax, [ct_svg]
    jmp .ctf_done
.ctf_ico:
    lea rax, [ct_ico]
    jmp .ctf_done

.ctf_done:
    pop r12
    pop rbx
    pop rbp
    ret
