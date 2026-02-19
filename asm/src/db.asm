; db.asm — SQLite wrappers: init, queries with json_group_array, insert, count
default rel

extern sqlite3_open, sqlite3_close, sqlite3_exec, sqlite3_errmsg
extern sqlite3_prepare_v2, sqlite3_step, sqlite3_finalize
extern sqlite3_column_text, sqlite3_column_int64
extern sqlite3_bind_text, sqlite3_bind_int64, sqlite3_bind_null
extern pthread_mutex_lock, pthread_mutex_unlock
extern snprintf, fprintf, strdup, strlen, free, malloc, memcpy, strcmp
extern stderr
extern log_ts

extern g_db, g_db_mutex
extern sql_schema, sql_pragma, sql_insert_reading
extern sql_readings_json, sql_where_ts, sql_and_device, sql_where_device
extern sql_order_limit, sql_order_limit_close
extern sql_downsample_json, sql_ds_group_by
extern sql_count_filtered, sql_count_where_ts, sql_count_and_device
extern sql_latest_json, sql_devices_json, sql_count_readings, sql_checkpoint
extern log_db_opened
extern str_all
extern MAX_QUERY_SIZE

section .text

; ── db_init(path) → 0 ok, -1 error ──────────────────────────────────────────
; rdi = path string
global db_init
db_init:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    sub rsp, 16

    mov r12, rdi            ; save path

    ; sqlite3_open(path, &g_db)
    lea rsi, [g_db]
    call sqlite3_open wrt ..plt
    test eax, eax
    jnz .di_fail

    ; Log
    call log_ts
    mov rdi, [rel stderr wrt ..got]
    mov rdi, [rdi]
    lea rsi, [log_db_opened]
    mov rdx, r12
    xor eax, eax
    call fprintf wrt ..plt

    ; Run pragmas
    mov rdi, [g_db]
    lea rsi, [sql_pragma]
    xor edx, edx
    xor ecx, ecx
    xor r8d, r8d
    call sqlite3_exec wrt ..plt

    ; Create schema
    mov rdi, [g_db]
    lea rsi, [sql_schema]
    xor edx, edx
    xor ecx, ecx
    xor r8d, r8d
    call sqlite3_exec wrt ..plt
    test eax, eax
    jnz .di_schema_fail

    xor eax, eax
    jmp .di_done

.di_schema_fail:
    ; Log error
    mov rdi, [g_db]
    call sqlite3_errmsg wrt ..plt
    mov r12, rax             ; save errmsg (r12 is callee-saved)
    call log_ts
    mov rdi, [rel stderr wrt ..got]
    mov rdi, [rdi]
    lea rsi, [.di_err_fmt]
    mov rdx, r12
    xor eax, eax
    call fprintf wrt ..plt
    mov eax, -1
    jmp .di_done

.di_fail:
    mov eax, -1

.di_done:
    add rsp, 16
    pop r12
    pop rbx
    pop rbp
    ret

section .rodata
.di_err_fmt: db `[db] Schema error: %s\n`, 0

section .text

; ── db_close() ───────────────────────────────────────────────────────────────
global db_close
db_close:
    push rbp
    mov rbp, rsp

    mov rdi, [g_db]
    test rdi, rdi
    jz .dc_done
    call sqlite3_close wrt ..plt

.dc_done:
    pop rbp
    ret

; ── db_query_json(sql) → strdup'd JSON string or NULL ────────────────────────
; Locks mutex, prepares sql, steps once, returns column_text(0) via strdup
; rdi = sql (null-terminated)
global db_query_json
db_query_json:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    sub rsp, 24

    mov r12, rdi            ; sql
    xor r13d, r13d          ; stmt = NULL (safe default for finalize)

    ; Lock mutex
    lea rdi, [g_db_mutex]
    call pthread_mutex_lock wrt ..plt

    ; Prepare
    mov rdi, [g_db]
    mov rsi, r12
    mov edx, -1             ; length = auto
    lea rcx, [rsp]          ; &stmt
    xor r8d, r8d            ; tail = NULL
    call sqlite3_prepare_v2 wrt ..plt
    test eax, eax
    jnz .qj_fail

    mov r13, [rsp]          ; stmt

    ; Step
    mov rdi, r13
    call sqlite3_step wrt ..plt
    cmp eax, 100            ; SQLITE_ROW
    jne .qj_no_row

    ; Get column text
    mov rdi, r13
    xor esi, esi            ; column 0
    call sqlite3_column_text wrt ..plt
    test rax, rax
    jz .qj_no_row

    ; strdup the result
    mov rdi, rax
    call strdup wrt ..plt
    mov rbx, rax
    jmp .qj_finalize

.qj_no_row:
    ; Return "[]"
    lea rdi, [.empty_arr]
    call strdup wrt ..plt
    mov rbx, rax
    jmp .qj_finalize

.qj_fail:
    xor ebx, ebx           ; return NULL
    ; r13 already zeroed at function entry — finalize will skip

.qj_finalize:
    ; Finalize statement if we have one
    test r13, r13
    jz .qj_unlock
    mov rdi, r13
    call sqlite3_finalize wrt ..plt

.qj_unlock:
    lea rdi, [g_db_mutex]
    call pthread_mutex_unlock wrt ..plt

    mov rax, rbx

    add rsp, 24
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

section .rodata
.empty_arr: db `[]`, 0

section .text

; ── db_query_json_bind1(sql, bind_text) → strdup'd JSON or NULL ──────────────
; Like db_query_json but optionally binds one text parameter (param index 1)
; rdi = sql, rsi = text to bind (or NULL for no binding)
global db_query_json_bind1
db_query_json_bind1:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    sub rsp, 16             ; 5 pushes + ret = 6×8=48, 48+16=64, 64%16=0

    mov r12, rdi            ; sql
    mov r14, rsi            ; bind_text (or NULL)
    xor r13d, r13d          ; stmt = NULL

    ; Lock mutex
    lea rdi, [g_db_mutex]
    call pthread_mutex_lock wrt ..plt

    ; Prepare
    mov rdi, [g_db]
    mov rsi, r12
    mov edx, -1
    lea rcx, [rsp]
    xor r8d, r8d
    call sqlite3_prepare_v2 wrt ..plt
    test eax, eax
    jnz .qjb_fail

    mov r13, [rsp]          ; stmt

    ; Bind text param if non-NULL
    test r14, r14
    jz .qjb_step

    mov rdi, r13
    mov esi, 1              ; param index 1
    mov rdx, r14
    mov ecx, -1             ; auto length
    xor r8d, r8d            ; SQLITE_STATIC (string lives through step)
    call sqlite3_bind_text wrt ..plt

.qjb_step:
    mov rdi, r13
    call sqlite3_step wrt ..plt
    cmp eax, 100            ; SQLITE_ROW
    jne .qjb_no_row

    mov rdi, r13
    xor esi, esi
    call sqlite3_column_text wrt ..plt
    test rax, rax
    jz .qjb_no_row

    mov rdi, rax
    call strdup wrt ..plt
    mov rbx, rax
    jmp .qjb_finalize

.qjb_no_row:
    lea rdi, [.qjb_empty]
    call strdup wrt ..plt
    mov rbx, rax
    jmp .qjb_finalize

.qjb_fail:
    xor ebx, ebx

.qjb_finalize:
    test r13, r13
    jz .qjb_unlock
    mov rdi, r13
    call sqlite3_finalize wrt ..plt

.qjb_unlock:
    lea rdi, [g_db_mutex]
    call pthread_mutex_unlock wrt ..plt

    mov rax, rbx

    add rsp, 16
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

section .rodata
.qjb_empty: db `[]`, 0

section .text

; ── db_get_readings(from, to, device, limit) → JSON string ──────────────────
; rdi = from (int64), rsi = to (int64), rdx = device (string or NULL), rcx = limit (int)
; Uses parameterized binding for device filter to prevent SQL injection
global db_get_readings
db_get_readings:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 4120           ; query buffer

    mov r12, rdi            ; from
    mov r13, rsi            ; to
    mov r14, rdx            ; device
    mov r15d, ecx           ; limit

    ; Start building query: sql_readings_json base
    lea rdi, [rsp]
    mov esi, 4096
    lea rdx, [fmt_str]
    lea rcx, [sql_readings_json]
    xor eax, eax
    call snprintf wrt ..plt
    mov ebx, eax            ; current length

    ; Add WHERE timestamp clause if from or to specified
    cmp r12, 0
    jne .gr_add_where
    cmp r13, 0
    je .gr_check_device
.gr_add_where:
    ; Set defaults
    test r12, r12
    jnz .gr_from_ok
    xor r12d, r12d          ; from = 0
.gr_from_ok:
    test r13, r13
    jnz .gr_to_ok
    mov r13, 0x7FFFFFFFFFFFFFFF  ; far future
.gr_to_ok:

    lea rdi, [rsp + rbx]
    mov esi, 4096
    sub esi, ebx
    lea rdx, [sql_where_ts]
    mov rcx, r12
    mov r8, r13
    xor eax, eax
    call snprintf wrt ..plt
    add ebx, eax

    ; Add device filter if specified (parameterized — just appends " AND device_id = ?")
    ; "all" means no filter (same as NULL)
    test r14, r14
    jz .gr_add_order
    cmp byte [r14], 0
    je .gr_clear_device
    mov rdi, r14
    lea rsi, [str_all]
    call strcmp wrt ..plt
    test eax, eax
    jz .gr_clear_device

    lea rdi, [rsp + rbx]
    mov esi, 4096
    sub esi, ebx
    lea rdx, [fmt_str]
    lea rcx, [sql_and_device]
    xor eax, eax
    call snprintf wrt ..plt
    add ebx, eax
    jmp .gr_add_order

.gr_check_device:
    ; No timestamp filter — check device only
    test r14, r14
    jz .gr_add_order
    cmp byte [r14], 0
    je .gr_clear_device
    mov rdi, r14
    lea rsi, [str_all]
    call strcmp wrt ..plt
    test eax, eax
    jz .gr_clear_device

    lea rdi, [rsp + rbx]
    mov esi, 4096
    sub esi, ebx
    lea rdx, [fmt_str]
    lea rcx, [sql_where_device]
    xor eax, eax
    call snprintf wrt ..plt
    add ebx, eax
    jmp .gr_add_order

.gr_clear_device:
    xor r14d, r14d          ; clear device so we don't try to bind

.gr_add_order:
    ; Add ORDER BY ... LIMIT ... closing paren and semicolon
    lea rdi, [rsp + rbx]
    mov esi, 4096
    sub esi, ebx
    lea rdx, [sql_order_limit_close]
    mov ecx, r15d
    xor eax, eax
    call snprintf wrt ..plt

    ; Execute query with optional device binding
    lea rdi, [rsp]
    mov rsi, r14            ; device (NULL = no binding, non-NULL = bind as param 1)
    call db_query_json_bind1

    add rsp, 4120
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

section .rodata
fmt_str: db `%s`, 0
str_null: db `NULL`, 0
fmt_count_end: db `;`, 0

section .text

; ── db_get_readings_downsampled(from, to, device, limit, bucket_ms) → JSON ──
; rdi = from (int64), rsi = to (int64), rdx = device (string or NULL),
; ecx = limit (int), r8 = bucket_ms (int64)
global db_get_readings_downsampled
db_get_readings_downsampled:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 4120           ; query buffer

    mov r12, rdi            ; from
    mov r13, rsi            ; to
    mov r14, rdx            ; device
    mov r15d, ecx           ; limit
    mov [rsp + 4104], r8    ; save bucket_ms at end of buffer area

    ; Build downsample query base with bucket_ms interpolated
    mov rcx, [rsp + 4104]   ; bucket_ms (1st %lld)
    mov r8, [rsp + 4104]    ; bucket_ms (2nd %lld)
    lea rdi, [rsp]
    mov esi, 4096
    lea rdx, [sql_downsample_json]
    xor eax, eax
    call snprintf wrt ..plt
    mov ebx, eax            ; current length

    ; Add WHERE clause — timestamp filter always present
    ; Set defaults
    test r12, r12
    jnz .grd_from_ok
    xor r12d, r12d
.grd_from_ok:
    test r13, r13
    jnz .grd_to_ok
    mov r13, 0x7FFFFFFFFFFFFFFF
.grd_to_ok:

    lea rdi, [rsp + rbx]
    mov esi, 4096
    sub esi, ebx
    lea rdx, [sql_where_ts]
    mov rcx, r12            ; from
    mov r8, r13             ; to
    xor eax, eax
    call snprintf wrt ..plt
    add ebx, eax

    ; Add device filter if specified ("all" = no filter)
    test r14, r14
    jz .grd_add_group
    cmp byte [r14], 0
    je .grd_clear_device
    mov rdi, r14
    lea rsi, [str_all]
    call strcmp wrt ..plt
    test eax, eax
    jz .grd_clear_device

    lea rdi, [rsp + rbx]
    mov esi, 4096
    sub esi, ebx
    lea rdx, [fmt_str]
    lea rcx, [sql_and_device]
    xor eax, eax
    call snprintf wrt ..plt
    add ebx, eax
    jmp .grd_add_group

.grd_clear_device:
    xor r14d, r14d

.grd_add_group:
    ; Add GROUP BY ... ORDER BY ... LIMIT ... closing paren
    lea rdi, [rsp + rbx]
    mov esi, 4096
    sub esi, ebx
    lea rdx, [sql_ds_group_by]
    mov rcx, [rsp + 4104]   ; bucket_ms
    mov r8d, r15d            ; limit
    xor eax, eax
    call snprintf wrt ..plt

    ; Execute query with optional device binding
    lea rdi, [rsp]
    mov rsi, r14
    call db_query_json_bind1

    add rsp, 4120
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

; ── db_get_filtered_count(from, to, device) → JSON string ─────────────────────
; rdi = from (int64), rsi = to (int64), rdx = device (string or NULL)
; Returns strdup'd JSON like {"count": 123}
global db_get_filtered_count
db_get_filtered_count:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 4120

    mov r12, rdi            ; from
    mov r13, rsi            ; to
    mov r14, rdx            ; device
    ; r15 not used here but pushed for alignment

    ; Build base: "SELECT json_object('count', COUNT(*)) FROM readings"
    lea rdi, [rsp]
    mov esi, 4096
    lea rdx, [fmt_str]
    lea rcx, [sql_count_filtered]
    xor eax, eax
    call snprintf wrt ..plt
    mov ebx, eax

    ; Add WHERE timestamp clause
    test r12, r12
    jnz .gfc_from_ok
    xor r12d, r12d
.gfc_from_ok:
    test r13, r13
    jnz .gfc_to_ok
    mov r13, 0x7FFFFFFFFFFFFFFF
.gfc_to_ok:

    lea rdi, [rsp + rbx]
    mov esi, 4096
    sub esi, ebx
    lea rdx, [sql_count_where_ts]
    mov rcx, r12
    mov r8, r13
    xor eax, eax
    call snprintf wrt ..plt
    add ebx, eax

    ; Add device filter if specified ("all" = no filter)
    test r14, r14
    jz .gfc_finalize
    cmp byte [r14], 0
    je .gfc_clear_device
    mov rdi, r14
    lea rsi, [str_all]
    call strcmp wrt ..plt
    test eax, eax
    jz .gfc_clear_device

    lea rdi, [rsp + rbx]
    mov esi, 4096
    sub esi, ebx
    lea rdx, [fmt_str]
    lea rcx, [sql_count_and_device]
    xor eax, eax
    call snprintf wrt ..plt
    add ebx, eax
    jmp .gfc_finalize

.gfc_clear_device:
    xor r14d, r14d

.gfc_finalize:
    ; Add semicolon
    mov byte [rsp + rbx], ';'
    mov byte [rsp + rbx + 1], 0

    ; Execute with optional device binding
    lea rdi, [rsp]
    mov rsi, r14
    call db_query_json_bind1

    add rsp, 4120
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

; ── db_get_latest() → JSON string ────────────────────────────────────────────
global db_get_latest
db_get_latest:
    push rbp
    mov rbp, rsp

    lea rdi, [sql_latest_json]
    call db_query_json

    pop rbp
    ret

; ── db_get_devices() → JSON string ───────────────────────────────────────────
global db_get_devices
db_get_devices:
    push rbp
    mov rbp, rsp

    lea rdi, [sql_devices_json]
    call db_query_json

    pop rbp
    ret

; ── db_get_count() → int64 ───────────────────────────────────────────────────
global db_get_count
db_get_count:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    sub rsp, 16

    ; Lock mutex
    lea rdi, [g_db_mutex]
    call pthread_mutex_lock wrt ..plt

    ; Prepare
    mov rdi, [g_db]
    lea rsi, [sql_count_readings]
    mov edx, -1
    lea rcx, [rsp]
    xor r8d, r8d
    call sqlite3_prepare_v2 wrt ..plt
    test eax, eax
    jnz .gc_fail

    mov r12, [rsp]          ; stmt

    ; Step
    mov rdi, r12
    call sqlite3_step wrt ..plt
    cmp eax, 100            ; SQLITE_ROW
    jne .gc_zero

    ; Get count
    mov rdi, r12
    xor esi, esi
    call sqlite3_column_int64 wrt ..plt
    mov rbx, rax
    jmp .gc_finalize

.gc_zero:
    xor ebx, ebx
    jmp .gc_finalize

.gc_fail:
    xor ebx, ebx
    xor r12d, r12d

.gc_finalize:
    test r12, r12
    jz .gc_unlock
    mov rdi, r12
    call sqlite3_finalize wrt ..plt

.gc_unlock:
    lea rdi, [g_db_mutex]
    call pthread_mutex_unlock wrt ..plt

    mov rax, rbx

    add rsp, 16
    pop r12
    pop rbx
    pop rbp
    ret

; ── db_insert_reading(timestamp, params) → 0 ok ─────────────────────────────
; Parameterized INSERT — prevents SQL injection
; rdi = timestamp (int64)
; rsi = pointer to array of 16 string pointers:
;   [0]  = device_id     (text, always bind)
;   [1]  = device_type   (text, always bind)
;   [2]  = device_ip     (text, always bind)
;   [3]  = pm01          (text or "NULL")
;   [4]  = pm02          (text or "NULL")
;   [5]  = pm10          (text or "NULL")
;   [6]  = pm02_comp     (text or "NULL")
;   [7]  = rco2          (text or "NULL")
;   [8]  = atmp          (text or "NULL")
;   [9]  = atmp_comp     (text or "NULL")
;   [10] = rhum          (text or "NULL")
;   [11] = rhum_comp     (text or "NULL")
;   [12] = tvoc          (text or "NULL")
;   [13] = nox           (text or "NULL")
;   [14] = wifi          (text or "NULL")
;   [15] = raw_json      (text, always bind)
global db_insert_reading
db_insert_reading:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 24

    mov r14, rdi            ; timestamp
    mov rbx, rsi            ; params array
    xor r13d, r13d          ; stmt = NULL
    xor r15d, r15d          ; result = 0

    ; Lock mutex
    lea rdi, [g_db_mutex]
    call pthread_mutex_lock wrt ..plt

    ; Prepare parameterized INSERT
    mov rdi, [g_db]
    lea rsi, [sql_insert_reading]
    mov edx, -1
    lea rcx, [rsp]
    xor r8d, r8d
    call sqlite3_prepare_v2 wrt ..plt
    test eax, eax
    jnz .dir_prepare_fail

    mov r13, [rsp]          ; stmt

    ; Bind param 1: timestamp (int64)
    mov rdi, r13
    mov esi, 1
    mov rdx, r14
    call sqlite3_bind_int64 wrt ..plt

    ; Bind params 2-17 from array (indices 0-15 → bind positions 2-17)
    xor r12d, r12d          ; array index

.dir_bind_loop:
    cmp r12d, 16
    jge .dir_step

    mov rdx, [rbx + r12 * 8]  ; params[r12]
    mov esi, r12d
    add esi, 2              ; bind position = array_index + 2

    ; Numeric fields at array indices 3-14: check for "NULL" string
    cmp r12d, 3
    jl .dir_bind_text
    cmp r12d, 15
    jge .dir_bind_text

    ; Check if value is "NULL"
    cmp byte [rdx], 'N'
    jne .dir_bind_text

    ; Bind NULL for this parameter
    mov rdi, r13
    ; esi already has bind position
    call sqlite3_bind_null wrt ..plt
    jmp .dir_bind_next

.dir_bind_text:
    mov rdi, r13
    ; esi already has bind position
    ; rdx already has string pointer
    mov ecx, -1             ; auto length
    xor r8d, r8d            ; SQLITE_STATIC
    call sqlite3_bind_text wrt ..plt

.dir_bind_next:
    inc r12d
    jmp .dir_bind_loop

.dir_step:
    mov rdi, r13
    call sqlite3_step wrt ..plt
    cmp eax, 101            ; SQLITE_DONE
    je .dir_finalize

    ; Step returned error
    mov r15d, eax
    mov rdi, [g_db]
    call sqlite3_errmsg wrt ..plt
    mov r14, rax             ; save errmsg
    call log_ts
    mov rdi, [rel stderr wrt ..got]
    mov rdi, [rdi]
    lea rsi, [.dir_err_fmt]
    mov rdx, r14
    xor eax, eax
    call fprintf wrt ..plt
    jmp .dir_finalize

.dir_prepare_fail:
    mov r15d, eax
    mov rdi, [g_db]
    call sqlite3_errmsg wrt ..plt
    mov r14, rax             ; save errmsg
    call log_ts
    mov rdi, [rel stderr wrt ..got]
    mov rdi, [rdi]
    lea rsi, [.dir_prep_err_fmt]
    mov rdx, r14
    xor eax, eax
    call fprintf wrt ..plt

.dir_finalize:
    test r13, r13
    jz .dir_unlock
    mov rdi, r13
    call sqlite3_finalize wrt ..plt

.dir_unlock:
    lea rdi, [g_db_mutex]
    call pthread_mutex_unlock wrt ..plt

    mov eax, r15d

    add rsp, 24
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

section .rodata
.dir_err_fmt: db `[db] INSERT step error: %s\n`, 0
.dir_prep_err_fmt: db `[db] INSERT prepare error: %s\n`, 0

section .text

; ── db_checkpoint() ──────────────────────────────────────────────────────────
global db_checkpoint
db_checkpoint:
    push rbp
    mov rbp, rsp

    lea rdi, [g_db_mutex]
    call pthread_mutex_lock wrt ..plt

    mov rdi, [g_db]
    lea rsi, [sql_checkpoint]
    xor edx, edx
    xor ecx, ecx
    xor r8d, r8d
    call sqlite3_exec wrt ..plt

    lea rdi, [g_db_mutex]
    call pthread_mutex_unlock wrt ..plt

    pop rbp
    ret
