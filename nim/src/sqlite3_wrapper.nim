# Minimal SQLite3 C FFI bindings — links against vendored ../c/sqlite3.c
{.compile("../../c/sqlite3.c", "-DSQLITE_THREADSAFE=2 -DSQLITE_OMIT_LOAD_EXTENSION").}

const
  SQLITE_OK* = 0
  SQLITE_ROW* = 100
  SQLITE_DONE* = 101
  SQLITE_NULL* = 5
  SQLITE_TRANSIENT* = cast[pointer](-1)
  SQLITE_STATIC* = cast[pointer](0)

type
  Sqlite3* = pointer
  Sqlite3Stmt* = pointer

proc sqlite3_open*(filename: cstring, ppDb: ptr Sqlite3): cint
  {.cdecl, importc: "sqlite3_open".}

proc sqlite3_close*(db: Sqlite3): cint
  {.cdecl, importc: "sqlite3_close".}

proc sqlite3_exec*(db: Sqlite3, sql: cstring, callback: pointer,
                   arg: pointer, errmsg: ptr cstring): cint
  {.cdecl, importc: "sqlite3_exec".}

proc sqlite3_free*(p: pointer)
  {.cdecl, importc: "sqlite3_free".}

proc sqlite3_errmsg*(db: Sqlite3): cstring
  {.cdecl, importc: "sqlite3_errmsg".}

proc sqlite3_prepare_v2*(db: Sqlite3, sql: cstring, nByte: cint,
                         ppStmt: ptr Sqlite3Stmt, pzTail: ptr cstring): cint
  {.cdecl, importc: "sqlite3_prepare_v2".}

proc sqlite3_step*(stmt: Sqlite3Stmt): cint
  {.cdecl, importc: "sqlite3_step".}

proc sqlite3_finalize*(stmt: Sqlite3Stmt): cint
  {.cdecl, importc: "sqlite3_finalize".}

proc sqlite3_bind_int64*(stmt: Sqlite3Stmt, idx: cint, val: int64): cint
  {.cdecl, importc: "sqlite3_bind_int64".}

proc sqlite3_bind_double*(stmt: Sqlite3Stmt, idx: cint, val: cdouble): cint
  {.cdecl, importc: "sqlite3_bind_double".}

proc sqlite3_bind_text*(stmt: Sqlite3Stmt, idx: cint, val: cstring,
                        nByte: cint, destructor: pointer): cint
  {.cdecl, importc: "sqlite3_bind_text".}

proc sqlite3_bind_null*(stmt: Sqlite3Stmt, idx: cint): cint
  {.cdecl, importc: "sqlite3_bind_null".}

proc sqlite3_column_int64*(stmt: Sqlite3Stmt, col: cint): int64
  {.cdecl, importc: "sqlite3_column_int64".}

proc sqlite3_column_double*(stmt: Sqlite3Stmt, col: cint): cdouble
  {.cdecl, importc: "sqlite3_column_double".}

proc sqlite3_column_text*(stmt: Sqlite3Stmt, col: cint): cstring
  {.cdecl, importc: "sqlite3_column_text".}

proc sqlite3_column_type*(stmt: Sqlite3Stmt, col: cint): cint
  {.cdecl, importc: "sqlite3_column_type".}
