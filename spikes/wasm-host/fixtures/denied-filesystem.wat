(module
  (import "wasi_snapshot_preview1" "path_open" (func $open (param i32 i32 i32 i32 i32 i64 i64 i32 i32) (result i32)))
  (import "extism:host/env" "input_length" (func $length (result i64)))
  (import "extism:host/env" "input_load_u8" (func $input (param i64) (result i32)))
  (import "extism:host/env" "alloc" (func $alloc (param i64) (result i64)))
  (import "extism:host/env" "store_u8" (func $store (param i64 i32)))
  (import "extism:host/env" "output_set" (func $output (param i64 i64)))
  (memory (export "memory") 1 4096)
  (data (i32.const 0) "private")
  (func (export "answer") (result i32)
    i32.const 3 i32.const 0 i32.const 0 i32.const 7 i32.const 0
    i64.const 2 i64.const 0 i32.const 0 i32.const 32 call $open
    if (result i32) i32.const 42 else i32.const 0 end)
  (func (export "handle") (result i32)
    (local $i i64) (local $n i64) (local $p i64)
    call $length local.set $n
    local.get $n call $alloc local.set $p
    (block $done (loop $copy
      local.get $i local.get $n i64.ge_u br_if $done
      local.get $p local.get $i i64.add local.get $i call $input call $store
      local.get $i i64.const 1 i64.add local.set $i br $copy))
    local.get $p local.get $n call $output i32.const 0))
