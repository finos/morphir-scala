(module
  (import "extism:host/env" "input_length" (func $length (result i64)))
  (import "extism:host/env" "input_load_u8" (func $input (param i64) (result i32)))
  (import "extism:host/env" "alloc" (func $alloc (param i64) (result i64)))
  (import "extism:host/env" "store_u8" (func $store (param i64 i32)))
  (import "extism:host/env" "output_set" (func $output (param i64 i64)))
  (import "extism:host/user" "increment" (func $increment (param i64) (result i64)))
  (func (export "answer") (result i32) i32.const 42)
  (func (export "handle") (result i32)
    (local $i i64) (local $n i64) (local $p i64)
    i64.const 41 call $increment i64.const 42 i64.ne
    if unreachable end
    call $length local.set $n
    local.get $n call $alloc local.set $p
    (block $done (loop $copy
      local.get $i local.get $n i64.ge_u br_if $done
      local.get $p local.get $i i64.add
      local.get $i call $input call $store
      local.get $i i64.const 1 i64.add local.set $i
      br $copy))
    local.get $p local.get $n call $output
    i32.const 0))
