(module
  (import "extism:host/env" "http_request" (func $http (param i64 i64) (result i64)))
  (import "extism:host/env" "alloc" (func $alloc (param i64) (result i64)))
  (import "extism:host/env" "store_u8" (func $store (param i64 i32)))
  (memory (export "memory") 1 1)
  (data (i32.const 0) "{\22url\22:\22https://example.invalid\22,\22method\22:\22GET\22}")
  (func (export "answer") (result i32) i32.const 42)
  (func (export "handle") (result i32)
    (local $i i32) (local $p i64)
    i64.const 48 call $alloc local.set $p
    (loop $copy
      local.get $p local.get $i i64.extend_i32_u i64.add
      local.get $i i32.load8_u call $store
      local.get $i i32.const 1 i32.add local.tee $i i32.const 48 i32.lt_u br_if $copy)
    local.get $p i64.const 0 call $http drop
    i32.const 0))
