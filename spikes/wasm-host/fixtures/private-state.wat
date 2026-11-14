(module
  (import "extism:host/env" "alloc" (func $alloc (param i64) (result i64)))
  (import "extism:host/env" "store_u8" (func $store (param i64 i32)))
  (import "extism:host/env" "load_u8" (func $load (param i64) (result i32)))
  (import "extism:host/env" "var_get" (func $get (param i64) (result i64)))
  (import "extism:host/env" "var_set" (func $set (param i64 i64)))
  (import "extism:host/env" "input_length" (func $length (result i64)))
  (import "extism:host/env" "input_load_u8" (func $input (param i64) (result i32)))
  (import "extism:host/env" "output_set" (func $output (param i64 i64)))
  (import "extism:host/user" "morphir_cache_ir" (func $cache (param i64 i64)))
  (import "extism:host/user" "morphir_get_cached_ir" (func $cached (param i64) (result i64)))
  (func $byte (param $value i32) (result i64)
    (local $p i64)
    i64.const 1 call $alloc local.tee $p local.get $value call $store
    local.get $p)
  (func $check
    i32.const 107 call $byte call $get call $load i32.const 42 i32.ne if unreachable end
    i32.const 107 call $byte call $cached call $load i32.const 42 i32.ne if unreachable end)
  (func (export "answer") (result i32)
    ;; Native Extism var_get/var_set consume key/value handles. Allocate each use separately.
    i32.const 107 call $byte call $get i64.eqz i32.eqz if unreachable end
    i32.const 107 call $byte call $cached i64.eqz i32.eqz if unreachable end
    i32.const 107 call $byte i32.const 42 call $byte call $set
    i32.const 107 call $byte i32.const 42 call $byte call $cache
    call $check i32.const 42)
  (func (export "handle") (result i32)
    (local $i i64) (local $n i64) (local $p i64)
    call $check
    call $length local.set $n
    local.get $n call $alloc local.set $p
    (block $done (loop $copy
      local.get $i local.get $n i64.ge_u br_if $done
      local.get $p local.get $i i64.add local.get $i call $input call $store
      local.get $i i64.const 1 i64.add local.set $i br $copy))
    local.get $p local.get $n call $output i32.const 0))
