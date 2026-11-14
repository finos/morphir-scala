(module
  (import "morphir_bytes_v1" "input_len" (func $length (result i32)))
  (import "morphir_bytes_v1" "input_byte" (func $input (param i32) (result i32)))
  (import "morphir_bytes_v1" "output_byte" (func $output (param i32)))
  (func (export "answer") (result i32) i32.const 42)
  ;; 100,000 dependent LCG steps with i32 wraparound. The checksum is consumed.
  (func $work (result i32)
    (local $i i32) (local $x i32)
    i32.const 305419896 local.set $x
    (loop $next
      local.get $x i32.const 1664525 i32.mul i32.const 1013904223 i32.add local.set $x
      local.get $i i32.const 1 i32.add local.tee $i i32.const 100000 i32.lt_u br_if $next)
    local.get $x)
  (func (export "handle") (result i32)
    (local $i i32) (local $n i32)
    call $work i32.const -2115620968 i32.ne if unreachable end
    call $length local.set $n
    (block $done (loop $copy
      local.get $i local.get $n i32.ge_u br_if $done
      local.get $i call $input call $output
      local.get $i i32.const 1 i32.add local.set $i br $copy))
    local.get $n))
