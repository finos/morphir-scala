(module
  (import "morphir_bytes_v1" "input_len" (func $length (result i32)))
  (import "morphir_bytes_v1" "input_byte" (func $input (param i32) (result i32)))
  (import "morphir_bytes_v1" "output_byte" (func $output (param i32)))
  (func (export "answer") (result i32) i32.const 42)
  (func (export "handle") (result i32)
    (local $i i32) (local $n i32)
    call $length local.set $n
    (block $done (loop $copy
      local.get $i local.get $n i32.ge_u br_if $done
      local.get $i call $input call $output
      local.get $i i32.const 1 i32.add local.set $i
      br $copy))
    local.get $n))
