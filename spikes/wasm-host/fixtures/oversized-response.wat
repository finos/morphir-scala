(module
  (import "morphir_bytes_v1" "output_byte" (func $output (param i32)))
  (func (export "answer") (result i32) i32.const 42)
  (func (export "handle") (result i32)
    (local $n i32)
    (loop $more
      i32.const 42 call $output
      local.get $n i32.const 1 i32.add local.tee $n
      i32.const 65537 i32.lt_u br_if $more)
    i32.const 65537))
