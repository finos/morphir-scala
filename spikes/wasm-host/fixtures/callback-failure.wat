(module
  (import "morphir_bytes_v1" "output_byte" (func $output (param i32)))
  (func (export "answer") (result i32) i32.const 42)
  (func (export "handle") (result i32) i32.const 256 call $output i32.const 0))
