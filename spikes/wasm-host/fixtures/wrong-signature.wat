(module
  (import "morphir_bytes_v1" "input_len" (func (result i64)))
  (func (export "answer") (result i32) i32.const 42)
  (func (export "handle") (result i32) i32.const 0))
