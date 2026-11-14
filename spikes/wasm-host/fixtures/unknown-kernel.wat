(module
  (import "extism:host/env" "not_a_kernel_function" (func))
  (func (export "answer") (result i32) i32.const 42)
  (func (export "handle") (result i32) i32.const 0))
