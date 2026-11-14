(module
  (func (export "answer") (result i32) i32.const 42)
  (func (export "handle") (result i32)
    (loop $forever br $forever) i32.const 0))
