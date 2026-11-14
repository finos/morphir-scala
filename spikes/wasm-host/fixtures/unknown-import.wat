(module
  (import "unapproved" "read_file" (func))
  (func $start (loop $forever br $forever))
  (start $start)
  (func (export "answer") (result i32) i32.const 42)
  (func (export "handle") (result i32) i32.const 0))
