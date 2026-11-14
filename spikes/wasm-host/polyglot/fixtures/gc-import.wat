(module
  (type $bytes (array (mut i8)))
  (import "bindings" "f" (func $f (param (ref null $bytes) i32 i32) (result i32)))
  (func (export "run") (result i32)
    i32.const 4 array.new_default $bytes
    i32.const 2 i32.const 3 call $f))
