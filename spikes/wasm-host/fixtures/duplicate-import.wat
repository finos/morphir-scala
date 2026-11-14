(module
  (import "bindings" "f" (func $two (param i32 i32) (result i32)))
  (import "bindings" "f" (func $three (param i32 i32 i32) (result i32)))
  (func (export "run") (result i32)
    i32.const 1 i32.const 2 call $two
    i32.const 3 i32.const 4 i32.const 5 call $three i32.add))
