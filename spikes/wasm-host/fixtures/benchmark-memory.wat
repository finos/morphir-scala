(module
  (import "morphir_bytes_v1" "input_len" (func $length (result i32)))
  (import "morphir_bytes_v1" "input_byte" (func $input (param i32) (result i32)))
  (import "morphir_bytes_v1" "output_byte" (func $output (param i32)))
  ;; Leave headroom for Extism's kernel/input allocation; the measured loop touches 64 KiB.
  (memory 1 4096)
  (func (export "answer") (result i32) i32.const 42)
  ;; Eight 64 KiB fill/read passes. Expected checksum: 8 * 256 * sum(0..255).
  (func $work (result i32)
    (local $pass i32) (local $i i32) (local $sum i32)
    (loop $passes
      i32.const 0 local.set $i
      (loop $fill
        local.get $i local.get $i i32.store8
        local.get $i i32.const 1 i32.add local.tee $i i32.const 65536 i32.lt_u br_if $fill)
      i32.const 0 local.set $i
      (loop $read
        local.get $sum local.get $i i32.load8_u i32.add local.set $sum
        local.get $i i32.const 1 i32.add local.tee $i i32.const 65536 i32.lt_u br_if $read)
      local.get $pass i32.const 1 i32.add local.tee $pass i32.const 8 i32.lt_u br_if $passes)
    local.get $sum)
  (func (export "handle") (result i32)
    (local $i i32) (local $n i32)
    call $work i32.const 66846720 i32.ne if unreachable end
    call $length local.set $n
    (block $done (loop $copy
      local.get $i local.get $n i32.ge_u br_if $done
      local.get $i call $input call $output
      local.get $i i32.const 1 i32.add local.set $i br $copy))
    local.get $n))
