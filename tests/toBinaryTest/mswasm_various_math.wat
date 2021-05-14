(module (memory 1)
  (func $loop (result i32)
    (local $x i32) (local $y i32) (local $i i32) (local $addr handle)

    (set_local $x (i32.const 5))
    (set_local $y (i32.const 13))
    (set_local $i (i32.const 1))
    (set_local $addr (new_segment (i32.const 4)))

    (block
      (loop
        (i32.segment_store (get_local $addr) (i32.xor (get_local $y) (get_local $x)))
        (set_local $y (i32.add (get_local $i) (get_local $y)))
        (set_local $x (i32.add (i32.segment_load (get_local $addr)) (get_local $x)))

        (set_local $i (i32.add (get_local $i) (i32.const 1)))
        (br_if 1 (i32.eq (get_local $i) (i32.const 100)))
        (br 0)
      )
    )
    
    (i32.segment_load (get_local $addr))
    (free_segment (get_local $addr))
  )
  (export "loop" (func $loop))

   (func (export "_main") (result i32)
      ;; Trampolines (sometimes referred to as indirect jump vectors) are memory
      ;; locations holding addresses pointing to interrupt service routines, I/O
      ;; routines, etc. Execution jumps into the trampoline and then immediately
      ;; jumps out, or bounces, hence the term trampoline.
      (call $loop)
   )
)