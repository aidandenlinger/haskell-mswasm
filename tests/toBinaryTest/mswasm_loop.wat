(module (memory 1)
  (func $loop (result i32)
    (local $x i32) (local $y i32) (local $i i32) (local $addr handle)
    (set_local $x (i32.const 5))
    (set_local $y (i32.const 7))
    (set_local $i (i32.const 0))
    (set_local $addr (new_segment (i32.const 32)))
    (i32.segment_store (get_local $addr) (i32.const 0))
    (block
      (loop
        (get_local $addr)
        (i32.segment_load (get_local $addr))
        (get_local $x)
        (i32.add)
        (get_local $y)
        (i32.add)
        (i32.segment_store)
        (get_local $i)
        (i32.const 1)
        (i32.add)
        (set_local $i)
        (br_if 1 (i32.eq (get_local $i) (i32.const 10000)))
        (br 0)
      )
    )
    (i32.segment_load (get_local $addr))
  )

   (func (export "_main") (result i32)
      (call $loop)
   )

)