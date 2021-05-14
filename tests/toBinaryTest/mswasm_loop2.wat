(module (memory 1)
  (func $loop (result i32)
    (local $x i32) (local $y i32) (local $i i32)
    (local $addr1 handle) (local $addr2 handle)
    (set_local $x (i32.const 13))
    (set_local $y (i32.const 31))
    (set_local $i (i32.const 0))
    (set_local $addr1 (new_segment (i32.const 64)))
    (set_local $addr2 (segment_slice (get_local $addr1) (i32.const 32) (i32.const 32)))
    (i32.segment_store (get_local $addr1) (i32.const 0))
    (block
      (loop
        (get_local $addr1)
        (i32.segment_load (get_local $addr1))
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
    (i32.segment_load (get_local $addr1))
  )

   (func (export "_main") (result i32)
      (call $loop)
   )

)