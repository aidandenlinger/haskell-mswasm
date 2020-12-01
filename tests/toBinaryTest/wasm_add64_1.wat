(module (memory 1)
 (func $ms_add64 (param $i1 i64) (param $i2 i64) (result i64)
    (local $i3 i64) (local $i4 i64)
    (set_local $i3 (i64.const 8))
    (set_local $i4 (i64.const 16))
    (i64.store16 (get_local $i3) (get_local $i1))
    (i64.store16 (get_local $i4) (get_local $i2))
    

    (i64.load16_s (get_local $i3))
    (i64.load16_s (get_local $i4))

    i64.add
  )

  (func (export "_main") (result i64)
    (call $ms_add64 (i64.const 1234567890) (i64.const -1234567890))
  )
  
)