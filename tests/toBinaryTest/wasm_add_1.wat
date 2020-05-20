(module (memory 1)
 (func $ms_add (param $i1 i32) (param $i2 i32) (result i32)
    (local $i3 handle) (local $i4 handle)
    (set_local $i3 (i32.const 8))
    (set_local $i4 (i32.const 16))
    (i32.store8 (get_local $i3) (get_local $i1))
    (i32.store8 (get_local $i4) (get_local $i2))

    (i32.load8_s (get_local $i3))
    (i32.load8_s (get_local $i4))

    i32.add
  )

 (func (export "_main") (result i32)
    (call $ms_add (i32.const 10) (i32.const 1))
 )
)