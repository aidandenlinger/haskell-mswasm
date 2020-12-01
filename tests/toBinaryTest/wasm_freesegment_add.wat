(module (memory 1)
 (func $ms_freesegment_add (param $i1 i32) (result i32)
    (local $i2 i32) (local $i3 i32)

    (set_local $i2 (i32.const 8))
    (set_local $i3 (i32.const 16))
    (i32.store8 (get_local $i2) (get_local $i1))
    
    (set_local $i1 (i32.add (i32.load8_s (get_local $i2)) (i32.const 1)))

    (get_local $i1)
  )

 (func (export "_main") (result i32)
    (call $ms_freesegment_add (i32.const 23))
 )
)