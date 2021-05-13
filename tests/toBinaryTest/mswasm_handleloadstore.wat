(module (memory 1)
(func $ms_handleloadstore (param $i1 i32) (result i32)
    (local $h1 handle) (local $h2 handle) (local $i2 i32)

    (set_local $h1 (new_segment (i32.const 8)))
    (handle.segment_store (get_local $h1) (new_segment (i32.const 8)))
    (i32.segment_store (handle.segment_load (get_local $h1)) (get_local $i1))

    (set_local $h2 (handle.segment_load (get_local $h1)))
    (set_local $i2 (i32.segment_load (get_local $h2)))

    (free_segment (get_local $h2))
    (get_local $i2)
  )

    (func (export "_main") (result i32)
        (call $ms_handleloadstore (i32.const 17)) 
    )

)