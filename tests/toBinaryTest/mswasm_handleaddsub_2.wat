(module (memory 1)
 (func $ms_handleaddsub (param $i1 i32) (param $i2 i32) (result i32)
    (local $h1 handle)

    (set_local $h1 (new_segment (i32.const 12)))
    (i32.segment_store (get_local $h1) (get_local $i1))

    (set_local $h1 (handle.add (i32.const 8) (get_local $h1)))

    (i32.segment_store (get_local $h1) (get_local $i2))

    (set_local $h1 (handle.sub (i32.const 8) (get_local $h1)))
    (i32.segment_load (get_local $h1))
    
    (set_local $h1 (handle.add (i32.const 8) (get_local $h1)))
    (i32.segment_load (get_local $h1))
    (free_segment (get_local $h1))

    i32.sub
  )

  (func (export "_main") (result i32)
    (call $ms_handleaddsub (i32.const 1) (i32.const 20))
  ) 
)