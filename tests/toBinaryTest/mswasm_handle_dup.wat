(module (memory 1)
 (func $ms_handle_dup
    (local $h1 handle) (local $h2 handle)
    
    (set_local $h1 (new_segment (i32.const 8)))
    (set_local $h2 (get_local $h1))

    (free_segment (get_local $h1))

    (i32.segment_store (get_local $h2) (i32.const 42))
  )

  (func (export "_main")
    (call $ms_handle_dup)
  ) 
)