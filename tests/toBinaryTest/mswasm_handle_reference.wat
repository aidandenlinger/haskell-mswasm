(module (memory 1)
 (func $ms_handle_reference (result i32)
    (local $h1 handle) (local $h2 handle)
    
    (set_local $h1 (new_segment (i32.const 8)))
    (set_local $h2 (get_local $h1))
    
    (set_local $h1 (handle.add (i32.const 1) (get_local $h1)))
    
    (handle.get_offset (get_local $h2))
  )

  (func (export "_main") (result i32)
    (call $ms_handle_reference)
  ) 
)