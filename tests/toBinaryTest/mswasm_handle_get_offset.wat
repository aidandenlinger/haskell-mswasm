(module (memory 1)
 (func $ms_handle_offset (result i32)
    (handle.get_offset (handle.add (i32.const 4) (new_segment (i32.const 8))))
  )

 (func (export "_main") (result i32)
    (call $ms_handle_offset)
 )
)