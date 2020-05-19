(module (memory 1)
 (func $ms_handleloadstore_trap (param $i i32) (result handle)
    (local $h1 handle) (local $h2 handle)

    (set_local $h1 (new_segment (i32.const 8)))
    (set_local $h2 (new_segment (i32.const 8)))

    (i32.segment_store (get_local $h1) (get_local $i))
    (handle.segment_store (get_local $h2) (get_local $h1))

    (handle.segment_load (get_local $h1))
  )

  (func (export "_main") (result i32)
    (call $ms_handleloadstore_trap (i32.const 20))
  ) 
)