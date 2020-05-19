(module (memory 1)
 (func $ms_add64 (param $i1 i64) (param $i2 i64) (result i64)
    (local $h1 handle) (local $h2 handle)
    (set_local $h1 (new_segment (i32.const 8)))
    (set_local $h2 (new_segment (i32.const 8)))
    (i64.segment_store (get_local $h1) (get_local $i1))
    (i64.segment_store (get_local $h2) (get_local $i2))

    (i64.segment_load (get_local $h1))
    (i64.segment_load (get_local $h2))

    (free_segment (get_local $h1))
    (free_segment (get_local $h2))

    i64.add
  )

  (func (export "_main") (result i64)
    (call $ms_add64 (i64.const 1234567890) (i64.const -1234567890))
  )
  
)