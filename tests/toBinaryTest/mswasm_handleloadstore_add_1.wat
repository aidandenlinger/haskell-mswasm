(module (memory 1)
(func $ms_handleloadstore_add (param $i1 i32) (param $i2 i32) (result i32)
    (local $h1 handle) (local $h2 handle)

    (set_local $h1 (new_segment (i32.const 8)))
    (set_local $h2 (new_segment (i32.const 8)))

    (i32.segment_store (get_local $h1) (get_local $i1))
    (i32.segment_store (get_local $h2) (get_local $i2))
    (set_local $i2 (i32.segment_load (get_local $h2)))

    (handle.segment_store (get_local $h2) (get_local $h1))
    (set_local $h1 (handle.segment_load (get_local $h2)))

    (i32.segment_store (get_local $h2) (get_local $i2))
    (i32.segment_load (get_local $h2))
    (i32.segment_load (get_local $h1))

    i32.add
    (free_segment (get_local $h2))
    (free_segment (get_local $h1))
  )

  (func (export "_main") (result i32)
    (call $ms_handleloadstore_add (i32.const 11) (i32.const -12))
 )
)