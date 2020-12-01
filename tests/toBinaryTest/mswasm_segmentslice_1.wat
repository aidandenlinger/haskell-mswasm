(module (memory 1)
 (func $ms_segmentslice (param $i1 i32) (param $i2 i32) (result i32)
    (local $h1 handle) (local $h2 handle)

    (set_local $h1 (new_segment (i32.const 32)))
    (i32.segment_store (get_local $h1) (get_local $i1))

    (set_local $h2 (segment_slice (get_local $h1) (get_local $i1) (get_local $i2)))
    (i32.segment_store (get_local $h2) (get_local $i2))

    (i32.segment_load (get_local $h2))
    (free_segment (get_local $h2))
  )

   (func (export "_main") (result i32)
    (call $ms_segmentslice (i32.const 5) (i32.const 10))
 )
)