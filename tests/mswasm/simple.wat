(module
  (func (export "addTwo") (result f32)
    (local $h handle)
    (set_local $h (new_segment (i32.const 4)))
    (get_local $h)
    (f32.add (f32.const 2.0) (f32.const 2.0))
    (f32.segment_store)
    (f32.segment_load (get_local $h))
  )
)