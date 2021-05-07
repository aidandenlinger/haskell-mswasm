(module
  (func (export "addTwo") (result i32)
    (local $h handle)
    (set_local $h (new_segment (i32.const 4)))
    (i32.add (i32.const 2) (i32.const 2))
    (i32.segment_store (get_local $h))
    (i32.segment_load (get_local $h))
  )
)