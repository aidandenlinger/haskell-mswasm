(module (memory 1)
  (func $ms_freesegment_trap
    (local $h1 handle)

    (set_local $h1 (new_segment (i32.const 8)))
    (free_segment (get_local $h1))
    (free_segment (get_local $h1))
  )

    (func (export "_main")
        (call $ms_freesegment_trap)
    )
)