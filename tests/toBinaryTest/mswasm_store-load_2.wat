(module (memory 1)
  (func $store_and_load (param $h handle) (param $i i32) (result i32) 
    (i32.segment_store (get_local $h) (get_local $i)) 
    (i32.segment_load (get_local $h)) 
    (free_segment (get_local $h))
  ) (export "ms_store_and_load" (func $store_and_load))

  (func (export "_main") (result i32)
    (call $store_and_load (new_segment (i32.const 8)) (i32.const 0xfedc6543))
  )
)