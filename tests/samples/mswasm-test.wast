;; MS-Wasm tests

(module
  (memory 1)
  (func $store_and_load (param $h handle) (param $i i32) (result i32)
    (i32.segment_store (get_local $h) (get_local $i))
    (i32.segment_load (get_local $h))
  ) (export "store_and_load" (func $store_and_load))
  (func (export "test_store_and_load") (param $i i32) (result i32)
    (call $store_and_load (new_segment (i32.const 8)) (get_local $i))
  )

  (func (export "ms_add") (param $i1 i32) (param $i2 i32) (result i32)
    (local $h1 handle) (local $h2 handle)
    (set_local $h1 (new_segment (i32.const 8)))
    (set_local $h2 (new_segment (i32.const 8)))
    (i32.segment_store (get_local $h1) (get_local $i1))
    (i32.segment_store (get_local $h2) (get_local $i2))

    (i32.segment_load (get_local $h1))
    (i32.segment_load (get_local $h2))
    i32.add
  )

 (func (export "ms_handleaddsub") (param $i1 i32) (param $i2 i32) (result i32)
    (local $h1 handle)

    (set_local $h1 (new_segment (i32.const 8)))
    (i32.segment_store (get_local $h1) (get_local $i1))

    (set_local $h1 (handle.add (i32.const 1) (get_local $h1)))

    (i32.segment_store (get_local $h1) (get_local $i2))

    (set_local $h1 (handle.sub (i32.const 1) (get_local $h1)))
    (i32.segment_load (get_local $h1))
    
    (set_local $h1 (handle.add (i32.const 1) (get_local $h1)))
    (i32.segment_load (get_local $h1))

    i32.sub
  )
)


(assert_return (invoke "test_store_and_load" (i32.const 10)) (i32.const 10))
(assert_return (invoke "test_store_and_load" (i32.const 0xfedc6543)) (i32.const 0xfedc6543))

(assert_return (invoke "ms_add" (i32.const 10) (i32.const 1)) (i32.const 11))
(assert_return (invoke "ms_add" (i32.const 45) (i32.const -45)) (i32.const 0))
(assert_return (invoke "ms_add" (i32.const 0) (i32.const 0x12345678)) (i32.const 0x12345678))

(assert_return (invoke "ms_handleaddsub" (i32.const 10) (i32.const 1)) (i32.const 9))

