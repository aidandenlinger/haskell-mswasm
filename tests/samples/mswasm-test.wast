;; MS-Wasm tests

(module
  (memory 1)
  (func $store_and_load (param $h handle) (param $i i32) (result i32)
    (i32.segment_store (get_local $h) (get_local $i))
    (i32.segment_load (get_local $h))
    (free_segment (get_local $h))
  ) (export "ms_store_and_load" (func $store_and_load))

  (func (export "ms_test_store_and_load") (param $i i32) (result i32)
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

    (free_segment (get_local $h1))
    (free_segment (get_local $h2))

    i32.add
  )

  (func (export "ms_add64") (param $i1 i64) (param $i2 i64) (result i64)
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
    (free_segment (get_local $h1))

    i32.sub
  )

  (func (export "ms_segmentslice") (param $i1 i32) (param $i2 i32) (result i32)
    (local $h1 handle) (local $h2 handle) (local )

    (set_local $h1 (new_segment (i32.const 32)))
    (i32.segment_store (get_local $h1) (get_local $i1))

    (set_local $h2 (segment_slice (get_local $h1) (get_local $i1) (get_local $i2)))
    (i32.segment_store (get_local $h2) (get_local $i2))

    (i32.segment_load (get_local $h2))
    (free_segment (get_local $h2))
  )

  (func (export "ms_handleloadstore") (param $i1 i32) (result i32)
    (local $h1 handle) (local $h2 handle) (local $i2 i32)

    (set_local $h1 (new_segment (i32.const 16)))

    (handle.segment_store (get_local $h1) (new_segment (i32.const 8)))
    (i32.segment_store (handle.segment_load (get_local $h1)) (get_local $i1))

    (set_local $h2 (handle.segment_load (get_local $h1)))
    (set_local $i2 (i32.segment_load (get_local $h2)))

    (free_segment (get_local $h2))
    (get_local $i2)
  )

  (func (export "ms_freesegment_add") (param $i1 i32) (result i32)
    (local $h1 handle) (local $h2 handle)

    (set_local $h1 (new_segment (i32.const 32)))
    (set_local $h2 (new_segment (i32.const 32)))
    (i32.segment_store (get_local $h1) (get_local $i1))
    
    (set_local $i1 (i32.add (i32.segment_load (get_local $h1)) (i32.const 1)))

    (free_segment (get_local $h1))
    (free_segment (get_local $h2))
    (get_local $i1)
  )

  (func (export "ms_freesegment_trap")
    (local $h1 handle)

    (set_local $h1 (new_segment (i32.const 8)))
    (free_segment (get_local $h1))
    (free_segment (get_local $h1))
  )

  (func (export "ms_handleloadstore_add") (param $i1 i32) (param $i2 i32) (result i32)
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

  (func (export "ms_handleloadstore_trap") (param $i i32) (result handle)
    (local $h1 handle) (local $h2 handle)

    (set_local $h1 (new_segment (i32.const 8)))
    (set_local $h2 (new_segment (i32.const 8)))

    (i32.segment_store (get_local $h1) (get_local $i))
    (handle.segment_store (get_local $h2) (get_local $h1))

    (handle.segment_load (get_local $h1))
  )
)

(assert_return (invoke "ms_test_store_and_load" (i32.const 10)) (i32.const 10))
(assert_return (invoke "ms_test_store_and_load" (i32.const 0xfedc6543)) (i32.const 0xfedc6543))

(assert_return (invoke "ms_add" (i32.const 10) (i32.const 1)) (i32.const 11))
(assert_return (invoke "ms_add" (i32.const 45) (i32.const -45)) (i32.const 0))
(assert_return (invoke "ms_add" (i32.const 0) (i32.const 0x12345678)) (i32.const 0x12345678))

(assert_return (invoke "ms_add64" (i64.const 1234567890) (i64.const -1234567890)) (i64.const 0))
(assert_return (invoke "ms_add64" (i64.const 1234567890) (i64.const 5)) (i64.const 1234567895))

(assert_return (invoke "ms_handleaddsub" (i32.const 10) (i32.const 1)) (i32.const 9))
(assert_return (invoke "ms_handleaddsub" (i32.const 1) (i32.const 20)) (i32.const -19))

(assert_return (invoke "ms_segmentslice" (i32.const 5) (i32.const 10)) (i32.const 10))
(assert_trap   (invoke "ms_segmentslice" (i32.const 40) (i32.const 10)) "out-of-bounds handle")

(assert_return (invoke "ms_handleloadstore" (i32.const 17)) (i32.const 17))

(assert_return (invoke "ms_freesegment_add" (i32.const 23)) (i32.const 24))
(assert_trap   (invoke "ms_freesegment_trap") "segment cannot be freed")

(assert_return (invoke "ms_handleloadstore_add" (i32.const 11) (i32.const -12)) (i32.const -1))
(assert_return (invoke "ms_handleloadstore_add" (i32.const 1023) (i32.const 17)) (i32.const 1040))
(assert_trap   (invoke "ms_handleloadstore_trap" (i32.const 20)) "invalid handle load")