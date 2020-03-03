;; MS-Wasm tests

(module
  (memory 1)
  (func (export "free new_segment") (param $i i32)
	(free_segment (new_segment (get_local $i)))
  )
  (func (export "i64_load") (param $i i64) (result i64)
	(i64.store8 (i32.const 8) (get_local $i))
	(i64.load8_s (i32.const 8))
  )
)


(assert_return (invoke "free new_segment" (i32.const 10)) (i32.const -1))
(assert_return (invoke "free new_segment" (i32.const 0xfedc6543)) (i32.const 0x43))

(assert_return (invoke "i64_load" (i64.const -1)) (i64.const -1))
(assert_return (invoke "i64_load" (i64.const 100)) (i64.const 100))
(assert_return (invoke "i64_load" (i64.const 0xfedcba9856346543)) (i64.const 0x43))
(assert_return (invoke "i64_load" (i64.const 0x3456436598bacdef)) (i64.const 0xffffffffffffffef))