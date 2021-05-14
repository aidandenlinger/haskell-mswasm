(module (memory 1)
  ;; (data (i32.const 0) "123AAAA89")
  (func $loop (result i32)
    (local $addr handle)
    (local $checki32 i32)

    ;; Find the 4 bytes that equal to "AAAA" (0x41414141) 1094795585
    (set_local $addr (new_segment (i32.const 12)))
    (set_local $checki32 (i32.const 0))

    (i64.segment_store (get_local $addr) (i64.const 4053592888132710961))
    (i32.segment_store (handle.add (i32.const 8) (get_local $addr)) (i32.const 57))

    (block
      (loop
      
        (set_local $checki32 (i32.segment_load (get_local $addr)))

        (br_if 1 (i32.eq (get_local $checki32) (i32.const 1094795585))) ;; should break once addr == 3

        (set_local $addr (handle.add (i32.const 1) (get_local $addr)))

        ;; should not break at this point (addr == 6 means something is logically wrong)
        (br_if 1 (i32.eq (handle.get_offset (get_local $addr)) (i32.const 6))) 
        (br 0)
      )
    )
    (handle.get_offset (get_local $addr))
  )
  (export "loop" (func $loop))

   (func (export "_main") (result i32)
      ;; Trampolines (sometimes referred to as indirect jump vectors) are memory
      ;; locations holding addresses pointing to interrupt service routines, I/O
      ;; routines, etc. Execution jumps into the trampoline and then immediately
      ;; jumps out, or bounces, hence the term trampoline.
      (call $loop)
   )
)