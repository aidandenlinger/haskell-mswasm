(module
  (memory 1)

  (func $salsa20 (result i32)
	(local $addr handle)
    (local $i i32)
    (local $scratch i32)
    (local $in_index i32)

	;; init segment
	(set_local $addr (new_segment (i32.const 128)))

    ;; init 'const u32 input[16]' --- (offset 0 - 63)
    (set_local $i (i32.const 0))
    (block
      (loop
        (br_if 1 (i32.ge_u (get_local $i) (i32.const 64)))
	  (i32.segment_store (handle.set_offset (get_local $addr) (get_local $i)) (i32.const 8888))
	  (set_local $i (i32.add (get_local $i) (i32.const 4)))
	  (br 0)
	)
      )

    ;; init local 'u32 x[16]' --- (offset 64 - 127)
    (set_local $i (i32.const 64))
    (block
      (loop
	(br_if 1 (i32.ge_u (get_local $i) (i32.const 128)))
	  (set_local $in_index (i32.sub (get_local $i) (i32.const 64)))
	  (i32.segment_store (handle.set_offset (get_local $addr) (get_local $i))
	  	                 (i32.segment_load (handle.set_offset (get_local $addr) (get_local $in_index))))
	  (set_local $i (i32.add (get_local $i) (i32.const 4)))
	  (br 0)
	)
      )

    ;; bit-muck
    (set_local $i (i32.const 0))
    (set_local $scratch (i32.const 0))
    (block
      (loop
        (br_if 1 (i32.ge_u (get_local $i) (i32.const 10)))
          ;; x[ 4] = XOR(x[ 4],ROTATE(PLUS(x[ 0],x[12]), 7));
          (set_local $scratch (i32.add (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 64))) 
		                               (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 112)))))
	  (set_local $scratch (i32.rotl (get_local $scratch) (i32.const 7)))
	  (set_local $scratch (i32.xor (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 80))) 
	                               (get_local $scratch)))
	  (i32.segment_store (handle.set_offset (get_local $addr) (i32.const 80)) (get_local $scratch))
          ;; x[ 8] = XOR(x[ 8],ROTATE(PLUS(x[ 4],x[ 0]), 9));
          (set_local $scratch (i32.add (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 80))) 
		                               (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 64)))))
	  (set_local $scratch (i32.rotl (get_local $scratch) (i32.const 9)))
	  (set_local $scratch (i32.xor (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 96))) 
	                               (get_local $scratch)))
	  (i32.segment_store (handle.set_offset (get_local $addr) (i32.const 96)) (get_local $scratch))
          ;; x[12] = XOR(x[12],ROTATE(PLUS(x[ 8],x[ 4]),13));
          (set_local $scratch (i32.add (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 96))) 
		                               (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 80)))))
	  (set_local $scratch (i32.rotl (get_local $scratch) (i32.const 13)))
	  ;; load at 112
	  (set_local $scratch (i32.xor (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 112))) 
	                               (get_local $scratch)))
	  ;; store at 112
	  (i32.segment_store (handle.set_offset (get_local $addr) (i32.const 112)) (get_local $scratch))
	  ;; x[ 0] = XOR(x[ 0],ROTATE(PLUS(x[12],x[ 8]),18));
	  ;; load at 112, 96
          (set_local $scratch (i32.add (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 112))) 
		                               (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 96)))))
	  (set_local $scratch (i32.rotl (get_local $scratch) (i32.const 18)))
	  ;; load 64
	  (set_local $scratch (i32.xor (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 64))) 
	                               (get_local $scratch)))
	  ;; store at 64
	  (i32.segment_store (handle.set_offset (get_local $addr) (i32.const 64)) (get_local $scratch))
	  ;; x[ 9] = XOR(x[ 9],ROTATE(PLUS(x[ 5],x[ 1]), 7));
	  ;; load at 84, 68
          (set_local $scratch (i32.add (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 84))) 
		                               (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 68)))))
	  (set_local $scratch (i32.rotl (get_local $scratch) (i32.const 7)))
	  ;; load at 100
	  (set_local $scratch (i32.xor (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 100))) 
	                               (get_local $scratch)))
	  ;; store at 100
	  (i32.segment_store (handle.set_offset (get_local $addr) (i32.const 100)) (get_local $scratch))
	  ;; x[13] = XOR(x[13],ROTATE(PLUS(x[ 9],x[ 5]), 9));
	  ;; load at 100, 84
          (set_local $scratch (i32.add (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 100))) 
		                               (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 84)))))
	  (set_local $scratch (i32.rotl (get_local $scratch) (i32.const 9)))
	  ;; load at 116
	  (set_local $scratch (i32.xor (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 116))) 
	                               (get_local $scratch)))
	  ;; store at 116
	  (i32.segment_store (handle.set_offset (get_local $addr) (i32.const 116)) (get_local $scratch))
	  ;; x[ 1] = XOR(x[ 1],ROTATE(PLUS(x[13],x[ 9]),13));
	  ;; load at 116, 100
          (set_local $scratch (i32.add (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 116))) 
		                               (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 100)))))
	  (set_local $scratch (i32.rotl (get_local $scratch) (i32.const 13)))
	  ;; load at 68
	  (set_local $scratch (i32.xor (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 68))) 
	                               (get_local $scratch)))
	  ;; store at 68
	  (i32.segment_store (handle.set_offset (get_local $addr) (i32.const 68)) (get_local $scratch))
	  ;; x[ 5] = XOR(x[ 5],ROTATE(PLUS(x[ 1],x[13]),18));
	  ;; load at 68, 116
          (set_local $scratch (i32.add (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 68)))
		                               (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 116)))))
	  (set_local $scratch (i32.rotl (get_local $scratch) (i32.const 18)))
	  ;; load 84
	  (set_local $scratch (i32.xor (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 84))) 
	                               (get_local $scratch)))
	  ;; store 84
	  (i32.segment_store (handle.set_offset (get_local $addr) (i32.const 84)) (get_local $scratch))
	  ;; x[14] = XOR(x[14],ROTATE(PLUS(x[10],x[ 6]), 7));
	  ;; load at 104, 88
          (set_local $scratch (i32.add (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 104))) 
		                               (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 88)))))
	  (set_local $scratch (i32.rotl (get_local $scratch) (i32.const 7)))
	  ;; load 120
	  (set_local $scratch (i32.xor (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 120))) 
	                               (get_local $scratch)))
	  ;; store 120
	  (i32.segment_store (handle.set_offset (get_local $addr) (i32.const 120)) (get_local $scratch))
	  ;; x[ 2] = XOR(x[ 2],ROTATE(PLUS(x[14],x[10]), 9));
	  ;; load at 120, 104
          (set_local $scratch (i32.add (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 120))) 
		                               (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 104)))))
	  (set_local $scratch (i32.rotl (get_local $scratch) (i32.const 9)))
	  ;; load at 
	  (set_local $scratch (i32.xor (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 72))) 
	                               (get_local $scratch)))
	  ;; store at 72
	  (i32.segment_store (handle.set_offset (get_local $addr) (i32.const 72)) (get_local $scratch))
	  ;; x[ 6] = XOR(x[ 6],ROTATE(PLUS(x[ 2],x[14]),13));
	  ;; load at 72, 120
          (set_local $scratch (i32.add (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 72))) 
		                               (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 120)))))
	  (set_local $scratch (i32.rotl (get_local $scratch) (i32.const 13)))
	  ;; load 88
	  (set_local $scratch (i32.xor (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 88))) 
	                               (get_local $scratch)))
	  ;; store 88
	  (i32.segment_store (handle.set_offset (get_local $addr) (i32.const 88)) (get_local $scratch))
	  ;; x[10] = XOR(x[10],ROTATE(PLUS(x[ 6],x[ 2]),18));
	  ;; load at 88, 72
          (set_local $scratch (i32.add (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 88))) 
		                               (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 72)))))
	  (set_local $scratch (i32.rotl (get_local $scratch) (i32.const 18)))
	  ;; load at 104
	  (set_local $scratch (i32.xor (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 104))) 
	                               (get_local $scratch)))
	  ;; store at 104
	  (i32.segment_store (handle.set_offset (get_local $addr) (i32.const 104)) (get_local $scratch))
	  ;; x[ 3] = XOR(x[ 3],ROTATE(PLUS(x[15],x[11]), 7));
	  ;; load at 124, 108
          (set_local $scratch (i32.add (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 124))) 
		                               (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 108)))))
	  (set_local $scratch (i32.rotl (get_local $scratch) (i32.const 7)))
	  ;; load at 76
	  (set_local $scratch (i32.xor (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 76))) 
	                               (get_local $scratch)))
	  ;; store at 76
	  (i32.segment_store (handle.set_offset (get_local $addr) (i32.const 76)) (get_local $scratch))
	  ;; x[ 7] = XOR(x[ 7],ROTATE(PLUS(x[ 3],x[15]), 9));
	  ;; load at 76, 124
          (set_local $scratch (i32.add (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 76))) 
		                               (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 124)))))
	  (set_local $scratch (i32.rotl (get_local $scratch) (i32.const 9)))
	  ;; load at 92
	  (set_local $scratch (i32.xor (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 92))) 
	                               (get_local $scratch)))
	  ;; store at 92
	  (i32.segment_store (handle.set_offset (get_local $addr) (i32.const 92)) (get_local $scratch))
	  ;; x[11] = XOR(x[11],ROTATE(PLUS(x[ 7],x[ 3]),13));
	  ;; load at 92, 76
          (set_local $scratch (i32.add (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 92))) 
		                               (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 76)))))
	  (set_local $scratch (i32.rotl (get_local $scratch) (i32.const 13)))
	  ;; load at 108
	  (set_local $scratch (i32.xor (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 108))) 
	                               (get_local $scratch)))
	  ;; store at 108
	  (i32.segment_store (handle.set_offset (get_local $addr) (i32.const 108)) (get_local $scratch))
	  ;; x[15] = XOR(x[15],ROTATE(PLUS(x[11],x[ 7]),18));
	  ;; load at 108, 92
          (set_local $scratch (i32.add (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 108))) 
		                               (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 92)))))
	  (set_local $scratch (i32.rotl (get_local $scratch) (i32.const 18)))
	  ;; lood at 124
	  (set_local $scratch (i32.xor (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 124))) 
	                               (get_local $scratch)))
	  ;; store at 124
	  (i32.segment_store (handle.set_offset (get_local $addr) (i32.const 124)) (get_local $scratch))
	  ;; x[ 1] = XOR(x[ 1],ROTATE(PLUS(x[ 0],x[ 3]), 7));
	  ;; load at 64, 76
          (set_local $scratch (i32.add (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 64))) 
		                               (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 76)))))
	  (set_local $scratch (i32.rotl (get_local $scratch) (i32.const 7)))
	  ;; load at 68
	  (set_local $scratch (i32.xor (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 68))) 
	                               (get_local $scratch)))
	  ;; store at 68
	  (i32.segment_store (handle.set_offset (get_local $addr) (i32.const 68)) (get_local $scratch))
	  ;; x[ 2] = XOR(x[ 2],ROTATE(PLUS(x[ 1],x[ 0]), 9));
	  ;; load at 68, 64
          (set_local $scratch (i32.add (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 68))) 
		                               (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 64)))))
	  (set_local $scratch (i32.rotl (get_local $scratch) (i32.const 9)))
	  ;; load at 72
	  (set_local $scratch (i32.xor (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 72))) 
	                               (get_local $scratch)))
	  ;; store at 72
	  (i32.segment_store (handle.set_offset (get_local $addr) (i32.const 72)) (get_local $scratch))
	  ;; x[ 3] = XOR(x[ 3],ROTATE(PLUS(x[ 2],x[ 1]),13));
	  ;; load at 72, 68
          (set_local $scratch (i32.add (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 72))) 
		                               (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 68)))))
	  (set_local $scratch (i32.rotl (get_local $scratch) (i32.const 13)))
	  ;; load at 76
	  (set_local $scratch (i32.xor (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 76))) 
	                               (get_local $scratch)))
	  ;; store at 76
	  (i32.segment_store (handle.set_offset (get_local $addr) (i32.const 76)) (get_local $scratch))
	  ;; x[ 0] = XOR(x[ 0],ROTATE(PLUS(x[ 3],x[ 2]),18));
	  ;; store at 76, 72
          (set_local $scratch (i32.add (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 76))) 
		                               (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 72)))))
	  (set_local $scratch (i32.rotl (get_local $scratch) (i32.const 18)))
	  ;; load at 64
	  (set_local $scratch (i32.xor (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 64))) 
	                               (get_local $scratch)))
	  ;; store at 64
	  (i32.segment_store (handle.set_offset (get_local $addr) (i32.const 64)) (get_local $scratch))
	  ;; x[ 6] = XOR(x[ 6],ROTATE(PLUS(x[ 5],x[ 4]), 7));
	  ;; load at 84, 80
          (set_local $scratch (i32.add (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 84))) 
		                               (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 80)))))
	  (set_local $scratch (i32.rotl (get_local $scratch) (i32.const 7)))
	  ;; load at 88
	  (set_local $scratch (i32.xor (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 88))) 
	                               (get_local $scratch)))
	  ;; store at 88
	  (i32.segment_store (handle.set_offset (get_local $addr) (i32.const 88)) (get_local $scratch))
	  ;; x[ 7] = XOR(x[ 7],ROTATE(PLUS(x[ 6],x[ 5]), 9));
	  ;; load at 88, 84
          (set_local $scratch (i32.add (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 88))) 
		                               (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 84)))))
	  (set_local $scratch (i32.rotl (get_local $scratch) (i32.const 9)))
	  ;; load at 92
	  (set_local $scratch (i32.xor (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 92))) 
	                               (get_local $scratch)))
	  ;; store at 92
	  (i32.segment_store (handle.set_offset (get_local $addr) (i32.const 92)) (get_local $scratch))
	  ;; x[ 4] = XOR(x[ 4],ROTATE(PLUS(x[ 7],x[ 6]),13));
	  ;; load at 92, 88
          (set_local $scratch (i32.add (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 92))) 
		                               (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 88)))))
	  (set_local $scratch (i32.rotl (get_local $scratch) (i32.const 13)))
	  ;; load at 80
	  (set_local $scratch (i32.xor (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 80))) 
	                               (get_local $scratch)))
	  ;; store at 80
	  (i32.segment_store (handle.set_offset (get_local $addr) (i32.const 80)) (get_local $scratch))
	  ;; x[ 5] = XOR(x[ 5],ROTATE(PLUS(x[ 4],x[ 7]),18));
	  ;; load at 80, 92
          (set_local $scratch (i32.add (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 80))) 
		                               (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 92)))))
	  (set_local $scratch (i32.rotl (get_local $scratch) (i32.const 18)))
	  ;; load at 84
	  (set_local $scratch (i32.xor (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 84))) 
	                               (get_local $scratch)))
	  ;; store at 84
	  (i32.segment_store (handle.set_offset (get_local $addr) (i32.const 84)) (get_local $scratch))
	  ;; x[11] = XOR(x[11],ROTATE(PLUS(x[10],x[ 9]), 7));
	  ;; load at 104, 100
          (set_local $scratch (i32.add (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 104))) 
		                               (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 100)))))
	  (set_local $scratch (i32.rotl (get_local $scratch) (i32.const 7)))
		;; load at 108
	  (set_local $scratch (i32.xor (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 108))) 
	                               (get_local $scratch)))
		;; store at 108
	  (i32.segment_store (handle.set_offset (get_local $addr) (i32.const 108)) (get_local $scratch))
	  ;; x[ 8] = XOR(x[ 8],ROTATE(PLUS(x[11],x[10]), 9));
	  ;; load at 108, 104
          (set_local $scratch (i32.add (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 108))) 
		                               (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 104)))))
	  (set_local $scratch (i32.rotl (get_local $scratch) (i32.const 9)))
	  ;; load at 96
	  (set_local $scratch (i32.xor (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 96))) 
	                               (get_local $scratch)))
	  ;; store at 96
	  (i32.segment_store (handle.set_offset (get_local $addr) (i32.const 96)) (get_local $scratch))
	  ;; x[ 9] = XOR(x[ 9],ROTATE(PLUS(x[ 8],x[11]),13));
	  ;; load at 96, 108
          (set_local $scratch (i32.add (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 96))) 
		                               (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 108)))))
	  ;; load at 13
	  (set_local $scratch (i32.rotl (get_local $scratch) (i32.const 13)))
	  ;; load at 100
	  (set_local $scratch (i32.xor (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 100))) 
	                               (get_local $scratch)))
	  ;; store at 100
	  (i32.segment_store (handle.set_offset (get_local $addr) (i32.const 100)) (get_local $scratch))
	  ;; x[10] = XOR(x[10],ROTATE(PLUS(x[ 9],x[ 8]),18));
	  ;; load at 100, 96
          (set_local $scratch (i32.add (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 100))) 
		                               (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 96)))))
	  (set_local $scratch (i32.rotl (get_local $scratch) (i32.const 18)))
	  ;; load at 104
	  (set_local $scratch (i32.xor (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 104))) 
	                               (get_local $scratch)))
	  ;; store at 104
	  (i32.segment_store (handle.set_offset (get_local $addr) (i32.const 104)) (get_local $scratch))
	  ;; x[12] = XOR(x[12],ROTATE(PLUS(x[15],x[14]), 7));
	  ;; load at 124, 120
          (set_local $scratch (i32.add (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 124))) 
		                               (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 120)))))
	  (set_local $scratch (i32.rotl (get_local $scratch) (i32.const 7)))
	  ;; load at 112
	  (set_local $scratch (i32.xor (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 112))) 
	                               (get_local $scratch)))
	  (i32.segment_store (handle.set_offset (get_local $addr) (i32.const 112)) (get_local $scratch))
	  ;; x[13] = XOR(x[13],ROTATE(PLUS(x[12],x[15]), 9));
	  ;; load at 112, 124
          (set_local $scratch (i32.add (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 112))) 
		                               (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 124)))))
	  (set_local $scratch (i32.rotl (get_local $scratch) (i32.const 9)))
	  ;; load at 116
	  (set_local $scratch (i32.xor (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 116))) 
	                               (get_local $scratch)))
	  (i32.segment_store (handle.set_offset (get_local $addr) (i32.const 116)) (get_local $scratch))
	  ;; x[14] = XOR(x[14],ROTATE(PLUS(x[13],x[12]),13));
	  ;; load at 116, 112
          (set_local $scratch (i32.add (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 116))) 
		                               (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 112)))))
	  (set_local $scratch (i32.rotl (get_local $scratch) (i32.const 13)))
	  ;; load at 120
	  (set_local $scratch (i32.xor (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 120))) 
	                               (get_local $scratch)))
	  (i32.segment_store (handle.set_offset (get_local $addr) (i32.const 120)) (get_local $scratch))
	  ;; x[15] = XOR(x[15],ROTATE(PLUS(x[14],x[13]),18));
	  ;; load at 120, 116
          (set_local $scratch (i32.add (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 120))) 
		                               (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 116)))))
	  ;; load at 18
	  (set_local $scratch (i32.rotl (get_local $scratch) (i32.const 18)))
	  ;; load at 124
	  (set_local $scratch (i32.xor (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 124))) 
	                               (get_local $scratch)))
	  (i32.segment_store (handle.set_offset (get_local $addr) (i32.const 124)) (get_local $scratch))
	  ;; update loop counter
	  (set_local $i (i32.add (get_local $i) (i32.const 1)))
          (br 0)
	)
	  )

    ;; further modify x by adding input vals by index
    (set_local $i (i32.const 64))
    (block
      (loop
        (br_if 1 (i32.ge_u (get_local $i) (i32.const 128)))
	  (set_local $in_index (i32.sub (get_local $i) (i32.const 64)))
	  (i32.segment_store (handle.set_offset (get_local $addr) (get_local $i))
	                     (i32.add (i32.segment_load (handle.set_offset (get_local $addr) (get_local $i)))
	                              (i32.segment_load (handle.set_offset (get_local $addr) (get_local $in_index)))))
	  (set_local $i (i32.add (get_local $i) (i32.const 4)))
	  (br 0)
	)
      )

      ;; return result & free mem
	  (i32.segment_load (handle.set_offset (get_local $addr) (i32.const 64)))
	  (free_segment (get_local $addr))
    )

  (export "salsa20" (func $salsa20))

   (func (export "_main") (result i32)
      (call $salsa20)
   )
)
