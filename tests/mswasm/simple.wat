(module
  (func (export "addTwo") (param i32 i32) (result i32)
    get_local 0
    get_local 1
    i32.add
    i32.segment_load))