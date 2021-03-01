(module
  (memory 1)
  (func $f (param $a i32) (result i32)
    (block (result i32)
      (if (result i32)
          (i32.gt_u (local.get $a)
                    (i32.const 0))
          (then
            (i32.const 1))
          (else
            (i32.const 0)))
      return))
  (export "f" (func $f))
)
