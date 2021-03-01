(module
  (memory 1)
  (func $f
    i32.const 0
    (i32.const 42)
    (block)
    (i32.store offset=4)
    )
  (export "my-func" (func $f))
)
