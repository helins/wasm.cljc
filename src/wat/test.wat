(module
  (type $foo
        (func (param $a i32)
              (param $b i32)
              (result i32)))
  (func $f
        (export "add5")
        (export "f3")
        (type $foo)
        (param $a
               i32)
        (param $b
               i32)
        (result i32)
    (block (result i32
                   i32)
      (local.get $a)
      ;; (local.get $b)
      (i32.const 456))
    i32.add)

  (export "f" (func $f))
  (export "f2" (func $f))

  
)
