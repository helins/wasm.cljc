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
    (local.get $a)
    (local.get $b)
    (block
      (local.set $b
                 (i32.const 10000)))
    ;;(block (result i32
    ;;               i32)
    ;;  (local.get $a)
    ;;  ;; (local.get $b)
    ;;  (i32.const 456))
    drop
    (local.get $b)
    i32.add)

  (export "f" (func $f))
  (export "f2" (func $f))

  
)
