(module
  (func (export "add4")
        (param $a
               i32)
        (param $b
               i32)
        (result i32)
    local.get $a
    local.get $b
    call $foo)
  (func $foo
        (param $a
               i32)
        (param $b
               i32)
        (result i32)
    (i32.add (local.get $a)
             (local.get $b)))
)
