(module


  (import "foo" "bar" (func $foo/bar (param i32)))
  ;; (export "foo/bar" (func $foo/bar))
  (import "some" "table" (table 1 15000 funcref))
  (import "some" "global" (global (mut f64)))



  (global $global-4
          (import "foo" "global")
          i32)

  (global $global-1
          (mut i64)
          (i64.const -42))


  (global $global-2
          f32
          (f32.const 1.0))


  (global $global-3
          i32
          (i32.const 100))


  (elem 0
        (offset (i32.const 10))
        $my-block)


  (memory 1)


  (data 0
        (offset (i32.const 0))
        "Hello"
        " "
        "world!")

  (data 0
        (offset (global.get $global-4))
        ;; (offset (i32.const 0))
        "\42\43\44")



  ;; Modifying and returning args
  ;;
  (func (export "args")
        (param $a i32)
        (param $b i64)
        (param $c f32)
        (param $d f64)
        (result i32
                i64
                f32
                f64)
    (local.set $a
               (i32.const 0))
    (local.set $b
               (i64.const 1))
    (local.set $c
               (f32.const 2))
    (local.set $d
               (f64.const 4))
    (local.get $a)
    (local.get $b)
    (local.get $c)
    (local.get $d))
    

  (func (export "unreachable")
    unreachable)


  (func (export "nop")
     nop)


  (func $my-block
        (export "block")
        (result i32)
    (block (result i32)
      (i32.const 42)))


  ;; Loop without iteration
  ;;
  (func (export "loop-simple")
    (loop
      $iter
      (i32.const 0)
      (br_if $iter)))


  (func $my-loop
        (export "loop")
        (result i32)
        (local $a i32)
    (local.set $a
               (i32.const 10))
    (loop
      $iter
      (result i32)
      (if 
        (result i32)
        (i32.gt_u (local.get $a)
                  (i32.const 0))
        (then
          (local.set $a
                     (i32.sub (local.get $a)
                              (i32.const 1)))
          (br $iter))
        (else
          (local.get $a)))))


  ;; Similar to "loop" but uses `br_if`
  ;;
  (func (export "loop-if")
        (result i32)
        (local $a i32)
    (local.set $a
               (i32.const 10))
    (loop
      $iter
      (local.set $a
                 (i32.sub (local.get $a)
                          (i32.const 1)))
      (local.get $a)
      (br_if $iter))
    (local.get $a))


  (func (export "if")
        (result i32)
    (if (result i32)
        (i32.gt_u (i32.const 1)
                  (i32.const 0))
        (then
          (i32.const 42))
        (else
          (i32.const 0))))


  (func (export "drop")
    (i64.const 42)
    drop)


  (func (export "select")
        (result i64)
    (i64.const 24)
    (i64.const 42)
    (i32.const 0)
    select)


  (func (export "local")
        (result i64)
        (local $a i64)
    (local.set $a
               (i64.const 42))
    (local.get $a))


  (func (export "local.tee")
        (result i64)
        (local $a i64)
    (local.tee $a
               (i64.const 42)))


  (func (export "global")
        (result i64)
    (global.set $global-1
                (i64.const 42))
    (global.get $global-1))


  ;; With params and locals
  ;;

  (func (export "params+locals")
        (param i32)
        (local i64))


  (func (export "trunc_sat")
        (param f32)
        (result i32)
    (local.get 0)
    i32.trunc_sat_f32_s)

  
  (func $start)


  (start $start)


)
