;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.test.wat

  ""

  {:author "Adam Helinski"}

  (:require [clojure.test    :as t]
            [helins.wasm.ir  :as ir]
            [helins.wasm.wat :as wat]))


;;;;;;;;;;


(defn ir->wat

  [ir]

  (-> ir
      wat/from-ir
      :wasm/form))


;;;;;;;;; Function types


(t/deftest local

  (t/is (= '(local i32)
           (ir->wat (ir/local 'i32))))

  (t/is (= '(local $foo
                   i32)
           (ir->wat (ir/local '$foo
                              'i32)))))



(t/deftest param

  (t/is (= '(param i32)
           (ir->wat (ir/param 'i32))))

  (t/is (= '(param $foo
                   i32)
           (ir->wat (ir/param '$foo
                              'i32)))))



(t/deftest result

  (t/is (= '(result i32)
           (ir->wat (ir/result 'i32)))))


;;;;;;;;;; Instructions numeric


(t/deftest i32-add

  (t/is (= '(i32.add (i32.const 42)
                     (i32.const 1))
           (ir->wat (ir/i32-add (ir/i32-const 42)
                                (ir/i32-const 1))))))


;;;;;;;;;; Instructions - Variables


(t/deftest local-get

  (t/is (= '(local.get $a)
           (ir->wat (ir/local-get '$a)))))


;;;;;;;;;; Module fields


(t/deftest export

  (t/is (= '(export "foo")
           (ir->wat (ir/export "foo")))))



(t/deftest func

  (t/is (= '(func $sum
                  (export "sum")
                  (param $a
                         i32)
                  (param $b
                         i32)
                  (result i32)
                  (local $c
                         i32)
              (i32.add (local.get $a)
                       (i32.add (local.get $b)
                                (i32.const 42))))
           (ir->wat (ir/func (-> {}
                                 (ir/ident   '$sum)
                                 (ir/export+ ["sum"])
                                 (ir/param+  [['$a 'i32]
                                              ['$b 'i32]])
                                 (ir/result+ ['i32])
                                 (ir/local+  [['$c 'i32]])
                                 (ir/instr+  [(ir/i32-add (ir/local-get '$a)
                                                          (ir/i32-add (ir/local-get '$b)
                                                                      (ir/i32-const 42)))])))))))


;;;;;;;;;; Values


(t/deftest i32-const

  (t/is (= '(i32.const 42)
           (ir->wat (ir/i32-const 42)))))
