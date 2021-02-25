;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasmeta.dev

  "CLJC playground during dev."

  {:author "Adam Helinski"}

  (:require [helins.wasm.ir         :as ir]
            [helins.wasm.wat        :as wat]
            [helins.wasmer          :as wasmer]
            [helins.wasmer.fn       :as wasmer.fn]
            [helins.wasmer.instance :as wasmer.instance]
            [helins.wasmer.mem      :as wasmer.mem]
            [helins.wasmer.module   :as wasmer.module]
            [helins.wasmeta         :as wasmeta]))


;;;;;;;;;;


(def ir-func
     (ir/func (-> {}
                  (ir/ident   '$sum)
                  (ir/export+ ["sum"])
                  (ir/param+  [['$a 'i32]
                               ['$b 'i32]])
                  (ir/local+  [['$c 'i32]])
                  (ir/result+ ['i32])
                  (ir/instr+  [(ir/i32-add (ir/local-get '$a)
                                           (ir/i32-add (ir/local-get '$b)
                                                       (ir/i32-const 42)))]))))

(comment


  (:wasm/form (wat/transl ir-func))


  )
