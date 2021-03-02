;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasmeta.dev

  "CLJC playground during dev."

  {:author    "Adam Helinski"
   :clj-kondo {:linters {:unused-namespace {:level :off}}}}

  (:require [clojure.edn]
            [helins.binf            :as binf]
            [helins.binf.buffer     :as binf.buffer]
            [helins.binf.int        :as binf.int]
            [helins.binf.leb128     :as binf.leb128]
            [helins.binf.string     :as binf.string]
            [helins.wasm.bin        :as wasm.bin]
            [helins.wasm.bin.read   :as wasm.bin.read]
            [helins.wasm.decompile  :as wasm.decompile]
            [helins.wasm.ir         :as ir]
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
                  (ir/result+ ['i32])
                  (ir/local+  [['$c 'i32]])
                  (ir/instr+  [(ir/i32-add (ir/local-get '$a)
                                           (ir/i32-add (ir/local-get '$b)
                                                       (ir/i32-const 42)))]))))

(comment


  (:wasm/form (wat/transl ir-func))



  (defn f

    [fname & arg+]

    (let [inst (wasmer.instance/from-source (wasmer.module/load-source "src/wasm/test.wasm"))
          ret  (apply (-> (wasmer.fn/find inst
                                          fname)
                          wasmer.fn/return-vec
                          wasmer.fn/normal-call)
                      arg+)]
      (wasmer.instance/discard inst)
      ret))

  (f "args"
     (int 0)
     (long 1)
     (float 1)
     (double 2))

  (f "global")





  (-> 
      (wasmer.module/load-source "src/wasm/test.wasm")
      ;(wasmer.module/load-source "src/wasm/import.wasm")
      ;(wasmer.module/load-source "src/wasm/simple.wasm")
      ;(wasmer.module/load-source "src/wasm/export.wasm")
      wasm.decompile/main
      :wasm.bin/codesec
      ;:wasm.bin/globalsec
      ;:wasm.bin/elemsec
      )





  )
