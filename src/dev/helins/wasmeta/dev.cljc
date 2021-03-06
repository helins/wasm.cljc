;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasmeta.dev

  "CLJC playground during dev."

  {:author "Adam Helinski"}

  (:require #?(:clj [clojure.edn])
            [clojure.pprint]
            [helins.binf            :as binf]
            [helins.binf.buffer     :as binf.buffer]
            [helins.binf.int        :as binf.int]
            [helins.binf.leb128     :as binf.leb128]
            [helins.binf.string     :as binf.string]
            [helins.wasm.bin        :as wasm.bin]
            [helins.wasm.count      :as wasm.count]
            [helins.wasm.decompile  :as wasm.decompile]
            [helins.wasm.read       :as wasm.read]
            [helins.wasm.write      :as wasm.write]
            [helins.wasm.ir         :as wasm.ir]
            [helins.wasmer.fn       :as wasmer.fn]
            [helins.wasmer.instance :as wasmer.instance]
            [helins.wasmer.mem      :as wasmer.mem]
            [helins.wasmer.module   :as wasmer.module]))


;;;;;;;;;;


(comment


  (defn f

    [fname & arg+]

    (let [inst (wasmer.instance/from-source (wasmer.module/load-source "src/wasm/test.wasm"))
          ret  (apply (-> (wasmer.fn/find inst
                                          fname)
                          wasmer.fn/return-vec
                          wasmer.fn/normal-call)
                      arg+)]
      ;;
      ;; Causes segfaults when GC kicks in
      ;;
      ;; (wasmer.instance/discard inst)
      ret))

  (f "args"
     (int 0)
     (long 1)
     (float 1)
     (double 2))

  (f "foo")





  (-> 
      (->> 
          (wasmer.module/load-source "src/wasm/test.wasm")
      ;    (wasmer.module/load-source "src/wasm/import.wasm")
      ;    (wasmer.module/load-source "src/wasm/simple.wasm")
      ;    (wasmer.module/load-source "src/wasm/export.wasm")
           (wasm.decompile/main))
      ;:wasm/typesec
      clojure.pprint/pprint
      )


  (->
      (take 42
            (seq (wasmer.module/load-source "src/wasm/test.wasm")))
      clojure.pprint/pprint)


  (-> (wasmer.module/load-source "src/wasm/test.wasm")
      wasm.decompile/main
      wasm.count/module
      wasm.write/main
      ;wasm.decompile/main

      ;wasm.count/typesec
      ;wasm.count/module

      ;(binf/rr-buffer 39)
      ;seq

      binf/backing-buffer
      wasm.decompile/main
      clojure.pprint/pprint
      )


  )
