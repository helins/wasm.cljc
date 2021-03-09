;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasmeta.dev

  "CLJC playground during dev."

  {:author "Adam Helinski"}

  (:require #?(:clj [clojure.edn])
            [clojure.pprint]
            [clojure.spec.alpha     :as s]
            [clojure.spec.gen.alpha :as sgen]
            [helins.binf            :as binf]
            [helins.binf.buffer     :as binf.buffer]
            [helins.binf.int        :as binf.int]
            [helins.binf.leb128     :as binf.leb128]
            [helins.binf.string     :as binf.string]
            [helins.wasm.bin        :as wasm.bin]
            [helins.wasm.count      :as wasm.count]
            [helins.wasm.decompile  :as wasm.decompile]
            [helins.wasm.ir         :as wasm.ir]
            [helins.wasm.read       :as wasm.read]
            [helins.wasm.spec       :as wasm.spec]
            [helins.wasm.write      :as wasm.write]
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
          (wasmer.module/load-source "src/wasm/import.wasm")
      ;    (wasmer.module/load-source "src/wasm/import.wasm")
      ;    (wasmer.module/load-source "src/wasm/simple.wasm")
      ;    (wasmer.module/load-source "src/wasm/export.wasm")
           (take 40)
           byte-array
           (wasm.decompile/main))
      ;:wasm/tablesec
      ;wasm.count/importsec'
      ;:wasm/write

      clojure.pprint/pprint
      )


  (->
      (take 40
            (seq (wasmer.module/load-source "src/wasm/import.wasm")))
      byte-array
      vec
      ;wasm.decompile/main
      ;clojure.pprint/pprint
      )


  (-> (wasmer.module/load-source "src/wasm/import.wasm")
      wasm.decompile/main
      wasm.count/module
      wasm.write/main

      ;:wasm/write
      ;wasm.count/typesec
      ;wasm.count/module

      ;(binf/rr-buffer 39)
      ;seq

      binf/backing-buffer
      wasm.decompile/main
      clojure.pprint/pprint
      )

  (s/valid? :wasm/module
            *1)


  (sgen/generate (s/gen :wasm/module))

  )
