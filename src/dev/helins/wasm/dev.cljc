;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.dev

  "CLJC playground during dev."

  {:author "Adam Helinski"}

  (:require [clojure.data]
            #?(:clj [clojure.edn])
            [clojure.pprint]
            [helins.binf            :as binf]
            [helins.binf.buffer     :as binf.buffer]
            [helins.binf.int        :as binf.int]
            [helins.binf.leb128     :as binf.leb128]
            [helins.binf.string     :as binf.string]
            [helins.wasm            :as wasm]
            [helins.wasm.bin        :as wasm.bin]
            [helins.wasm.count      :as wasm.count]
            [helins.wasm.ir         :as wasm.ir]
            [helins.wasm.read       :as wasm.read]
            [helins.wasm.schema     :as wasm.schema]
            [helins.wasm.test]
            [helins.wasm.write      :as wasm.write]
            ;#?@(:clj [[helins.wasmer.fn       :as wasmer.fn]
            ;          [helins.wasmer.instance :as wasmer.instance]
            ;          [helins.wasmer.mem      :as wasmer.mem]
            ;          [helins.wasmer.module   :as wasmer.module]])
            [malli.core             :as malli]
            [malli.generator        :as malli.gen]
            [malli.provider]
            [malli.registry]
            [malli.util]))


;;;;;;;;;; Compilation / Decompilation


(comment


  (def registry
       (-> (merge (malli/default-schemas)
                  (malli.util/schemas))
           wasm.schema/registry))



  (malli.gen/generate :wasm/module
                      {:registry (-> (merge (malli/default-schemas)
                                            (malli.util/schemas))
                                     registry)})



  (-> (wasm/decompile-file "src/wasm/test.wasm")
      ;(wasm/compile-file "/tmp/test.wasm")
      ;(wasm/decompile-file "/tmp/test.wasm")
      clojure.pprint/pprint
      )


  )


;;;;;;;;;; Using Wasmer (alias :wasmer must be used, see deps)


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
  )
