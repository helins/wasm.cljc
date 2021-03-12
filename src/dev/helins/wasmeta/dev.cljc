;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasmeta.dev

  "CLJC playground during dev."

  {:author "Adam Helinski"}

  (:require [clojure.data]
            #?(:clj [clojure.edn])
            [clojure.pprint]
            [clojure.spec.alpha     :as s]
            [clojure.spec.gen.alpha :as sgen]
            [helins.binf            :as binf]
            [helins.binf.buffer     :as binf.buffer]
            [helins.binf.int        :as binf.int]
            [helins.binf.leb128     :as binf.leb128]
            [helins.binf.string     :as binf.string]
            [helins.wasm            :as wasm]
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
            [helins.wasmer.module   :as wasmer.module]
            [malli.core             :as malli]
            [malli.generator        :as malli.gen]
            [malli.provider]
            [malli.registry]
            [malli.util]))


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



  (-> (wasm/decompile-file "src/wasm/test.wasm")
      (wasm/compile-file "/tmp/test.wasm")
      ;(wasm/decompile-file "/tmp/test.wasm")
      clojure.pprint/pprint
      )

  (->
      (wasm/decompile-file "/tmp/test.wasm")
      clojure.pprint/pprint
      )

  (-> 
      (->> 
         (wasmer.module/load-source "src/wasm/test.wasm")
         ;(wasmer.module/load-source "/tmp/test.wasm")
         wasm/buffer->view
      ;    (wasmer.module/load-source "src/wasm/import.wasm")
      ;    (wasmer.module/load-source "src/wasm/simple.wasm")
      ;    (wasmer.module/load-source "src/wasm/export.wasm")
         wasm/decompile
           )
      ;:wasm/tablesec
      ;wasm.count/importsec'
      ;:wasm/write

      ; :wasm/codesec
      ; (get 1)
      ; :wasm/expr
      ; (->> (take 7))
      ; wasm.count/expr'

      ;clojure.pprint/pprint
      )


  (-> 
      (wasmer.module/load-source "src/wasm/test.wasm")
      ;(wasmer.module/load-source "src/wasm/import.wasm")
      wasm/buffer->view
      wasm/decompile

      wasm/compile

      ;:wasm/write
      ;wasm.count/typesec
      ;wasm.count/module

      ;(binf/rr-buffer 39)
      ;seq

      binf/backing-buffer
      wasm/buffer->view
      wasm/decompile
      clojure.pprint/pprint
      )









  (s/valid? :wasm/module
            *1)


  (sgen/generate (s/gen :wasm/module))





  (def *reg
       (atom (assoc (malli/default-schemas)
                    :test-string (malli/-string-schema)
                    :foo       [:int
                                {:gen/gen (sgen/fmap (fn [x]
                                                       (swap! *reg
                                                              assoc
                                                              :vecstring
                                                              :boolean)
                                                       (println :got x)
                                                       x)
                                                     (s/gen int?))}]
                    :vecstring (malli/schema [:vector :string]))))


  (malli/validate [:vector
                   {:registry (malli.registry/mutable-registry (atom {:test-string (malli/-string-schema)}))}
                   :test-string]
                  ["ok"])

                  ;{:registry (malli.registry/mutable-registry *reg)})

  (malli/validate [:vector
                 ;  {:registry (malli.registry/mutable-registry (atom {:test-string :string}))
                   :test-string]
                  {:registry (malli.registry/mutable-registry *reg)})


  (def sch
       (malli/schema [:vector
                      :test-string
                      ]
                     {:registry (malli.registry/mutable-registry *reg)}))



(def registry*
  (atom {:string (malli/-string-schema)
         :maybe (malli/-maybe-schema)
         :map (malli/-map-schema)}))


  (malli/validate :foo
                  42
                  {:registry (malli.registry/mutable-registry *reg)})

       


  (malli/validate sch
                 {:a 42
                  :b 24}
                 {:registry (malli.registry/mutable-registry *reg)
                  
                  #_malli/default-registry})


  (malli.util/get-in sch
                     [:a
                      :registry])


  (malli.provider/provide [{:a 32}
                           {:b "ok"}
                           [:ok 'ok]])


  (malli.gen/generate [:cat :vecstring
                            :foo
                            :vecstring]
                      {:registry (malli.registry/mutable-registry *reg)})
  
  )
