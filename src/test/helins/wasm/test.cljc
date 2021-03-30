;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.test

  "Testing core features."

  {:author "Adam"}

  (:require [clojure.pprint]
            [clojure.test                    :as t]
            [clojure.test.check.clojure-test :as tc.ct]
            [clojure.test.check.generators   :as tc.gen]
            [clojure.test.check.properties   :as tc.prop]
            [helins.binf                     :as binf]
            [helins.binf.buffer              :as binf.buffer]
            [helins.wasm                     :as wasm]
            [helins.wasm.bin                 :as wasm.bin]
            [helins.wasm.count               :as wasm.count]
            [helins.wasm.ir                  :as wasm.ir]
            [helins.wasm.read                :as wasm.read]
            [helins.wasm.write               :as wasm.write]
            [malli.generator                 :as malli.gen]))


;;;;;;;;;;


(defn compile-direct

  ""

  [f-count f-write f-read value]

  (let [view (-> (f-count value)
                 binf.buffer/alloc
                 binf/view)]
    (f-write view
             value)
    (binf/seek view
               0)
    (f-read view)))




(defn generator

  ""

  [schema]

  (malli.gen/generator [:and
                        {:registry wasm.ir/registry}
                        schema]))



(defn test-direct

  ""

  [schema f-count f-write f-read]

  (tc.prop/for-all [x (generator schema)]
    (= x
       (compile-direct f-count
                       f-write
                       f-read
                       x))))


;;;;;;;;;;


(tc.ct/defspec byte'

  (test-direct :wasm/byte
               (constantly wasm.count/byte')
               wasm.write/byte'
               wasm.read/byte'))



(tc.ct/defspec u32'

  (test-direct :wasm/u32
               wasm.count/u32'
               wasm.write/u32'
               wasm.read/u32'))



(tc.ct/defspec valtype'

  (test-direct :wasm/valtype
               (constantly wasm.count/valtype')
               wasm.write/valtype'
               wasm.read/valtype'))



(tc.ct/defspec resulttype'

  (test-direct :wasm/resulttype
               wasm.count/resulttype'
               wasm.write/resulttype'
               wasm.read/resulttype'))



(tc.ct/defspec functype'
  
  (test-direct :wasm/functype
               wasm.count/functype'
               wasm.write/functype'
               wasm.read/functype'))



(tc.ct/defspec funcidx'

  (test-direct :wasm/funcidx
               wasm.count/funcidx'
               wasm.write/funcidx'
               wasm.read/funcidx'))



(tc.ct/defspec idx

  (test-direct :wasm/funcidx
               wasm.count/idx
               wasm.write/idx
               wasm.read/idx))



(tc.ct/defspec typesec'

  (tc.prop/for-all [type+ (tc.gen/vector (generator :wasm/type))]
    (let [ctx    (reduce wasm.ir/assoc-type
                         wasm/ctx
                         type+)
          ctx-2  (wasm.count/typesec' ctx)
          n-byte (get-in ctx-2
                         [:wasm/write
                          :wasm.count/typesec])]
      (if n-byte
         (let [view (-> (wasm.count/section' n-byte)
                        binf.buffer/alloc
                        binf/view)]
           (wasm.write/typesec' view
                                ctx-2)
           (binf/seek view
                      0)
           (= [wasm.bin/section-id-type
               n-byte
               ctx]
              [(wasm.read/section-id' view)
               (wasm.read/u32' view)
               (wasm.read/typesec' wasm/ctx
                                   view)]))
        true))))
