;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.test

  "Testing core features."

  {:author "Adam"}

  (:require [clojure.data]
            [clojure.pprint]
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
            [helins.wasm.schema              :as wasm.schema]
            [helins.wasm.write               :as wasm.write]
            [malli.core                      :as malli]
            [malli.util]
            [malli.generator                 :as malli.gen]))


;;;;;;;;;; Setup


(alter-var-root #'wasm.count/-flatten-idx
                (constantly (fn [_hmap idx]
                              idx)))



(alter-var-root #'wasm.write/-flatten-idx
                (constantly (fn [_hmap idx]
                              idx)))



(def registry
     (-> (merge (malli/default-schemas)
                (malli.util/schemas))
         wasm.schema/registry))


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



(defn compile-instr

  ""

  [value]

  (compile-direct (partial wasm.count/instr'
                           {})
                  (fn write [view opvec]
                    (wasm.write/instr' view
                                       {}
                                       opvec))
                  (fn read [view]
                    (wasm.read/instr' wasm/ctx
                                      view))
                  value))



(defn generator

  ""

  [schema]

  (malli.gen/generator schema
                       {:registry registry}))



(defn test-direct

  ""

  [schema f-count f-write f-read]

  (tc.prop/for-all [x (generator schema)]
    (= x
       (compile-direct f-count
                       f-write
                       f-read
                       x))))



(defn test-section

  ""

  [section-id gen f-count k-count f-write f-read]

  (tc.prop/for-all [ctx gen]
    (let [ctx-2  (f-count (assoc ctx
                                 :wasm/write
                                 {}))
          n-byte (get-in ctx-2
                         [:wasm/write
                          k-count])]
      (if (and n-byte
               (pos? n-byte))
        (let [view (-> (wasm.count/section' n-byte)
                        binf.buffer/alloc
                        binf/view)]
           (f-write view
                    ctx-2)
           (binf/seek view
                      0)
           (let [section-id (wasm.read/section-id' view)
                 n-byte-2   (wasm.read/u32' view)
                 ctx-3      (f-read wasm/ctx
                                    view)
                 diff       (clojure.data/diff ctx
                                               ctx-3)
                 diff-A     (nth diff
                                 0)
                 diff-B     (nth diff
                                 1)]
             (when (not= diff-A
                         diff-B)
               (clojure.pprint/pprint [:a ctx-2 :b ctx-3 :diff diff]))
             (= [section-id
                 n-byte
                 diff-A]
                [section-id
                 n-byte-2
                 diff-B])))
        true))))


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



(tc.ct/defspec idx

  (test-direct :wasm/funcidx
               wasm.count/idx
               wasm.write/idx
               wasm.read/idx))


;;;;;;;;;; Instructions


(tc.ct/defspec instr'
               2000

  (test-direct :wasm/instr
               (partial wasm.count/instr'
                        {})
               (fn write [view opvec]
                 (wasm.write/instr' view
                                    {}
                                    opvec))
               (fn read [view]
                 (wasm.read/instr' wasm/ctx
                                   view))))


;;;;;;;;;; Modules / Type Section


(tc.ct/defspec typesec'

  (test-section wasm.bin/section-id-type
                (tc.gen/fmap (fn [type+]
                               (reduce wasm.ir/assoc-type
                                       wasm/ctx
                                       type+))
                             (tc.gen/vector (generator :wasm/type)))
                wasm.count/typesec'
                :wasm.count/typesec
                wasm.write/typesec'
                wasm.read/typesec'))


;;;;;;;;;; Modules / Import Section


(tc.ct/defspec importsec'

  (test-section wasm.bin/section-id-import
                (tc.gen/fmap (fn [[func+
                                   global+
                                   mem+
                                   table+]]
                               (as-> wasm/ctx
                                     ctx  
                                 (reduce wasm.ir/import-func
                                         ctx
                                         func+)
                                 (reduce wasm.ir/import-global
                                         ctx
                                         global+)
                                 (reduce wasm.ir/import-mem
                                         ctx
                                         mem+)
                                 (reduce wasm.ir/import-table
                                         ctx
                                         table+)))
                             (tc.gen/tuple
                               (tc.gen/vector (malli.gen/generator :wasm.import/func
                                                                   {:registry registry}))
                               (tc.gen/vector (malli.gen/generator :wasm.import/global
                                                                   {:registry registry}))
                               (tc.gen/vector (malli.gen/generator :wasm.import/mem
                                                                   {:registry registry}))
                               (tc.gen/vector (malli.gen/generator :wasm.import/table
                                                                   {:registry registry}))))
                wasm.count/importsec'
                :wasm.count/importsec
                wasm.write/importsec'
                wasm.read/importsec'))


;;;;;;;;;; Modules / Function Section


(tc.ct/defspec funcsec'

  (test-section wasm.bin/section-id-func
                (tc.gen/fmap (fn [type+]
                               (reduce wasm.ir/assoc-func
                                       wasm/ctx
                                       type+))
                             (tc.gen/vector (generator :wasm/func)))
                wasm.count/funcsec'
                :wasm.count/funcsec
                wasm.write/funcsec'
                wasm.read/funcsec'))


;;;;;;;;;; Modules / Table Section


(tc.ct/defspec tablesec'

  (test-section wasm.bin/section-id-table
                (tc.gen/fmap (fn [table+]
                               (reduce wasm.ir/assoc-table
                                       wasm/ctx
                                       table+))
                             (tc.gen/vector (generator :wasm/table)))
                wasm.count/tablesec'
                :wasm.count/tablesec
                wasm.write/tablesec'
                wasm.read/tablesec'))


;;;;;;;;;; Modules / Memory Section


(tc.ct/defspec memsec'

  (test-section wasm.bin/section-id-table
                (tc.gen/fmap (fn [mem+]
                               (reduce wasm.ir/assoc-mem
                                       wasm/ctx
                                       mem+))
                             (tc.gen/vector (generator :wasm/mem)))
                wasm.count/memsec'
                :wasm.count/memsec
                wasm.write/memsec'
                wasm.read/memsec'))


;;;;;;;;;; Modules / Global Section


(tc.ct/defspec globalsec'

  (test-section wasm.bin/section-id-table
                (tc.gen/fmap (fn [global+]
                               (reduce wasm.ir/assoc-global
                                       wasm/ctx
                                       global+))
                             (tc.gen/vector (generator :wasm/global)))
                wasm.count/globalsec'
                :wasm.count/globalsec
                wasm.write/globalsec'
                wasm.read/globalsec'))


;;;;;;;;;; Modules / Export Section


(tc.ct/defspec exportsec'

  (test-section wasm.bin/section-id-table
                (tc.gen/fmap (fn [exportsec]
                               (update wasm/ctx
                                      :wasm/exportsec
                                      merge
                                      (into (sorted-map)
                                            exportsec)))
                             (generator :wasm/exportsec))
                wasm.count/exportsec'
                :wasm.count/exportsec
                wasm.write/exportsec'
                wasm.read/exportsec'))
