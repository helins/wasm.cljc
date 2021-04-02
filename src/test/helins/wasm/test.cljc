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

  [section-id kv-schema prepare f-count k-count f-write f-read]

  (tc.prop/for-all [ctx (generator [:map
                                    {:gen/fmap (partial prepare
                                                        wasm/ctx)}
                                    kv-schema])]
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
             #_(when (not= diff-A
                         diff-B)
               (clojure.pprint/pprint [:ctx ctx :ctx-3 ctx-3 :diff diff]))
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


(def Typesec
     [::typesec
      [:vector
       :wasm/type]])



(defn prepare-typesec

  [ctx {::keys [typesec]}]

  (reduce wasm.ir/assoc-type
          ctx
          typesec))



(tc.ct/defspec typesec'

  (test-section wasm.bin/section-id-type
                Typesec
                prepare-typesec
                wasm.count/typesec'
                :wasm.count/typesec
                wasm.write/typesec'
                wasm.read/typesec'))


;;;;;;;;;; Modules / Import Section


(def Importsec
     [::importsec
      [:tuple
       [:vector :wasm.import/func]
       [:vector :wasm.import/global]
       [:vector :wasm.import/mem]
       [:vector :wasm.import/table]]])



(defn prepare-importsec

  [ctx {[func+
         global+
         mem+
         table+] ::importsec}]

  (as-> ctx
        ctx-2
    (reduce wasm.ir/import-func
            ctx-2
            func+)
    (reduce wasm.ir/import-global
            ctx-2
            global+)
    (reduce wasm.ir/import-mem
            ctx-2
            mem+)
    (reduce wasm.ir/import-table
            ctx-2
            table+)))



(tc.ct/defspec importsec'

  (test-section wasm.bin/section-id-import
                Importsec
                prepare-importsec
                wasm.count/importsec'
                :wasm.count/importsec
                wasm.write/importsec'
                wasm.read/importsec'))


;;;;;;;;;; Modules / Function Section


(def Funcsec
     [::funcsec
      [:vector
       :wasm/func]])



(defn prepare-funcsec

  [ctx {::keys [funcsec]}]

  (reduce wasm.ir/assoc-func
          ctx
          funcsec))


(tc.ct/defspec funcsec'

  (test-section wasm.bin/section-id-func
                Funcsec
                prepare-funcsec
                wasm.count/funcsec'
                :wasm.count/funcsec
                wasm.write/funcsec'
                wasm.read/funcsec'))


;;;;;;;;;; Modules / Table Section


(def Tablesec
     [::tablesec
      [:vector
       :wasm/table]])



(defn prepare-tablesec

  [ctx {::keys [tablesec]}]

  (reduce wasm.ir/assoc-table
          ctx
          tablesec))



(tc.ct/defspec tablesec'

  (test-section wasm.bin/section-id-table
                Tablesec
                prepare-tablesec
                wasm.count/tablesec'
                :wasm.count/tablesec
                wasm.write/tablesec'
                wasm.read/tablesec'))


;;;;;;;;;; Modules / Memory Section


(def Memsec
     [::memsec
      [:vector
       :wasm/mem]])



(defn prepare-memsec

  [ctx {::keys [memsec]}]

  (reduce wasm.ir/assoc-mem
          ctx
          memsec))



(tc.ct/defspec memsec'

  (test-section wasm.bin/section-id-table
                Memsec
                prepare-memsec
                wasm.count/memsec'
                :wasm.count/memsec
                wasm.write/memsec'
                wasm.read/memsec'))


;;;;;;;;;; Modules / Global Section


(def Globalsec
     [::globalsec
      [:vector
       :wasm/global]])



(defn prepare-globalsec

  [ctx {::keys [globalsec]}]

  (reduce wasm.ir/assoc-global
          ctx
          globalsec))



(tc.ct/defspec globalsec'

  (test-section wasm.bin/section-id-table
                Globalsec
                prepare-globalsec
                wasm.count/globalsec'
                :wasm.count/globalsec
                wasm.write/globalsec'
                wasm.read/globalsec'))


;;;;;;;;;; Modules / Export Section


(def Exportsec
     [::exportsec
      :wasm/exportsec])



(defn prepare-exportsec

  [ctx {::keys [exportsec]}]

  (update ctx
          :wasm/exportsec
          merge
          (into (sorted-map)
                exportsec)))



(tc.ct/defspec exportsec'

  (test-section wasm.bin/section-id-table
                Exportsec
                prepare-exportsec
                wasm.count/exportsec'
                :wasm.count/exportsec
                wasm.write/exportsec'
                wasm.read/exportsec'))


;;;;;;;;;; Modules / Start Section


(def Startsec
     [::startsec
      :wasm/startsec])



(defn prepare-startsec

  [ctx {::keys [startsec]}]

  (assoc ctx
         :wasm/startsec
         startsec))



(tc.ct/defspec startsec'

  (test-section wasm.bin/section-id-table
                Startsec
                prepare-startsec
                wasm.count/startsec'
                :wasm.count/startsec
                wasm.write/startsec'
                wasm.read/startsec'))


;;;;;;;;;; Modules / Element Section


(def Elemsec
     [::elemsec
      [:vector
       :wasm/elem]])



(defn prepare-elemsec

  [ctx {::keys [elemsec]}]

  (reduce wasm.ir/assoc-elem
          ctx
          elemsec))



(tc.ct/defspec elemsec'

  (test-section wasm.bin/section-id-table
                Elemsec
                prepare-elemsec
                wasm.count/elemsec'
                :wasm.count/elemsec
                wasm.write/elemsec'
                wasm.read/elemsec'))


;;;;;;;;;; Modules / Code Section


(def Codesec
     [::codesec
      [:vector
       :wasm/code]])



(defn prepare-codesec

  [ctx {::keys [codesec]}]

  (reduce (fn [ctx-2 code]
            (update ctx-2
                    :wasm/codesec
                    (fn [codesec]
                      (assoc codesec
                             (count codesec)
                             code))))
          ctx
          codesec))



(tc.ct/defspec codesec'

  (test-section wasm.bin/section-id-table
                Codesec
                prepare-codesec
                wasm.count/codesec'
                :wasm.count/codesec
                wasm.write/codesec'
                (comp #(dissoc %
                               :wasm/source)
                      wasm.read/codesec'2
                      wasm.read/codesec')))


;;;;;;;;;; Modules / Data Section


(def Datasec
     [::datasec
      [:vector
       :wasm/data]])



(defn prepare-datasec

  [ctx {::keys [datasec]}]

  (reduce wasm.ir/assoc-data
          ctx
          datasec))



(tc.ct/defspec datasec'

  (test-section wasm.bin/section-id-table
                Datasec
                prepare-datasec
                wasm.count/datasec'
                :wasm.count/datasec
                wasm.write/datasec'
                wasm.read/datasec'))


;;;;;;;;;; Modules / Data Count Section


(def Datacountsec
     [::datacountsec
      :wasm/datacountsec])



(defn prepare-datacountsec

  [ctx {::keys [datacountsec]}]

  (assoc ctx
         :wasm/datacountsec
         datacountsec))



(tc.ct/defspec datacountsec'

  (test-section wasm.bin/section-id-table
                Datacountsec
                prepare-datacountsec
                wasm.count/datacountsec'
                :wasm.count/datacountsec
                wasm.write/datacountsec'
                wasm.read/datacountsec'))
