;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.schema

  ""

  {:author "Adam Helinski"}

  (:require [clojure.test.check.generators :as tc.gen]
            [helins.binf.string            :as binf.string]
            [helins.wasm.bin               :as wasm.bin]
            [helins.wasm.count             :as wasm.count]
            [helins.wasm.write             :as wasm.write]
            [malli.core                    :as malli]
            [malli.generator               :as malli.gen]
            [malli.util]))


;;;;;;;;;; Values / Byte


(def byte'
     [:int
      {:max 255
       :min 0}])


;;;;;;;;;; Values / Integers


(def u32'
     [:int
      {:max 4294967295
       :min 0}])


;;;;;;;;;; Types / Number Types


(def numtype'
     [:enum
      wasm.bin/numtype-i32
      wasm.bin/numtype-i64
      wasm.bin/numtype-f32
      wasm.bin/numtype-f64])


;;;;;;;;;; Values / Names


(def name'
     (malli/-simple-schema {:pred            #?(:clj  (let [klass (class (byte-array 0))]
                                                        #(instance? klass
                                                                    %))
                                                :cljs nil)
                            :type            :wasm/name
                            :type-properties {:error/message "Must be BinF buffer"
                                              :gen/gen       (tc.gen/fmap binf.string/encode
                                                                          (malli.gen/generator [:string
                                                                                                {:min 1}])

                                                                          #_tc.gen/string)}}))


;;;;;;;;;; Types / Reference Types


(def reftype'
     [:enum
      wasm.bin/funcref
      wasm.bin/externref])


;;;;;;;;;; Types / Value Types


(def valtype'
     [:or
      :wasm/numtype
      :wasm/reftype])


;;;;;;;;;; Types / Result Types


(def resulttype'
     [:maybe
      [:vector
       {:min 1}
       :wasm/valtype]])


;;;;;;;;;; Types / Function Types


(def functype'
     [:tuple
      :wasm/resulttype
      :wasm/resulttype])


;;;;;;;;;; Types / Limits


(def limits'
     [:and

      [:map
       [:wasm.limit/min
        :wasm/u32]
       [:wasm.limit/max
        {:optional true}
        :wasm/u32]]

      ;(malli/-simple-schema {:pred

      #_[:fn (fn [{:wasm.limit/keys [max
                                   min]}]
             (if max
               (>= max
                   min)
               true))]])


;;;;;;;;;; Types / Memory Types


(def memtype'
     :wasm/limits)


;;;;;;;;;; Types / Memory Types


(def tabletype'
     [:merge
      limits'
      [:map
       :wasm/reftype]])


;;;;;;;;;; Types / Global types


(def globaltype'
     [:map
      :wasm/mutable?
      :wasm/valtype])


;;;;;;;;;; Instructions / Control Instructions


(def blocktype'
     [:or
      :nil
      [:tuple
       [:= :wasm/valtype]
       :wasm/valtype]
      [:tuple
       [:= :wasm/typeidx]
       :wasm/typeidx]])



(def block'
     [:tuple
      [:= wasm.bin/block]
      blocktype'
      :wasm/instr+])



(def loop'
     [:tuple
      [:= wasm.bin/loop-]
      blocktype'
      :wasm/instr+])



(def if'
     [:cat
      {:gen/fmap vec}
      [:= wasm.bin/if-]
      blocktype'
      :wasm/instr+
      [:repeat
       {:max 1
        :min 0}
       :wasm/instr+]])



(def br'
     [:tuple
      [:= wasm.bin/br]
      :wasm/labelidx])



(def br_if'
     [:tuple
      [:= wasm.bin/br_if]
      :wasm/labelidx])



(def br_table'
     [:cat
      {:gen/fmap vec}
      [:= wasm.bin/br_table]
      [:+ :wasm/labelidx]])



(def call'
     [:tuple
      [:= wasm.bin/call]
      :wasm/funcidx])



(def call_indirect'
     [:tuple
      [:= wasm.bin/call_indirect]
      :wasm/typeidx
      :wasm/tableidx])


;;;;;;;;;; Instructions / Expressions


(def instr'
     [:multi
      {:dispatch first}
      ;[wasm.bin/block block']
      ;[wasm.bin/loop- loop']
      ;[wasm.bin/if-   if']
      [wasm.bin/br    br']
      [wasm.bin/br_if br_if']
      [wasm.bin/br_table br_table']
      [wasm.bin/call call']
      [wasm.bin/call_indirect call_indirect']
      ])



(def instr+
  instr'
     #_[:vector
      :wasm/instr])



(def expr'
     instr+)


;;;;;;;;;; Modules / Indices


(def idx
  
  ""

  :wasm/u32)



(def funcidx'
     idx)

(def globalidx'
     idx)

(def labelidx'
     idx)

(def memidx'
     idx)

(def tableidx'
     idx)

(def typeidx'
     idx)


;;;;;;;;;; Modules / Type Section


(def typesec'
     [:map-of
      :wasm/funcidx
      :wasm/type])



(def type-
  
  ""

  [:map
   :wasm/signature])



(def signature
     :wasm/functype)


;;;;;;;;;; Modules / Import Section


(def func

  ""
  
  [:map
   :wasm/typeidx])



(def imported-base

  ""

  [:map
   :wasm.import/module
   :wasm.import/name])



(def imported-func

  ""

  [:merge
   imported-base
   func])


(def imported-global

  ""

  [:merge
   imported-base
   globaltype'])



(def imported-mem

  ""

  [:merge
   imported-base
   memtype'])



(def imported-table

  ""

  [:merge
   imported-base
   tabletype'])
   


(def importsec'
     [:map
      [:wasm.import/func   [:map-of
                            :wasm/funcidx
                            imported-func]]
      [:wasm.import/global [:map-of
                            :wasm/globalidx
                            imported-global]]
      [:wasm.import/mem    [:map-of
                            :wasm/memidx
                            imported-mem]]
      [:wasm.import/table  [:map-of
                            :wasm/tableidx
                            imported-table]]])


;;;;;;;;;; Modules / Function Section


(def funcsec'
     [:map-of
      :wasm/funcidx
      :wasm/func])


;;;;;;;;;; Modules / Table Section


(def tablesec'
     [:map-of
      :wasm/tableidx
      :wasm/table])



(def table'
     :wasm/tabletype)


;;;;;;;;;; Modules / Memory Section


(def memsec'
     [:map-of
      :wasm/memidx
      :wasm/mem])



(def mem'
     :wasm/memtype)


;;;;;;;;;; Registry


(def registry

  "Registry gathering all schemas related to WASM."

  (merge (malli/default-schemas)
         (malli.util/schemas)
         {
          :wasm/block         block'  
          :wasm/blocktype     blocktype'
          :wasm/br            br'
          :wasm/br_if         br_if'
          :wasm/br_table      br_table'
          :wasm/byte          byte'
          :wasm/call          call'
          :wasm/call_indirect call_indirect'
          :wasm/expr          expr'
          :wasm/func          func
          :wasm/funcidx       funcidx'
          :wasm/functype      functype'
          :wasm/globalidx     globalidx'
          :wasm/idx           idx
          :wasm/if            if'
          :wasm/instr         instr'
          :wasm/instr+        instr+
          :wasm/importsec     importsec'
          :wasm/labelidx      labelidx'
          :wasm/loop          loop'
          :wasm/limits        limits'
          :wasm/mem           mem'
          :wasm/memidx        memidx'
          :wasm/memsec        memsec'
          :wasm/memtype       memtype'
          :wasm/mutable?      :boolean
          :wasm/name          name'
          :wasm/numtype       numtype'
          :wasm/reftype       reftype'
          :wasm/resulttype    resulttype'
          :wasm/signature     signature
          :wasm/table         table'
          :wasm/tableidx      tableidx'
          :wasm/tablesec      tablesec'
          :wasm/tabletype     tabletype'
          :wasm/type          type-
          :wasm/typeidx       typeidx'
          :wasm/typesec       typesec'
          :wasm/u32           u32'
          :wasm/valtype       valtype'
          :wasm.import/module name'
          :wasm.import/name   name'
          }))





(comment


  (malli/validate nil
                  [1 2])



  (malli.gen/generate instr'
                      {:registry registry})



  (malli.gen/generator [:and
                        {:registry registry}
                        :wasm/name])



  )
