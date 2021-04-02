;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.schema

  ""

  {:author "Adam Helinski"}

  (:require [clojure.test.check.generators :as tc.gen]
            [helins.binf.float             :as binf.float]
            #?(:cljs [helins.binf.gen	   :as binf.gen])
            [helins.binf.string            :as binf.string]
            [helins.wasm.bin               :as wasm.bin]
            [malli.core                    :as malli]
            [malli.error]
            [malli.generator               :as malli.gen]
            [malli.util]))


;;;;;;;;;; Registry


(defn registry

  "Registry gathering all schemas related to WASM.
  
   Either created from scratch or by upgrading the given one.
  
   Must be used alongside `malli.core/default-schemas` and `malli.util/schemas`."

  
  ([]

   (registry nil))


  ([registry]

   (let [imported-base
         [:map
          :wasm.import/module
          :wasm.import/name]
         
         instr-without-immediate+
         [[0x45 :wasm/i32.eqz]
          [0x46 :wasm/i32.eq]
          [0x47 :wasm/i32.ne]
          [0x48 :wasm/i32.lt_s]
          [0x49 :wasm/i32.lt_u]
          [0x4A :wasm/i32.gt_s]
          [0x4B :wasm/i32.gt_u]
          [0x4C :wasm/i32.le_s]
          [0x4D :wasm/i32.le_u]
          [0x4E :wasm/i32.ge_s]
          [0x4F :wasm/i32.ge_u]
          [0x50 :wasm/i64.eqz]
          [0x51 :wasm/i64.eq]
          [0x52 :wasm/i64.ne]
          [0x53 :wasm/i64.lt_s]
          [0x54 :wasm/i64.lt_u]
          [0x55 :wasm/i64.gt_s]
          [0x56 :wasm/i64.gt_u]
          [0x57 :wasm/i64.le_s]
          [0x58 :wasm/i64.le_u]
          [0x59 :wasm/i64.ge_s]
          [0x5A :wasm/i64.ge_u]
          [0x5B :wasm/f32.eq]
          [0x5C :wasm/f32.be]
          [0x5D :wasm/f32.lt]
          [0x5E :wasm/f32.gt]
          [0x5F :wasm/f32.le]
          [0x60 :wasm/f32.ge]
          [0x61 :wasm/f64.eq]
          [0x62 :wasm/f64.be]
          [0x63 :wasm/f64.lt]
          [0x64 :wasm/f64.gt]
          [0x65 :wasm/f64.le]
          [0x66 :wasm/f64.ge]
          [0x67 :wasm/i32.clz]
          [0x68 :wasm/i32.ctz]
          [0x69 :wasm/i32.popcnt]
          [0x6A :wasm/i32.and]
          [0x6B :wasm/i32.sub]
          [0x6C :wasm/i32.mul]
          [0x6D :wasm/i32.div_s]
          [0x6E :wasm/i32.div_u]
          [0x6F :wasm/i32.rem_s]
          [0x70 :wasm/i32.rem_u]
          [0x71 :wasm/i32.and]
          [0x72 :wasm/i32.or]
          [0x73 :wasm/i32.xor]
          [0x74 :wasm/i32.shl]
          [0x75 :wasm/i32.shr_s]
          [0x76 :wasm/i32.shr_u]
          [0x77 :wasm/i32.rotl]
          [0x78 :wasm/i32.rotr]
          [0x79 :wasm/i64.clz]
          [0x7A :wasm/i64.ctz]
          [0x7B :wasm/i64.popcnt]
          [0x7C :wasm/i64.and]
          [0x7D :wasm/i64.sub]
          [0x7E :wasm/i64.mul]
          [0x7F :wasm/i64.div_s]
          [0x80 :wasm/i64.div_u]
          [0x81 :wasm/i64.rem_s]
          [0x82 :wasm/i64.rem_u]
          [0x83 :wasm/i64.and]
          [0x84 :wasm/i64.or]
          [0x85 :wasm/i64.xor]
          [0x86 :wasm/i64.shl]
          [0x87 :wasm/i64.shr_s]
          [0x88 :wasm/i64.shr_u]
          [0x89 :wasm/i64.rotl]
          [0x8A :wasm/i64.rotr]
          [0x8B :wasm/f32.abs]
          [0x8C :wasm/f32.neg]
          [0x8D :wasm/f32.ceil]
          [0x8E :wasm/f32.floor]
          [0x8F :wasm/f32.trunc]
          [0x90 :wasm/f32.nearest]
          [0x91 :wasm/f32.sqrt]
          [0x92 :wasm/f32.add]
          [0x93 :wasm/f32.sub]
          [0x94 :wasm/f32.mul]
          [0x95 :wasm/f32.div]
          [0x96 :wasm/f32.min]
          [0x97 :wasm/f32.max]
          [0x98 :wasm/f32.copysign]
          [0x99 :wasm/f64.abs]
          [0x9A :wasm/f64.neg]
          [0x9B :wasm/f64.ceil]
          [0x9C :wasm/f64.floor]
          [0x9D :wasm/f64.trunc]
          [0x9E :wasm/f64.nearest]
          [0x9F :wasm/f64.sqrt]
          [0xA0 :wasm/f64.add]
          [0xA1 :wasm/f64.sub]
          [0xA2 :wasm/f64.mul]
          [0xA3 :wasm/f64.div]
          [0xA4 :wasm/f64.min]
          [0xA5 :wasm/f64.max]
          [0xA6 :wasm/f64.copysign]
          [0xA7 :wasm/i32.wrap_i64]
          [0xA8 :wasm/i32.trunc_f32_s]
          [0xA9 :wasm/i32.trunc_f32_u]
          [0xAA :wasm/i32.trunc_f64_s]
          [0xAB :wasm/i32.trunc_f64_u]
          [0xAC :wasm/i64.extend_i32_s]
          [0xAD :wasm/i64.extend_i32_u]
          [0xAE :wasm/i64.trunc_f32_s]
          [0xAF :wasm/i64.trunc_f32_u]
          [0xB0 :wasm/i64.trunc_f64_s]
          [0xB1 :wasm/i64.trunc_f64_u]
          [0xB2 :wasm/f32.convert_i32_s]
          [0xB3 :wasm/f32.convert_i32_u]
          [0xB4 :wasm/f32.convert_i64_s]
          [0xB5 :wasm/f32.convert_i64_u]
          [0xB6 :wasm/f32.demote_f64]
          [0xB7 :wasm/f64.convert_i32_s]
          [0xB8 :wasm/f64.convert_i32_u]
          [0xB9 :wasm/f64.convert_i64_s]
          [0xBA :wasm/f64.convert_i64_u]
          [0xBB :wasm/f64.promote_f32]
          [0xBC :wasm/i32.reinterpret_f32]
          [0xBD :wasm/i64.reinterpret_f64]
          [0xBE :wasm/f32.reinterpret_i32]
          [0xBF :wasm/f32.reinterpret_i64]
          [0xC0 :wasm/i32.extend8_s]
          [0xC1 :wasm/i32.extend16_s]
          [0xC2 :wasm/i64.extend8_s]
          [0xC3 :wasm/i64.extend16_s]
          [0xC4 :wasm/i64.extend32_s]
          [0xD1 :wasm/ref.is_null]]

         instr-local
         (fn [opcode]
           [:tuple
            [:= opcode]
            :wasm/localidx])

         instr-global
         (fn [opcode]
           [:tuple
            [:= opcode]
            :wasm/globalidx])

         instr-memarg
         (fn [opcode]
           [:tuple
            [:= opcode]
            :wasm.memory/align
            :wasm.memory/offset])

         instr-trunc-sat
         (fn [opcode-2]
           [:tuple
            :wasm.opcode/misc
            [:= opcode-2]])

         instr
         (into [:multi
                {:dispatch (fn [opvec]
                             (let [opcode (opvec 0)]
                               (if (= opcode
                                      wasm.bin/misc)
                                 [opcode
                                  (opvec 1)]
                                 opcode)))}
                [wasm.bin/block                 :wasm/block]
                [wasm.bin/loop-                 :wasm/loop]
                [wasm.bin/if-                   :wasm/if]
                [wasm.bin/br                    :wasm/br]
                [wasm.bin/br_if                 :wasm/br_if]
                [wasm.bin/br_table              :wasm/br_table]
                [wasm.bin/call                  :wasm/call]
                [wasm.bin/call_indirect         :wasm/call_indirect]
                [wasm.bin/ref-null              :wasm/ref.null]
                [wasm.bin/ref-func              :wasm/ref.func]
                [wasm.bin/select-t              :wasm/select-t]
                [wasm.bin/local-get             :wasm/local.get]
                [wasm.bin/local-set             :wasm/local.set]
                [wasm.bin/local-tee             :wasm/local.tee]
                [wasm.bin/global-get            :wasm/global.get]
                [wasm.bin/global-set            :wasm/global.set]
                [wasm.bin/table-get             :wasm/table.get]
                [wasm.bin/table-set             :wasm/table.set]
                [[wasm.bin/misc
                  wasm.bin/table-init]          :wasm/table.init]
                [[wasm.bin/misc
                  wasm.bin/elem-drop]           :wasm/elem.drop]
                [[wasm.bin/misc
                  wasm.bin/table-copy]          :wasm/table.copy]
                [[wasm.bin/misc
                  wasm.bin/table-grow]          :wasm/table.grow]
                [[wasm.bin/misc
                  wasm.bin/table-size]          :wasm/table.size]
                [[wasm.bin/misc
                  wasm.bin/table-fill]          :wasm/table.fill]
                [wasm.bin/i32-load              :wasm/i32.load]
                [wasm.bin/i64-load              :wasm/i64.load]
                [wasm.bin/f32-load              :wasm/f32.load]
                [wasm.bin/f64-load              :wasm/f64.load]
                [wasm.bin/i32-load8_s           :wasm/i32.load8_s]
                [wasm.bin/i32-load8_u           :wasm/i32.load8_u]
                [wasm.bin/i32-load16_s          :wasm/i32.load16_s]
                [wasm.bin/i32-load16_u          :wasm/i32.load16_u]
                [wasm.bin/i64-load8_s           :wasm/i64.load8_s]
                [wasm.bin/i64-load8_u           :wasm/i64.load8_u]
                [wasm.bin/i64-load16_s          :wasm/i64.load16_s]
                [wasm.bin/i64-load16_u          :wasm/i64.load16_u]
                [wasm.bin/i64-load32_s          :wasm/i64.load32_s]
                [wasm.bin/i64-load32_u          :wasm/i64.load32_u]
                [wasm.bin/i32-store             :wasm/i32.store]
                [wasm.bin/i64-store             :wasm/i64.store]
                [wasm.bin/f32-store             :wasm/f32.store]
                [wasm.bin/f64-store             :wasm/f64.store]
                [wasm.bin/i32-store8            :wasm/i32.store8]
                [wasm.bin/i32-store16           :wasm/i32.store16]
                [wasm.bin/i64-store8            :wasm/i64.store8]
                [wasm.bin/i64-store16           :wasm/i64.store16]
                [wasm.bin/i64-store32           :wasm/i64.store32]
                [wasm.bin/memory-size           :wasm/memory.size]
                [wasm.bin/memory-grow           :wasm/memory.grow]
                [[wasm.bin/misc
                  wasm.bin/memory-init]         :wasm/memory.init]
                [[wasm.bin/misc
                  wasm.bin/data-drop]           :wasm/data.drop]
                [[wasm.bin/misc
                  wasm.bin/memory-copy]         :wasm/memory.copy]
                [[wasm.bin/misc
                  wasm.bin/memory-fill]         :wasm/memory.fill]
			    [wasm.bin/i32-const             :wasm/i32.const]
			    [wasm.bin/i64-const             :wasm/i32.const]
                [[wasm.bin/misc
                  wasm.bin/i32-trunc_sat_f32_s] :wasm/i32.trunc_sat_f32_s]
                [[wasm.bin/misc
                  wasm.bin/i32-trunc_sat_f32_u] :wasm/i32.trunc_sat_f32_u]
                [[wasm.bin/misc
                  wasm.bin/i32-trunc_sat_f64_s] :wasm/i32.trunc_sat_f64_s]
                [[wasm.bin/misc
                  wasm.bin/i32-trunc_sat_f64_u] :wasm/i32.trunc_sat_f64_u]
                [[wasm.bin/misc
                  wasm.bin/i64-trunc_sat_f32_s] :wasm/i64.trunc_sat_f32_s]
                [[wasm.bin/misc
                  wasm.bin/i64-trunc_sat_f32_u] :wasm/i64.trunc_sat_f32_u]
                [[wasm.bin/misc
                  wasm.bin/i64-trunc_sat_f64_s] :wasm/i64.trunc_sat_f64_s]
                [[wasm.bin/misc
                  wasm.bin/i64-trunc_sat_f64_u] :wasm/i64.trunc_sat_f64_u]]
               instr-without-immediate+)

         registry-2
         (reduce (fn [registry-2 [opcode kw]]
                   (assoc registry-2
                          kw
                          [:tuple
                           [:= opcode]]))
                 registry
                 instr-without-immediate+)

         elem-not-active
         (fn [kw-type]
           (let [base [:map
                       [:wasm.elem/mode [:= kw-type]]]]
             [:multi
              {:dispatch :wasm.elem/resolve}
              [:expr
               (conj base
                     :wasm/reftype
                     [:wasm.elem/resolve [:= :expr]]
                     [:wasm.elem/vec [:vector
                                      :wasm.const/expr]])]
              [:idx
               (conj base
                     :wasm/elemkind
                     [:wasm.elem/resolve [:= :idx]]
                     [:wasm.elem/vec [:vector
                                      :wasm/funcidx]])]]))

         registry-3
         (assoc registry-2
                :wasm/block               [:tuple
                                           [:= wasm.bin/block]
                                           :wasm/blocktype
                                           [:ref :wasm/instr+]]
                :wasm/blocktype           [:or
                                           :nil
                                           [:tuple
                                            [:= :wasm/valtype]
                                            :wasm/valtype]
                                           [:tuple
                                            [:= :wasm/typeidx]
                                            :wasm/typeidx]]
                :wasm/br                  [:tuple
                                           [:= wasm.bin/br]
                                           :wasm/labelidx]
                :wasm/br_if               [:tuple
                                           [:= wasm.bin/br_if]
                                           :wasm/labelidx]
                :wasm/br_table            [:tuple
                                           [:= wasm.bin/br_table]
                                           [:vector
                                            :wasm/labelidx]
                                           :wasm/labelidx]
                :wasm/buffer              [:fn
                                           {:error/message "Must be BinF buffer"
                                            :gen/gen       (tc.gen/fmap binf.string/encode
                                                                        (malli.gen/generator [:string]))}
                                           #?(:clj  (let [klass (class (byte-array 0))]
                                                      #(instance? klass
                                                                  %))
                                              :cljs (if (exists? js/SharedArrayBuffer)
                                                      #(or (instance? js/ArrayBuffer
                                                                      %)
                                                           (instance? js/SharedArrayBuffer
                                                                      %))
                                                      #(instance? js/ArrayBuffer
                                                                  %)))]
                :wasm/byte                [:int
                                           {:max 255
                                            :min 0}]
                :wasm/call                [:tuple
                                           [:= wasm.bin/call]
                                           :wasm/funcidx]
                :wasm/call_indirect       [:tuple
                                           [:= wasm.bin/call_indirect]
                                           :wasm/typeidx
                                           :wasm/tableidx]
                :wasm/code                [:map
                                           :wasm/expr
                                           :wasm/locals]
                :wasm/codesec             [:map-of
                                           :wasm/funcidx
                                           :wasm/code]
                :wasm/data                [:multi
                                           {:dispatch :wasm.data/mode}
                                           [:active  :wasm.data/active]
                                           [:passive :wasm.data/passive]]
                :wasm/data.drop           [:tuple
                                           :wasm.opcode/misc
                                           [:= wasm.bin/data-drop]
                                           :wasm/dataidx]
                :wasm/datacountsec        [:map
                                           :wasm.data/n-seg]
                :wasm/dataidx             :wasm/idx
                :wasm/datasec             [:map-of
                                           :wasm/dataidx
                                           :wasm/data]
                :wasm/elem                [:multi
                                           {:dispatch    :wasm.elem/mode}
                                           [:active      :wasm.elem/active]
                                           [:declarative :wasm.elem/declarative]
                                           [:passive     :wasm.elem/passive]]
                :wasm/elem.drop           [:tuple
                                           :wasm.opcode/misc
                                           [:= wasm.bin/elem-drop]
                                           :wasm/elemidx]
                :wasm/elemidx             :wasm/idx
                :wasm/elemkind            [:enum
                                           wasm.bin/elemkind-funcref]
                :wasm/elemsec             [:map-of
                                           :wasm/elemidx
                                           :wasm/elem]
                :wasm/exportsec           [:map
                                           [:wasm.export/func
                                            [:map-of
                                             :wasm/funcidx
                                             :wasm.export/base]]
                                           [:wasm.export/global
                                            [:map-of
                                             :wasm/globalidx
                                             :wasm.export/base]]
                                           [:wasm.export/mem
                                            [:map-of
                                             :wasm/memidx
                                             :wasm.export/base]]
                                           [:wasm.export/table
                                            [:map-of
                                             :wasm/tableidx
                                             :wasm.export/base]]]
                :wasm/expr                :wasm/instr+
                :wasm/f32                 [:double
                                           {:gen/fmap binf.float/f32
                                            :max      3.402823466e38
                                            :min      1.175494351e-38}]
                :wasm/f32.const           [:tuple
	                                       [:= wasm.bin/f32-const]
	                                       :wasm/f32]
                :wasm/f32.load            (instr-memarg wasm.bin/f32-load)
                :wasm/f32.store           (instr-memarg wasm.bin/f32-store)
                :wasm/f64                 :double
                :wasm/f64.const           [:tuple
	                                       [:= wasm.bin/f64-const]
	                                       :wasm/f64]
                :wasm/f64.load            (instr-memarg wasm.bin/f64-load)
                :wasm/f64.store           (instr-memarg wasm.bin/f64-store)
                :wasm/func                [:map
                                           :wasm/typeidx]
                :wasm/funcidx             :wasm/idx
                :wasm/funcsec             [:map-of
                                           :wasm/funcidx
                                           :wasm/func]
                :wasm/functype            [:tuple
                                           :wasm/resulttype
                                           :wasm/resulttype]
                :wasm/global              [:merge
                                           :wasm/globaltype
                                           [:map
                                            [:wasm/expr :wasm.const/expr]]]
                :wasm/global.get          (instr-global wasm.bin/global-get)
                :wasm/global.set          (instr-global wasm.bin/global-set)
                :wasm/globalidx           :wasm/idx
                :wasm/globalsec           [:map-of
                                           :wasm/globalidx
                                           :wasm/global]
                :wasm/globaltype          [:map
                                           :wasm/mutable?
                                           :wasm/valtype]
                :wasm/i32                 :wasm/s32
                :wasm/i32.const           [:tuple
	                                       [:= wasm.bin/i32-const]
	                                       :wasm/i32]
                :wasm/i32.load            (instr-memarg wasm.bin/i32-load)
                :wasm/i32.load8_s         (instr-memarg wasm.bin/i32-load8_s)
                :wasm/i32.load8_u         (instr-memarg wasm.bin/i32-load8_u)
                :wasm/i32.load16_s        (instr-memarg wasm.bin/i32-load16_s)
                :wasm/i32.load16_u        (instr-memarg wasm.bin/i32-load16_u)
                :wasm/i32.store           (instr-memarg wasm.bin/i32-store)
                :wasm/i32.store8          (instr-memarg wasm.bin/i32-store8)
                :wasm/i32.store16         (instr-memarg wasm.bin/i32-store16)
                :wasm/i32.trunc_sat_f32_s (instr-trunc-sat wasm.bin/i32-trunc_sat_f32_s)
                :wasm/i32.trunc_sat_f32_u (instr-trunc-sat wasm.bin/i32-trunc_sat_f32_u)
                :wasm/i32.trunc_sat_f64_s (instr-trunc-sat wasm.bin/i32-trunc_sat_f64_s)
                :wasm/i32.trunc_sat_f64_u (instr-trunc-sat wasm.bin/i32-trunc_sat_f64_u)
                :wasm/i64                 :wasm/s64
                :wasm/i64.const           [:tuple
	                                       [:= wasm.bin/i64-const]
	                                       :wasm/i64]
                :wasm/i64.load            (instr-memarg wasm.bin/i64-load)
                :wasm/i64.load8_s         (instr-memarg wasm.bin/i64-load8_s)
                :wasm/i64.load8_u         (instr-memarg wasm.bin/i64-load8_u)
                :wasm/i64.load16_s        (instr-memarg wasm.bin/i64-load16_s)
                :wasm/i64.load16_u        (instr-memarg wasm.bin/i64-load16_u)
                :wasm/i64.load32_s        (instr-memarg wasm.bin/i64-load32_s)
                :wasm/i64.load32_u        (instr-memarg wasm.bin/i64-load32_u)
                :wasm/i64.store           (instr-memarg wasm.bin/i64-store)
                :wasm/i64.store8          (instr-memarg wasm.bin/i64-store8)
                :wasm/i64.store16         (instr-memarg wasm.bin/i64-store16)
                :wasm/i64.store32         (instr-memarg wasm.bin/i64-store32)
                :wasm/i64.trunc_sat_f32_s (instr-trunc-sat wasm.bin/i64-trunc_sat_f32_s)
                :wasm/i64.trunc_sat_f32_u (instr-trunc-sat wasm.bin/i64-trunc_sat_f32_u)
                :wasm/i64.trunc_sat_f64_s (instr-trunc-sat wasm.bin/i64-trunc_sat_f64_s)
                :wasm/i64.trunc_sat_f64_u (instr-trunc-sat wasm.bin/i64-trunc_sat_f64_u)
                :wasm/idx                 :wasm/u32
                :wasm/if                  [:tuple
                                           [:= wasm.bin/if-]
                                           :wasm/blocktype
                                           [:ref :wasm/instr+]
                                           [:ref :wasm/instr+]]
                :wasm/instr               instr
                :wasm/instr+              [:vector
                                           :wasm/instr]
                :wasm/importsec           [:map
                                           [:wasm.import/func   [:map-of
                                                                 :wasm/funcidx
                                                                 :wasm.import/func]]
                                           [:wasm.import/global [:map-of
                                                                 :wasm/globalidx
                                                                 :wasm.import/global]]
                                           [:wasm.import/mem    [:map-of
                                                                 :wasm/memidx
                                                                 :wasm.import/mem]]
                                           [:wasm.import/table  [:map-of
                                                                 :wasm/tableidx
                                                                 :wasm.import/table]]]
                :wasm/labelidx            :wasm/idx
                :wasm/local.get           (instr-local wasm.bin/local-get)
                :wasm/local.set           (instr-local wasm.bin/local-set)
                :wasm/local.tee           (instr-local wasm.bin/local-tee)
                :wasm/localidx            :wasm/idx
                :wasm/locals              [:vector
                                           [:tuple
                                            :wasm/u32
                                            :wasm/valtype]]
                :wasm/loop                [:tuple
                                           [:= wasm.bin/loop-]
                                           :wasm/blocktype
                                           [:ref :wasm/instr+]]
                :wasm/limits              [:and
                                           [:map
                                            [:wasm.limit/min
                                             :wasm/u32]
                                            [:wasm.limit/max
                                             {:optional true}
                                             :wasm/u32]]
                                           [:fn
                                            {:error/message "Max limit must be >= min limit"}
                                            (fn [{:wasm.limit/keys [max
                                                                    min]}]
                                              (if max
                                                (>= max
                                                    min)
                                                true))]]
                :wasm/mem                 :wasm/memtype
                :wasm/memidx              :wasm/idx
                :wasm/memory.copy         [:tuple
                                           :wasm.opcode/misc
                                           [:= wasm.bin/memory-copy]
                                           [:= 0]
                                           [:= 0]]
                :wasm/memory.fill         [:tuple
                                           :wasm.opcode/misc
                                           [:= wasm.bin/memory-fill]
                                           [:= 0]]
                :wasm/memory.grow         [:tuple
                                           [:= wasm.bin/memory-grow]
                                           [:= 0]]
                :wasm/memory.init         [:tuple
                                           :wasm.opcode/misc
                                           [:= wasm.bin/memory-init]
                                           :wasm/dataidx
                                           [:= 0]]
                :wasm/memory.size         [:tuple
                                           [:= wasm.bin/memory-size]
                                           [:= 0]]
                :wasm/memsec              [:map-of
                                           :wasm/memidx
                                           :wasm/mem]
                :wasm/memtype             :wasm/limits
                :wasm/module              [:map
                                           [:wasm/version
                                            {:optional true}]
                                           [:wasm/typesec
                                            {:optional true}]
                                           [:wasm/importsec
                                            {:optional true}]
                                           [:wasm/funcsec
                                            {:optional true}]
                                           [:wasm/tablesec
                                            {:optional true}]
                                           [:wasm/memsec
                                            {:optional true}]
                                           [:wasm/globalsec
                                            {:optional true}]
                                           [:wasm/exportsec
                                            {:optional true}]
                                           [:wasm/startsec
                                            {:optional true}]
                                           [:wasm/elemsec
                                            {:optional true}]
                                           [:wasm/datacountsec
                                            {:optional true}]
                                           [:wasm/codesec
                                            {:optional true}]
                                           [:wasm/datasec
                                            {:optional true}]]
                :wasm/mutable?            :boolean
                :wasm/name                :wasm/buffer
                :wasm/numtype             [:enum
                                           wasm.bin/numtype-i32
                                           wasm.bin/numtype-i64
                                           wasm.bin/numtype-f32
                                           wasm.bin/numtype-f64]
                :wasm/offset              :wasm.const/expr
                :wasm/ref.null            [:tuple
                                           [:= wasm.bin/ref-null]
                                           :wasm/reftype]
                :wasm/ref.func            [:tuple
                                           [:= wasm.bin/ref-func]
                                           :wasm/funcidx]
                :wasm/reftype             [:enum
                                           wasm.bin/funcref
                                           wasm.bin/externref]
                :wasm/resulttype          [:maybe
                                           [:vector
                                            {:min 1}
                                            :wasm/valtype]]
                :wasm/s32                 [:int
	                                       {:max 2147483647
	                                        :min -2147483648}]
                :wasm/s64                 #?(:clj  int?
	                                         :cljs [:fn
	                               	           	  {:gen/gen binf.gen/i64}
	                               	           	  #(and (instance? js/BigInt
	                               	             	            	   %)
	                               	                 	    (<= (js/BigInt. "-9223372036854775808")
	                               	                 		    %
	                               	                 	        (js/BigInt. "9223372036854775807")))])
                :wasm/start               [:map
                                           :wasm/funcidx]
                :wasm/startsec            :wasm/start
                :wasm/select-t            [:tuple
                                           [:= wasm.bin/select-t]
                                           [:vector
                                            :wasm/valtype]]
                :wasm/signature           :wasm/functype
                :wasm/table.copy          [:tuple
                                           :wasm.opcode/misc
                                           [:= wasm.bin/table-copy]
                                           :wasm/tableidx
                                           :wasm/tableidx]
                :wasm/table.fill          [:tuple
                                           :wasm.opcode/misc
                                           [:= wasm.bin/table-fill]
                                           :wasm/tableidx]
                :wasm/table.get           [:tuple
                                           [:= wasm.bin/table-get]
                                           :wasm/tableidx]
                :wasm/table.grow          [:tuple
                                           :wasm.opcode/misc
                                           [:= wasm.bin/table-grow]
                                           :wasm/tableidx]
                :wasm/table.init          [:tuple
                                           :wasm.opcode/misc
                                           [:= wasm.bin/table-init]
                                           :wasm/elemidx
                                           :wasm/tableidx]
                :wasm/table.set           [:tuple
                                           [:= wasm.bin/table-set]
                                           :wasm/tableidx]
                :wasm/table.size          [:tuple
                                           :wasm.opcode/misc
                                           [:= wasm.bin/table-size]
                                           :wasm/tableidx]
                :wasm/table               :wasm/tabletype
                :wasm/tableidx            :wasm/idx
                :wasm/tablesec            [:map-of
                                           :wasm/tableidx
                                           :wasm/table]
                :wasm/tabletype           [:merge
                                           :wasm/limits
                                           [:map
                                            :wasm/reftype]]
                :wasm/type                [:map
                                           :wasm/signature]
                :wasm/typeidx             :wasm/idx
                :wasm/typesec             [:map
                                           :wasm/signature]
                :wasm/u32                 [:int
                                           {:max 4294967295
                                            :min 0}]
                :wasm/u64                 #?(:clj  int?
	                                         :cljs [:fn
	                               	      	      {:gen/gen binf.gen/u64}
	                               	      	      #(and (instance? js/BigInt
	                               	      	   	     		   	   %)
	                               	      			    (<= (js/BigInt. 0)
	                               	      			 	    %
	                               	      				    (js/BigInt. "18446744073709551615")))])
                :wasm/valtype             [:or
                                           :wasm/numtype
                                           :wasm/reftype]
                :wasm/version             [:enum
                                           1]
                :wasm.const/expr          [:vector
                                           :wasm.const/instr]
                :wasm.const/instr         [:multi
	                                       {:dispatch first}
	                                       [wasm.bin/i32-const  :wasm/i32.const]
	                                       [wasm.bin/i64-const  :wasm/i64.const]
	                                       [wasm.bin/f32-const  :wasm/f32.const]
	                                       [wasm.bin/f64-const  :wasm/f64.const]
                                           [wasm.bin/ref-null   :wasm/ref.null]
                                           [wasm.bin/ref-func   :wasm/ref.func]
                                           [wasm.bin/global-get :wasm/global.get]]


                :wasm.data/active         [:map
                                           :wasm/buffer
                                           [:wasm/memidx
                                            {:optional true}]
                                           :wasm/offset
                                           [:wasm.data/mode
                                            [:= :active]]]
                :wasm.data/n-seg          :wasm/u32
                :wasm.data/passive        [:map
                                           :wasm/buffer
                                           [:wasm.data/mode
                                            [:= :passive]]]
                :wasm.elem/active         (let [base [:map
                                                      :wasm/offset
                                                      [:wasm.elem/mode [:= :active]]]]
                                            [:multi
                                             {:dispatch :wasm/resolve}
                                             [:expr
                                              (let [base-2 (conj base
                                                                 [:wasm.elem/resolve [:= :expr]]
                                                                 [:wasm.elem/vec     [:vector
                                                                                      :wasm.const/expr]])]
                                                [:multi
                                                 {:dispatch :wasm.elem/tableidx}
                                                 [:nil
                                                  base-2]
                                                 [:malli/default
                                                  (conj base-2
                                                        :wasm/reftype
                                                        :wasm/tableidx)]])]
                                             [:idx
                                              (let [base-2 (conj base
                                                                 [:wasm.elem/resolve [:= :idx]]
                                                                 [:wasm.elem/vec     [:vector
                                                                                      :wasm/funcidx]])]
                                                [:multi
                                                 {:dispatch :wasm.elem/tableidx}
                                                 [nil
                                                  base-2]
                                                 [:malli/default
                                                  (conj base-2
                                                        :wasm/elemkind
                                                        :wasm/tableidx)]])]])
                :wasm.elem/declarative    (elem-not-active :declarative)
                :wasm.elem/passive        (elem-not-active :passive)
                :wasm.export/base         [:vector
                                           {:min 1}
                                           [:map
                                            :wasm/name]]
                :wasm.import/func         [:merge
                                           imported-base
                                           :wasm/func]
                :wasm.import/global       [:merge
                                           imported-base
                                           :wasm/globaltype]
                :wasm.import/mem          [:merge
                                           imported-base
                                           :wasm/memtype]
                :wasm.import/module       :wasm/name
                :wasm.import/name         :wasm/name
                :wasm.import/table        [:merge
                                           imported-base
                                           :wasm/tabletype]
                :wasm.memory/align        :wasm/u32
                :wasm.memory/offset       :wasm/u32
                :wasm.opcode/misc         [:= wasm.bin/misc])
         ]
  registry-3)))





(comment


  (def reg
       (-> (merge (malli/default-schemas)
                  (malli.util/schemas))
           registry))


  (-> (malli/explain [:merge
                      [:map [:a :int]]
                      [:or [:map [:b :string]]]]
                     {:a 42 :b "ok"}
                     {:registry (-> (merge (malli/default-schemas)
                                           (malli.util/schemas))
                                    registry)})
      malli.error/humanize)



  (malli.gen/generate :wasm/module
                      {:registry (-> (merge (malli/default-schemas)
                                            (malli.util/schemas))
                                     registry)})



  (malli.gen/generator [:and
                        {:registry registry}
                        :wasm/name])




  )
