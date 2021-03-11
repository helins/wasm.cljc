;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.count

  ""

  {:author "Adam Helinski"}

  (:require [helins.binf        :as binf]
            [helins.binf.buffer :as binf.buffer]
            [helins.binf.int64  :as binf.int64]
            [helins.binf.leb128 :as binf.leb128]
            [helins.binf.string :as binf.string]
            [helins.wasm.bin    :as wasm.bin]
            [helins.wasm.ir     :as wasm.ir]))


(declare instr'+
         u32)


;;;;;;;;;; Values


(def byte'
     1)



(defn name'

  [buffer]

  (let [n-byte (count buffer)]
    (+ (u32 n-byte)
       n-byte)))



(defn f32'

  [_f32]

  binf/sz-f32)



(defn f64'

  [_f64]

  binf/sz-f64)




(defn i32'

  [i32]

  (binf.leb128/n-byte-i32 i32))



(defn i64'

  [i64]

  (binf.leb128/n-byte-i32 i64))



(defn s33'

  [s33]

  (binf.leb128/n-byte-i32 s33))


(defn u32

  [u32]

  (binf.leb128/n-byte-u32 u32))


;;;;;;;;;; Indices


(defn idx

  [idx]

  (u32 idx))



(def funcidx'

  ""

  idx)



(def globalidx'

  ""

  idx)



(def labelidx'

  ""

  idx)


(def localidx'

  ""

  idx)


(def memidx'

  ""

  idx)



(def tableidx'

  ""

  idx)



(def typeidx'
  
  ""

  idx)


;;;;;;;;;; Types


(def valtype
     1)



(defn resulttype

  ""

  [valtype+]

  (let [n-valtype (count valtype+)]
    (+ (u32 n-valtype)
       (* n-valtype
          valtype))))



(defn functype

  ""

  [[param+ result+]]

  (+ 1 ;; 0x60 functype
     (resulttype param+)
     (resulttype result+)))



(defn func

  ""

  [flatidx-type {:wasm/keys [typeidx]}]

  (typeidx' (flatidx-type typeidx)))
  


(def elemtype'
     1)



(defn globaltype'

  [_global]

  2)



(defn limits

  ""

  [{min- :wasm.limit/min
    max- :wasm.limit/max}]

  (let [n-byte (+ 1             ;; Byte specifying if there is a maximum or not
                  (u32 min-))]
    (if max-
      (+ n-byte
         (u32 max-))
      n-byte)))



(defn memtype'

  ""

  [mem]

  (limits mem))



(defn tabletype'

  ""

  [table]

  (+ elemtype'
     (limits table)))


;;;;;;;;;; Instructions - Control


(defn blocktype'

  [blocktype]

  (if (nil? blocktype)
    1
    (case (blocktype 0)
      :wasm/typeidx (s33' (blocktype 1))
      :wasm/valtype 1)))



(defn block'

  [opvec]

  (+ (blocktype' (opvec 1))
     (instr'+ (opvec 2))
     1  ;;  END
     ))



(defn loop'

  [opvec]

  (block' opvec))



(defn else'

  [instr+]

  (if (seq instr+)
    (+ 1  ;; `else` opcode
       (instr'+ instr+))
    0))



(defn if'

  [opvec]

  (+ (blocktype' (opvec 1))
     (instr'+ (opvec 2))
     (else' (opvec 3))
     1  ;;  END
     ))



(defn br'

  [opvec]

  (labelidx' (opvec 1)))



(defn br_if'

  [opvec]

  (br' opvec))



(defn br_table'

  [opvec]

  (let [choice+ (opvec 1)]
    (+ (u32 (count choice+))
       (reduce (fn [sum labelidx]
                 (+ sum
                    (labelidx' labelidx)))
               0
               choice+)
       (labelidx' (opvec 2)))))



(defn call'

  [opvec]

  (funcidx' (opvec 1)))



(defn call_indirect'

  [opvec]

  (+ (typeidx' (opvec 1))
     byte'))


;;;;;;;;;; Instructions / Variables


(defn op-var-local

  ""

  [opvec]

  (localidx' (opvec 1)))



(defn op-var-global

  ""
  
  [opvec]

  (globalidx' (opvec 1)))


;;;;;;;;;; Instructions / Memory


(defn memarg'

  ""

  [[align offset]]

  (+ (u32 align)
     (u32 offset)))



(defn op-memarg

  ""

  [opvec]

  (memarg' (rest opvec)))



(defn op-memory

  ""

  [opvec]

  (memidx' (opvec 1)))


;;;;;;;;;;; Numeric instructions / Constants


(defn op-constval

  ""


  ([f-value]

   (partial op-constval
            f-value))


  ([f-value opvec]

   (f-value (opvec 1))))


;;;;;;;;;;; Numeric instructions / Saturated truncation


(defn trunc_sat

  ""

  [opvec]

  (u32 (opvec 1)))
  

;;;;;;;;;; Instructions / Registry


(def opcode'
     byte')



(def opcode->f

  ""

  {wasm.bin/block         block'
   wasm.bin/loop-         loop'
   wasm.bin/if-           if'
   wasm.bin/br            br'
   wasm.bin/br_if         br_if'
   wasm.bin/br_table      br_table'
   wasm.bin/call          call'
   wasm.bin/call_indirect call_indirect'
   wasm.bin/local-get     op-var-local
   wasm.bin/local-set     op-var-local
   wasm.bin/local-tee     op-var-local
   wasm.bin/global-get    op-var-global
   wasm.bin/global-set    op-var-global
   wasm.bin/i32-load      op-memarg 
   wasm.bin/i64-load      op-memarg
   wasm.bin/f32-load      op-memarg
   wasm.bin/f64-load      op-memarg
   wasm.bin/i32-load8_s   op-memarg
   wasm.bin/i32-load8_u   op-memarg
   wasm.bin/i32-load16_s  op-memarg
   wasm.bin/i32-load16_u  op-memarg
   wasm.bin/i64-load8_s   op-memarg
   wasm.bin/i64-load8_u   op-memarg
   wasm.bin/i64-load16_s  op-memarg
   wasm.bin/i64-load16_u  op-memarg
   wasm.bin/i64-load32_s  op-memarg
   wasm.bin/i64-load32_u  op-memarg
   wasm.bin/i32-store     op-memarg
   wasm.bin/i64-store     op-memarg
   wasm.bin/f32-store     op-memarg
   wasm.bin/f64-store     op-memarg
   wasm.bin/i32-store8    op-memarg
   wasm.bin/i32-store16   op-memarg
   wasm.bin/i64-store8    op-memarg
   wasm.bin/i64-store16   op-memarg
   wasm.bin/i64-store32   op-memarg
   wasm.bin/memory-size   op-memory
   wasm.bin/memory-grow   op-memory
   wasm.bin/i32-const     (op-constval i32')
   wasm.bin/i64-const     (op-constval i64')
   wasm.bin/f32-const     (op-constval f32')
   wasm.bin/f64-const     (op-constval f64')
   wasm.bin/trunc_sat     trunc_sat})



(defn instr'

  ""

  [opvec]

  (if-some [f (opcode->f (opvec 0))]
    (+ opcode'
       (f opvec))
    opcode'))



(defn instr'+

  ""

  [opvec+]

  (reduce (fn [sum opvec]
            (+ sum
               (instr' opvec)))
          0
          opvec+))



(defn expr'

  ""

  [opvec+]

  (+ (instr'+ opvec+)
     1  ;;  END
     ))


;;;;;;;;;; Sections / Helpers


(defn section-externval

  ""

  [ctx k-section k-flatidx k-count count-item]

  (if-some [sec (not-empty (ctx k-section))]
    (update ctx
            :wasm/write
            (fn [ctx-write]
              (as-> ctx-write
                    ctx-write-2
                (assoc ctx-write-2
                       k-count
                       0)
                (reduce-kv (fn [ctx-write-3 idx item]
                             (-> ctx-write-3
                                 (update k-flatidx
                                         #(assoc %
                                                 idx
                                                 (count %)))
                                 (update k-count
                                         #(+ %
                                             (count-item item)))))
                           ctx-write-2
                           sec)
                (update ctx-write-2
                        k-count
                        #(+ %
                            (u32 (count sec)))))))
    ctx))


;;;;;;;;; Sections / Code


(defn locals'

  [local+]

  (reduce (fn [sum [n _valtype]]
            (+ sum
               (u32 n)
               valtype))
          0
          local+))



(defn func'

  [{:wasm/keys [expr
                local+]}]

  (+ (u32 (count local+))
     (locals' local+)
     (expr' expr)))



(defn code'

  ""

  [ctx-write code]

  (let [n-byte (func' code)]
    (-> ctx-write
        (update :wasm.count/codesec
                #(+ %
                    (u32 n-byte)
                    n-byte))
        (update :wasm.codesec/func-size
                conj
                n-byte))))



(defn codesec'

  ""

  [{:as        ctx
    :wasm/keys [codesec]}]

  (if (seq codesec)
    (update ctx
            :wasm/write
            (fn [ctx-write]
              (reduce code'
                      (assoc ctx-write
                             :wasm.codesec/func-size []
                             :wasm.count/codesec     (u32 (count codesec)))
                      (vals codesec))))
    ctx))


;;;;;;;;;; Sections / Elem


(defn elemsec'

  ""

  [{:as        ctx
    :wasm/keys [elemsec]}]

  (if (seq elemsec)
    (update ctx
            :wasm/write
            (fn [{:as           ctx-write
                  flatidx-func  :wasm.flatidx/func
                  flatidx-table :wasm.flatidx/table}]
              (let [ctx-write-2 (reduce-kv (fn [ctx-write-2 tableidx elem+]
                                             (let [n-byte-tableidx (-> tableidx
                                                                       flatidx-table
                                                                       tableidx')
                                                   funcidx'2       (comp funcidx'
                                                                         flatidx-func)
                                                   n-elem          (count elem+)]
                                               (reduce (fn [ctx-write-3 {:wasm/keys [funcidx+
                                                                                     offset]}]
                                                         (update ctx-write-3
                                                                 :wasm.count/elemsec
                                                                 #(+ %
                                                                     n-byte-tableidx
                                                                     (expr' offset)
                                                                     (u32 n-elem)
                                                                     (reduce (fn [sum funcidx]
                                                                               (+ sum
                                                                                  (funcidx'2 funcidx)))
                                                                             0
                                                                             funcidx+))))
                                                       (update ctx-write-2
                                                               :wasm.elem/n
                                                               #(+ %
                                                                   n-elem))
                                                       elem+)))
                                           (assoc ctx-write
                                                  :wasm.count/elemsec 0
                                                  :wasm.elem/n        0)
                                           elemsec)]
                (update ctx-write-2
                        :wasm.count/elemsec
                        #(+ (u32 (ctx-write-2 :wasm.elem/n))
                            %)))))
    ctx))


;;;;;;;;;; Sections / Export


(defn export'

  ""

  [ctx-write space k-flatidx count-idx]

  (let [count-idx-2 (comp count-idx
                          (ctx-write k-flatidx))]
    (reduce-kv (fn [ctx-write-2 idx name+]
                 (let [n-byte-exportdesc (+ byte'
                                            (count-idx-2 idx))]
                   (reduce (fn [ctx-write-3 {buffer :wasm/name}]
                             (let [n-byte-name (count buffer)]
                               (update ctx-write-3
                                       :wasm.count/exportsec
                                       #(+ % n-byte-exportdesc
                                           (u32 n-byte-name)
                                           n-byte-name))))
                           ctx-write-2
                           name+)))
               (update ctx-write
                       :wasm.export/n
                       #(+ %
                           (count space)))
               space)))



(defn exportsec'

  ""

  [{:as        ctx
    :wasm/keys [exportsec]}]

  (update ctx
          :wasm/write
          (fn [ctx-write]
            (let [ctx-write-2 (-> ctx-write
                                  (assoc :wasm.count/exportsec 0
                                         :wasm.export/n        0)
                                  (export' (exportsec :wasm.export/func)
                                           :wasm.flatidx/func
                                           funcidx')
                                  (export' (exportsec :wasm.export/global)
                                           :wasm.flatidx/global
                                           globalidx')
                                  (export' (exportsec :wasm.export/mem)
                                           :wasm.flatidx/mem
                                           memidx')
                                  (export' (exportsec :wasm.export/table)
                                           :wasm.flatidx/table
                                           tableidx'))]
              (update ctx-write-2
                      :wasm.count/exportsec
                      (fn [n-byte]
                        (if (pos? n-byte)
                          (+ (u32 (ctx-write-2 :wasm.export/n))
                             n-byte)
                          0)))))))


;;;;;;;;;; Sections / Func


(defn funcsec'

  ""

  [ctx]

  (section-externval ctx
                     :wasm/funcsec
                     :wasm.flatidx/func
                     :wasm.count/funcsec
                     (partial func
                              (get-in ctx
                                      [:wasm/write
                                       :wasm.flatidx/type]))))


;;;;;;;;;; Sections / Global


(defn global'

  [global]

  (+ (globaltype' global)
     (expr' (global :wasm/expr))))



(defn globalsec'

  [ctx]

  (section-externval ctx
                     :wasm/globalsec
                     :wasm.flatidx/global
                     :wasm.count/globalsec
                     global'))


;;;;;;;;;; Sections / Import


(defn importdesc

  ""

  [ctx-write space k-flatidx f-item]

  (if (seq space)
    (-> (reduce-kv (fn [ctx-write-2 idx hmap]
                     (-> ctx-write-2
                         (update k-flatidx
                                 (fn [flatidx]
                                   (assoc flatidx
                                          idx
                                          (count flatidx))))
                         (update :wasm.count/importsec
                                 #(+ %
                                     (name' (hmap :wasm.import/module))
                                     (name' (hmap :wasm.import/name))
                                     1  ;; byte specifying importdesc type
                                     (f-item hmap)))))
                   ctx-write
                   space)
        (update :wasm.import/n
                #(+ %
                    (count space))))
    ctx-write))



(defn importsec'

  [{:as        ctx
    :wasm/keys [importsec]}]

  (let [ctx-write   (ctx :wasm/write)
        ctx-write-2 (-> ctx-write
                        (assoc :wasm.count/importsec 0
                               :wasm.import/n        0)
                        (importdesc (importsec :wasm.import/func)
                                    :wasm.flatidx/func
                                    (partial func
                                             (ctx-write :wasm.flatidx/type)))
                        (importdesc (importsec :wasm.import/global)
                                    :wasm.flatidx/global
                                    globaltype')
                        (importdesc (importsec :wasm.import/mem)
                                    :wasm.flatidx/mem
                                    memtype')
                        (importdesc (importsec :wasm.import/table)
                                    :wasm.flatidx/table
                                    tabletype'))]
    (assoc ctx
           :wasm/write
           (update ctx-write-2
                   :wasm.count/importsec
                   (fn [n-byte]
                     (if (zero? n-byte)
                       0
                       (+ (u32 (ctx-write-2 :wasm.import/n))
                          n-byte)))))))


;;;;;;;;;; Sections / Mem


(defn memsec'

  [ctx]

  (section-externval ctx
                     :wasm/memsec
                     :wasm.flatidx/tablesec
                     :wasm.count/memsec
                     memtype'))


;;;;;;;;;; Sections / Start


(defn startsec'

  [{:as        ctx
    :wasm/keys [startsec]}]

  (if startsec
    (update ctx
            :wasm/write
            (fn [ctx-write]
              (assoc ctx-write
                     :wasm.count/startsec
                     (funcidx' (get-in ctx-write
                                       [:wasm.flatidx/func
                                        (startsec :wasm/funcidx)])))))
    ctx))


;;;;;;;;;; Sections / Table


(defn tablesec'

  [ctx]

  (section-externval ctx
                     :wasm/tablesec
                     :wasm.flatidx/table
                     :wasm.count/tablesec
                     tabletype'))


;;;;;;;;;; Sections / Type


(defn typesec

  [{:as        ctx
    :wasm/keys [typesec]}]

  (let [n (count typesec)]
    (if (pos? n)
      (loop [idx-real                    0
             idx-resolve                 {}
             n-byte                      0
             [[idx
               {:wasm/keys [signature]}]
              & typesec-2]               typesec]
        (let [idx-resolve-2 (assoc idx-resolve
                                   idx
                                   idx-real)
              n-byte-2      (+ n-byte
                               (functype signature))]
          (if typesec-2
            (recur (inc idx-real)
                   idx-resolve-2
                   n-byte-2
                   typesec-2)
            (update ctx
                    :wasm/write
                    #(assoc %
                            :wasm.count/typesec (+ (u32 n)
                                                   n-byte-2)
                            :wasm.flatidx/type  idx-resolve-2)))))
      ctx)))


;;;;;;;;;; Sections / Main
 

(def section-id
     1)



(defn section'

  ""

  [n-byte]

  (if (and n-byte
           (pos? n-byte))
    (+ section-id
       (u32 n-byte)
       n-byte)
    0))



(defn section+

  ""

  [{{:wasm.count/keys [codesec
                       elemsec
                       exportsec
                       funcsec
                       globalsec
                       importsec
                       memsec
                       startsec
                       tablesec
                       typesec]} :wasm/write}]

  (reduce (fn [n-byte-total n-byte-section]
            (if n-byte-section
              (+ n-byte-total
                 (section' n-byte-section))
              n-byte-total))
          0
          [codesec
           elemsec
           exportsec
           funcsec
           globalsec
           importsec
           memsec
           startsec
           tablesec
           typesec]))
  

;;;;;;;;;; Module


(def magic'
     4)



(def version'
     4)



(defn module

  ""

  [ctx]

  (let [ctx-2 (-> ctx
                  typesec
                  importsec'
                  funcsec'
                  tablesec'
                  memsec'
                  globalsec'
                  exportsec'
                  startsec'
                  elemsec'
                  codesec'
                  )]
    (assoc-in ctx-2
              [:wasm/write
               :wasm.count/module]
              (+ magic'
                 version'
                 (section+ ctx-2)))))
