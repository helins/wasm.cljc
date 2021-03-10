;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.write

  ""

  {:author "Adam Helinski"}

  (:require [helins.binf        :as binf]
            [helins.binf.buffer :as binf.buffer]
            [helins.binf.int64  :as binf.int64]
            [helins.binf.leb128 :as binf.leb128]
            [helins.wasm.bin    :as wasm.bin]
            [helins.wasm.count  :as wasm.count]
            [helins.wasm.ir     :as wasm.ir]))


(declare end'
         instr'+
         u32)


;;;;;;;;;; Values


(defn byte'

  [view b8]

  (binf/wr-b8 view
              b8))



(defn f32'

  [view f32]

  (binf/wr-f32 view
               f32))



(defn f64'

  [view f64]

  (binf/wr-f64 view
               f64))



(defn i32'

  [view i32]

  (binf.leb128/wr-i32 view
                      i32))



(defn i64'

  [view i64]

  (binf.leb128/wr-i64 view
                      i64))



(defn name'

  ""

  [view buffer]

  (-> view
      (u32 (count buffer))
      (binf/wr-buffer buffer)))



(defn s33'

  [view s33]

  (binf.leb128/wr-i64 view
                      (binf.int64/i* s33)
                      33))



(defn u32

  [view u32]

  (binf.leb128/wr-u32 view
                      u32))


;;;;;;;;;; Indices


(defn idx

  ""

  [view idx]

  (u32 view
       idx))



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


;;;;;;;;;;


(defn n-byte

  ""

  [view n-byte]

  (u32 view
       n-byte))



(defn section-id

  [view section-id]

  (binf/wr-b8 view
              section-id))


;;;;;;;;;; Types


(defn func

  ""

  [view flatidx-type {:wasm/keys [typeidx]}]

  (typeidx' view
            (flatidx-type typeidx)))



(defn valtype'

  ""

  [view vt]

  (binf/wr-b8 view
              vt))



(defn resulttype

  ""

  [view valtype+]

  (binf.leb128/wr-u32 view
                      (count valtype+))
  (doseq [vt valtype+]
    (valtype' view
              vt))
  view)



(defn functype

  [view [param+ result+]]

  (-> view
      (binf/wr-b8 0x60)
      (resulttype param+)
      (resulttype result+)))



(defn mut'

  [view mutable?]

  (binf/wr-b8 view
              (if mutable?
                wasm.bin/mut-var
                wasm.bin/mut-const)))



(defn globaltype'

  [view {:wasm/keys [mutable?
                     valtype]}]

  (-> view
      (valtype' valtype)
      (mut' mutable?)))



(defn limits'

  ""

  [view {min- :wasm.limit/min
         max- :wasm.limit/max}]

  (if max-
    (-> view
        (binf/wr-b8 wasm.bin/limits-minmax)
        (u32 min-)
        (u32 max-))
    (-> view
        (binf/wr-b8 wasm.bin/limits-min)
        (u32 min-))))



(defn memtype'

  [view hmap]

  (limits' view
           hmap))



(defn elemtype'

  [view elemtype]

  (binf/wr-b8 view
              elemtype))



(defn tabletype'

  [view hmap]

  (-> view
      (elemtype' (hmap :wasm/elemtype))
      (limits' hmap)))


;;;;;;;;;; Instructions - Control


(defn blocktype'

  [view blocktype]

  (if (nil? blocktype)
    (binf/wr-b8 view
                wasm.bin/blocktype-nil)
    ((case (blocktype 0)
       :wasm/typeidx s33'
       :wasm/valtype binf/wr-b8 view
     view
     (blocktype 1)))))



(defn block'

  [view opvec]

  (-> view
      (blocktype' (opvec 1))
      (instr'+ (opvec 2))
      end'))



(defn end'

  [view]

  (binf/wr-b8 view
              wasm.bin/end))



(defn loop'

  [view opvec]

  (block' view
          opvec))



(defn else'

  [view instr+]

  (when (seq instr+)
    (-> view
        (binf/wr-b8 wasm.bin/else)
        (instr'+ instr+)))
  view)



(defn if'

  [view opvec]

  (-> view
      (blocktype' (opvec 1))
      (instr'+ (opvec 2))
      (else' (opvec 3))
      end'))



(defn br'

  [view opvec]

  (labelidx' view
             (opvec 1)))



(defn br_if'

  [view opvec]

  (br' view
       opvec))



(defn br_table'

  [view opvec]

  (let [choice+ (opvec 1)]
    (u32 view
         (count choice+))
    (doseq [labelidx choice+]
      (labelidx' view
                 labelidx))
    (labelidx' (opvec 2))))



(defn call'

  [view opvec]

  (funcidx' view
            (opvec 1)))



(defn call_indirect'

  [view opvec]

  (-> view
      (typeidx' (opvec 1))
      (binf/wr-b8 0)))


;;;;;;;;;; Instructions / Variables


(defn op-var-local

  ""

  [view opvec]

  (localidx' view
             (opvec 1)))



(defn op-var-global

  ""
  
  [view opvec]

  (globalidx' view
              (opvec 1)))


;;;;;;;;;; Instructions / Memory


(defn memarg'

  ""

  [view [align offset]]

  (-> view
      (u32 align)
      (u32 offset)))



(defn op-memarg

  ""

  [view opvec]

  (memarg' view
           (rest opvec)))



(defn op-memory

  ""

  [view opvec]

  (memidx' view
           (opvec 1)))


;;;;;;;;;;; Numeric instructions / Constants


(defn op-constval

  ""


  ([f-value]

   (partial op-constval
            f-value))


  ([f-value view opvec]

   (f-value view
            (opvec 1))))


;;;;;;;;;;; Numeric instructions / Saturated truncation


(defn trunc_sat

  ""

  [view opvec]

  (u32 view
       (opvec 1)))
  

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

  [view opvec]

  (let [opcode (opvec 0)]
    (opcode' view
             opcode)
    (when-some [f (opcode->f opcode)]
      (f view
         opvec)))
  view)



(defn instr'+

  ""

  [view opvec+]

  (doseq [opvec opvec+]
    (instr' view
            opvec))
  view)



(defn expr'

  ""

  [view opvec+]

  (-> view
      (instr'+ opvec+)
      end'))


;;;;;;;;;; Sections / Helper


(defn section-externval

  ""

  [view ctx k-section bin-section-id k-count compile-item]

  (when-some [section (not-empty (ctx k-section))]
    (-> view
        (section-id bin-section-id)
        (n-byte (get-in ctx
                        [:wasm/write
                         k-count]))
        (u32 (count section)))
    (doseq [item (vals section)]
      (compile-item view
                    item)))
  view)


;;;;;;;;;; Sections / Export


(defn export'

  [view space flatidx bin-export-type compile-idx]

  (doseq [[idx
           name+] space]
    (let [idx-2 (flatidx idx)]
      (doseq [{buffer :wasm/name} name+]
        (-> view
            (name' buffer)
            (binf/wr-b8 bin-export-type)
            (compile-idx idx-2)))))
  view)



(defn exportsec'

  [view {:wasm/keys                     [exportsec]
         {:as      ctx-write
          n-byte-  :wasm.count/exportsec
          n-export :wasm.export/n}      :wasm/write}]

  (when (pos? n-export)
    (println :n n-export :byte n-byte-)
    (-> view
        (section-id wasm.bin/section-id-export)
        (n-byte n-byte-)
        (u32 n-export)
        (export' (exportsec :wasm.export/func)
                 (ctx-write :wasm.flatidx/func)
                 wasm.bin/exportdesc-func
                 funcidx')
        (export' (exportsec :wasm.export/global)
                 (ctx-write :wasm.flatidx/global)
                 wasm.bin/exportdesc-global
                 globalidx')
        (export' (exportsec :wasm.export/mem)
                 (ctx-write :wasm.flatidx/mem)
                 wasm.bin/exportdesc-mem
                 memidx')
        (export' (exportsec :wasm.export/table)
                 (ctx-write :wasm.flatidx/table)
                 wasm.bin/exportdesc-table
                 tableidx')))
  view)


;;;;;;;;;; Sections / Func


(defn funcsec'

  [view ctx]

  (section-externval view
                     ctx
                     :wasm/funcsec
                     wasm.bin/section-id-func
                     :wasm.count/funcsec
                     (let [flatidx-type (get-in ctx
                                                [:wasm/write
                                                 :wasm.flatidx/type])]
                       (fn compile-item [view item]
                         (func view
                               flatidx-type
                               item)))))


;;;;;;;;;; Sections / Global


(defn global'

  [view global]

  (-> view
      (globaltype' global)
      (expr' (global :wasm/expr))))

  

(defn globalsec'

  [view ctx]

  (section-externval view
                     ctx
                     :wasm/globalsec
                     wasm.bin/section-id-global
                     :wasm.count/globalsec
                     global'))


;;;;;;;;;; Sections / Import


(defn import'+

  ""

  [view space import-type f]

  (doseq [{:as           hmap
           buffer-module :wasm.import/module
           buffer-name   :wasm.import/name}  (vals space)]
    (-> view
        (name' buffer-module)
        (name' buffer-name)
        (binf/wr-b8 import-type)
        (f hmap)))
  view)



(defn importsec'

  ""

  [view {:wasm/keys                          [importsec]
         {flatidx-type :wasm.flatidx/type
          n-byte-      :wasm.count/importsec
          n-import     :wasm.import/n}       :wasm/write}]

  (when (pos? n-byte-)
    (-> view
        (section-id wasm.bin/section-id-import)
        (n-byte n-byte-)
        (u32 n-import)
        (import'+ (importsec :wasm.import/func)
                  wasm.bin/importdesc-func
                  (fn [view hmap]
                    (func view
                          flatidx-type
                          hmap)))
        (import'+ (importsec :wasm.import/global)
                  wasm.bin/importdesc-global
                  globaltype')
        (import'+ (importsec :wasm.import/mem)
                  wasm.bin/importdesc-mem
                  memtype')
        (import'+ (importsec :wasm.import/table)
                  wasm.bin/importdesc-table
                  tabletype')))
  view)


;;;;;;;;;; Sections / mem


(defn memsec'

  [view ctx]

  (section-externval view
                     ctx
                     :wasm/memsec
                     wasm.bin/section-id-mem
                     :wasm.count/memsec
                     memtype'))


;;;;;;;;;; Sections / Start


(defn startsec'

  [view {:as        ctx
         :wasm/keys [startsec]}]

  (when startsec
    (let [ctx-write (ctx :wasm/write)]
      (-> view
          (section-id wasm.bin/section-id-start)
          (n-byte (ctx-write :wasm.count/startsec))
          (funcidx' (get-in ctx-write
                            [:wasm.flatidx/func
                             (startsec :wasm/funcidx)])))))
  view)


;;;;;;;;;; Sections / Table


(defn tablesec'

  [view ctx]

  (section-externval view
                     ctx
                     :wasm/tablesec
                     wasm.bin/section-id-table
                     :wasm.count/tablesec
                     tabletype'))


;;;;;;;;;; Sections / Type


(defn typesec

  ""

  [view {:as        ctx
         :wasm/keys [typesec]}]

  (when (seq typesec)
    (-> view
        (section-id wasm.bin/section-id-type)
        (n-byte (get-in ctx
                        [:wasm/write
                         :wasm.count/typesec]))
        (u32 (count typesec)))
    (doseq [signature (map :wasm/signature
                           (vals typesec))]
      (functype view
                signature)))
  view)


;;;;;;;;;; Module


(defn magic

  ""

  [view]

  (binf/wr-b32 view
               wasm.bin/magic))



(defn version

  ""

  [view {:wasm/keys [version]}]

  (binf/wr-b32 view
               (condp =
                      version
                 wasm.bin/version-1 wasm.bin/version-1)))




(defn main

  ""

  [ctx]

  (-> (get-in ctx
              [:wasm/write
               :wasm.count/module])
      binf.buffer/alloc
      binf/view
      (binf/endian-set :little-endian)
      magic
      (version ctx)
      (typesec ctx)
      (importsec' ctx)
      (funcsec' ctx)
      (tablesec' ctx)
      (memsec' ctx)
      (globalsec' ctx)
      (exportsec' ctx)
      (startsec' ctx)
      ))
