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


(declare code'
         elemtype'
         end'
         export'
         func'
         funcidx'
         global'
         globalidx'
         import'+
         instr'+
         labelidx'
         locals'
         localidx'
         memidx'
         mut'
         tableidx'
         typeidx'
         u32')


;;;;;;;;;; Values / Byte


(defn byte'

  [view b8]

  (binf/wr-b8 view
              b8))


;;;;;;;;;; Values / Integers


(defn i32'

  [view i32]

  (binf.leb128/wr-i32 view
                      i32))



(defn i64'

  [view i64]

  (binf.leb128/wr-i64 view
                      i64))



(defn s33'

  [view s33]

  (binf.leb128/wr-i64 view
                      (binf.int64/i* s33)
                      33))



(defn u32'

  [view u32]

  (binf.leb128/wr-u32 view
                      u32))


;;;;;;;;;; Values / Floating-Point


(defn f32'

  [view f32]

  (binf/wr-f32 view
               f32))



(defn f64'

  [view f64]

  (binf/wr-f64 view
               f64))


;;;;;;;;;; Values / Names


(defn name'

  ""

  [view buffer]

  (-> view
      (u32' (count buffer))
      (binf/wr-buffer buffer)))


;;;;;;;;;; Types / Value Types


(defn valtype'

  ""

  [view vt]

  (binf/wr-b8 view
              vt))


;;;;;;;;;; Types / Result Types


(defn resulttype'

  ""

  [view valtype+]

  (binf.leb128/wr-u32 view
                      (count valtype+))
  (doseq [vt valtype+]
    (valtype' view
              vt))
  view)


;;;;;;;;;; Types / Function Types


(defn functype'

  [view [param+ result+]]

  (-> view
      (binf/wr-b8 0x60)
      (resulttype' param+)
      (resulttype' result+)))


;;;;;;;;;; Types / Limits


(defn limits'

  ""

  [view {min- :wasm.limit/min
         max- :wasm.limit/max}]

  (if max-
    (-> view
        (binf/wr-b8 wasm.bin/limits-minmax)
        (u32' min-)
        (u32' max-))
    (-> view
        (binf/wr-b8 wasm.bin/limits-min)
        (u32' min-))))


;;;;;;;;;; Types / Memory Types


(defn memtype'

  [view hmap]

  (limits' view
           hmap))


;;;;;;;;;; Types / Table types


(defn tabletype'

  [view hmap]

  (-> view
      (elemtype' (hmap :wasm/elemtype))
      (limits' hmap)))



(defn elemtype'

  [view elemtype]

  (binf/wr-b8 view
              elemtype))


;;;;;;;;;; Types / Global types


(defn globaltype'

  [view {:wasm/keys [mutable?
                     valtype]}]

  (-> view
      (valtype' valtype)
      (mut' mutable?)))



(defn mut'

  [view mutable?]

  (binf/wr-b8 view
              (if mutable?
                wasm.bin/mut-var
                wasm.bin/mut-const)))


;;;;;;;;;; Instructions / Control Instructions


(defn blocktype'

  [view blocktype]

  (if (nil? blocktype)
    (binf/wr-b8 view
                wasm.bin/blocktype-nil)
    ((case (blocktype 0)
       :wasm/typeidx s33'
       :wasm/valtype binf/wr-b8 view)
     view
     (blocktype 1))))



(defn block'

  [view flatidx opvec]

  (-> view
      (blocktype' (opvec 1))
      (instr'+ flatidx
               (opvec 2))
      end'))



(defn end'

  [view]

  (binf/wr-b8 view
              wasm.bin/end))



(defn loop'

  [view flatidx opvec]

  (block' view
          flatidx
          opvec))



(defn else'

  [view flatidx instr+]

  (when (seq instr+)
    (-> view
        (binf/wr-b8 wasm.bin/else)
        (instr'+ flatidx
                 instr+)))
  view)



(defn if'

  [view flatidx opvec]

  (-> view
      (blocktype' (opvec 1))
      (instr'+ flatidx
               (opvec 2))
      (else' flatidx
             (opvec 3))
      end'))



(defn br'

  [view _flatidx opvec]

  (labelidx' view
             (opvec 1)))



(defn br_if'

  [view _flatidx opvec]

  (br' view
       nil
       opvec))



(defn br_table'

  [view _flatidx opvec]

  (let [choice+ (opvec 1)]
    (u32' view
          (count choice+))
    (doseq [labelidx choice+]
      (labelidx' view
                 labelidx))
    (labelidx' (opvec 2))))



(defn call'

  [view flatidx opvec]

  (funcidx' view
            ((flatidx :wasm.flatidx/func)
             (opvec 1))))



(defn call_indirect'

  [view flatidx opvec]

  (-> view
      (typeidx' ((flatidx :wasm.flatidx/type)
                 (opvec 1)))
      (binf/wr-b8 0)))


;;;;;;;;;; Instructions / Variable Instructions


(defn op-var-local

  ""

  [view _flatidx opvec]

  (localidx' view
             (opvec 1)))



(defn op-var-global

  ""
  
  [view flatidx opvec]

  (globalidx' view
              ((flatidx :wasm.flatidx/global)
               (opvec 1))))


;;;;;;;;;; Instructions / Memory Instructions


(defn memarg'

  ""

  [view [align offset]]

  (-> view
      (u32' align)
      (u32' offset)))



(defn op-memarg

  ""

  [view _flatidx opvec]

  (memarg' view
           (rest opvec)))



(defn op-memory

  ""

  [view flatidx opvec]

  (memidx' view
           ((flatidx :wasm.flatidx/mem')
            (opvec 1))))


;;;;;;;;;; Instructions / Numeric Instructions


(defn op-constval

  ""


  ([f-value]

   (partial op-constval
            f-value))


  ([f-value view _flatidx opvec]

   (f-value view
            (opvec 1))))



(defn trunc_sat

  ""

  [view _flatidx opvec]

  (u32' view
        (opvec 1)))
  

;;;;;;;;;; Instructions / Expressions


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

   wasm.bin/misc     trunc_sat})



(defn instr'

  ""

  [view flatidx opvec]

  (let [opcode (opvec 0)]
    (opcode' view
             opcode)
    (when-some [f (opcode->f opcode)]
      (f view
         flatidx
         opvec)))
  view)



(defn instr'+

  ""

  [view flatidx opvec+]

  (doseq [opvec opvec+]
    (instr' view
            flatidx
            opvec))
  view)



(defn expr'

  ""

  [view flatidx opvec+]

  (-> view
      (instr'+ flatidx
               opvec+)
      end'))


;;;;;;;;;; Modules / Indices


(defn idx

  ""

  [view idx]

  (u32' view
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


;;;;;;;;;; Modules / Sections


(defn section-id

  [view section-id]

  (binf/wr-b8 view
              section-id))


;;;;;;;;;; Sections / (Helpers)


(defn func

  ""

  [view flatidx-type {:wasm/keys [typeidx]}]

  (typeidx' view
            (flatidx-type typeidx)))



(defn section-externval

  ""

  [view ctx k-section bin-section-id k-count compile-item]

  (when-some [section (not-empty (ctx k-section))]
    (-> view
        (section-id bin-section-id)
        (u32' (get-in ctx
                        [:wasm/write
                         k-count]))
        (u32' (count section)))
    (doseq [item (vals section)]
      (compile-item view
                    item)))
  view)


;;;;;;;;;; Modules / Type Section


(defn typesec'

  ""

  [view {:as        ctx
         :wasm/keys [typesec]}]

  (when (seq typesec)
    (-> view
        (section-id wasm.bin/section-id-type)
        (u32' (get-in ctx
                      [:wasm/write
                       :wasm.count/typesec]))
        (u32' (count typesec)))
    (doseq [signature (map :wasm/signature
                           (vals typesec))]
      (functype' view
                 signature)))
  view)


;;;;;;;;;; Modules / Import Section


(defn importsec'

  ""

  [view {:wasm/keys                          [importsec]
         {flatidx-type :wasm.flatidx/type
          n-byte-      :wasm.count/importsec
          n-import     :wasm.import/n}       :wasm/write}]

  (when (pos? n-byte-)
    (-> view
        (section-id wasm.bin/section-id-import)
        (u32' n-byte-)
        (u32' n-import)
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


;;;;;;;;;; Modules / Function Section


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


;;;;;;;;;; Modules / Table Section


(defn tablesec'

  [view ctx]

  (section-externval view
                     ctx
                     :wasm/tablesec
                     wasm.bin/section-id-table
                     :wasm.count/tablesec
                     tabletype'))


;;;;;;;;;; Modules / Memory Section


(defn memsec'

  [view ctx]

  (section-externval view
                     ctx
                     :wasm/memsec
                     wasm.bin/section-id-mem
                     :wasm.count/memsec
                     memtype'))


;;;;;;;;;; Modules / Global section


(defn globalsec'

  [view {:as       ctx
         ctx-write :wasm/write}]

  (section-externval view
                     ctx
                     :wasm/globalsec
                     wasm.bin/section-id-global
                     :wasm.count/globalsec
                     (fn [view global]
                       (global' view
                                ctx-write
                                global))))



(defn global'

  [view flatidx global]

  (-> view
      (globaltype' global)
      (expr' flatidx
             (global :wasm/expr))))


;;;;;;;;;; Modules / Export Section


(defn exportsec'

  [view {:wasm/keys                     [exportsec]
         {:as      ctx-write
          n-byte-  :wasm.count/exportsec
          n-export :wasm.export/n}      :wasm/write}]

  (when (pos? n-export)
    (-> view
        (section-id wasm.bin/section-id-export)
        (u32' n-byte-)
        (u32' n-export)
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


;;;;;;;;;; Modules / Start Section


(defn startsec'

  [view {:as        ctx
         :wasm/keys [startsec]}]

  (when startsec
    (let [ctx-write (ctx :wasm/write)]
      (-> view
          (section-id wasm.bin/section-id-start)
          (u32' (ctx-write :wasm.count/startsec))
          (funcidx' (get-in ctx-write
                            [:wasm.flatidx/func
                             (startsec :wasm/funcidx)])))))
  view)


;;;;;;;;;; Modules / Element Section


(defn elemsec'

  ""

  [view {:as        ctx
         :wasm/keys [elemsec]}]

  (when (seq elemsec)
    (let [{:as           ctx-write
           flatidx-func  :wasm.flatidx/func
           flatidx-table :wasm.flatidx/table} (ctx :wasm/write)]
      (-> view
          (section-id wasm.bin/section-id-elem)
          (u32' (ctx-write :wasm.count/elemsec))
          (u32' (ctx-write :wasm.elem/n)))
      (doseq [[tableidx
               elem+]   elemsec]
        (let [tableidx-2 (flatidx-table tableidx)]
          (doseq [{:wasm/keys [funcidx+
                               offset]} elem+]
            (-> view
                (tableidx' tableidx-2)
                (expr' ctx-write
                       offset)
                (u32' (count funcidx+)))
            (doseq [funcidx funcidx+]
              (funcidx' view
                        (flatidx-func funcidx))))))))
  view)


;;;;;;;;;; Modules / Code Section


(defn codesec'

  ""

  [view {:as        ctx
         :wasm/keys [codesec]}]

  (when (seq codesec)
    (let [ctx-write (ctx :wasm/write)]
      (-> view
          (section-id wasm.bin/section-id-code)
          (u32' (ctx-write :wasm.count/codesec))
          (u32' (count codesec)))
      (doseq [[n-byte
               code]  (partition 2
                                 (interleave (ctx-write :wasm.codesec/func-size)
                                             (vals codesec)))]
        (code' view
               ctx-write
               n-byte
               code))))
  view)



(defn code'

  ""

  [view flatidx n-byte code]

  (-> view
      (u32' n-byte)
      (func' flatidx
             code)))



(defn func'

  [view flatidx {:wasm/keys [expr
                             local+]}]

  (-> view
      (u32' (count local+))
      (locals' local+)
      (expr' flatidx
             expr)))



(defn locals'

  [view local+]

  (doseq [[n
           valtype] local+]
    (-> view
        (u32' n)
        (valtype' valtype)))
  view)


;;;;;;;;;; Modules / Data Section


(defn datasec'

  ""

  [view {:as        ctx
         :wasm/keys [datasec]}]

  (when (seq datasec)
    (let [{:as         ctx-write
           flatidx-mem :wasm.flatidx/mem} (ctx :wasm/write)]
      (-> view
          (section-id wasm.bin/section-id-data)
          (u32' (ctx-write :wasm.count/datasec))
          (u32' (ctx-write :wasm.data/n)))
      (doseq [[memidx
               data+] datasec]
        (let [memidx-2 (flatidx-mem memidx)]
          (doseq [{:wasm/keys [data
                               offset]} data+]
            (-> view
                (memidx' memidx-2)
                (expr' ctx-write
                       offset)
                (u32' (count data))
                (binf/wr-buffer data)))))))
  view)


;;;;;;;;;; Modules / Modules


(defn magic'

  ""

  [view]

  (binf/wr-b32 view
               wasm.bin/magic))



(defn version'

  ""

  [view {:wasm/keys [version]}]

  (binf/wr-b32 view
               (condp =
                      version
                 wasm.bin/version-1 wasm.bin/version-1)))



(defn section'+

  ""

  [view ctx]

  (-> view
      (typesec' ctx)
      (importsec' ctx)
      (funcsec' ctx)
      (tablesec' ctx)
      (memsec' ctx)
      (globalsec' ctx)
      (exportsec' ctx)
      (startsec' ctx)
      (elemsec' ctx)
      (codesec' ctx)
      (datasec' ctx)))



(defn module'

  ""

  [view ctx]

  (-> view
      magic'
      (version' ctx)
      (section'+ ctx)))
