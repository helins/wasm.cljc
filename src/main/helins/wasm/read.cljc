;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.read

  ""

  {:author "Adam Helinski"}

  (:require [helins.binf        :as binf]
            [helins.binf.int64  :as binf.int64]
            [helins.binf.leb128 :as binf.leb128]
            [helins.wasm.bin    :as wasm.bin]
            [helins.wasm.ir     :as wasm.ir]))


(declare code'
         custom'
         data'
         elem'
         elemtype'
         export'
         exportdesc'
         expr'
         func'
         funcidx'
         global'
         globalidx'
         import'
         importdesc'
         instr'
         instr'+
         labelidx'
         localidx'
         locals'
         mem'
         memidx'
         mut'
         opcode->f
         s32'
         s64'
         start'
         table'
         typeidx'
         u32')


;;;;;;;;;; Conventions


(defn vec'

  ""


  ([f view]

   (loop [i (u32' view)
          v []]
     (if (pos? i)
       (recur (dec i)
              (conj v
                    (f view)))
       v)))


  ([ctx f view]

   (loop [ctx-2 ctx
          i     (u32' view)]
     (if (pos? i)
       (recur (f ctx-2
                 view)
              (dec i))
       ctx-2))))


;;;;;;;;;; Values / Byte


(defn byte'

  ""

  [view]

  (binf/rr-u8 view))


;;;;;;;;;; Values / Integers


(defn i32'

  ""

  [view]

  (s32' view))



(defn i64'

  ""

  [view]

  (s64' view))



(defn s32'

  ""

  [view]

  (binf.leb128/rr-i32 view))



(defn s33'

  ""

  [view]

  (binf.leb128/rr-i64 view
                      33))



(defn s64'

  ""

  [view]

  (binf.leb128/rr-i64 view))



(defn u32'

  ""

  [view]

  (binf.leb128/rr-u32 view))



(defn u64'

  ""

  [view]

  (binf.leb128/rr-u64 view))


;;;;;;;;;; Values / Floating-Point


(defn f32'

  ""

  [view]

  (binf/rr-f32 view))



(defn f64'

  ""

  [view]

  (binf/rr-f64 view))


;;;;;;;;;; Values / Names


(defn name'

  ""

  [view]

  (binf/rr-buffer view
                  (u32' view)))


;;;;;;;;;; Types / Value Types


(defn -valtype

  ""

  [b8]

  ;; Leveraging the fact that valtypes are contiguous.
  ;;
  (when (or (< b8
               wasm.bin/valtype-f64)
            (> b8
               wasm.bin/valtype-i32))
    (throw (ex-info (str "Unknown valtype: "
                         b8)
                    {})))
  b8)



(defn valtype'

  ""

  [view]

  (-valtype (byte' view)))


;;;;;;;;;; Types / Result Types


(defn resulttype'

  ""

  [view]

  (not-empty (vec' valtype'
             view)))

;;;;;;;;;; Types / Function Types


(defn funcref'

  ""

  [view]

  (let [b8-1 (byte' view)]
    (when (not= b8-1
                wasm.bin/functype)
      (throw (ex-info (str "Function type should start with 0x60, not: "
                           b8-1)
                      {}))))
  nil)



(defn functype'

  ""

  [view]

  (funcref' view)
  [(resulttype' view)
   (resulttype' view)])


;;;;;;;;;; Types / Limits


(defn limits-min

  ""

  [hmap view]

  (assoc hmap
         :wasm.limit/min
         (u32' view)))



(defn limits-minmax

  ""

  [hmap view]

  (assoc (limits-min hmap
                     view)
         :wasm.limit/max
         (u32' view)))



(defn limits'

  ""

  [hmap view]

  (let [flag (byte' view)
        f    (condp =
               flag
          wasm.bin/limits-min    limits-min
          wasm.bin/limits-minmax limits-minmax
          (throw (ex-info (str "Unknown limit type: "
                               flag)
                          {})))]
    (f hmap
       view)))


;;;;;;;;;; Types / Memory Types


(defn memtype'

  ""

  [hmap view]

  (limits' hmap
           view))


;;;;;;;;;; Types / Table types


(defn tabletype'

  ""

  [hmap view]

  (-> hmap
      (assoc :wasm/elemtype
             (elemtype' view))
      (limits' view)))



(defn elemtype'

  ""

  [view]

  (byte' view))


;;;;;;;;;; Types / Global types


(defn globaltype'

  ""

  [hmap view]

  (assoc hmap
         :wasm/valtype  (valtype' view)
         :wasm/mutable? (mut' view)))



(defn mut'
   
  ""

  [view]

  (let [flag (byte' view)]
    (condp =
           flag
      wasm.bin/mut-const false
      wasm.bin/mut-var   true
      (throw (ex-info (str "Unknown mutability flag for global: "
                           flag)
                      {})))))


;;;;;;;;;; Instructions / Control Instructions


(defn blocktype'

  [view]

  (let [x (s33' view)]
    (if (< x
           (binf.int64/i* 0))
      (let [x-2 (bit-and 0x7F
                         (binf.int64/u8 x))]
        (if (= x-2
               wasm.bin/blocktype-nil)
          nil
          [:wasm/valtype
           (-valtype x-2)]))
      [:wasm/typeidx (binf.int64/u32 x)])))



(defn block'

  [opvec ctx view]

  (conj opvec
        (blocktype' view)
        (instr'+ ctx
                 view)))



(defn loop'

  [opvec ctx view]

  (conj opvec
        (blocktype' view)
        (instr'+ ctx
                 view)))



(defn else'

  [ctx view]

  (instr'+ ctx
           view))



(defn if'

  [opvec ctx view]

  (let [opvec-2 (conj opvec
                      (blocktype' view))]
    (loop [instr+ []]
      (let [opcode (byte' view)]
        (condp =
               opcode
          wasm.bin/else (conj opvec-2
                              instr+
                              (else' ctx
                                     view))
          wasm.bin/end  (conj opvec-2
                              instr+
                              [])
          (recur (conj instr+
                       (instr' ctx
                               opcode
                               view))))))))



(defn br'

  [opvec _ctx view]

  (conj opvec
        (labelidx' view)))



(defn br_if'

  [opvec _ctx view]

  (br' opvec
       _ctx
       view))



(defn br_table'

  [opvec _ctx view]

  (conj opvec
        (vec' labelidx'
              view)
        (labelidx' view)))



(defn call'

  [opvec _ctx view]

  (conj opvec
        (funcidx' view)))



(defn call_indirect'

  [opvec _ctx view]

  (conj opvec
        (typeidx' view)
        (byte' view)))


;;;;;;;;;; Instructions / Variable Instructions


(defn op-var-local

  ""

  [opvec _ctx view]

  (conj opvec
        (localidx' view)))



(defn op-var-global

  ""

  [opvec _ctx view]

  (conj opvec
        (globalidx' view)))


;;;;;;;;;; Instructions / Memory Instructions


(defn memarg'

  ""

  [vect view]

  (conj vect
        (u32' view)
        (u32' view)))



(defn op-memarg

  ""

  [opvec _ctx view]

  (memarg' opvec
           view))



(defn op-memory

  ""

  [opvec _ctx view]

  (conj opvec
        (memidx' view)))


;;;;;;;;;; Instructions / Numeric Instructions


(defn op-constval

  ""


  ([const]

   (partial op-constval
            const))


  ([const opvec _ctx view]

   (conj opvec
         (const view))))



(defn trunc_sat

  ""

  [opvec _ctx view]

  (conj opvec
        (u32' view)))
  

;;;;;;;;;; Instructions / Expressions


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

  [ctx opcode view]

  (let [opvec [opcode]]
    (if-some [f (opcode->f opcode)]
      (f opvec
         ctx
         view)
      (do
        (when-not (contains? wasm.bin/opcode->opsym
                             opcode)
          (throw (ex-info (str "This opcode is not a recognized instruction: "
                               opcode)
                          {})))
        opvec))))



(defn instr'+

  ""

  [ctx view]

  (expr' ctx
         view))



(defn expr'

  ""

  [ctx view]

  (loop [instr+ []]
    (let [opcode (byte' view)]
      (if (= opcode
             wasm.bin/end)
        instr+
        (recur (conj instr+
                     (instr' ctx
                             opcode
                             view)))))))


;;;;;;;;;; Modules / Indices


(defn idx

  ""

  [view]

  (u32' view))



(def typeidx'

  "Alias to [[idx]].

   Defined to mimick non-terminal symbol in WASM specification."

  idx)



(def funcidx'

  "Alias to [[idx]].

   Defined to mimick non-terminal symbol in WASM specification."

  idx)



(def tableidx'

  "Alias to [[idx]].

   Defined to mimick non-terminal symbol in WASM specification."

  idx)



(def memidx'

  "Alias to [[idx]].

   Defined to mimick non-terminal symbol in WASM specification."

  idx)



(def globalidx'

  "Alias to [[idx]].

   Defined to mimick non-terminal symbol in WASM specification."

  idx)



(def localidx'

  "Alias to [[idx]].

   Defined to mimick non-terminal symbol in WASM specification."

  idx)



(def labelidx'

  "Alias to [[idx]].

   Defined to mimick non-terminal symbol in WASM specification."

  idx)


;;;;;;;;;; Modules / Sections


(defn section'

  ""

  [view]

  (let [id (byte' view)]
    (when-not (wasm.bin/section-id? id)
      (throw (ex-info (str "Unknown section ID: "
                           id)
                      {})))
    (let [n-byte (u32' view)
          start  (binf/position view)]
      (binf/skip view
                 n-byte)
      {:wasm.section/id    id
       :wasm.section/view (binf/view view
                                     start
                                     n-byte)})))


;;;;;;;;;; Modules / (Helpers)


(defn func

  ""

  [hmap view]

  (assoc hmap
         :wasm/typeidx
         (typeidx' view)))


;;;;;;;;;; Modules / Custom Section


(defn customsec'

  ""

  [ctx view]

  (custom' ctx
           view))



(defn custom'

  ""

  [{:as                  ctx
    :wasm.customsec/keys [bin]}
   view]

  (bin ctx
       (name' view)
       (binf/view view
                  (binf/position view)
                  (binf/remaining view))))



(defn custom-default

  ""

  [ctx custom-name view]

  (update-in ctx
             [:wasm/customsec
              custom-name]
             (fnil conj
                   [])
             view))


;;;;;;;;;; Modules / Type Section


(defn typesec'

  ""

  [ctx view]

  (vec' ctx
        (fn [ctx-2 view]
          (wasm.ir/assoc-type ctx-2
                              {:wasm/signature (functype' view)}))
        view))


;;;;;;;;;; Modules / Import Section


(defn importsec'

  ""

  [ctx view]

  (vec' ctx
        import'
        view))



(defn import'

  ""

  [ctx view]

  (importdesc' ctx
               view
               {:wasm.import/module (name' view)
                :wasm.import/name   (name' view)}))



(defn importdesc-any

  ""

  [ctx hmap k-import-type k-idx]
  
  (-> ctx
      (assoc-in [:wasm/importsec
                k-import-type
                (ctx k-idx)]
               hmap)
      (update k-idx
              inc)))



(defn importdesc-func

  ""

  [ctx view hmap]

  (importdesc-any ctx
                  (func hmap
                        view)
                  :wasm.import/func
                  :wasm/funcidx))



(defn importdesc-table

  ""

  [ctx view hmap]

  (importdesc-any ctx
                  (tabletype' hmap
                              view)
                  :wasm.import/table
                  :wasm/tableidx))



(defn importdesc-mem

  ""

  [ctx view hmap]

  (importdesc-any ctx
                  (memtype' hmap
                            view)
                  :wasm.import/mem
                  :wasm/memidx))



(defn importdesc-global

  ""

  [ctx view hmap]

  (importdesc-any ctx
                  (globaltype' hmap
                               view)
                  :wasm.import/global
                  :wasm/globalidx))


(defn importdesc'

  ""

  [ctx view hmap]

  (let [type (byte' view)
        f    (condp =
                    type
               wasm.bin/importdesc-func   importdesc-func
               wasm.bin/importdesc-table  importdesc-table
               wasm.bin/importdesc-mem    importdesc-mem
               wasm.bin/importdesc-global importdesc-global
               (throw (ex-info (str "Unknown type in import description: "
                                    type)
                               {})))]

    (f ctx
       view
       hmap)))


;;;;;;;;;; Modules / Function Section


(defn funcsec'

  ""

  [ctx view]

  (vec' ctx
        (fn [ctx-2 view]
          (wasm.ir/assoc-func ctx-2
                              (func {}
                                    view)))
        view))


;;;;;;;;;; Modules / Table Section


(defn tablesec'

  ""

  [ctx view]

  (vec' ctx
        table'
        view))



(defn table'

  ""

  [ctx view]

  (wasm.ir/assoc-table ctx
                       (tabletype' {}
                                   view)))


;;;;;;;;;; Modules / Memory Section


(defn memsec'

  ""

  [ctx view]

  (vec' ctx
        mem'
        view))



(defn mem'

  ""

  [ctx view]

  (wasm.ir/assoc-mem ctx
                     (memtype' {}
                               view)))


;;;;;;;;;; Modules / Global section


(defn globalsec'

  ""

  [ctx view]

  (vec' ctx
        global'
        view))



(defn global'

  ""

  [ctx view]

  (wasm.ir/assoc-global ctx
                        (-> {}
                            (globaltype' view)
                            (assoc :wasm/expr
                                   (expr' ctx
                                          view)))))


;;;;;;;;;; Modules / Export Section


(defn exportsec'

  ""

  [ctx view]

  (vec' ctx
        export'
        view))



(defn export'

  ""

  [ctx view]

  (exportdesc' ctx
               view
               {:wasm/name (name' view)}))



(defn exportdesc-any

  ""

  [ctx hmap k-space idx]

  (update-in ctx
             [:wasm/exportsec
              k-space
              idx]
             (fnil conj
                   [])
             hmap))



(defn exportdesc-func

  ""

  [ctx view hmap]

  (exportdesc-any ctx
                  hmap
                  :wasm.export/func
                  (funcidx' view)))



(defn exportdesc-table

  ""

  [ctx view hmap]

  (exportdesc-any ctx
                  hmap
                  :wasm.export/table
                  (tableidx' view)))



(defn exportdesc-mem

  ""

  [ctx view hmap]

  (exportdesc-any ctx
                  hmap
                  :wasm.export/memsec
                  (memidx' view)))



(defn exportdesc-global

  ""

  [ctx view hmap]

  (exportdesc-any ctx
                  hmap
                  :wasm.export/globalsec
                  (globalidx' view)))



(defn exportdesc'

  ""

  [ctx view hmap]

  (let [export-type (byte' view)
        f           (condp =
                           export-type
                      wasm.bin/exportdesc-func   exportdesc-func
                      wasm.bin/exportdesc-table  exportdesc-table
                      wasm.bin/exportdesc-mem    exportdesc-mem
                      wasm.bin/exportdesc-global exportdesc-global
                      (throw (ex-info (str "Unknown type in export description: "
                                           type)
                                      {})))]
    (f ctx
       view
       hmap)))


;;;;;;;;;; Modules / Start Section


(defn startsec'

  ""

  [ctx view]

  (assoc ctx
         :wasm/startsec
         (start' {}
                 view)))



(defn start'

  ""

  [hmap view]

  (assoc hmap
         :wasm/funcidx
         (funcidx' view)))


;;;;;;;;;; Modules / Element Section


(defn elemsec'

  ""

  [ctx view]

  (vec' ctx
        elem'
        view))
 


(defn elem'

  ""

  [ctx view]

  (update-in ctx
             [:wasm/elemsec
              (tableidx' view)]
             (fnil conj
                   [])
             {:wasm/offset   (expr' ctx
                                    view)
              :wasm/funcidx+ (vec' funcidx'
                                   view)}))


;;;;;;;;;; Modules / Code Section


(defn codesec'

  ""

  [ctx view]

  (assoc-in ctx
            [:wasm/source
             :wasm.source/codesec]
            (reduce (fn [source-codesec _idx]
                      (conj source-codesec
                            (code' {}
                                   view)))
                    []
                    (range (u32' view)))))



(defn code'

  ""

  [hmap view]

  (let [n-byte (u32' view)
        start  (binf/position view)]
    (binf/skip view
               n-byte)
    (assoc hmap
           :wasm.codesec/view
           (binf/view view
                      start
                      n-byte))))



(defn func'

  ""

  [hmap ctx view]

  (-> hmap
      (assoc :wasm/local+ (locals' view)
             :wasm/expr   (expr' ctx
                                 view))
      (dissoc :wasm/code)))



(defn locals'

  ""

  [view]

  (vec' (fn [view]
          [(u32' view)
           (valtype' view)])
        view)

  ;; Decompressed
  ;;
  #_(into []

        (mapcat identity)
        (vec' (fn [view]
                (repeat (u32' view)
                        (valtype' view)))
              view)))



(defn codesec'2

  ""

  [ctx]

  (let [idx-offset (or (ffirst (ctx :wasm/funcsec))
                       (ctx :wasm/funcidx))]
    (update ctx
            :wasm/codesec
            (fn [codesec]
              (reduce-kv (fn [codesec-2 idx {:wasm.codesec/keys [view]}]
                           (assoc codesec-2
                                  (+ idx-offset
                                     idx)
                                  (func' {}
                                         ctx
                                         view)))
                         codesec
                         (get-in ctx
                                 [:wasm/source
                                  :wasm.source/codesec]))))))


;;;;;;;;;; Modules / Data Section


(defn datasec'

  ""

  [ctx view]

  (vec' ctx
        data'
        view))



(defn data'

  ""

  [ctx view]

  (update-in ctx
             [:wasm/datasec
              (memidx' view)]
             (fnil conj
                   [])
             {:wasm/offset (expr' ctx
                                  view)
              :wasm/data   (binf/rr-buffer view
                                           (u32' view))}))


;;;;;;;;;; Modules / Modules


(defn magic'

  ""

  ;; Checking for "\0asm" BOM (reversed u32 because view is little-endian).

  [view]

  (when (not= (binf/rr-u32 view)
              wasm.bin/magic)
    (throw (ex-info "WASM file does not start with magic word"
                    {}))))



(defn version'

  ""

  [view]

  (binf/rr-u32 view))



(defn section'+

  ""

  [ctx]

  (reduce (fn [ctx-2 {:wasm.section/keys [id
                                          view]}]
            (if (= id
                   wasm.bin/section-id-custom)
              (update ctx-2
                      :wasm.bin/customsec
                      (fnil conj
                            [])
                      (customsec' ctx-2
                                  view))
              ((condp =
                      id
                  wasm.bin/section-id-type   typesec'
                  wasm.bin/section-id-import importsec'
                  wasm.bin/section-id-func   funcsec'
                  wasm.bin/section-id-table  tablesec'
                  wasm.bin/section-id-mem    memsec'
                  wasm.bin/section-id-global globalsec'
                  wasm.bin/section-id-export exportsec'
                  wasm.bin/section-id-start  startsec'
                  wasm.bin/section-id-elem   elemsec'
                  wasm.bin/section-id-code   codesec'
                  wasm.bin/section-id-data   datasec')
               ctx-2
               view)))
          ctx
          (get-in ctx
                  [:wasm/source
                   :wasm.source/section+])))



(defn module'

  ""

  [ctx view]

  (magic' view)
  (-> ctx
      (assoc :wasm/version
             (version' view))
      (assoc-in [:wasm/source
                 :wasm.source/section+]
                (loop [section+ []]
                  (if (pos? (binf/remaining view))
                    (recur (conj section+
                                 (section' view)))
                    section+)))))
