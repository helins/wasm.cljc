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
         dataidx'
         elem'
         elemkind'
         elemidx'
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
         tableidx'
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


;;;;;;;;;; Types / Reference Types


(defn reftype'

  ""

  [view]

  (byte' view))


;;;;;;;;;; Types / Value Types


(defn valtype'

  ""

  [view]

  (byte' view))


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


(defn limits'

  ""

  [hmap view]

  (let [flag (byte' view)]
    (condp =
           flag
      wasm.bin/limits-min    (wasm.ir/limits' hmap
                                              (u32' view))
      wasm.bin/limits-minmax (wasm.ir/limits' hmap
                                              (u32' view)
                                              (u32' view))
      (throw (ex-info (str "Unknown limit type: "
                           flag)
                      {})))))


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

  (let [reftype (reftype' view)]
    (wasm.ir/tabletype' (limits' hmap
                                 view)
                        reftype)))


;;;;;;;;;; Types / Global types


(defn globaltype'

  ""

  [hmap view]

  (wasm.ir/globaltype' hmap
                       (valtype' view)
                       (mut' view)))



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
          [:wasm/valtype x-2]))
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
        (tableidx' view)))


;;;;;;;;; Instructions / Reference Instructions


(defn ref-null'

  [opvec _ctx view]

  (conj opvec
        (reftype' view)))



(defn ref-func'

  [opvec _ctx view]

  (conj opvec
        (funcidx' view)))


;;;;;;;;;; Instructions / Parametric Instructions


(defn select-t'

  [opvec _ctx view]

  (conj opvec
        (vec' valtype'
              view)))


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


;;;;;;;;;; Instructions / Table Instructions


(defn op-table

  "Table instruction involving a table index immediate."

  [opvec _ctx view]

  (conj opvec
        (tableidx' view)))



(defn table-init'

  ""

  [opvec _ctx view]

  (conj opvec
        (elemidx' view)
        (tableidx' view)))



(defn elem-drop'

  ""

  [opvec _ctx view]

  (conj opvec
        (elemidx' view)))



(defn table-copy'

  ""

  [opvec _ctx view]

  (conj opvec
        (tableidx' view)
        (tableidx' view)))


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



(defn memory-init'

  ""

  [opvec _ctx view]

  (conj opvec
        (dataidx' view)
        (byte' view)))



(defn data-drop'

  ""

  [opvec _ctx view]

  (conj opvec
        (dataidx' view)))



(defn memory-copy'

  ""

  [opvec _ctx view]

  (conj opvec
        (byte' view)
        (byte' view)))



(defn memory-fill'

  ""

  [opvec _ctx view]

  (conj opvec
        (byte' view)))


;;;;;;;;;; Instructions / Numeric Instructions


(defn op-constval

  ""


  ([const]

   (partial op-constval
            const))


  ([const opvec _ctx view]

   (conj opvec
         (const view))))


;;;;;;;;;; Instructions / Expressions


(def op-main->f

  ""

  {wasm.bin/block         block'
   wasm.bin/loop-         loop'
   wasm.bin/if-           if'
   wasm.bin/br            br'
   wasm.bin/br_if         br_if'
   wasm.bin/br_table      br_table'
   wasm.bin/call          call'
   wasm.bin/call_indirect call_indirect'
   wasm.bin/ref-null      ref-null'
   wasm.bin/ref-func      ref-func'
   wasm.bin/select-t      select-t'
   wasm.bin/local-get     op-var-local
   wasm.bin/local-set     op-var-local
   wasm.bin/local-tee     op-var-local
   wasm.bin/global-get    op-var-global
   wasm.bin/global-set    op-var-global
   wasm.bin/table-get     op-table
   wasm.bin/table-set     op-table
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
   wasm.bin/f64-const     (op-constval f64')})



(def op-misc->f

  ""

  {wasm.bin/memory-init memory-init'
   wasm.bin/data-drop   data-drop'
   wasm.bin/memory-copy memory-copy'
   wasm.bin/memory-fill memory-fill'
   wasm.bin/table-init  table-init'
   wasm.bin/elem-drop   elem-drop'
   wasm.bin/table-copy  table-copy'
   wasm.bin/table-grow  op-table
   wasm.bin/table-size  op-table 
   wasm.bin/table-fill  op-table})



(defn instr'

  ""


  ([ctx view]

   (instr' ctx
           (byte' view)
           view))


  ([ctx opcode view]

   (let [opvec [opcode]]
     (if (= opcode
            wasm.bin/misc)
       (let [opcode-2 (u32' view)
             opvec-2  (conj opvec 
                            opcode-2)]
         (if-some [f-2 (op-misc->f opcode-2)]
           (f-2 opvec-2
                ctx
                view)
           (do
             (when-not (contains? wasm.bin/opcode-misc->opsym
                                  opcode-2)
               (throw (ex-info (str "Secondary opcode for miscellaneous opcode is not a recognized instruction: "
                                    opcode-2)
                               {})))
             opvec-2)))
       (if-some [f (op-main->f opcode)]
         (f opvec
            ctx
            view)
         (do
           (when-not (contains? wasm.bin/opcode-main->opsym
                                opcode)
             (throw (ex-info (str "This opcode is not a recognized instruction: "
                                  opcode)
                             {})))
           opvec))))))



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

  "Alias to [[idx]]."

  idx)



(def funcidx'

  "Alias to [[idx]]."

  idx)



(def tableidx'

  "Alias to [[idx]]."

  idx)



(def memidx'

  "Alias to [[idx]]."

  idx)



(def globalidx'

  "Alias to [[idx]]."

  idx)



(def elemidx'

  "Alias to [[idx]]."

  idx)



(def dataidx'

  "Alias to [[idx]]."

  idx)



(def localidx'

  "Alias to [[idx]]."

  idx)



(def labelidx'

  "Alias to [[idx]]."

  idx)


;;;;;;;;;; Modules / Sections


(defn section-id'

  ""

  [view]

  (byte' view))



(defn section'

  ""

  [view]

  (let [id (section-id' view)]
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

  (wasm.ir/func hmap
                (typeidx' view)))


;;;;;;;;;; Modules / Custom Section


(defn customsec'

  ""

  [ctx view]

  (custom' ctx
           view))



(defn custom'

  ""

  [ctx view]

  (update ctx
          :wasm/customsec
          (fnil conj
                [])
          {:wasm/name (name' view)
           :wasm/data (binf/view view
                                 (binf/position view)
                                 (binf/remaining view))}))


;;;;;;;;;; Modules / Type Section


(defn typesec'

  ""

  [ctx view]

  (vec' ctx
        (fn [ctx-2 view]
          (wasm.ir/assoc-type ctx-2
                              (wasm.ir/type-signature {}
                                                      (functype' view))))
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
               (wasm.ir/import' {}
                                (name' view)
                                (name' view))))



(defn importdesc-func

  ""

  [ctx view hmap]

  (wasm.ir/import-func ctx
                       (func hmap
                             view)))



(defn importdesc-table

  ""

  [ctx view hmap]

  (wasm.ir/import-table ctx
                        (tabletype' hmap
                                    view)))



(defn importdesc-mem

  ""

  [ctx view hmap]

  (wasm.ir/import-mem ctx
                      (memtype' hmap
                                view)))



(defn importdesc-global

  ""

  [ctx view hmap]

  (wasm.ir/import-global ctx
                         (globaltype' hmap
                                      view)))



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
                  :wasm.export/mem
                  (memidx' view)))



(defn exportdesc-global

  ""

  [ctx view hmap]

  (exportdesc-any ctx
                  hmap
                  :wasm.export/global
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

  (wasm.ir/startsec' ctx
                     (start' {}
                             view)))



(defn start'

  ""

  [hmap view]

  (wasm.ir/start' hmap
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

  ;; Spec says that the initial byte can be interpreted as a bitfield.
  ;; In practise, this is cumbersome and causing a couple of problems.
  ;; Given a bit of repetition, it is best to be explicit.

  [ctx view]

  (wasm.ir/assoc-elem ctx
                      (let [flag (byte' view)
                            _    (when-not (<= 0x00
                                               flag
                                               0x07)
                                   (throw (ex-info (str "Element segment flag is not in [0;7]: "
                                                        flag)
                                                   {})))
                            hmap (if (even? flag)
                                   (-> (if (or (= flag
                                                  0x02)
                                               (= flag
                                                  0x06))
                                         {:wasm/tableidx (tableidx' view)}
                                         {})
                                       (assoc :wasm/offset    (expr' ctx
                                                                     view)
                                              :wasm.elem/mode :active))
                                   {:wasm.elem/mode (if (pos? (bit-and flag
                                                                       2r010))
                                                      :declarative
                                                      :passive)})]
                        (if (pos? (bit-and flag
                                           2r100))
                          (-> (if (= flag
                                     0x04)
                                hmap
                                (assoc hmap
                                       :wasm/reftype
                                       (reftype' view)))
                              (assoc :wasm.elem/resolve :expr
                                     :wasm.elem/vec     (vec' (partial expr'
                                                                       ctx)
                                                              view)))
                          (-> (if (= flag
                                     0x00)
                                hmap
                                (assoc hmap
                                       :wasm/elemkind
                                       (elemkind' view)))
                              (assoc :wasm.elem/resolve :idx
                                     :wasm.elem/vec     (vec' funcidx'
                                                              view)))))))



(defn elemkind'

  ""

  [view]

  (byte' view))


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
      (assoc :wasm/locals (locals' view)
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
                       0)]
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

  (-> ctx
      (update :wasm/dataidx
              inc)
      (assoc-in [:wasm/datasec
                 (ctx :wasm/dataidx)]
                (let [flag (byte' view)]
                  (when-not (<= 0x00
                                flag
                                0x02)
                    (throw (ex-info (str "Data segment flat is not in [0;2]: "
                                         flag)
                                    {})))
                  (->  (if (= flag
                              0x01)
                         {:wasm.data/mode :passive}
                         (-> (if (= flag
                                    0x02)
                               {:wasm/memidx (memidx' view)}
                               {})
                             (assoc :wasm/offset    (expr' ctx
                                                           view)
                                    :wasm.data/mode :active)))
                       (assoc :wasm/buffer
                              (binf/rr-buffer view
                                              (u32' view))))))))


;;;;;;;;;; Modules / Data Count Section


(defn datacountsec'

  ""

  [ctx view]

  (wasm.ir/datacountsec' ctx
                         (u32' view)))


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
                  wasm.bin/section-id-type      typesec'
                  wasm.bin/section-id-import    importsec'
                  wasm.bin/section-id-func      funcsec'
                  wasm.bin/section-id-table     tablesec'
                  wasm.bin/section-id-mem       memsec'
                  wasm.bin/section-id-global    globalsec'
                  wasm.bin/section-id-export    exportsec'
                  wasm.bin/section-id-start     startsec'
                  wasm.bin/section-id-elem      elemsec'
                  wasm.bin/section-id-code      codesec'
                  wasm.bin/section-id-data      datasec'
                  wasm.bin/section-id-datacount datacountsec')
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
  (let [ctx-2 (assoc ctx
                     :wasm/version
                     (version' view))]
    (cond->
      ctx-2
      (pos? (binf/remaining view))
      (assoc-in [:wasm/source
                 :wasm.source/section+]
                (loop [section+ []]
                  (if (pos? (binf/remaining view))
                    (recur (conj section+
                                 (section' view)))
                    section+))))))
