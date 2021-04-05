;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.count

  "Given WASM items described in Clojure data structures, computes their size in bytes.

   This namespace is used before compilation for pre-computing how much memory to allocate
   while recomputing indices so they fit in dense lists.

   Everything is computed in a map that is being assoc'ed in the `ctx` at `:wasm/write`.

   See README for namespace organization and naming scheme."

  {:author "Adam Helinski"}

  (:require [helins.binf        :as binf]
            [helins.binf.int64  :as binf.int64]
            [helins.binf.leb128 :as binf.leb128]
            [helins.wasm.bin    :as wasm.bin]))


(declare code'
         dataidx'
         elemidx'
         elemtype'
         export'
         func'
         funcidx'
         globalidx'
         importdesc'
         instr'+
         labelidx'
         localidx'
         locals'
         memidx'
         tableidx'
         typeidx'
         u32')


;;;;;;;;;; Private - Miscellaneous


(defn ^:no-doc -flatten-idx

  ;; Indirection so that tests can handle non-existing indices by altering this function.

  [hmap idx]

  (get hmap
       idx))


;;;;;;;;;; Conventions


(defn vec'

  [count-item coll]

  (+ (u32' (count coll))
     (reduce (fn [sum item]
               (+ sum
                  (count-item item)))
             0
             coll)))


;;;;;;;;;; Values / Byte


(def byte'
     1)


;;;;;;;;;; Values / Integers


(defn i32'

  [i32]

  (binf.leb128/n-byte-i32 i32))



(defn i64'

  [i64]

  (binf.leb128/n-byte-i64 i64))



(defn s33'

  [s33]

  (binf.leb128/n-byte-i64 (binf.int64/i* s33)))



(defn u32'

  [u32]

  (binf.leb128/n-byte-u32 u32))


;;;;;;;;;; Values / Floating-Point


(defn f32'

  [_f32]

  4)



(defn f64'

  [_f64]

  8)


;;;;;;;;;; Values / Names


(defn name'

  [buffer]

  (let [n-byte (count buffer)]
    (+ (u32' n-byte)
       n-byte)))


;;;;;;;;;; Types / Reference Types


(def reftype'
     1)


;;;;;;;;;; Types / Value Types


(def valtype'
     1)


;;;;;;;;;; Types / Result Types


(defn resulttype'

  [valtype+]

  (let [n-valtype (count valtype+)]
    (+ (u32' n-valtype)
       (* n-valtype
          valtype'))))


;;;;;;;;;; Types / Function Types


(defn functype'

  [[param+ result+]]

  (+ 1 ;; 0x60 functype
     (resulttype' param+)
     (resulttype' result+)))


;;;;;;;;;; Types / Limits


(defn limits'

  [{min- :wasm.limit/min
    max- :wasm.limit/max}]

  (let [n-byte (+ 1             ;; Byte specifying if there is a maximum or not
                  (u32' min-))]
    (if max-
      (+ n-byte
         (u32' max-))
      n-byte)))


;;;;;;;;;; Types / Memory Types


(defn memtype'

  [mem]

  (limits' mem))


;;;;;;;;;; Types / Table types


(defn tabletype'

  [table]

  (+ reftype'
     (limits' table)))


;;;;;;;;;; Types / Global types


(defn globaltype'

  [_global]

  2)


;;;;;;;;;; Instructions / Control Instructions


(defn blocktype'

  [blocktype]

  (if (nil? blocktype)
    1
    (case (blocktype 0)
      :wasm/typeidx (s33' (blocktype 1))
      :wasm/valtype 1)))



(defn block'

  [flatidx opvec]

  (+ (blocktype' (opvec 1))
     (instr'+ flatidx
              (opvec 2))
     1  ;;  END
     ))



(defn loop'

  [flatidx opvec]

  (block' flatidx
          opvec))



(defn else'

  [flatidx instr+]

  (if (seq instr+)
    (+ 1  ;; `else` opcode
       (instr'+ flatidx
                instr+))
    0))



(defn if'

  [flatidx opvec]

  (+ (blocktype' (opvec 1))
     (instr'+ flatidx
              (opvec 2))
     (else' flatidx
            (opvec 3))
     1  ;;  END
     ))



(defn br'

  [_flatidx opvec]

  (labelidx' (opvec 1)))



(defn br_if'

  [_flatidx opvec]

  (br' nil
       opvec))



(defn br_table'

  [_flatidx opvec]

  (let [choice+ (opvec 1)]
    (+ (u32' (count choice+))
       (reduce (fn [sum labelidx]
                 (+ sum
                    (labelidx' labelidx)))
               0
               choice+)
       (labelidx' (opvec 2)))))



(defn call'

  [flatidx opvec]

  (funcidx' (-flatten-idx (flatidx :wasm.flatidx/func)
                          (opvec 1))))



(defn call_indirect'

  [flatidx opvec]

  (+ (typeidx' (-flatten-idx (flatidx :wasm.flatidx/type)
                             (opvec 1)))
     (tableidx' (opvec 2))))


;;;;;;;;; Instructions / Reference Instructions


(defn ref-null'

  [_flatidx _opvec]

  reftype')



(defn ref-func'

  [flatidx opvec]

  (funcidx' (-flatten-idx (flatidx :wasm.flatidx/funcidx)
                          (opvec 1))))


;;;;;;;;;; Instructions / Parametric Instructions


(defn select-t'

  [_flatidx opvec]

  (let [n-valtype (count (opvec 1))]
    (+ (u32' n-valtype)
       n-valtype)))


;;;;;;;;;; Instructions / Variable Instructions


(defn op-var-local

  "Used for local variable instructions."

  [_flatidx opvec]

  (localidx' (opvec 1)))



(defn op-var-global

  "Used for global variable instructions."
  
  [flatidx opvec]

  (globalidx' (-flatten-idx (flatidx :wasm.flatidx/global)
                            (opvec 1))))


;;;;;;;;;; Instructions / Table Instructions


(defn op-table

  "Table instruction involving a table index immediate."

  [flatidx opvec]

  (tableidx' (-flatten-idx (flatidx :wasm.flatidx/table)
                           (opvec 1))))



(defn op-table-misc

  [flatidx opvec]

  (tableidx' (-flatten-idx (flatidx :wasm.flatidx/table)
                           (opvec 2))))



(defn table-init'

  [flatidx opvec]

  (+ (elemidx' (-flatten-idx (flatidx :wasm.flatidx/elem)
                             (opvec 2)))
     (tableidx' (-flatten-idx (flatidx :wasm.flatidx/table)
                              (opvec 3)))))



(defn elem-drop'

  [flatidx opvec]

  (elemidx' (-flatten-idx (flatidx :wasm.flatidx/elem)
                          (opvec 2))))



(defn table-copy'

  [flatidx opvec]
  
  (let [flatidx-table (flatidx :wasm.flatidx/table)]
    (+ (tableidx' (-flatten-idx flatidx-table
                                (opvec 2)))
       (tableidx' (-flatten-idx flatidx-table
                                (opvec 3))))))


;;;;;;;;;; Instructions / Memory Instructions


(defn memarg'

  [[align offset]]

  (+ (u32' align)
     (u32' offset)))



(defn op-memarg

  "Used for memory instructions that have a [[memarg']]."

  [_flatidx opvec]

  (memarg' (rest opvec)))



(defn op-memory

  "Used for memory instructions that have a [[memidx']] as only immediate."

  [flatidx opvec]

  (memidx' (-flatten-idx (flatidx :wasm.flatidx/mem)
                         (opvec 1))))



(defn memory-init'

  [flatidx opvec]

  (+ (dataidx' (-flatten-idx (flatidx :wasm.flatidx/data)
                             (opvec 2)))
     byte'))



(defn data-drop'

  [flatidx opvec]

  (dataidx' (-flatten-idx (flatidx :wasm.flatidx/data)
                          (opvec 2))))



(defn memory-copy'

  [_flatidx _opvec]

  (* 2
     byte'))



(defn memory-fill'

  [_flatidx _opvec]

  byte')


;;;;;;;;;; Instructions / Numeric Instructions


(defn op-constval

  "Used for numerical operations declaring a constant value."


  ([f-value]

   (partial op-constval
            f-value))


  ([f-value _flatidx opvec]

   (f-value (opvec 1))))


;;;;;;;;;; Instructions / Expressions


(def opcode'
     byte')



(def op-main->f

  "Map of **opcode** -> **reading function** for opcodes which:
  
   - Have any kind of immediate(s)
   - Is not 0xFC (the \"misc\" opcode that leads to a second-level opcode"

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

  "Map of **immediate to \"misc\" opcode (0xFC)** -> **reading function**."

  {wasm.bin/memory-init memory-init'
   wasm.bin/data-drop   data-drop'
   wasm.bin/memory-copy memory-copy'
   wasm.bin/memory-fill memory-fill'
   wasm.bin/table-init  table-init'
   wasm.bin/elem-drop   elem-drop'
   wasm.bin/table-copy  table-copy'
   wasm.bin/table-grow  op-table-misc
   wasm.bin/table-size  op-table-misc
   wasm.bin/table-fill  op-table-misc})



(defn instr'

  [flatidx opvec]

  (let [opcode (opvec 0)]
    (if (= opcode
           wasm.bin/misc)
      (let [opcode-2 (opvec 1)]
        (if-some [f-2 (op-misc->f opcode-2)]
          (+ opcode'
             (u32' opcode-2)
             (f-2 flatidx
                  opvec))
          (do
            (when-not (contains? wasm.bin/opcode-misc->opsym
                                 opcode-2)
              (throw (ex-info (str "Unknown immediate to miscellaneous opcode: "
                                 opcode-2)
                            {:wasm/opvec opvec})))
            (+ opcode'
               (u32' opcode-2)))))
      (if-some [f (op-main->f opcode)]
        (+ opcode'
           (f flatidx
              opvec))
        (do
          (when-not (contains? wasm.bin/opcode-main->opsym
                               opcode)
            (throw (ex-info (str "Unknown opcode: "
                                 opvec
                                 opcode)
                            {:wasm/opvec opvec})))
          opcode')))))



(defn instr'+

  "Behaves same as [[expr']]."

  [flatidx opvec+]

  (reduce (fn [sum opvec]
            (+ sum
               (instr' flatidx
                       opvec)))
          0
          opvec+))



(defn expr'

  "For a vector of [[instr']]."

  [flatidx opvec+]

  (+ (instr'+ flatidx
              opvec+)
     1  ;;  END
     ))


;;;;;;;;;; Modules / Indices


(defn idx

  "For the time being at least, all WASM indices are represented as [[u32']] and hence,
   are read by this function."

  [idx]

  (u32' idx))



(def typeidx'
     idx)



(def funcidx'
     idx)



(def tableidx'
     idx)



(def memidx'
     idx)



(def globalidx'
     idx)



(def elemidx'
     idx)



(def dataidx'
     idx)



(def localidx'
     idx)



(def labelidx'
     idx)


;;;;;;;;;; Modules / Sections


(def section-id'
     1)



(defn section'

  ""

  [n-byte]

  (if (and n-byte
           (pos? n-byte))
    (+ section-id'
       (u32' n-byte)
       n-byte)
    0))


;;;;;;;;;; Modules / (Helpers)


(defn func

  "For functions found in the funcsec and in imports.
  
   WASM specification does not have a special name for it since binary-wise
   it is simply a type index."

  [flatidx-type {:wasm/keys [typeidx]}]

  (typeidx' (-flatten-idx flatidx-type
                          typeidx)))



(defn section-space

  "Used by most WASM sections.
  
   Takes care of counting everything and maintaining a flattened index."

  [ctx k-section k-flatidx k-count count-item]

  (if-some [section (not-empty (ctx k-section))]
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
                           section)
                (update ctx-write-2
                        k-count
                        #(+ %
                            (u32' (count section)))))))
    ctx))


;;;;;;;;;; Modules / Custom Section


;; Undecided.


;;;;;;;;;; Modules / Type Section


(defn typesec'

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
                               (functype' signature))]
          (if typesec-2
            (recur (inc idx-real)
                   idx-resolve-2
                   n-byte-2
                   typesec-2)
            (update ctx
                    :wasm/write
                    #(assoc %
                            :wasm.count/typesec (+ (u32' n)
                                                   n-byte-2)
                            :wasm.flatidx/type  idx-resolve-2)))))
      ctx)))


;;;;;;;;;; Modules / Import Section


(defn importsec'

  [{:as        ctx
    :wasm/keys [importsec]}]

  (let [ctx-write   (ctx :wasm/write)
        ctx-write-2 (-> ctx-write
                        (assoc :wasm.count/importsec 0
                               :wasm.import/n        0)
                        (importdesc' (importsec :wasm.import/func)
                                     :wasm.flatidx/func
                                     (partial func
                                              (:wasm.flatidx/type ctx-write)))
                        (importdesc' (importsec :wasm.import/global)
                                     :wasm.flatidx/global
                                     globaltype')
                        (importdesc' (importsec :wasm.import/mem)
                                     :wasm.flatidx/mem
                                     memtype')
                        (importdesc' (importsec :wasm.import/table)
                                     :wasm.flatidx/table
                                     tabletype'))]
    (assoc ctx
           :wasm/write
           (update ctx-write-2
                   :wasm.count/importsec
                   (fn [n-byte]
                     (if (zero? n-byte)
                       0
                       (+ (u32' (ctx-write-2 :wasm.import/n))
                          n-byte)))))))



(defn importdesc'

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


;;;;;;;;;; Modules / Function Section


(defn funcsec'

  [ctx]

  (section-space ctx
                 :wasm/funcsec
                 :wasm.flatidx/func
                 :wasm.count/funcsec
                 (partial func
                          (get-in ctx
                                  [:wasm/write
                                   :wasm.flatidx/type]))))


;;;;;;;;;; Modules / Table Section


(defn tablesec'

  [ctx]

  (section-space ctx
                 :wasm/tablesec
                 :wasm.flatidx/table
                 :wasm.count/tablesec
                 tabletype'))


;;;;;;;;;; Modules / Memory Section


(defn memsec'

  [ctx]

  (section-space ctx
                 :wasm/memsec
                 :wasm.flatidx/mem
                 :wasm.count/memsec
                 memtype'))


;;;;;;;;;; Modules / Global section



(defn global'

  [flatidx global]

  (+ (globaltype' global)
     (expr' flatidx
            (global :wasm/expr))))



(defn globalsec'

  [ctx]

  (section-space ctx
                 :wasm/globalsec
                 :wasm.flatidx/global
                 :wasm.count/globalsec
                 (partial global'
                          (ctx :wasm/write))))


;;;;;;;;;; Modules / Export Section


(defn exportsec'

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
                          (+ (u32' (ctx-write-2 :wasm.export/n))
                             n-byte)
                          0)))))))



(defn export'

  [ctx-write space k-flatidx count-idx]

  (let [flatidx     (ctx-write k-flatidx)
        count-idx-2 (fn [idx]
                      (count-idx (-flatten-idx flatidx
                                               idx)))]
    (reduce-kv (fn [ctx-write-2 idx name+]
                 (let [n-byte-exportdesc (+ byte'
                                            (count-idx-2 idx))]
                   (reduce (fn [ctx-write-3 {buffer :wasm/name}]
                             (update ctx-write-3
                                     :wasm.count/exportsec
                                     #(+ %
                                         n-byte-exportdesc
                                         (name' buffer))))
                           (update ctx-write-2
                                   :wasm.export/n
                                   #(+ %
                                       (count name+)))
                           name+)))
               ctx-write
               space)))


;;;;;;;;;; Modules / Start Section


(defn startsec'

  [{:as        ctx
    :wasm/keys [startsec]}]

  (if startsec
    (update ctx
            :wasm/write
            (fn [ctx-write]
              (assoc ctx-write
                     :wasm.count/startsec
                     (funcidx' (-flatten-idx (:wasm.flatidx/func ctx-write)
                                             (startsec :wasm/funcidx))))))
    ctx))


;;;;;;;;;; Modules / Element Section


(defn elemsec'

  [{:as                               ctx
    {:as          ctx-write
     flatidx-func :wasm.flatidx/func} :wasm/write}]

  (section-space ctx
                 :wasm/elemsec
                 :wasm.flatidx/elem
                 :wasm.count/elemsec
                 (fn [{:as        hmap
                       :wasm/keys [offset]
                       eresolve   :wasm.elem/resolve
                       etype      :wasm.elem/type
                       evec       :wasm.elem/vec}]
                   (+ byte'       ;;  flag
                      (if offset  ;;  if active
                        (+ (if-some [tableidx (hmap :wasm/tableidx)]
                             (tableidx' (-flatten-idx (ctx-write :wasm.flatidx/table)
                                                      tableidx))
                             0)
                           (expr' ctx-write
                                  offset))
                        0)
                      (if (hmap (case eresolve
                                  :expr :wasm/reftype
                                  :idx  :wasm/elemkind))
                        byte'
                        0)
                      (vec' (case eresolve
                              :expr (partial expr' 
                                             ctx-write)
                              :idx  (fn [idx]
                                      (funcidx' (-flatten-idx flatidx-func
                                                              idx))))
                            evec)))))


;;;;;;;;;; Modules / Code Section


(defn codesec'

  [{:as        ctx
    :wasm/keys [codesec]}]

  (if (seq codesec)
    (update ctx
            :wasm/write
            (fn [ctx-write]
              (reduce code'
                      (assoc ctx-write
                             :wasm.codesec/func-size []
                             :wasm.count/codesec     (u32' (count codesec)))
                      (vals codesec))))
    ctx))



(defn code'

  [ctx-write code]

  (let [n-byte (func' ctx-write
                      code)]
    (-> ctx-write
        (update :wasm.count/codesec
                #(+ %
                    (u32' n-byte)
                    n-byte))
        (update :wasm.codesec/func-size
                conj
                n-byte))))



(defn func'

  [flatidx {:wasm/keys [expr
                        locals]}]

  (+ (u32' (count locals))
     (locals' locals)
     (expr' flatidx
            expr)))



(defn locals'

  [local+]

  (reduce (fn [sum [n _valtype]]
            (+ sum
               (u32' n)
               valtype'))
          0
          local+))


;;;;;;;;;; Modules / Data Section


(defn datasec'

  [{:as                             ctx
    {:as         ctx-write
     flatidx-mem :wasm.flatidx/mem} :wasm/write}]

  (section-space ctx
                 :wasm/datasec
                 :wasm.flatidx/data
                 :wasm.count/datasec
                 (fn [{:as        hmap
                       :wasm/keys [buffer
                                   offset]}]
                   (let [n-byte-data (count buffer)]
                     (+ byte'  ;;  flag
                        (if offset
                          (+ (expr' ctx-write
                                    offset)
                             (if-some [memidx (hmap :wasm/memidx)]
                               (memidx' (-flatten-idx flatidx-mem
                                                      memidx))
                               0))
                          0)
                        (u32' n-byte-data)
                        n-byte-data)))))
                      

;;;;;;;;;; Modules / Data Count Section


(defn datacountsec'

  [ctx]

  (if-some [n-seg (get-in ctx
                          [:wasm/datacountsec
                           :wasm.data/n-seg])]
    (assoc-in ctx
              [:wasm/write
               :wasm.count/datacountsec]
              (u32' n-seg))
    ctx))


;;;;;;;;;; Modules / Modules
 

(def magic'
     4)



(def version'
     4)



(defn section'+

  "Sums all precomputed sections.
  
   Used by [[module']]."

  [{{:wasm.count/keys [codesec
                       datasec
                       datacountsec
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
           datasec
           datacountsec
           elemsec
           exportsec
           funcsec
           globalsec
           importsec
           memsec
           startsec
           tablesec
           typesec]))



(defn module'

  [ctx]

  (let [ctx-2 (-> ctx
                  typesec'
                  importsec'
                  funcsec'
                  tablesec'
                  memsec'
                  globalsec'
                  exportsec'
                  startsec'
                  elemsec'
                  codesec'
                  datasec'
                  datacountsec')]
    (assoc-in ctx-2
              [:wasm/write
               :wasm.count/module]
              (+ magic'
                 version'
                 (section'+ ctx-2)))))
