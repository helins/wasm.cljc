;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.bin.read

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
         exportdesc-func'
         exportdesc-global'
         exportdesc-mem'
         exportdesc-table'
         expr'
         func'
         funcidx'
         global'
         globalidx'
         import'
         importdesc'
         importdesc-func'
         importdesc-global'
         importdesc-mem'
         importdesc-table'
         labelidx'
         localidx'
         locals'
         mem'
         mut'
         opcode->f
         opcode-const->f
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


;;;;;;;;;; Values


;;;;; Byte


(defn byte'

  ""

  [view]

  (binf/rr-u8 view))


;;;;; Integers


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


;;;;; Floating-Point


(defn f32'

  ""

  [view]

  (binf/rr-f32 view))



(defn f64'

  ""

  [view]

  (binf/rr-f64 view))


;;;;; Names


(defn name'

  ""

  [view]

  (binf/rr-string view
                  (u32' view)))


;;;;;;;;;; Types


;;;;; Value types


(defn -valtype

  ""

  [b8]
  (condp =
         b8
    wasm.bin/valtype-i32 'i32
    wasm.bin/valtype-i64 'i64
    wasm.bin/valtype-f32 'f32
    wasm.bin/valtype-f64 'f64
    (throw (ex-info (str "Unknown value type: "
                         b8)
                    {}))))



(defn valtype'

  ""

  [view]

  (-valtype (byte' view)))


;;;;; Result types


(defn resulttype'

  ""

  [view]

  (not-empty (vec' valtype'
             view)))



(defn resulttype

  ""

  [sym view]

  (when-some [valtype+ (not-empty (resulttype' view))]
    (cons sym
          valtype+)))


;;;;; Funtion types


(defn func

  ""

  [typesec view]


  (let [typeidx (typeidx' view)]
    (when (>= typeidx
              (count typesec))
      (throw (ex-info (str "Imported function type index out of bounds: "
                           typeidx)
                      {})))
    {:wasm/type (get typesec
                     typeidx)}))



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


;;;;; Limits


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


;;;;; Memory types


(defn memtype'

  ""

  [hmap view]

  (limits' hmap
           view))


;;;;; Table types


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

  (let [type (byte' view)]
    (when (not= type
                wasm.bin/elemtype)
      (throw (ex-info (str "Unknown element type: "
                           type)
                      {})))
    'funcref))


;;;;; Global types


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


;;;;;;;;;; Instructions


(defn op-1

  ""


  ([fread]

   (partial op-1
            fread))


  ([fread opsym view]

   (list opsym
         (fread view))))



(defn op-2

  ""


  ([fread]

   (op-2 fread
         fread))


  ([fread-1 fread-2]

   (partial op-2
            fread-1
            fread-2))


  ([fread opsym view]

   (op-2 fread
         fread
         opsym
         view))


  ([fread-1 fread-2 opsym view]

   (list opsym
         (fread-1 view)
         (fread-2 view))))


;;;;; Control instructions


(defn blocktype'

  [view]

  (let [x (s33' view)]
    (if (< x
           (binf.int64/i* 0))
      (let [x-2 (bit-and 0x7F
                         (binf.int64/u8 x))]
        (if (= x-2
               0x40)
          nil
          (list 'result
                (-valtype x-2))))
      (binf.int64/u32 x))))



(defn block-expr

  ""


  ([view]

   (block-expr (blocktype' view)
               (expr' view)))


  ([blocktype expr]

   (if blocktype
     (cons blocktype
           expr)
     expr)))



(defn block'

  [_opsym view]

  (list* 'block
         (block-expr view)))



(defn loop'

  [_opsym view]

  (list* 'loop
         (block-expr view)))



(defn else'

  [view]

  (list* 'else
         (expr' view)))



(defn if'

  [_opsym view]

  (let [blocktype (blocktype' view)]
    (loop [instr+ []]
      (let [opcode (byte' view)]
        (condp =
               opcode
          (wasm.bin/opcode* 'else) (cons 'if
                                         (block-expr blocktype
                                                     [(list* 'then
                                                              instr+)
                                                       (else' view)]))
          (wasm.bin/opcode* 'end)  (cons 'if
                                         (block-expr blocktype
                                                     [(list* 'then
                                                             instr+)]))
          (recur (conj instr+
                       (if-some [fread (opcode->f opcode)]
                         (fread view)
                         (or (wasm.bin/opcode->opsym opcode)
                             (throw (ex-info (str "Opcode is not a recognized instruction: "
                                                  opcode)
                                             {})))))))))))



(defn br_table'

  [_opsym view]

  (cons 'br_table
        (conj (vec' labelidx'
                    view)
              (labelidx' view))))


;;;;; Variable instructions


(defn op-var-local

  ""

  [opsym view]

  (op-1 localidx'
        opsym
        view))



(defn op-var-global

  ""

  [opsym view]

  (op-1 globalidx'
        opsym
        view))


;;;;;; Memory instructions


(defn memarg'

  ""

  [view]

  [(symbol (str "align="
                (u32' view)))
   (symbol (str "offset="
                (u32' view)))])



(defn op-memarg

  ""

  [opsym view]

  (list* opsym
         (memarg' view)))



(defn op-memory

  ""

  [opsym view]

  (op-1 byte'
        opsym
        view))


;;;;; Numeric instructions


;;; Constants


(defn op-constval

  ""


  ([fread]

   (partial op-constval
            fread))


  ([fread opsym view]

   (list opsym
         (fread view))))


;;;;;


(defn expr'

  ""

  [view]

  (loop [instr+ []]
    (let [opcode (byte' view)]
      (if (= opcode
             (wasm.bin/opcode* 'end))
        instr+
        (recur (conj instr+
                     (if-some [fread (opcode->f opcode)]
                       (fread view)
                       (or (wasm.bin/opcode->opsym opcode)
                           (throw (ex-info (str "Opcode is not a recognized instruction: "
                                                opcode)
                                           {}))))))))))



(defn expr-const

  ""

  [view]

  (loop [instr+ []]
    (let [opcode (byte' view)]
      (if (= opcode
             wasm.bin/end)
        instr+
        (recur (conj instr+
                     (if-some [fread (opcode-const->f opcode)]
                       (fread view)
                       (throw (ex-info (str "Given opcode is illegal in constant expression: "
                                            opcode)
                                       {})))))))))


;;;;;;;;;; Modules


;;;;; Indices


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


;;;;; Sections


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
      {:wasm.section/id     id
       :wasm.section/n-byte n-byte
       :wasm.section/start  start})))


;;;;; Custom section


(defn customsec'

  ""

  [view]

  (custom' view))



(defn custom'

  ""

  [view]

  [(name' view)
   (binf/view view
              (binf/position view)
              (binf/remaining view))])


;;;;; Type section


(defn typesec'

  ""

  [ctx view]

  (vec' ctx
        (fn [ctx-2 view]
          (update ctx-2
                  :wasm/typesec
                  conj
                  (functype' view)))
        view))


;;;;; Import section


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
               [(name' view)
                (name' view)]
               view))



(defn importdesc'

  ""

  [ctx import-path view]

  (let [type (byte' view)
        f    (condp =
                    type
               wasm.bin/importdesc-func   importdesc-func'
               wasm.bin/importdesc-table  importdesc-table'
               wasm.bin/importdesc-mem    importdesc-mem'
               wasm.bin/importdesc-global importdesc-global'
               (throw (ex-info (str "Unknown type in import description: "
                                    type)
                               {})))]

    (f ctx
       import-path
       view)))



(defn importdesc-any

  ""

  [ctx import-path assoc-externval k-section k-id hmap]

  (-> ctx
      (assoc-externval (assoc hmap
                              :wasm/import
                              import-path))
      (assoc-in (concat [:wasm/importsec
                         k-section]
                        import-path)
                (get ctx
                     k-id))))



(defn importdesc-func'

  ""

  [ctx import-path view]

  (importdesc-any ctx
                  import-path
                  wasm.ir/assoc-func
                  :wasm/funcsec
                  :wasm/funcidx
                  (func (get ctx
                             :wasm/typesec)
                        view)))



(defn importdesc-table'

  ""

  [ctx import-path view]

  (importdesc-any ctx
                  import-path
                  wasm.ir/assoc-table
                  :wasm/tablesec
                  :wasm/tableidx
                  (tabletype' {}
                              view)))



(defn importdesc-mem'

  ""

  [ctx import-path view]

  (importdesc-any ctx
                  import-path
                  wasm.ir/assoc-mem
                  :wasm/memsec
                  :wasm/memidx
                  (memtype' {}
                            view)))



(defn importdesc-global'

  ""

  [ctx import-path view]

  (importdesc-any ctx
                  import-path
                  wasm.ir/assoc-global
                  :wasm/globalsec
                  :wasm/globalidx
                  (globaltype' {}
                               view)))


;;;;; Function section


(defn funcsec'

  ""

  [{:as        ctx
    :wasm/keys [typesec]}
   view]

  (vec' ctx
        (fn [ctx-2 view]
          (wasm.ir/assoc-func ctx-2
                              (func typesec
                                    view)))
        view))


;;;;; Table section


(defn tablesec'

  ""

  [view]

  (vec' table'
        view))



(defn table'

  ""

  [view]

  (tabletype' view))


;;;;; Memory section


(defn memsec'

  ""

  [view]

  (vec' mem'
        view))



(defn mem'

  ""

  [view]

  (memtype' view))


;;;;; Global section


(defn globalsec'

  ""

  [view]

  (vec' global'
        view))



(defn global'

  ""

  [view]

  (cons (globaltype' view)
        (expr-const view)))


;;;;; Export section


(defn exportsec'

  ""

  [view]

  (vec' export'
        view))



(defn export'

  ""

  [view]

  [(name' view)
   (exportdesc' view)])

  

(defn exportdesc'

  ""

  [view]

  (let [type (byte' view)
        f    (condp =
                    type
               wasm.bin/exportdesc-func   exportdesc-func'
               wasm.bin/exportdesc-table  exportdesc-table'
               wasm.bin/exportdesc-mem    exportdesc-mem'
               wasm.bin/exportdesc-global exportdesc-global'
               (throw (ex-info (str "Unknown type in export description: "
                                    type)
                               {})))]
    (f view)))



(defn exportdesc-func'

  ""

  [view]

  (list 'func
        (typeidx' view)))




(defn exportdesc-table'

  ""

  [view]

  (list 'table
        (tableidx' view)))



(defn exportdesc-mem'

  ""

  [view]

  (list 'memory
        (memidx' view)))



(defn exportdesc-global'

  ""

  [view]

  (list 'global
        (globalidx' view)))


;;;;; Start section


(defn startsec'

  ""

  [view]

  (start' view))



(defn start'

  ""

  [view]

  (funcidx' view))


;;;;; Element section


(defn elemsec'

  ""

  [view]

  (vec' elem'
        view))
 


(defn elem'

  ""

  [view]

  (list* (concat [(tableidx' view)
                  (cons 'offset
                        (expr-const view))]
                 (vec' funcidx'
                       view))))


;;;;; Code section


(defn codesec'

  ""

  [view]

  (vec' code'
        view))



(defn code'

  ""

  [view]

  (let [n-byte (u32' view)
        start  (binf/position view)]
    (binf/skip view
               n-byte)
    (binf/view view
               start
               n-byte)))



(defn func'

  ""

  [view]

  (into (locals' view)
        (expr' view)))



(defn locals'

  ""

  [view]

  (into []
        (mapcat identity)
        (vec' (fn [view]
                (repeat (u32' view)
                        (list 'local
                              (valtype' view))))
              view)))


;;;;; Data section


(defn datasec'

  ""

  [view]

  (vec' data'
        view))



(defn data'

  ""

  [view]

  [(memidx' view)
   (cons 'offset
         (expr-const view))
   (binf/rr-buffer view
                   (u32' view))])


;;;;; Module


(defn magic'

  ""

  ;; Checking for "\0asm" BOM (reversed u32 because view is little-endian).

  [view]

  (when (not= (binf/rr-u32 view)
              0x6d736100)
    (throw (ex-info "WASM file does not start with magic word"
                    {}))))



(defn version'

  ""

  [view]

  (binf/rr-u32 view))


;;;;;;;;;; All operations which needs to process more than their opsym


(def opcode->f

  ""

  (reduce-kv (fn [opcode->f opsym f]
               (let [opcode (wasm.bin/opsym->opcode opsym)]
                 (when-not opcode
                   (throw (ex-info (str "Opcode not found for: "
                                        opsym)
                                   {})))
                 (assoc opcode->f
                        opcode
                        (partial f
                                 opsym))))
             {}
             {'block         block'
              'loop          loop'
              'if            if'
              'br            (op-1 labelidx')
              'br_if         (op-1 labelidx')
              'br_table      (op-1 (partial vec'
                                            labelidx'))
              'call          (op-1 funcidx')
              'call_indirect (op-2 typeidx'
                                   byte')

              'local.get     op-var-local
              'local.set     op-var-local
              'local.tee     op-var-local
              'global.get    op-var-global
              'global.set    op-var-global

              'i32.load      op-memarg 
              'i64.load      op-memarg
              'f32.load      op-memarg
              'f64.load      op-memarg
              'i32.load8_s   op-memarg
              'i32.load8_u   op-memarg
              'i32.load16_s  op-memarg
              'i32.load16_u  op-memarg
              'i64.load8_s   op-memarg
              'i64.load8_u   op-memarg
              'i64.load16_s  op-memarg
              'i64.load16_u  op-memarg
              'i64.load32_s  op-memarg
              'i64.load32_u  op-memarg
              'i32.store     op-memarg
              'i64.store     op-memarg
              'f32.store     op-memarg
              'f64.store     op-memarg
              'i32.store8    op-memarg
              'i32.store16   op-memarg
              'i64.store8    op-memarg
              'i64.store16   op-memarg
              'i64.store32   op-memarg
              'memory.size   op-memory
              'memory.grow   op-memory

              'i32.const     (op-constval i32')
              'i64.const     (op-constval i64')
              'f32.const     (op-constval f32')
              'f64.const     (op-constval f64')}))



(def opcode-const->f

  ""

  (select-keys opcode->f
               [(wasm.bin/opcode* 'f32.const)
                (wasm.bin/opcode* 'f64.const)
                (wasm.bin/opcode* 'global.get)
                (wasm.bin/opcode* 'i32.const)
                (wasm.bin/opcode* 'i64.const)]))

