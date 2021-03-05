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
         instr
         instr'+
         labelidx'
         localidx'
         locals'
         mem'
         memidx'
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


;;;;; Control instructions


(defn blocktype'

  [ctx view]

  (let [x (s33' view)]
    (if (< x
           (binf.int64/i* 0))
      (let [x-2 (bit-and 0x7F
                         (binf.int64/u8 x))]
        [nil
         (if (= x-2
                0x40)
           nil
           [(-valtype x-2)])])
      (get-in ctx
              [:wasm/typsec
               (binf.int64/u32 x)]))))



(defn block'

  [hmap ctx view]

  (assoc hmap
         :wasm/type   (blocktype' ctx
                                  view)
         :wasm/instr+ (instr'+ ctx
                               view)))



(defn loop'

  [hmap ctx view]

  (assoc hmap
         :wasm/type   (blocktype' ctx
                                  view)
         :wasm/instr+ (instr'+ ctx
                               view)))



(defn else'

  [hmap ctx view]

  (assoc hmap
         :wasm/else
         (instr'+ ctx
                  view)))



(defn if'

  [hmap ctx view]

  (let [hmap-2 (assoc hmap
                      :wasm/type
                      (blocktype' ctx
                                  view))]
    (loop [instr+ []]
      (let [opcode (byte' view)]
        (condp =
               opcode
          wasm.bin/else (-> hmap-2
                            (assoc :wasm/instr+
                                   instr+)
                            (else' ctx
                                   view))
          wasm.bin/end  (assoc hmap-2
                               :wasm/instr+
                               instr+)
          (recur (conj instr+
                       (instr ctx
                              opcode
                              view))))))))



(defn br'

  [hmap _ctx view]

  (assoc hmap
         :wasm/labelidx
         (labelidx' view)))



(defn br_if'

  [hmap _ctx view]

  (br' hmap
       _ctx
       view))



(defn br_table'

  [hmap _ctx view]

  (assoc hmap
         :wasm.labelidx/table   (vec' labelidx'
                                      view)
         :wasm.labelidx/default (labelidx' view)))



(defn call'

  [hmap _ctx view]

  (assoc hmap
         :wasm/funcidx
         (funcidx' view)))



(defn call_indirect'

  [hmap ctx view]

  (assoc hmap
         :wasm/type     (get-in ctx
                                [:wasm/typesec
                                 (typeidx' view)])
         :wasm/tableidx (byte' view)))


;;;;; Variable instructions


(defn op-var-local

  ""

  [hmap _ctx view]

  (assoc hmap
         :wasm/localidx
         (localidx' view)))



(defn op-var-global

  ""

  [hmap _ctx view]

  (assoc hmap
         :wasm/globalidx
         (globalidx' view)))


;;;;;; Memory instructions


(defn memarg'

  ""

  [hmap view]

  (assoc hmap
         :wasm.mem/align  (u32' view)
         :wasm.mem/offset (u32' view)))



(defn op-memarg

  ""

  [hmap _ctx view]

  (memarg' hmap
           view))



(defn op-memory

  ""

  [hmap _ctx view]

  (assoc hmap
         :wasm/memidx
         (memidx' view)))


;;;;; Numeric instructions


;;; Constants


(defn op-constval

  ""


  ([const k-const]

   (partial op-constval
            const
            k-const))


  ([const k-const hmap _ctx view]

   (assoc hmap
          k-const
          (const view))))



(defn trunc_sat

  ""

  [hmap _ctx view]

  (assoc hmap
         :wasm.trunc_sat/type
         (u32' view)))

  

;;;;;


(defn instr'+

  ""

  [ctx view]

  (expr' ctx view))



(defn expr'

  ""

  [ctx view]

  (loop [instr+ []]
    (let [opcode (byte' view)]
      (if (= opcode
             wasm.bin/end)
        instr+
        (recur (conj instr+
                     (instr ctx
                            opcode
                            view)))))))



(defn expr-idx

  ""

  [_ctx view]

  (let [opcode (byte' view)
        instr  (cond
                 (= opcode
                    wasm.bin/i32-const)  (list 'i32.const
                                               (i32' view))
                 (= opcode
                    wasm.bin/global-get) (list 'global.get
                                               (globalidx' view))
                 :else                   (throw (ex-info (str "Illegal opcode for index constant expression: "
                                                              opcode)
                                                         {})))]
    (when-not (= (byte' view)
                 wasm.bin/end)
      (throw (ex-info "Constant expression for index should not contain more than 1 instruction"
                      {})))
    instr))



(defn expr-const

  ""

  [ctx view]

  (loop [instr+ []]
    (let [opcode (byte' view)]
      (if (= opcode
             wasm.bin/end)
        instr+
        (recur (conj instr+
                     (if-some [fread (opcode-const->f opcode)]
                       (fread {:wasm.op/code opcode}
                              ctx
                              view)
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
      (update-in (cons :wasm/importsec
                       import-path)
                 (fn [nothing]
                   (when nothing
                     (throw (ex-info (str "Double import for: "
                                          import-path)
                                     {})))
                   [k-section
                    (get ctx
                         k-id)]))))



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

  (vec' (assoc ctx
               :wasm.funcsec/offset
               (ctx :wasm/funcidx))
        (fn [ctx-2 view]
          (wasm.ir/assoc-func ctx-2
                              (func typesec
                                    view)))
        view))


;;;;; Table section


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


;;;;; Memory section


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


;;;;; Global section


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
                                   (expr-const ctx
                                               view)))))


;;;;; Export section


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
               (name' view)
               view))



(defn exportdesc'

  ""

  [ctx export-name view]

  (let [export-type (byte' view)
        f           (condp =
                           export-type
                      wasm.bin/exportdesc-func   exportdesc-func'
                      wasm.bin/exportdesc-table  exportdesc-table'
                      wasm.bin/exportdesc-mem    exportdesc-mem'
                      wasm.bin/exportdesc-global exportdesc-global'
                      (throw (ex-info (str "Unknown type in export description: "
                                           type)
                                      {})))]
    (f ctx
       export-name
       view)))



(defn exportdesc-any

  ""

  [ctx export-name k-section idx]

  (let [path-externval [k-section
                        idx]]
    (-> ctx
        (update-in path-externval
                   (fn [externval]
                     (when-not externval
                       (throw (ex-info (str "Exporting missing externval at: "
                                            path-externval)
                                       {})))
                     (update externval
                             :wasm/export
                             (fnil conj
                                   #{})
                             export-name)))
        (assoc-in [:wasm/exportsec
                   export-name]
                  path-externval))))



(defn exportdesc-func'

  ""

  [ctx export-name view]

  (exportdesc-any ctx
                  export-name
                  :wasm/funcsec
                  (funcidx' view)))



(defn exportdesc-table'

  ""

  [ctx export-name view]

  (exportdesc-any ctx
                  export-name
                  :wasm/tablesec
                  (tableidx' view)))



(defn exportdesc-mem'

  ""

  [ctx export-name view]

  (exportdesc-any ctx
                  export-name
                  :wasm/memsec
                  (memidx' view)))



(defn exportdesc-global'

  ""

  [ctx export-name view]

  (exportdesc-any ctx
                  export-name
                  :wasm/globalsec
                  (globalidx' view)))


;;;;; Start section


(defn startsec'

  ""

  [ctx view]

  (assoc ctx
         :wasm/startsec
         (start' view)))



(defn start'

  ""

  [view]

  (funcidx' view))


;;;;; Element section


(defn elemsec'

  ""

  [ctx view]

  (vec' ctx
        elem'
        view))
 


(defn elem'

  ""

  [{:as        ctx
    :wasm/keys [funcsec]}
   view]

  (let [tableidx (tableidx' view)]
    (update-in ctx
               [:wasm/tablesec
                tableidx]
               (fn [table]
                 (when-not table
                   (throw (ex-info (str "In element segment, table index out of bounds: "
                                        tableidx)
                                   {})))
                 (let [expr (expr-idx ctx
                                      view)]
                   (when (= (first expr)
                            'global.get)
                     (let [globalidx (second expr)
                           global    (get-in ctx
                                             [:wasm/globalsec
                                              globalidx])]
                       (when-not global
                         (throw (ex-info (str "In element, global required for index does not exist: "
                                              globalidx)
                                         {})))
                       (when (not= (global :wasm/valtype)
                                   'i32)
                         (throw (ex-info (str "In element, global required for index is not u32: "
                                              globalidx)
                                         {})))
                       (when (global :wasm/mutable?)
                         (throw (ex-info (str "In element, global required for index is mutable: "
                                              globalidx)
                                         {})))))
                   (update-in table
                              [:wasm/elemsec
                               expr]
                              (fnil conj
                                    [])
                              (vec' []
                                    (fn [func+ view]
                                      (conj func+
                                            (let [funcidx (funcidx' view)]
                                              (or funcidx
                                                  (throw (ex-info (str "In element segment, function index out of bounds: "
                                                                       funcidx)
                                                                  {}))))))
                                    view)))))))


;;;;; Code section


(defn codesec'

  ""

  [{:as                ctx
    :wasm/keys         [funcsec]
    :wasm.funcsec/keys [offset]}
   view]


  (let [n-func (u32' view)]
    (loop [funcsec-2 funcsec
           i         0]
      (if (< i
             n-func)
        (recur (update funcsec-2
                       (+ offset
                          i)
                       assoc
                       :wasm/code
                       (code' view))
               (inc i))
        (assoc ctx
               :wasm/funcsec
               funcsec-2)))))



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

  [func ctx view]

  (-> func
      (assoc :wasm/local+ (locals' (or (get-in func
                                               [:wasm/type
                                                0])
                                       [])
                                   view)
             :wasm/expr   (expr' ctx
                                 view))
      (dissoc :wasm/code)))



(defn locals'

  ""


  ([view]

   (locals' []
            view))


  ([vect view]

   (into vect
         (mapcat identity)
         (vec' (fn [view]
                 (repeat (u32' view)
                         (valtype' view)))
               view))))



(defn codesec'-2

  ""

  [{:as                ctx
    :wasm.funcsec/keys [offset]}]

  (update ctx
          :wasm/funcsec
          (fn [funcsec]
            (reduce (fn [funcsec-2 [funcidx {:as  func
                                             view :wasm/code}]]
                      (assoc funcsec-2
                             funcidx
                             (func' func
                                    ctx
                                    view)))
                    funcsec
                    (subseq funcsec
                            >=
                            offset)))))


;;;;; Data section


(defn datasec'

  ""

  [ctx view]

  (vec' ctx
        data'
        view))



(defn data'

  ""

  [ctx view]

  (let [memidx (memidx' view)]
    (update-in ctx
               [:wasm/memsec
                memidx]
               (fn [mem]
                 (when-not mem
                   (throw (ex-info (str "In data segment, memory index out of bounds: "
                                        memidx)
                                   {})))
                 (update-in mem
                            [:wasm/datasec
                             (expr-idx ctx
                                       view)]
                            (fnil conj
                                  [])
                            (binf/rr-buffer view
                                            (u32' view)))))))


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
   wasm.bin/i32-const     (op-constval i32'
                                       :wasm.i32/const)
   wasm.bin/i64-const     (op-constval i64'
                                       :wasm.i64/const)
   wasm.bin/f32-const     (op-constval f32'
                                       :wasm.f32/const)
   wasm.bin/f64-const     (op-constval f64'
                                       :wasm.f64/const)
   wasm.bin/trunc_sat     trunc_sat})



(def opcode-const->f

  ""

  (select-keys opcode->f
               [wasm.bin/f32-const
                wasm.bin/f64-const
                wasm.bin/global-get
                wasm.bin/i32-const
                wasm.bin/i64-const]))



(defn instr

  ""

  [ctx opcode view]

  (let [hmap {:wasm.op/code opcode}]
    (if-some [f (opcode->f opcode)]
      (f hmap
         ctx
         view)
      (do
        (when-not (contains? wasm.bin/opcode->opsym
                             opcode)
          (throw (ex-info (str "Opcode is not a recognized instruction: "
                               opcode)
                          {})))
        hmap))))
