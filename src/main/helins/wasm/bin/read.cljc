;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.bin.read

  ""

  {:author "Adam Helinski"}

  (:require [helins.binf        :as binf]
            [helins.binf.leb128 :as binf.leb128]
            [helins.wasm.bin    :as wasm.bin])
  ;;
  ;; <!> Keep in mind exclusions <!>
  ;;
  (:refer-clojure :exclude [byte
                            import
                            mut
                            name
                            vec]))


(declare code
         custom
         data
         elem
         elemtype
         export
         exportdesc
         exportdesc-func
         exportdesc-global
         exportdesc-mem
         exportdesc-table
         func
         global
         import
         importdesc
         importdesc-func
         importdesc-global
         importdesc-mem
         importdesc-table
         localidx
         globalidx
         locals
         mem
         mut
         s32
         s64
         start
         table
         u32)


;;;;;;;;;; Conventions


(defn vec

  ""

  [f view]

  (loop [i (u32 view)
         v []]
    (if (pos? i)
      (recur (dec i)
             (conj v
                   (f view)))
      v)))



(defn -vec-byte

  ""

  [view]

  (binf/rr-buffer view
                  (u32 view)))


;;;;;;;;;; Values


;;;;; Byte


(defn byte

  ""

  [view]

  (binf/rr-u8 view))


;;;;; Integers


(defn i32

  ""

  [view]

  (s32 view))



(defn i64

  ""

  [view]

  (s64 view))



(defn s32

  ""

  [view]

  (binf.leb128/rr-i32 view))



(defn s64

  ""

  [view]

  (binf.leb128/rr-i64 view))



(defn u32

  ""

  [view]

  (binf.leb128/rr-u32 view))



(defn u64

  ""

  [view]

  (binf.leb128/rr-u64 view))


;;;;; Floating-Point


(defn f32

  ""

  [view]

  (binf/rr-f32 view))



(defn f64

  ""

  [view]

  (binf/rr-f64 view))


;;;;; Names


(defn name

  ""

  [view]

  (binf/rr-string view
                  (u32 view)))


;;;;;;;;;; Types


;;;;; Value types


(defn valtype

  ""

  [view]

  (let [b8 (u32 view)]
    (condp =
           b8
      wasm.bin/valtype-i32 'i32
      wasm.bin/valtype-i64 'i64
      wasm.bin/valtype-f32 'f32
      wasm.bin/valtype-f64 'f64
      (throw (ex-info (str "Unknown value type: "
                           b8)
                      {})))))


;;;;; Result types


(defn resulttype

  ""

  [view]

  (vec valtype
       view))


;;;;; Funtion types


(defn functype

  ""

  [view]

  (let [b8-1 (byte view)]
    (when (not= b8-1
                wasm.bin/functype)
      (throw (ex-info (str "Function type should start with 0x60, not: "
                           b8-1)
                      {})))
    [(resulttype view)
     (resulttype view)]))


;;;;; Limits


(defn limits

  ""

  [view]

  (let [flag (byte view)]
    (condp =
           flag
      wasm.bin/limits-min    [(u32 view)]
      wasm.bin/limits-minmax [(u32 view)
                              (u32 view)]
      (throw (ex-info (str "Unknown limite type: "
                           flag)
                      {})))))


;;;;; Memory types


(defn memtype

  ""

  [view]

  (limits view))


;;;;; Table types


(defn tabletype

  ""

  [view]

  [(elemtype view)
   (limits view)])



(defn elemtype

  ""

  [view]

  (let [type (byte view)]
    (when (not= type
                wasm.bin/elemtype)
      (throw (ex-info (str "Unknown element type: "
                           type)
                      {})))
    'funcref))


;;;;; Global types


(defn globaltype

  ""

  [view]

  [(valtype view)
   (mut view)])



(defn mut
   
  ""

  [view]

  (let [flag (byte view)]
    (condp =
           flag
      wasm.bin/mut-const 'const
      wasm.bin/mut-var   'var
      (throw (ex-info (str "Unknown mutability flag for global: "
                           flag)
                      {})))))


;;;;;;;;;; Instructions


(defn -op-1

  ""

  [fread opsym view]

  (list opsym
        (fread view)))



(defn -op-2

  ""

  ([fread opsym view]

   (-op-2 fread
          fread
          opsym
          view))


  ([fread-1 fread-2 opsym view]

   (list opsym
         (fread-1 view)
         (fread-2 view))))


;;;;; Variable instructions


(defn -op-var-local

  ""

  [opsym view]

  (-op-1 localidx
         opsym
         view))



(defn -op-var-global

  ""

  [opsym view]

  (-op-1 globalidx
         opsym
         view))




(defn local-get

  ""

  [view]

  (list 'local.get
        (localidx view)))



(defn local-set

  ""

  [view]

  (list 'local.set
        (localidx view)))



(defn local-tee

  ""

  [view]

  (list 'local.tee
        (localidx view)))



(defn global-get

  ""

  [view]

  (list 'global.get
        (globalidx view)))



(defn global-set

  ""

  [view]

  (list 'global.set
        (globalidx view)))


;;;;;; Memory instructions


(defn memarg

  ""

  [view]

  [(symbol (str "align="
                (u32 view)))
   (symbol (str "offset="
                (u32 view)))])



(defn -op-memarg

  ""

  [opsym view]

  (list* opsym
         (memarg view)))



(defn -op-memory

  ""

  [opsym view]

  (-op-1 byte
         opsym
         view))


;;;;; Numeric instructions


;;; Constants


(defn -op-constval

  ""

  [fread opsym view]

  (list opsym
        (fread view)))


;;;;;


(def -opcode->f

  ""

  {
   ;; Variables
   wasm.bin/local-get    ['local.get
                          -op-var-local]
   wasm.bin/local-set    ['local.set
                          -op-var-local]
   wasm.bin/local-tee    ['local.tee
                          -op-var-local]
   wasm.bin/global-get   ['global.get
                          -op-var-global]
   wasm.bin/global-set   ['globalr.set
                          -op-var-global]
   ;; Memory
   wasm.bin/i32-load     ['i32.load
                          -op-memarg]
   wasm.bin/i64-load     ['i64.load
                          -op-memarg]
   wasm.bin/f32-load     ['f32.load
                          -op-memarg]
   wasm.bin/f64-load     ['f64.load
                          -op-memarg]
   wasm.bin/i32-load8_s  ['i32.load8_s
                          -op-memarg]
   wasm.bin/i32-load8_u  ['i32.load8_u
                          -op-memarg]
   wasm.bin/i32-load16_s ['i32.load16_s
                          -op-memarg]
   wasm.bin/i32-load16_u ['i32.load16_u
                          -op-memarg]
   wasm.bin/i64-load8_s  ['i64.load8_s
                          -op-memarg]
   wasm.bin/i64-load8_u  ['i64.load8_u
                          -op-memarg]
   wasm.bin/i64-load16_s ['i64.load16_s
                          -op-memarg]
   wasm.bin/i64-load16_u ['i64.load16_u
                          -op-memarg]
   wasm.bin/i64-load32_s ['i64.load32_s
                          -op-memarg]
   wasm.bin/i64-load32_u ['i64.load32_u
                          -op-memarg]
   wasm.bin/i32-store    ['i32.store
                          -op-memarg]
   wasm.bin/i64-store    ['i64.store
                          -op-memarg]
   wasm.bin/f32-store    ['f32.store
                          -op-memarg]
   wasm.bin/f64-store    ['f64.store
                          -op-memarg]
   wasm.bin/i32-store8   ['i32.store8
                          -op-memarg]
   wasm.bin/i32-store16  ['i32.store16
                          -op-memarg]
   wasm.bin/i64-store8   ['i64.store8
                          -op-memarg]
   wasm.bin/i64-store16  ['i64.store16
                          -op-memarg]
   wasm.bin/i64-store32  ['i64.store32
                          -op-memarg]
   wasm.bin/memory-size  ['memory.size
                          -op-memory]
   wasm.bin/memory-grow  ['memory.grow
                          -op-memory]
   ;; Numeric constants
   wasm.bin/i32-const    ['i32.const
                          (partial -op-constval
                                   i32)]
   wasm.bin/i64-const    ['i64.const
                          (partial -op-constval
                                   i64)]
   wasm.bin/f32-const    ['f32.const
                          (partial -op-constval
                                   f32)]
   wasm.bin/f64-const    ['f64.const
                          (partial -op-constval
                                   f64)]
   })



(def -opcode-const->f

  ""

  (select-keys -opcode->f
               [wasm.bin/f32-const
                wasm.bin/f64-const
                wasm.bin/global-get
                wasm.bin/i32-const
                wasm.bin/i64-const]))



(defn expr

  ""

  [view]

  (loop [instr+ []]
    (let [opcode (byte view)]
      (if (= opcode
             wasm.bin/end)
        instr+
        (recur (conj instr+
                     (when-some [[opsym
                                  fread] (-opcode->f opcode)]
                       (fread opsym
                              view))))))))



(defn expr-const

  ""

  [view]

  (loop [instr+ []]
    (let [opcode (byte view)]
      (if (= opcode
             wasm.bin/end)
        instr+
        (recur (conj instr+
                     (if-some [[opsym
                                fread] (-opcode-const->f opcode)]
                       (fread opsym
                              view)
                       (throw (ex-info (str "Given instruction is illegal in constant expression: "
                                            opcode)
                                       {})))))))))


;;;;;;;;;; Modules


;;;;; Indices


(defn idx

  ""

  [view]

  (u32 view))



(def typeidx

  "Alias to [[idx]].

   Defined to mimick non-terminal symbol in WASM specification."

  idx)




(def funcidx

  "Alias to [[idx]].

   Defined to mimick non-terminal symbol in WASM specification."

  idx)




(def tableidx

  "Alias to [[idx]].

   Defined to mimick non-terminal symbol in WASM specification."

  idx)




(def memidx

  "Alias to [[idx]].

   Defined to mimick non-terminal symbol in WASM specification."

  idx)




(def globalidx

  "Alias to [[idx]].

   Defined to mimick non-terminal symbol in WASM specification."

  idx)




(def localidx

  "Alias to [[idx]].

   Defined to mimick non-terminal symbol in WASM specification."

  idx)




(def labelidx

  "Alias to [[idx]].

   Defined to mimick non-terminal symbol in WASM specification."

  idx)


;;;;; Sections


(defn section

  ""

  [view]

  (let [id (byte view)]
    (when-not (wasm.bin/section-id? id)
      (throw (ex-info (str "Unknown section ID: "
                           id)
                      {})))
    (let [n-byte (u32 view)
          start  (binf/position view)]
      (binf/skip view
                 n-byte)
      {:wasm.section/id     id
       :wasm.section/n-byte n-byte
       :wasm.section/start  start})))


;;;;; Custom section


(defn customsec

  ""

  [view]

  (custom view))



(defn custom

  ""

  [view]

  [(name view)
   (binf/rr-buffer view
                   (binf/remaining view))])


;;;;; Type section


(defn typesec

  ""

  [view]

  (vec functype
       view))


;;;;; Import section


(defn importsec

  ""

  [view]

  (vec import
       view))



(defn import

  ""

  [view]

  [(name view)
   (name view)
   (importdesc view)])



(defn importdesc

  ""

  [view]

  (let [type (byte view)
        f    (condp =
                    type
               wasm.bin/importdesc-func   importdesc-func
               wasm.bin/importdesc-table  importdesc-table
               wasm.bin/importdesc-mem    importdesc-mem
               wasm.bin/importdesc-global importdesc-global
               (throw (ex-info (str "Unknown type in import description: "
                                    type)
                               {})))]
    (f view)))



(defn importdesc-func

  ""

  [view]

  (list 'func
        (typeidx view)))




(defn importdesc-table

  ""

  [view]

  (list 'table
        (tabletype view)))



(defn importdesc-mem

  ""

  [view]

  (list 'memory
        (memtype view)))



(defn importdesc-global

  ""

  [view]

  (list 'global
        (globaltype view)))
  

;;;;; Function section


(defn funcsec

  ""

  [view]

  (vec idx
       view))


;;;;; Table section


(defn tablesec

  ""

  [view]

  (vec table
       view))



(defn table

  ""

  [view]

  (tabletype view))


;;;;; Memory section


(defn memsec

  ""

  [view]

  (vec mem
       view))



(defn mem

  ""

  [view]

  (memtype view))


;;;;; Global section


(defn globalsec

  ""

  [view]

  (vec global
       view))



(defn global

  ""

  [view]

  [(globaltype view)
   (expr-const view)])


;;;;; Export section


(defn exportsec

  ""

  [view]

  (vec export
       view))



(defn export

  ""

  [view]

  [(name view)
   (exportdesc view)])

  

(defn exportdesc

  ""

  [view]

  (let [type (byte view)
        f    (condp =
                    type
               wasm.bin/exportdesc-func   exportdesc-func
               wasm.bin/exportdesc-table  exportdesc-table
               wasm.bin/exportdesc-mem    exportdesc-mem
               wasm.bin/exportdesc-global exportdesc-global
               (throw (ex-info (str "Unknown type in export description: "
                                    type)
                               {})))]
    (f view)))



(defn exportdesc-func

  ""

  [view]

  (list 'func
        (typeidx view)))




(defn exportdesc-table

  ""

  [view]

  (list 'table
        (tableidx view)))



(defn exportdesc-mem

  ""

  [view]

  (list 'memory
        (memidx view)))



(defn exportdesc-global

  ""

  [view]

  (list 'global
        (globalidx view)))


;;;;; Start section


(defn startsec

  ""

  [view]

  (start view))



(defn start

  ""

  [view]

  (funcidx view))


;;;;; Element section


(defn elemsec

  ""

  [view]

  (vec elem
       view))



(defn elem

  ""

  [view]

  [(tableidx view)
   (expr-const view)
   (vec funcidx
        view)])


;;;;; Code section


(defn codesec

  ""

  [view]

  (vec code
       view))



(defn code

  ""

  [view]

  [{:wasm/n-byte (u32 view)
    :wasm/start  (binf/position view)}
   (func view)])



(defn func

  ""

  [view]

  [(locals view)
   (expr view)])



(defn locals

  ""

  [view]

  (vec (fn [view]
         [(u32 view)
          (valtype view)])
       view))


;;;;; Data section


(defn datasec

  ""

  [view]

  (vec data
       view))



(defn data

  ""

  [view]

  [(memidx view)
   (expr-const view)
   (-vec-byte view)])


;;;;; Module


(defn magic

  ""

  ;; Checking for "\0asm" BOM (reversed u32 because view is little-endian).

  [view]

  (when (not= (binf/rr-u32 view)
              0x6d736100)
    (throw (ex-info "WASM file does not start with magic word"
                    {}))))



(defn version

  ""

  [view]

  (binf/rr-u32 view))
