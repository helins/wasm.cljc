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


(declare elemtype
         export
         exportdesc
         exportdesc-func
         exportdesc-global
         exportdesc-mem
         exportdesc-table
         global
         import
         importdesc
         importdesc-func
         importdesc-global
         importdesc-mem
         importdesc-table
         mem
         mut
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


;;;;;;;;;; Values


;;;;; Byte


(defn byte

  ""

  [view]

  (binf/rr-u8 view))


;;;;; Integers



(defn s32

  ""

  [view]

  (binf.leb128/rr-i32 view))



(defn u32

  ""

  [view]

  (binf.leb128/rr-u32 view))


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


(defn expr

  ""

  [view]

  (let [position (binf/position view)]
    (loop [n-byte 0]
      (if (= wasm.bin/end
             (byte view))
        (binf/ra-buffer view
                        position
                        n-byte)
        (recur (inc n-byte))))))


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


;(defn customsec
;
;  ""
;
;  [view]
;
;  )


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
   (expr view)])


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


;;;;;;;;;; Start section


(defn startsec

  ""

  [view]

  (vec start
       view))



(defn start

  ""

  [view]

  (funcidx view))
