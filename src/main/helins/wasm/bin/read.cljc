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
                            name
                            vec]))


(declare elemtype
         mut
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


(defn mut

  ""

  [view]

  (let [type (valtype view)
        flag (byte view)]
    (condp =
           flag
      wasm.bin/mut-const 'const
      wasm.bin/mut-var   'var
      (throw (ex-info (str "Unknown mutability flag for global: "
                           flag)
                      {})))))


;;;;;;;;;; Modules


;;;;; Indices


(defn idx

  ""

  [view]

  (u32 view))


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


;;;;;;;;;; Type section


(defn typesec

  ""

  [view]

  (vec functype
       view))


;;;;;;;;;; Function section


(defn funcsec

  ""

  [view]

  (vec idx
       view))
