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


(declare u32)


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

  (let [b8 (byte view)]
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
