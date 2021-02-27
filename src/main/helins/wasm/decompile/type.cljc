;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.decompile.type

  ""

  {:author "Adam Helinski"}

  (:require [helins.binf                :as binf]
            [helins.wasm.compile.type   :as wasm.compile.type]
            [helins.wasm.decompile.misc :as wasm.decompile.misc])
  (:refer-clojure :exclude [val]))


;;;;;;;;;; Types


(defn val

  ""

  [view]

  (let [b8 (binf/rr-u8 view)]
    (condp =
           b8
      wasm.compile.type/i32 'i32
      wasm.compile.type/i64 'i64
      wasm.compile.type/f32 'f32
      wasm.compile.type/f64 'f64
      (throw (ex-info (str "Unknown value type: "
                           b8)
                      {})))))



(defn result

  ""

  [view]

  (wasm.decompile.misc/vector (fn [_i-val]
                                (val view))
                              view))



(defn func

  ""

  [view]

  (let [b-1 (binf/rr-u8 view)]
    (when (not= b-1
                0x60)
      (throw (ex-info (str "Function type should start with 0x60, not: "
                           b-1)
                      {})))
    [(result view)
     (result view)]))
