;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.decompile.misc

  ""

  {:author "Adam Helinski"}

  (:require [helins.binf        :as binf]
            [helins.binf.leb128 :as binf.leb128])
  ;;
  ;; <!> Keep in mind exclusions <!>
  ;;
  (:refer-clojure :exclude [vector]))


(declare u32)


;;;;;;;;;;


(defn index

  ""

  [view]

  (u32 view))



(defn vector

  ""

  [f view]

  (mapv f
        (range (u32 view))))



(defn u32

  ""

  [view]

  (binf.leb128/rr-u32 view))
