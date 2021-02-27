;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.decompile

  ""

  {:author "Adam Helinski"}

  (:require [helins.binf                   :as binf]
            [helins.wasm.decompile.section :as wasm.decompile.section]))


;;;;;;;;;;


(defn source-view

  ""

  [source]

  (-> source
      binf/view
      (binf/endian-set :little-endian)))



(defn validate-magic

  ""

  [view]

  (when (not= (binf/rr-u32 view)
               0x6d736100)
    (throw (ex-info "WASM file does not start with magic word"
                    {}))))



(defn version

  ""

  [ctx view]

  (assoc ctx
         :wasm/version
         (binf/rr-u32 view)))


;;;;;;;;;;


(defn main

  ""

  [source]

  (let [view (source-view source)]
    (validate-magic view)
    (-> {}
        (version view)
        (wasm.decompile.section/main view))))
