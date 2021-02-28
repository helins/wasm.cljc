;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.decompile

  ""

  {:author "Adam Helinski"}

  (:require [helins.binf          :as binf]
            [helins.wasm.bin      :as wasm.bin]
            [helins.wasm.bin.read :as wasm.bin.read]))


;;;;;;;;;; Start of a WASM file


(defn source->view

  ""

  [source]

  (-> source
      binf/view
      (binf/endian-set :little-endian)))



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

  [ctx view]

  (assoc ctx
         :wasm/version
         (binf/rr-u32 view)))


;;;;;;;;;; Find and parsing sections


(defn section-find+

  ""

  [ctx view]

  (assoc ctx
         :wasm/header+
         (loop [section+ []]
           (if (pos? (binf/remaining view))
             (recur (conj section+
                          (wasm.bin.read/section view)))
             section+))))



(defn section-read+

  ""

  [ctx view]

  (reduce (fn [ctx-2 {:wasm.section/keys [id
                                          n-byte
                                          start]}]
            (if-some [[k
                       f] (condp =
                                 id
                            wasm.bin/section-id-type     [:wasm.bin/typesec
                                                          wasm.bin.read/typesec]
                            wasm.bin/section-id-import   [:wasm.bin/importsec
                                                          wasm.bin.read/importsec]
                            wasm.bin/section-id-function [:wasm.bin/funcsec
                                                          wasm.bin.read/funcsec]
                            wasm.bin/section-id-table    [:wasm.bin/tablesec
                                                          wasm.bin.read/tablesec]
                            wasm.bin/section-id-memory   [:wasm.bin/memsec
                                                          wasm.bin.read/memsec]
                            wasm.bin/section-id-global   [:wasm.bin/globalsec
                                                          wasm.bin.read/globalsec]
                            nil)]
              (assoc ctx-2
                     k
                     (f (binf/view view
                                   start
                                   n-byte)))
              ctx-2))
          ctx
          (ctx :wasm/header+)))


;;;;;;;;;;


(defn main

  ""

  [source]

  (let [view (source->view source)]
    (magic view)
    (-> {}
        (version view)
        (section-find+ view)
        (section-read+ view))))
