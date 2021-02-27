;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.decompile

  ""

  {:author "Adam Helinski"}

  (:require [helins.binf        :as binf]
            [helins.binf.leb128 :as binf.leb128]))


;;;;;;;;;;



(def section-id+

  ""

  [:custom
   :type
   :import
   :function
   :table
   :memory
   :global
   :export
   :start
   :element
   :code
   :data])



(defn section+

  ""

  [ctx view]

  (assoc ctx
         :wasm/section+
         (loop [section+ []]
           (if (pos? (binf/remaining view))
             (recur (conj section+
                          (let [start  (binf/position view)
                                id     (binf/rr-u8 view)]
                            (when-not (<= 0
                                          id
                                          12)
                              (throw (ex-info (str "Unknown section ID: "
                                                   id)
                                              {})))
                            (let [n-byte (binf.leb128/rr-u32 view)]
                              (binf/skip view
                                         n-byte)
                              {:wasm.section/id     id
                               :wasm.section/n-byte n-byte
                               :wasm.section/name   (get section-id+
                                                         id)
                               :wasm.section/start  start}))))
             section+))))



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



(defn from-source

  ""

  [source]

  (let [view (binf/view source)]
    (binf/endian-set view
                     :little-endian)
    (validate-magic view)
    (-> {}
        (version view)
        (section+ view))))
