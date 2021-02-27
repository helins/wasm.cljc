;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.decompile.section

  ""

  {:author "Adam Helinski"}

  (:require [helins.binf                :as binf]
            [helins.binf.leb128         :as binf.leb128]
            [helins.wasm.decompile.type :as wasm.decompile.type])
  (:refer-clojure :exclude [type]))


;;;;;;;;;;


(def id->name 

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



(defn valid-id?

  ""

  [id]

  (<= 0
      id
      (count id->name)))


;;;;;;;;;;


(defn type

  ""

  [ctx header view]

  (assoc ctx
         :wasm.section/type
         (mapv (fn [_i-type]
                 (wasm.decompile.type/func view))
               (range (binf.leb128/rr-u32 view)))))


;;;;;;;;;;


(defn find-header+

  ""

  [ctx view]

  (assoc ctx
         :wasm/header+
         (loop [section+ []]
           (if (pos? (binf/remaining view))
             (recur (conj section+
                          (let [id (binf/rr-u8 view)]
                            (when-not (valid-id? id)
                              (throw (ex-info (str "Unknown section ID: "
                                                   id)
                                              {})))
                            (let [n-byte (binf.leb128/rr-u32 view)
                                  start  (binf/position view)]
                              (binf/skip view
                                         n-byte)
                              {:wasm.section/id     id
                               :wasm.section/n-byte n-byte
                               :wasm.section/name   (id->name id)
                               :wasm.section/start  start}))))
             section+))))



(defn parse-header+

  ""

  [ctx view]

  (reduce (fn [ctx-2 {:as                header
                      :wasm.section/keys [id
                                          n-byte
                                          start]}]
            (if-some [f (case id
                          1 type
                          nil)]
              (f ctx-2
                 header
                 (binf/view view
                            start
                            n-byte))
              ctx-2))
          ctx
          (ctx :wasm/header+)))


;;;;;;;;;;


(defn main

  ""

  [ctx view]

  (-> ctx
      (find-header+ view)
      (parse-header+ view)))
