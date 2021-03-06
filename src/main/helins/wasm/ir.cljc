;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.ir

  ""

  {:author "Adam Helinski"})


;;;;;;;;;;


(defn assoc-resource

  ""

  [ctx k-section k-idx externval]

  (let [idx (get ctx
                 k-idx)]
    (-> ctx
        (assoc-in [k-section
                   idx]
                  externval)
        (assoc k-idx
               (inc idx)))))

;;;;;;


(defn assoc-func

  ""

  [ctx func]

  (assoc-resource ctx
                  :wasm/funcsec
                  :wasm/funcidx
                  func))



(defn assoc-global

  ""

  [ctx global]

  (assoc-resource ctx
                  :wasm/globalsec
                  :wasm/globalidx
                  global))



(defn assoc-mem

  ""

  [ctx mem]

  (assoc-resource ctx
                  :wasm/memsec
                  :wasm/memidx
                  mem))



(defn assoc-table

  ""

  [ctx table]

  (assoc-resource ctx
                  :wasm/tablesec
                  :wasm/tableidx
                  table))



(defn assoc-type

  ""

  [ctx type]

  (assoc-resource ctx
                  :wasm/typesec
                  :wasm/typeidx
                  type))
