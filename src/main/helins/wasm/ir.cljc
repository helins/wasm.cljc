;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.ir

  ""

  {:author "Adam Helinski"})


;;;;;;;;;;


(defn assoc-externval

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



(defn assoc-func

  ""

  [ctx func]

  (assoc-externval ctx
                   :wasm/funcsec
                   :wasm/funcidx
                   func))



(defn assoc-table

  ""

  [ctx table]

  (assoc-externval ctx
                   :wasm/tablesec
                   :wasm/tableidx
                   table))



(defn assoc-mem

  ""

  [ctx mem]

  (assoc-externval ctx
                   :wasm/memsec
                   :wasm/memidx
                   mem))



(defn assoc-global

  ""

  [ctx global]

  (assoc-externval ctx
                   :wasm/globalsec
                   :wasm/globalidx
                   global))
