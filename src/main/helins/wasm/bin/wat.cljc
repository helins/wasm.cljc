;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.bin.wat

  ""

  {:author "Adam Helinski"}

  (:require [helins.wasm.bin      :as wasm.bin]
            [helins.wasm.bin.read :as wasm.bin.read]))


(declare functype'
         import'
         importdesc'
         importdesc-func'
         importdesc-global'
         importdesc-mem'
         importdesc-table'
         table
         )


;;;;;;;;;; Miscellaneous


(defn funcsign

  ""

  [[param+ result+]]

  (cond->
    (mapv (fn [valtype i-param]
            (list 'param
                  (symbol (str "$param-"
                               i-param))
                  valtype))
          (rest param+)
          (range))
    result+
    (conj result+)))



(defn vec'

  ""

  [f ctx view]

  (loop [ctx-2 ctx
         i     (wasm.bin.read/u32' view)]
    (if (pos? i)
      (recur (f ctx-2
                view)
             (dec i))
      ctx-2)))


;;;;;;;;;; Type section


(defn typesec'

  ""

  [ctx view]

  (assoc-in ctx
            [:wasm/wat
             :wasm.wat/typesec]
            (wasm.bin.read/typesec' view)))


;;;;;;;;;; Import section


(defn importsec'

  ""

  [ctx view]

  (vec' import'
        ctx
        view))



(defn import'

  ""

  [ctx view]

  (importdesc' ctx
               (wasm.bin.read/name' view)
               (wasm.bin.read/name' view)
               view))



(defn import-sym

  ""

  [module name-]

  (symbol (str "$"
               module
               "/"
               name-)))



(defn importdesc'

  ""

  [ctx module name- view]

  (let [type (wasm.bin.read/byte' view)
        f    (condp =
                    type
               wasm.bin/importdesc-func   importdesc-func'
               wasm.bin/importdesc-table  importdesc-table'
               wasm.bin/importdesc-mem    importdesc-mem'
               wasm.bin/importdesc-global importdesc-global'
               (throw (ex-info (str "Unknown type in import description: "
                                    type)
                               {})))]
    (f ctx
       (import-sym module
                   name-)
       (list 'import
             module
             name-)
       view)))



(defn importdesc-any

  ""

  [ctx sym k-sec k-idx k-resolve entry]

  (update ctx
          :wasm/wat
          (fn [wat]
            (let [idx (or (wat k-idx)
                          0)]
              (-> wat
                  (assoc-in [k-sec
                             sym]
                            entry)
                  (assoc-in [k-resolve
                             idx]
                            sym)
                  (assoc k-idx
                         (inc idx)))))))



(defn importdesc-func'

  ""

  [ctx sym import- view]

  (importdesc-any ctx
                  sym
                  :wasm.wat/func
                  :wasm.wat.func/idx
                  :wasm.wat.func/idx-resolve
                  (list* 'func
                         sym
                         import-
                         (funcsign (get-in (ctx :wasm/wat)
                                           [:wasm.wat/typesec
                                            (wasm.bin.read/typeidx' view)])))))



(defn importdesc-table'

  ""

  [ctx sym import- view]

  (importdesc-any ctx
                  sym
                  :wasm.wat/table
                  :wasm.wat.table/idx
                  :wasm.wat.table/idx-resolve
                  (list* 'table
                         sym
                         import-
                         (table view))))



(defn importdesc-mem'

  ""

  [ctx sym import- view]

  (importdesc-any ctx
                  sym
                  :wasm.wat/memory
                  :wasm.wat.memory/idx
                  :wasm.wat.memory/idx-resolve
                  (list* 'table
                         sym
                         import-
                         (wasm.bin.read/limits' view))))



(defn importdesc-global'

  ""

  [ctx sym import- view]

  (importdesc-any ctx
                  sym
                  :wasm.wat/global
                  :wasm.wat.global/idx
                  :wasm.wat.global/idx-resolve
                  (list* 'global
                         sym
                         import-
                         (let [valtype (wasm.bin.read/valtype' view)]
                           (if (= (wasm.bin.read/mut' view)
                                  'var)
                             (list 'mut
                                   valtype)
                             valtype))
                         (wasm.bin.read/expr' view))))


;;;;;;;;;; Function section


(defn funcsec'

  ""

  [ctx view]

  (update ctx
          :wasm/wat
          (fn [{:as            wat
                :wasm.wat/keys [typesec]}]
            (assoc wat
                   :wasm.wat/funcsec
                   (mapv (fn [typeidx]
                           (if (<= typeidx
                                   (count typesec))
                             (funcsign (get typesec
                                            typeidx))
                             (throw (ex-info (str "Function type index overflow: "
                                                  typeidx)
                                             {}))))
                         (wasm.bin.read/funcsec' view))))))


;;;;;;;;;; Table section


(defn tablesec'

  ""

  [ctx view]

  (assoc-in ctx
            [:wasm/wat
             :wasm.wat/tabelsec]
            (wasm.bin.read/vec' table
                                view)))



(defn table

  ""

  [view]

  (let [elemtype (wasm.bin.read/elemtype' view)
        limits   (wasm.bin.read/limits' view)]
    (conj limits
          elemtype)))
