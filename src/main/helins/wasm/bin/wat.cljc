;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.bin.wat

  ""

  {:author "Adam Helinski"}

  (:require [helins.wasm.bin.read :as wasm.bin.read]))


(declare functype'
         import'
         importdesc'
         importdesc-func'
         importdesc-global'
         importdesc-mem'
         importdesc-table'
         table)


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


;;;;;;;;;; Import section


(defn import-id

  ""

  [module name-]

  (symbol (str "$"
               module
               "/"
               name-)))



(defn importsec'

  ""

  [{:as                          ctx
    {:wasm.bin/keys [importsec]} :wasm/bin}]

  (reduce (fn [ctx-2 import-]
            (import' ctx-2
                     import-))
          ctx
          importsec))



(defn import'

  ""

  [ctx [module name- importdesc]]

  (importdesc' ctx
               (import-id module
                          name-)
               (list 'import
                     module
                     name-)
               importdesc))



(defn importdesc'

  ""

  [ctx import-id import-abbr importdesc]

  ((condp =
          (first importdesc)
     'func   importdesc-func'
     'table  importdesc-table'
     'mem    importdesc-mem'
     'global importdesc-global')
   ctx
   import-id
   import-abbr
   importdesc))



(defn importdesc-any

  ""

  [ctx import-id k-sec k-idx k-resolve entry]

  (update ctx
          :wasm/wat
          (fn [wat]
            (let [idx (or (get wat
                               k-idx)
                          0)]
              (-> wat
                  (assoc-in [k-sec
                             import-id]
                            entry)
                  (assoc-in [k-resolve
                             idx]
                            import-id)
                  (assoc k-idx
                         (inc idx)))))))



(defn importdesc-func'

  ""

  [ctx import-id import-abbr importdesc]

  (importdesc-any ctx
                  import-id
                  :wasm.wat/func
                  :wasm.wat.func/idx
                  :wasm.wat.func/idx-resolve
                  (list* 'func
                         import-id
                         import-abbr
                         (funcsign (get-in ctx
                                           [:wasm/bin
                                            :wasm.bin/typesec
                                            (second importdesc)])))))



(defn importdesc-table'

  ""

  [ctx import-id import-abbr importdesc]

  (importdesc-any ctx
                  import-id
                  :wasm.wat/table
                  :wasm.wat.table/idx
                  :wasm.wat.table/idx-resolve
                  (list* 'table
                         import-id
                         import-abbr
                         (rest importdesc))))



(defn importdesc-mem'

  ""

  [ctx import-id import-abbr importdesc]

  (importdesc-any ctx
                  import-id
                  :wasm.wat/memory
                  :wasm.wat.memory/idx
                  :wasm.wat.memory/idx-resolve
                  (list* 'memory
                         import-id
                         import-abbr
                         (rest importdesc))))



(defn importdesc-global'

  ""

  [ctx import-id import-abbr importdesc]

  (importdesc-any ctx
                  import-id
                  :wasm.wat/global
                  :wasm.wat.global/idx
                  :wasm.wat.global/idx-resolve
                  (list* 'global
                         import-id
                         import-abbr
                         (rest importdesc))))


;;;;;;;;;; Function section


#_(defn funcsec'

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


#_(defn tablesec'

  ""

  [ctx view]

  (assoc-in ctx
            [:wasm/wat
             :wasm.wat/tabelsec]
            (wasm.bin.read/vec' table
                                view)))



#_(defn table

  ""

  [view]

  (let [elemtype (wasm.bin.read/elemtype' view)
        limits   (wasm.bin.read/limits' view)]
    (conj limits
          elemtype)))
