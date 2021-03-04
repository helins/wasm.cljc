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
         locals'
         table)


;;;;;;;;;; Miscellaneous


(defn funcsign

  ""

  [[param+ result+ :as type-func]]

  (loop [i-param     0
         id->idx     {}
         param-sign+ []
         param-2+    param+]
    (if (seq param-2+)
      (let [valtype  (first param-2+)
            param-id (symbol (str "$param-"
                                  i-param))]
        (recur (inc i-param)
               (assoc id->idx
                      param-id
                      i-param)
               (conj param-sign+
                     (list 'param
                           param-id
                           valtype))
               (rest param-2+)))
      (with-meta (cond->
                   param-sign+
                   result+
                   (conj (cons 'result
                               result+)))
                 {:wasm.func/type     type-func
                  :wasm.local/id->idx id->idx}))))



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
                  (let [funcsign- (funcsign (get-in ctx
                                                    [:wasm/bin
                                                     :wasm.bin/typesec
                                                     (second importdesc)]))]
                    (with-meta (list* 'func
                                      import-id
                                      import-abbr
                                      funcsign-)
                               (meta funcsign-)))))



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


;;;;;;;;;; Export section


(defn export-id

  ""

  [wat-exportsec resource-type idx]

  (when-some [string (get-in wat-exportsec
                             [resource-type
                              idx])]
    (symbol (str "$"
                 string))))



(defn exportsec'

  ""

  [{:as                          ctx
    {:wasm.bin/keys [exportsec]} :wasm/bin}]

  (assoc-in ctx
            [:wasm/wat
             :wasm.wat/exportsec]
            (reduce (fn [acc [string exportdesc]]
                      (assoc-in acc
                                exportdesc
                                string))
                    {}
                    exportsec)))


;;;;;;;;;;


(defn resource

  ""

  [ctx resource+ resource-type k-section k-idx k-idx-resolve]

  (update ctx
          :wasm/wat
          (fn [{:as            wat
                wat-exportsec :wasm.wat/exportsec}]
            (reduce (fn [wat-2 resource]
                      (let [idx (or (get wat-2
                                         k-idx)
                                    0)
                            id  (or (export-id wat-exportsec
                                               resource-type
                                               idx)
                                    (symbol (str "$"
                                                 resource-type
                                                 "-"
                                                 idx)))]
                        (-> wat-2
                            (assoc-in [k-section
                                       id]
                                      (with-meta (list* resource-type
                                                        id
                                                        resource)
                                                 (meta resource)))
                            (assoc k-idx
                                   (inc idx))
                            (assoc-in [k-idx-resolve
                                       idx]
                                      id))))
                    wat
                    resource+))))


;;;;;;;;;; Function section


(defn funcsec'

  ""

  [{:as                             ctx
    {bin-funcsec :wasm.bin/funcsec
     bin-typesec :wasm.bin/typesec} :wasm/bin}]

  (-> ctx
      (update :wasm/wat
              (fn [wat]
                (assoc wat
                       :wasm.wat.codesec/offset
                       (or (get wat
                                :wasm.wat.func/idx)
                           0))))
      (resource (map (fn [typeidx]
                       (when (> typeidx
                                (count bin-typesec))
                         (throw (ex-info (str "Function type index out of bounds: "
                                              typeidx)
                                         {})))
                       (funcsign (get bin-typesec
                                      typeidx)))
                     bin-funcsec)
                'func
                :wasm.wat/func
                :wasm.wat.func/idx
                :wasm.wat.func/idx-resolve)))


;;;;;;;;;; Table section


(defn tablesec'

  ""
  
  [{:as                               ctx
    {bin-tablesec :wasm.bin/tablesec} :wasm/bin}]

  (resource ctx
            bin-tablesec
            'table
            :wasm.wat/table
            :wasm.wat.table/idx
            :wasm.wat.table/idx-resolve))


;;;;;;;;;; Memory section


(defn memsec'

  ""

  [{:as                           ctx
    {bin-memsec :wasm.bin/memsec} :wasm/bin}]

  (resource ctx
            bin-memsec
            'memory
            :wasm.wat/memory
            :wasm.wat.memory/idx
            :wasm.wat.memory/idx-resolve))


;;;;;;;;;; Global section


(defn globalsec'

  ""

  [{:as                                 ctx
    {bin-globalsec :wasm.bin/globalsec} :wasm/bin}]

  (resource ctx
            bin-globalsec
            'global
            :wasm.wat/global
            :wasm.wat.global/idx
            :wasm.wat.global/idx-resolve))


;;;;;;;;;; Start section


(defn startsec'

  ""

  [{:as                          ctx
    {funcidx :wasm.bin/startsec} :wasm/bin}]

  (cond->
    ctx
    funcidx
    (update :wasm/wat
            (fn [wat]
              (assoc wat
                     :wasm.wat/start
                     (get-in wat
                             [:wasm.wat.func/idx-resolve
                              funcidx]))))))


;;;;;;;;;; Element section


(defn elemsec'

  ""

  [{:as          ctx
    {bin-elemsec :wasm.bin/elemsec} :wasm/bin}]

  (cond->
    ctx
    (not-empty bin-elemsec)
    (update :wasm/wat
            (fn [{:as               wat
                  idx-resolve-func  :wasm.wat.func/idx-resolve
                  idx-resolve-table :wasm.wat.table/idx-resolve}]
              (update wat
                      :wasm/element
                      (fn [wat-element]
                        (reduce (fn [wat-element-2 [tableidx offset & funcidx+]]
                                  (let [table-id (or (get idx-resolve-table
                                                          tableidx)
                                                     (throw (ex-info (str "In element, table index out of bounds: "
                                                                          tableidx)
                                                                     {})))]
                                    (update wat-element-2
                                            table-id
                                            (fnil conj
                                                  [])
                                            (list* 'elem
                                                   table-id
                                                   offset
                                                   (mapv (fn [funcidx]
                                                           (or (get idx-resolve-func
                                                                    funcidx)
                                                               (throw (ex-info (str "In element, function index out of bound: "
                                                                                    funcidx)
                                                                               {}))))
                                                         funcidx+)))))
                               wat-element
                                bin-elemsec)))))))


;;;;;;;;;; Data section


(defn datasec'

  ""

  [{:as                             ctx
    {bin-datasec :wasm.bin/datasec} :wasm/bin}]

  (cond->
    ctx
    (not-empty bin-datasec)
    (update :wasm/wat
            (fn [{:as                   wat
                  :wasm.wat.memory/keys [idx-resolve]}]
              (update wat
                      :wasm/data
                      (fn [wat-datasec]
                        (reduce (fn [wat-datasec-2 [memidx & rs]]
                                  (let [mem-id (or (get idx-resolve
                                                        memidx)
                                                   (throw (ex-info (str "In data segment, memory index out of bounds: "
                                                                        memidx)
                                                                   {})))]
                                    (update wat-datasec-2
                                            mem-id
                                            (fnil conj
                                                  [])
                                            (list* 'data
                                                   mem-id
                                                   rs))))
                                wat-datasec
                                bin-datasec)))))))


;;;;;;;;;; Code section


(defn codesec'

  ""

  [{:as                             ctx
    {bin-codesec :wasm.bin/codesec} :wasm/bin}]

  (update ctx
          :wasm/wat
          (fn [{:as                    wat
                :wasm.wat.codesec/keys [offset]
                :wasm.wat.func/keys    [idx-resolve]}]
            (update wat
                    :wasm.wat/func
                    (fn [wat-funcsec]
                      (reduce (fn [wat-funcsec-2 [i view]]
                                (update wat-funcsec-2
                                        (get idx-resolve
                                             (+ offset
                                                i))
                                        (fn [func]
                                          (locals' func
                                                   view))))
                              wat-funcsec
                              (map vector
                                   (range)
                                   bin-codesec)))))))


(defn locals'

  ""

  [func view]

  (let [meta-        (meta func)
        id->idx      (meta- :wasm.local/id->idx)
        local-offset (count id->idx)]
    (loop [i-local   0
           id->idx-2 id->idx
           local+    []
           valtype+  (mapcat identity
                             (wasm.bin.read/vec' (fn [view]
                                                   (repeat (wasm.bin.read/u32' view)
                                                           (wasm.bin.read/valtype' view)))
                                                 view))]
      (if (seq valtype+)
        (let [local-id (symbol (str "$local-"
                                    i-local))]
          (recur (inc i-local)
                 (assoc id->idx-2
                        (+ local-offset
                           i-local)
                        local-id)
                 (conj local+
                       (list 'local
                             local-id
                             (first valtype+)))
                 (rest valtype+)))
        (with-meta (concat func
                           local+)
                   (assoc meta-
                          :wasm.local/id->idx
                          id->idx-2))))))
