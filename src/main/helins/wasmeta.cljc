;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasmeta

  ""

  {:author "Adam Helinski"})


(declare op
         op+)


;;;;;;;;;;


(defn splice

  ""


  ([coll]

   (splice coll
           []))


  ([coll acc]

   (reduce (fn [acc-2 item]
             (if (list? item)
               (splice item
                       acc-2)
               (conj acc-2
                     item)))
           acc
           coll)))


;;;;;;;;;;


(def f
     {:wasm/syntax  :func
      :wasm/name    '$sum
      :wasm/export  "sum"
      :wasm/param+  [{:wasm/name '$a
                      :wasm/type 'i32}
                     {:wasm/name '$b
                      :wasm/type 'i32}]
      :wasm/local+  [{:wasm/name '$c
                      :wasm/type 'i32}]
      :wasm/result+ [{:wasm/type 'i32}
                     #_{:wasm/type 'i64}]
      :wasm/op+     [{:wasm/op            'i32.add
                      :wasm.i32.add/arg-0 {:wasm/op        'local.get
                                           :wasm.local/get '$a}
                      :wasm.i32.add/arg-1 {:wasm/op            'i32.add
                                           :wasm.i32.add/arg-0 {:wasm/op        'local.get
                                                                :wasm.local/get '$b}
                                           :wasm.i32.add/arg-1 {:wasm/op        'i32.const
                                                                :wasm.i32/const 42}}}]})


(defn param

  ""

  [{:wasm/keys [name
                type]}]

  (if name
    (list 'param
          name
          type)
    (list 'param
          type)))



(defn local

  ""

  [{:wasm/keys [name
                type]}]

  (if name
    (list 'local
          name
          type)
    (list 'local
          type)))



(defn export

  ""

  [string-name]

  (list 'export
        string-name))



(defn result

  ""

  [{:wasm/keys [type]}]

  (list 'result
        type))



(defn i32-add

  ""

  [ir]

  (list* 'i32.add
         [(op (:wasm.i32.add/arg-0 ir))
          (op (:wasm.i32.add/arg-1 ir))]))



(defn i32-const

  ""

  [ir]

  (list 'i32.const
        (:wasm.i32/const ir)))



(defn local-get

  ""

  [ir]

  (list 'local.get
        (:wasm.local/get ir)))



(def op-map

  ""

  {'i32.add   i32-add
   'i32.const i32-const
   'local.get local-get})



(defn op

  ""

  [{:as        ir
    :wasm/keys [op]}]

  ((op-map op) ir))




(defn op+

  ""

  [ir+]

  (map op
       ir+))






(defn func

  ""

  [ir]

  (list*
    (concat ['func]
            (when-some [n (:wasm/name ir)]
              [n])
            (when-some [e (:wasm/export ir)]
              [(export e)])
            (map param
                 (:wasm/param+ ir))
            (map result
                 (:wasm/result+ ir))
            (map local
                 (:wasm/local+ ir))
            (op+ (:wasm/op+ ir)))))
