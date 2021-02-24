;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.wat

  ""

  {:author "Adam Helinski"})



(declare op
         op+)


;;;;;;;;;;


(def fir
     {:wasm/ir      :func
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


;;;;;;;;;; Variables and miscellaneous


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



(defn result

  ""

  [{:wasm/keys [type]}]

  (list 'result
        type))


;;;;;;;;;; Op code


(defn i32-add

  ""

  [op-map ir]

  (list* 'i32.add
         [(op op-map
              (:wasm.i32.add/arg-0 ir))
          (op op-map
              (:wasm.i32.add/arg-1 ir))]))



(defn i32-const

  ""

  [_op-map ir]

  (list 'i32.const
        (:wasm.i32/const ir)))



(defn local-get

  ""

  [_op-map ir]

  (list 'local.get
        (:wasm.local/get ir)))


;;;;;


(def op-map

  ""

  {'i32.add   i32-add
   'i32.const i32-const
   'local.get local-get})



(defn op

  ""

  [op-map {:as        ir
           :wasm/keys [op]}]

  ((op-map op) op-map
               ir))



(defn op+

  ""

  [op-map ir+]

  (map (fn pass-op-map [ir]
         (op op-map
             ir))
       ir+))


;;;;;;;;;;


(defn export

  ""

  [string-name]

  (list 'export
        string-name))


;;;;;;;;;


(defn func

  ""

  [op-map ir]

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
            (op+ op-map
                 (:wasm/op+ ir)))))
