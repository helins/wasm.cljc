;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.wat

  ""

  {:author "Adam Helinski"})


(declare transl
         transl+)


;;;;;;;;;;


(def fir
     {:wasm/target  'func
      :wasm/name    '$sum
      :wasm/export  {:wasm/name   "sum"
                     :wasm/target 'export}
      :wasm/param+  [{:wasm/ident  '$a
                      :wasm/target 'param
                      :wasm/type   'i32}
                     {:wasm/ident  '$b
                      :wasm/target 'param
                      :wasm/type   'i32}]
      :wasm/local+  [{:wasm/ident  '$c
                      :wasm/target 'local
                      :wasm/type   'i32}]
      :wasm/result+ [{:wasm/target 'result
                      :wasm/type   'i32}
                     #_{:wasm/type 'i64}]
      :wasm/instr+  [{:wasm/target        'i32.add
                      :wasm.i32.add/arg-0 {:wasm/target    'local.get
                                           :wasm.local/get '$a}
                      :wasm.i32.add/arg-1 {:wasm/target        'i32.add
                                           :wasm.i32.add/arg-0 {:wasm/target    'local.get
                                                                :wasm.local/get '$b}
                                           :wasm.i32.add/arg-1 {:wasm/target   'i32.const
                                                                :wasm.i32/const 42}}}]})


;;;;;;;;;; Instructions


(defn i32-add

  ""

  [ctx ir]

  (list* 'i32.add
         [(transl ctx
                  (:wasm.i32.add/arg-0 ir))
          (transl ctx
                  (:wasm.i32.add/arg-1 ir))]))



(defn i32-const

  ""

  [_ctx ir]

  (list 'i32.const
        (:wasm.i32/const ir)))



(defn local-get

  ""

  [_ctx ir]

  (list 'local.get
        (:wasm.local/get ir)))


;;;;;;;;;;


(defn export

  ""

  [{:wasm/keys [name]}]

  (list 'export
        name))


;;;;;;;;; Function types


(defn func

  ""

  [ctx ir]

  (list*
    (concat ['func]
            (when-some [n (:wasm/name ir)]
              [n])
            (transl+ ctx
                     'export
                     [(:wasm/export ir)])
            (transl+ ctx
                     'param
                     (:wasm/param+ ir))
            (transl+ ctx
                     'result
                     (:wasm/result+ ir))
            (transl+ ctx
                     'local
                     (:wasm/local+ ir))
            (transl+ ctx
                     (:wasm/instr+ ir)))))



(defn local

  ""

  [_ctx {:wasm/keys [name
                     type]}]

  (if name
    (list 'local
          name
          type)
    (list 'local
          type)))



(defn param

  ""

  [_ctx {:wasm/keys [name
                     type]}]

  (if name
    (list 'param
          name
          type)
    (list 'param
          type)))



(defn result

  ""

  [_ctx   {:wasm/keys [type]}]

  (list 'result
        type))


;;;;;;;;;;


(def ctx

  ""

  {:wasm.wat/table {'export    export
                    'func      func
                    'i32.add   i32-add
                    'i32.const i32-const
                    'local     local
                    'local.get local-get
                    'param     param
                    'result    result}})


;;;;;;;;;;


(defn- -throw-no-transl

  ;;

  [ctx target]

  (throw (ex-info (str "No function IR -> WAT found for target: "
                       target)
                  {:wasm/ctx ctx})))



(defn- -throw-target-mismatch

  ;;

  [ctx target target-ir]

  (throw (ex-info (str "Target mismatch: '"
                       target-ir
                       "' instead of '"
                       target
                       "'")
                  {:wasm/ctx ctx})))
;;;;;;;;;;


(defn transl

  ""

  [ctx {:as        ir
        :wasm/keys [target]}]

  (if-some [f (get-in ctx
                      [:wasm.wat/table
                       target])]
    (f ctx
       ir)
    (-throw-no-transl ctx
                      target)))



(defn transl+

  ""

  ([ctx ir+]

   (let [table (ctx :wasm.wat/table)]
     (map (fn [{:as        ir
                :wasm/keys [target]}]
            (if-some [f (get table
                             target)]
              (f ctx
                 ir)
              (-throw-no-transl ctx
                                target)))
          ir+)))


  ([ctx target ir+]

   (let [f (get-in ctx
                   [:wasm.wat/table
                    target])]
     (when-not f
       (-throw-no-transl ctx
                         target))
     (map (fn [{:as       ir
                target-ir :wasm/target}]
            (when (not= target-ir
                        target)
              (-throw-target-mismatch ctx
                                      target
                                      target-ir))
            (f ctx
               ir))
          ir+))))
