;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.wat

  ""

  {:author "Adam Helinski"})


(declare conj-form
         transl
         transl+)


;;;;;;;;;;


(def ir-func
     {:wasm/target  'func
      :wasm/ident   '$sum
      :wasm/export+ [{:wasm/name   "sum"
                      :wasm/target 'export}]
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



(def ir-add
     {:wasm/target        'i32.add
      :wasm.i32.add/arg-0 {:wasm/target    'local.get
                           :wasm.local/get '$a}
      :wasm.i32.add/arg-1 {:wasm/target        'i32.add
                           :wasm.i32.add/arg-0 {:wasm/target    'local.get
                                                :wasm.local/get '$b}
                           :wasm.i32.add/arg-1 {:wasm/target   'i32.const
                                                :wasm.i32/const 42}}})



;;;;;;;;;; Instructions


(defn i32-add

  ""

  [ctx {:wasm.i32.add/keys [arg-0
                            arg-1]}]

  (let [ctx-2 (transl ctx
                      arg-0)
        ctx-3 (transl ctx-2
                      arg-1)]
    (assoc ctx-3
           :wasm/form
           (list 'i32.add
                 (ctx-2 :wasm/form)
                 (ctx-3 :wasm/form)))))



(defn i32-const

  ""

  [ctx {:wasm.i32/keys [const]}]

  (assoc ctx
         :wasm/form
         (list 'i32.const
               const)))



(defn local-get

  ""

  [ctx {ident :wasm.local/get}]

  (assoc ctx
         :wasm/form
         (list 'local.get
               ident)))


;;;;;;;;;;


(defn export

  ""

  [ctx {:wasm/keys [name]}]

  (assoc ctx
         :wasm/form
         (list 'export
               name)))


;;;;;;;;; Function types


(defn func

  ""

  [ctx {:wasm/keys [export+
                    ident
                    instr+
                    local+
                    param+
                    result+]}]

  (-> ctx
      (transl+ (concat export+
                       param+
                       result+
                       local+
                       instr+))
      (update :wasm/form
              (fn [form+]
                (list* 'func
                       (if ident
                         (cons ident
                               form+)
                         form+))))))



(defn local

  ""

  [ctx {:wasm/keys [name
                    type]}]

  (assoc ctx
         :wasm/form
         (if name
           (list 'local
                 name
                 type)
           (list 'local
                 type))))



(defn param

  ""

  [ctx {:wasm/keys [name
                    type]}]

  (assoc ctx
         :wasm/form
         (if name
           (list 'param
                 name
                 type)
           (list 'param
                 type))))



(defn result

  ""

  [ctx {:wasm/keys [type]}]

  (assoc ctx
         :wasm/form
         (list 'result
               type)))


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


(defn conj-form

  ""

  [ctx form]

  (update ctx
          :wasm/form
          conj
          form))


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


  [ctx ir+]

  (let [table (ctx :wasm.wat/table)]
    (reduce (fn [ctx-2 {:as        ir
                        :wasm/keys [target]}]
              (let [ctx-3 (if-some [f (get table
                                           target)]
                            (f ctx-2
                               ir)
                            (-throw-no-transl ctx-2
                                              target))]
                (update ctx-3
                        :wasm/form
                        #(conj (ctx-2 :wasm/form)
                               %))))
            (assoc ctx
                   :wasm/form
                   [])
            ir+)))
