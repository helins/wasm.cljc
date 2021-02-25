;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.wat

  ""

  {:author "Adam Helinski"})


(declare transl
         transl+)


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

  [ctx {:wasm/keys [export]}]

  (assoc ctx
         :wasm/form
         (list 'export
               export)))


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


(def ctx-default

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


  ([ir]

   (transl ctx-default
           ir))


  ([ctx {:as        ir
         :wasm/keys [target]}]
 
   (if-some [f (get-in ctx
                       [:wasm.wat/table
                        target])]
     (f ctx
        ir)
     (-throw-no-transl ctx
                       target))))



(defn transl+

  ""


  ([ir]

   (transl+ ctx-default
            ir))


  ([ctx ir+]

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
             ir+))))
