;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.wat

  ""

  {:author "Adam Helinski"})


(declare from-ir
         from-ir+)


;;;;;;;;; Function types


(defn local

  ""

  [ctx {:wasm/keys [ident
                    type]}]

  (assoc ctx
         :wasm/form
         (if ident
           (list 'local
                 ident
                 type)
           (list 'local
                 type))))



(defn param

  ""

  [ctx {:wasm/keys [ident
                    type]}]

  (assoc ctx
         :wasm/form
         (if ident
           (list 'param
                 ident
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


;;;;;;;;;; Instructions - Numeric


(defn i32-add

  ""

  [ctx {:wasm.i32.add/keys [arg-0
                            arg-1]}]

  (let [ctx-2 (from-ir ctx
                       arg-0)
        ctx-3 (from-ir ctx-2
                       arg-1)]
    (assoc ctx-3
           :wasm/form
           (list 'i32.add
                 (ctx-2 :wasm/form)
                 (ctx-3 :wasm/form)))))


;;;;;;;;;; Instructions - Variables


(defn local-get

  ""

  [ctx {ident :wasm.local/get}]

  (assoc ctx
         :wasm/form
         (list 'local.get
               ident)))


;;;;;;;;;; Module fields


(defn export

  ""

  [ctx {:wasm/keys [export]}]

  (assoc ctx
         :wasm/form
         (list 'export
               export)))


(defn func

  ""

  [ctx {:wasm/keys [export+
                    ident
                    instr+
                    local+
                    param+
                    result+]}]

  (-> ctx
      (from-ir+ (concat export+
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



(defn module

  ""

  [ctx {:wasm/keys [module-field+]}]

  (update (from-ir+ ctx
                    module-field+)
          :wasm/form
          #(list* 'module
                  %)))


;;;;;;;;;; Values


(defn i32-const

  ""

  [ctx {:wasm.i32/keys [const]}]

  (assoc ctx
         :wasm/form
         (list 'i32.const
               const)))


;;;;;;;;;;


(def ctx-default

  ""

  {:wasm.wat/table {'export    export
                    'func      func
                    'i32.add   i32-add
                    'i32.const i32-const
                    'local     local
                    'local.get local-get
                    'module    module
                    'param     param
                    'result    result}})


;;;;;;;;;;


(defn- -throw-no-transl

  ;;

  [ctx target]

  (throw (ex-info (str "No function IR -> WAT found for target: "
                       target)
                  {:wasm/ctx ctx})))


;;;;;;;;;;


(defn from-ir

  ""


  ([ir]

   (from-ir ctx-default
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



(defn from-ir+

  ""


  ([ir]

   (from-ir+ ctx-default
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
