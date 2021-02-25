;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.ir

  ""

  {:author "Adam Helinski"})


;;;;;;;;;; Miscellaneous


(defn ident

  ""

  [ir ident]

  (assoc ir
         :wasm/ident
         ident))



(defn instr+

  ""

  [ir instr+]

  (assoc ir
         :wasm/instr+
         instr+))


;;;;;;;;;; Instructions > Numeric > Constants


(defn i32-const

  ""

  [value]

  {:wasm/target    'i32.const
   :wasm.i32/const value})



(defn i64-const

  ""

  [value]

  {:wasm/target    'i64.const
   :wasm.i64/const value})



(defn f32-const

  ""

  [value]

  {:wasm/target    'f32.const
   :wasm.f32/const value})



(defn f64-const

  ""

  [value]

  {:wasm/target    'f64.const
   :wasm.f64/const value})


;;;;;;;;;; Instructions > Numeric > i32


(defn i32-add

  ""

  [arg-0 arg-1]

  {:wasm/target        'i32.add
   :wasm.i32.add/arg-0 arg-0
   :wasm.i32.add/arg-1 arg-1})


;;;;;;;;;; Instructions - Variables


(defn local-get

  ""

  [variable]

  {:wasm/target    'local.get
   :wasm.local/get variable})


;;;;;;;;;; Module fields


(defn export

  ""

  [string]

  {:wasm/export string
   :wasm/target 'export})



(defn export+

  ""

  [ir string+]

  (assoc ir
         :wasm/export+
         (mapv export
               string+)))


(defn func

  ""

  [ir]

  (assoc ir
         :wasm/target
         'func))



(defn module

  ""

  [module-field+]

  {:wasm/module-field+ module-field+
   :wasm/target        'module})


;;;;;;;;;; Types - Functions


(defn local

  ""

  ([type]

   {:wasm/target 'local
    :wasm/type   type})


  ([ident' type]

   (ident (local type)
          ident')))



(defn local+

  ""

  [ir local+]

  (assoc ir
         :wasm/local+
         (mapv #(if (vector? %)
                  (let [[ident
                         type] %]
                    (local ident
                           type))
                  (local %))
               local+)))



(defn param

  ""

  ([type]

   {:wasm/target 'param
    :wasm/type   type})


  ([ident' type]

   (ident (param type)
          ident')))



(defn param+

  ""

  [ir param+]

  (assoc ir
         :wasm/param+
         (mapv #(if (vector? %)
                  (let [[ident
                         type] %]
                    (param ident
                           type))
                  (param %))
               param+)))



(defn result

  ""

  [type]

  {:wasm/target 'result
   :wasm/type   type})



(defn result+

  ""

  [ir type+]

  (assoc ir
         :wasm/result+
         (mapv result
               type+)))
