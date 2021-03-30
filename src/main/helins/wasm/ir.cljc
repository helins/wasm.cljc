;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.ir

  ""

  {:author "Adam Helinski"}

  (:require [helins.wasm.bin :as wasm.bin]
            [malli.core      :as malli]
            [malli.generator :as malli.gen]))


;;;;;;;;;; Private - Miscellaneous helpers


(defn -assoc-resource

  ""

  [ctx k-section k-idx externval]

  (let [idx (get ctx
                 k-idx)]
    (-> ctx
        (assoc-in [k-section
                   idx]
                  externval)
        (assoc k-idx
               (inc idx)))))


;;;;;;;;; Types / Limits


(defn limits'

  ""


  ([hmap limit-min]

   (assoc hmap
          :wasm.limit/min
          limit-min))


  ([hmap limit-min limit-max]

   (-> hmap
       (limits' limit-min)
       (assoc :wasm.limit/max
              limit-max))))


;;;;;;;;;; Types / Memory Types


(defn memtype'

  ""

  [limits]

  limits)


;;;;;;;;;; Types / Table Types


(defn tabletype'

  ""

  [limits reftype]

  (assoc limits
         :wasm/reftype
         reftype))


;;;;;;;;;; Types / Global Types


(defn globaltype'

  ""

  [hmap valtype mutable?]

  (assoc hmap
         :wasm/mutable? mutable?
         :wasm/valtype  valtype))


;;;;;;;;;; Modules / Type Section


(defn assoc-type

  ""

  [ctx type-signature]

  (-assoc-resource ctx
                   :wasm/typesec
                   :wasm/typeidx
                   type-signature))



(defn type-signature

  ""

  [hmap signature]

  (assoc hmap
         :wasm/signature
         signature))


;;;;;;;;;; Modules / Import Section


(defn import'

  ""

  [hmap buffer-module buffer-name]

  (assoc hmap
         :wasm.import/module buffer-module
         :wasm.import/name   buffer-name))


;;;;;;;;;; Modules / Function Section


(defn assoc-func

  ""

  [ctx func]

  (-assoc-resource ctx
                   :wasm/funcsec
                   :wasm/funcidx
                   func))



(defn func

  ""

  [hmap typeidx]

  (assoc hmap
         :wasm/typeidx
         typeidx))


;;;;;;;;;; Modules / Table Section


(defn assoc-table

  ""

  [ctx table]

  (-assoc-resource ctx
                   :wasm/tablesec
                   :wasm/tableidx
                   table))



(defn table'

  ""

  [limits reftype]

  (tabletype' limits
              reftype))


;;;;;;;;;; Modules / Memory Section


(defn assoc-mem

  ""

  [ctx mem]

  (-assoc-resource ctx
                   :wasm/memsec
                   :wasm/memidx
                   mem))


;;;;;;;;;; Modules / Global section


(defn assoc-global

  ""

  [ctx global]

  (-assoc-resource ctx
                   :wasm/globalsec
                   :wasm/globalidx
                   global))



(defn global'

  ""

  [hmap valtype mutable? expr]

  (-> hmap
      (globaltype' valtype
                   mutable?)
      (assoc :wasm/expr
             expr)))


;;;;;;;;;; Modules / Export Section


(defn export'

  ""

  [hmap buffer-name]

  (assoc hmap
         :wasm/name
         buffer-name))


;;;;;;;;;; Modules / Start Section


(defn startsec'

  ""

  [ctx start]

  (assoc ctx
         :wasm/startsec
         start))



(defn start'

  ""

  [hmap funcidx]

  (assoc hmap
         :wasm/funcidc
         funcidx))


;;;;;;;;;; Modules / Element Section


(defn elem'00

  ""

  [hmap expr-offset funcidx+]

  (assoc hmap
         :wasm/offset       expr-offset
         :wasm.elem/mode    :active
         :wasm.elem/resolve :idx
         :wasm.elem/vec     funcidx+))



(defn elem'01

  ""

  [hmap _elemkind funcidx+]

  (assoc hmap
         :wasm.elem/mode    :passive
         :wasm.elem/resolve :idx
         :wasm.elem/type    :func
         :wasm.elem/vec     funcidx+))



(defn elem'02

  ""

  [hmap tableidx expr-offset _elemkind funcidx+]

  (assoc hmap
         :wasm/offset       expr-offset
         :wasm/tableidx     tableidx
         :wasm.elem/mode    :active
         :wasm.elem/resolve :idx
         :wasm.elem/type    :func
         :wasm.elem/vec     funcidx+))



(defn elem'03

  ""

  [hmap _elemkind funcidx+]

  (assoc hmap
         :wasm.elem/mode    :declarative
         :wasm.elem/resolve :idx
         :wasm.elem/type    :func
         :wasm.elem/vec     funcidx+))



(defn elem'04

  ""

  [hmap expr-offset expr-elem+]

  (assoc hmap
         :wasm/offset       expr-offset
         :wasm.elem/mode    :active
         :wasm.elem/resolve :expr
         :wasm.elem/vec     expr-elem+))



(defn elem'05

  ""

  [hmap _reftype expr-elem+]

  (assoc hmap
         :wasm.elem/mode    :passive
         :wasm.elem/resolve :expr
         :wasm.elem/type    :func
         :wasm.elem/vec     expr-elem+))



(defn elem'06

  ""

  [hmap tableidx expr-offset _reftype expr-elem+]

  (assoc hmap
         :wasm/offset       expr-offset
         :wasm/tableidx     tableidx
         :wasm.elem/mode    :active
         :wasm.elem/resolve :expr
         :wasm.elem/type    :func
         :wasm.elem/vec     expr-elem+))



(defn elem'07

  ""

  [hmap _reftype expr-elem+]

  (assoc hmap
         :wasm.elem/mode    :declarative
         :wasm.elem/resolve :expr
         :wasm.elem/type    :func
         :wasm.elem/vec     expr-elem+))


;;;;;;;;;; Modules / Code Section


(defn func'

  ""

  [hmap local+ expr]

  (assoc hmap
         :wasm/expr   expr
         :wasm/local+ local+))


;;;;;;;;;; Modules / Data Section


(defn data'00

  ""

  [hmap expr buffer-data]

  (assoc hmap
         :wasm/data      buffer-data
         :wasm/expr      expr
         :wasm.data/mode :active))



(defn data'01

  ""

  [hmap buffer-data]

  (assoc hmap
         :wasm/data      buffer-data
         :wasm.data/mode :passive))



(defn data'02

  ""

  [hmap memidx expr buffer-data]

  (assoc hmap
         :wasm/data       buffer-data
         :wasm/expr       expr
         :wasm/memidx     memidx
         :wasm.data/mode :active))


;;;;;;;;;; Modules / Data Count Section


(defn datacountsec'

  ""

  [ctx n-data-seg]

  (assoc-in ctx
            [:wasm/datacountsec
             :wasm.data/n-seg]
            n-data-seg))


;;;;;;;;;; Modules / Modules


(defn version'

  ""

  [ctx]

  (assoc ctx
         :wasm/version
         1))


;;;;;;;;;; Schema


(def registry

  ""

  {;; Values / Byte

   :wasm/byte [:int
               {:max 255
                :min 0}]


   ;; Values / Integers

   :wasm/u32  [:int
               {:max 4294967295
                :min 0}]


   ;; Types / Number Types

   :wasm/numtype [:enum
                  wasm.bin/numtype-i32
                  wasm.bin/numtype-i64
                  wasm.bin/numtype-f32
                  wasm.bin/numtype-f64]


   ;; Types / Reference Type

   :wasm/reftype [:enum
                  wasm.bin/funcref
                  wasm.bin/externref]


   ;; Types / Value types

   :wasm/valtype [:or
                  :wasm/numtype
                  :wasm/reftype]


   ;; Types / Result Types

   :wasm/resulttype [:maybe
                     [:vector
                      {:min 1}
                      :wasm/valtype]]


   ;; Types / Function Types

   :wasm/functype [:tuple
                   :wasm/resulttype
                   :wasm/resulttype]


   ;; Modules / Indices

   :wasm/funcidx :wasm/idx

   :wasm/idx     :wasm/u32


   ;; Module / Type Section

   :wasm/typesec   [:map-of
                    :wasm/funcidx
                    :wasm/type]

   :wasm/type      [:map
                    :wasm/signature]

   :wasm/signature :wasm/functype
   })





(comment


  (malli/validate [:and
                   {:registry registry}
                   :wasm/funcidx]
                  420
                  )


  (malli.gen/sample [:and
                     {:registry registry}
                     :wasm/typesec])

  )
