;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.ir

  ""

  {:author "Adam Helinski"})


;;;;;;;;;; Private - Miscellaneous helpers

(defn -assoc-import

  ;;

  [ctx k-import-type k-idx hmap]

  (-> ctx
      (assoc-in [:wasm/importsec
                k-import-type
                (ctx k-idx)]
                hmap)
      (update k-idx
              inc)))
  


(defn ^:no-doc -assoc-resource

  ;;

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



(defn import-func

  ""

  [ctx func]

  (-assoc-import ctx
                 :wasm.import/func
                 :wasm/funcidx
                 func))



(defn import-global

  ""

  [ctx global]

  (-assoc-import ctx
                 :wasm.import/global
                 :wasm/globalidx
                 global))



(defn import-mem

  ""

  [ctx mem]

  (-assoc-import ctx
                 :wasm.import/mem
                 :wasm/memidx
                 mem))



(defn import-table

  ""

  [ctx table]

  (-assoc-import ctx
                 :wasm.import/table
                 :wasm/tableidx
                 table))


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
         :wasm/funcidx
         funcidx))


;;;;;;;;;; Modules / Element Section


(defn assoc-elem

  ""

  [ctx elem]
  
  (-assoc-resource ctx
                   :wasm/elemsec
                   :wasm/elemidx
                   elem))



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

  [hmap tableidx expr-offset elemkind funcidx+]

  (assoc hmap
         :wasm/elemkind     elemkind
         :wasm/offset       expr-offset
         :wasm/tableidx     tableidx
         :wasm.elem/mode    :active
         :wasm.elem/resolve :idx
         :wasm.elem/vec     funcidx+))



(defn elem'03

  ""

  [hmap elemkind funcidx+]

  (assoc hmap
         :wasm/elemkind     elemkind
         :wasm.elem/mode    :declarative
         :wasm.elem/resolve :idx
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

  [hmap reftype expr-elem+]

  (assoc hmap
         :wasm/reftype      reftype
         :wasm.elem/mode    :passive
         :wasm.elem/resolve :expr
         :wasm.elem/vec     expr-elem+))



(defn elem'06

  ""

  [hmap tableidx expr-offset reftype expr-elem+]

  (assoc hmap
         :wasm/offset       expr-offset
         :wasm/reftype      reftype
         :wasm/tableidx     tableidx
         :wasm.elem/mode    :active
         :wasm.elem/resolve :expr
         :wasm.elem/vec     expr-elem+))



(defn elem'07

  ""

  [hmap reftype expr-elem+]

  (assoc hmap
         :wasm/reftype      reftype
         :wasm.elem/mode    :declarative
         :wasm.elem/resolve :expr
         :wasm.elem/vec     expr-elem+))


;;;;;;;;;; Modules / Code Section


(defn func'

  ""

  [hmap local+ expr]

  (assoc hmap
         :wasm/expr   expr
         :wasm/local+ local+))


;;;;;;;;;; Modules / Data Section


(defn assoc-data

  ""

  [ctx data]
  
  (-assoc-resource ctx
                   :wasm/datasec
                   :wasm/dataidx
                   data))



(defn data'00

  ""

  [hmap expr buffer]

  (assoc hmap
         :wasm/buffer    buffer
         :wasm/expr      expr
         :wasm.data/mode :active))



(defn data'01

  ""

  [hmap buffer]

  (assoc hmap
         :wasm/buffer    buffer
         :wasm.data/mode :passive))



(defn data'02

  ""

  [hmap memidx expr buffer]

  (assoc hmap
         :wasm/buffer     buffer
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
