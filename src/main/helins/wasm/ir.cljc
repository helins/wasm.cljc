;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.ir

  "Provides helpers for working with the intermediary representation (WASM binaries
   expressed as Clojure data structures).
  
   Directly relates to the [[helins.wasm.schema]] namespace.
  
   Once again, names ending with the ' character relates to \"non-terminal\" symbols
   from the WASM binary specification and can be found in the registry :as `:wasm/XXX`.

   For instance, [[globaltype']] can used for creating a map describing a global variable
   as defined by `:wasm/globaltype` in the registry.
  
   See README."

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

  "See [[limits']]."

  [limits]

  limits)


;;;;;;;;;; Types / Table Types


(defn tabletype'

  "See [[limits']]."

  [limits reftype]

  (assoc limits
         :wasm/reftype
         reftype))


;;;;;;;;;; Types / Global Types


(defn globaltype'

  [hmap valtype mutable?]

  (assoc hmap
         :wasm/mutable? mutable?
         :wasm/valtype  valtype))


;;;;;;;;;; Modules / Type Section


(defn assoc-type

  "Adds a [[type-signature]] to the typesec while updating the `:wasm/typeidx` index."

  [ctx type-signature]

  (-assoc-resource ctx
                   :wasm/typesec
                   :wasm/typeidx
                   type-signature))



(defn type-signature

  "Create a map describing a type signature for the typesec."

  [hmap signature]

  (assoc hmap
         :wasm/signature
         signature))


;;;;;;;;;; Modules / Import Section


(defn import'

  "Creates a generic map for imports.
  
   Used by other functions such as [[import-func]]."

  [hmap buffer-module buffer-name]

  (assoc hmap
         :wasm.import/module buffer-module
         :wasm.import/name   buffer-name))



(defn import-func

  "Adds in the importsec the given [[func]]."

  [ctx func]

  (-assoc-import ctx
                 :wasm.import/func
                 :wasm/funcidx
                 func))



(defn import-global

  "Adds in the importsec the given [[global']]."

  [ctx global]

  (-assoc-import ctx
                 :wasm.import/global
                 :wasm/globalidx
                 global))



(defn import-mem

  "Adds in the importsec the given [[mem']]."

  [ctx mem]

  (-assoc-import ctx
                 :wasm.import/mem
                 :wasm/memidx
                 mem))



(defn import-table

  "Adds in the importsec the given [[table']]."

  [ctx table]

  (-assoc-import ctx
                 :wasm.import/table
                 :wasm/tableidx
                 table))


;;;;;;;;;; Modules / Function Section


(defn assoc-func

  "Adds a [[func]] to the funcsec while updating the `:wasm/funcidx` index."

  [ctx func]

  (-assoc-resource ctx
                   :wasm/funcsec
                   :wasm/funcidx
                   func))



(defn func

  "Creates a map describing a function for the funcsec.
  
   See [assoc-func]."

  [hmap typeidx]

  (assoc hmap
         :wasm/typeidx
         typeidx))


;;;;;;;;;; Modules / Table Section


(defn assoc-table

  "Adds a [[table']] to the tablesec while updating the `:wasm/tableidx` index."

  [ctx table]

  (-assoc-resource ctx
                   :wasm/tablesec
                   :wasm/tableidx
                   table))



(defn table'

  [limits reftype]

  (tabletype' limits
              reftype))


;;;;;;;;;; Modules / Memory Section


(defn assoc-mem

  "Adds a [[mem']] to the memsec while udpating the `:wasm/memidx` index."

  [ctx mem]

  (-assoc-resource ctx
                   :wasm/memsec
                   :wasm/memidx
                   mem))



(defn mem'

  "See [[memtype']]."

  [limits]

  (memtype' limits))


;;;;;;;;;; Modules / Global section


(defn assoc-global

  "Adds a [[global']] to the globalsec while updating the `:wasm/globalidx` index."

  [ctx global]

  (-assoc-resource ctx
                   :wasm/globalsec
                   :wasm/globalidx
                   global))



(defn global'

  [hmap valtype mutable? expr]

  (-> hmap
      (globaltype' valtype
                   mutable?)
      (assoc :wasm/expr
             expr)))


;;;;;;;;;; Modules / Export Section


(defn export'

  [hmap buffer-name]

  (assoc hmap
         :wasm/name
         buffer-name))


;;;;;;;;;; Modules / Start Section


(defn startsec'

  [ctx start]

  (assoc ctx
         :wasm/startsec
         start))



(defn start'

  [hmap funcidx]

  (assoc hmap
         :wasm/funcidx
         funcidx))


;;;;;;;;;; Modules / Element Section


(defn assoc-elem

  "Adds an `elem'XX` to the elemsec while updating the `:wasm/elemidx` index.
  
   Those `elem'XX` functions from the namespace refer to the different elem types
   described in the WASM binary specification, each having an `XX` flag."

  [ctx elem]
  
  (-assoc-resource ctx
                   :wasm/elemsec
                   :wasm/elemidx
                   elem))



(defn elem'00

  "See [[assoc-elem]]."

  [hmap expr-offset funcidx+]

  (assoc hmap
         :wasm/offset       expr-offset
         :wasm.elem/mode    :active
         :wasm.elem/resolve :idx
         :wasm.elem/vec     funcidx+))



(defn elem'01

  "See [[assoc-elem]]."

  [hmap _elemkind funcidx+]

  (assoc hmap
         :wasm.elem/mode    :passive
         :wasm.elem/resolve :idx
         :wasm.elem/type    :func
         :wasm.elem/vec     funcidx+))



(defn elem'02

  "See [[assoc-elem]]."

  [hmap tableidx expr-offset elemkind funcidx+]

  (assoc hmap
         :wasm/elemkind     elemkind
         :wasm/offset       expr-offset
         :wasm/tableidx     tableidx
         :wasm.elem/mode    :active
         :wasm.elem/resolve :idx
         :wasm.elem/vec     funcidx+))



(defn elem'03

  "See [[assoc-elem]]."

  [hmap elemkind funcidx+]

  (assoc hmap
         :wasm/elemkind     elemkind
         :wasm.elem/mode    :declarative
         :wasm.elem/resolve :idx
         :wasm.elem/vec     funcidx+))



(defn elem'04

  "See [[assoc-elem]]."

  [hmap expr-offset expr-elem+]

  (assoc hmap
         :wasm/offset       expr-offset
         :wasm.elem/mode    :active
         :wasm.elem/resolve :expr
         :wasm.elem/vec     expr-elem+))



(defn elem'05

  "See [[assoc-elem]]."

  [hmap reftype expr-elem+]

  (assoc hmap
         :wasm/reftype      reftype
         :wasm.elem/mode    :passive
         :wasm.elem/resolve :expr
         :wasm.elem/vec     expr-elem+))



(defn elem'06

  "See [[assoc-elem]]."

  [hmap tableidx expr-offset reftype expr-elem+]

  (assoc hmap
         :wasm/offset       expr-offset
         :wasm/reftype      reftype
         :wasm/tableidx     tableidx
         :wasm.elem/mode    :active
         :wasm.elem/resolve :expr
         :wasm.elem/vec     expr-elem+))



(defn elem'07

  "See [[assoc-elem]]."

  [hmap reftype expr-elem+]

  (assoc hmap
         :wasm/reftype      reftype
         :wasm.elem/mode    :declarative
         :wasm.elem/resolve :expr
         :wasm.elem/vec     expr-elem+))


;;;;;;;;;; Modules / Code Section


(defn func'

  [hmap local+ expr]

  (assoc hmap
         :wasm/expr   expr
         :wasm/local+ local+))


;;;;;;;;;; Modules / Data Section


(defn assoc-data

  "Adds a `data'XX` to the datasec while updating the `:wasm/dataidx` index.
  
   Those `data'XX` functions from the namespace refer to the different elem types
   described in the WASM binary specification, each having an `XX` flag."

  [ctx data]
  
  (-assoc-resource ctx
                   :wasm/datasec
                   :wasm/dataidx
                   data))



(defn data'00

  "See [[assoc-data]]."

  [hmap expr buffer]

  (assoc hmap
         :wasm/buffer    buffer
         :wasm/expr      expr
         :wasm.data/mode :active))



(defn data'01

  "See [[assoc-data]]."

  [hmap buffer]

  (assoc hmap
         :wasm/buffer    buffer
         :wasm.data/mode :passive))



(defn data'02

  "See [[assoc-data]]."

  [hmap memidx expr buffer]

  (assoc hmap
         :wasm/buffer     buffer
         :wasm/expr       expr
         :wasm/memidx     memidx
         :wasm.data/mode :active))


;;;;;;;;;; Modules / Data Count Section


(defn datacountsec'

  [ctx n-data-seg]

  (assoc-in ctx
            [:wasm/datacountsec
             :wasm.data/n-seg]
            n-data-seg))


;;;;;;;;;; Modules / Modules


(defn version'

  "Adds a version to the `ctx`.
  
   For the time being, on version 1 exists."

  [ctx]

  (assoc ctx
         :wasm/version
         1))
