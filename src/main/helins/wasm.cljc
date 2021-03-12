;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm

  ""

  {:author "Adam Helinski"}

  (:require [helins.binf        :as binf]
            [helins.binf.buffer :as binf.buffer]
            [helins.wasm.count  :as wasm.count]
            [helins.wasm.read   :as wasm.read]
            [helins.wasm.write  :as wasm.write])
  (:refer-clojure :exclude [compile]))


(declare prepare-view)


;;;;;;;;;;


(def ctx

  ""

  {:wasm/codesec       (sorted-map)
   :wasm/exportsec     {:wasm.export/func   (sorted-map)
                        :wasm.export/global (sorted-map)
                        :wasm.export/mem    (sorted-map)
                        :wasm.export/table  (sorted-map)}
   :wasm/funcidx       0
   :wasm/funcsec       (sorted-map)
   :wasm/importsec     {:wasm.import/func   (sorted-map)
                        :wasm.import/global (sorted-map)
                        :wasm.import/mem    (sorted-map)
                        :wasm.import/table  (sorted-map)}
   :wasm/globalidx     0
   :wasm/globalsec     (sorted-map)
   :wasm/memidx        0
   :wasm/memsec        (sorted-map)
   :wasm/tableidx      0
   :wasm/tablesec      (sorted-map)
   :wasm/typeidx       0
   :wasm/typesec       (sorted-map)})


;;;;;;;;;;


(defn buffer->view

  ""

  [buffer]

  (-> buffer
      binf/view
      prepare-view))



(defn prepare-view

  ""

  [view]

  (binf/endian-set view
                   :little-endian))


;;;;;;;;;;


(defn compile

  ""

  [ctx]

  (let [ctx-2 (wasm.count/module' ctx)]
    (-> (get-in ctx-2
                [:wasm/write
                 :wasm.count/module])
        binf.buffer/alloc
        binf/view
        prepare-view
        (wasm.write/module' ctx-2))))



(defn decompile

  ""


  ([view]

   (decompile ctx
              view))


  ([ctx view]

   (-> ctx
       (wasm.read/module' view)
       wasm.read/section'+
       wasm.read/codesec'2
       (dissoc :wasm/source))))
