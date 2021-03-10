;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.decompile

  ""

  {:author "Adam Helinski"}

  (:require [helins.binf      :as binf]
            [helins.wasm.bin  :as wasm.bin]
            [helins.wasm.read :as wasm.read]))


;;;;;;;;;


(def ctx-default

  ""

  {:wasm.customsec/bin wasm.read/custom-default
   :wasm/codesec       (sorted-map)
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


;;;;;;;;;; Start of a WASM file


(defn source->view

  ""

  [source]

  (-> source
      binf/view
      (binf/endian-set :little-endian)))


;;;;;;;;;; Find and parsing sections


(defn section-find+

  ""

  [ctx view]

  (assoc-in ctx
            [:wasm/source
             :wasm.source/section+]
            (loop [section+ []]
              (if (pos? (binf/remaining view))
                (recur (conj section+
                             (wasm.read/section' view)))
                section+))))



(defn section-read+

  ""

  [ctx view]

  (reduce (fn [ctx-2 {:wasm.section/keys [id
                                          n-byte
                                          start]}]
            (if (= id
                   wasm.bin/section-id-custom)
              (update ctx-2
                      :wasm.bin/customsec
                      (fnil conj
                            [])
                      (wasm.read/customsec' (binf/view view
                                                       start
                                                       n-byte)))
              ((condp =
                      id
                  wasm.bin/section-id-custom wasm.read/customsec'
                  wasm.bin/section-id-type   wasm.read/typesec'
                  wasm.bin/section-id-import wasm.read/importsec'
                  wasm.bin/section-id-func   wasm.read/funcsec'
                  wasm.bin/section-id-table  wasm.read/tablesec'
                  wasm.bin/section-id-mem    wasm.read/memsec'
                  wasm.bin/section-id-global wasm.read/globalsec'
                  wasm.bin/section-id-export wasm.read/exportsec'
                  wasm.bin/section-id-start  wasm.read/startsec'
                  wasm.bin/section-id-elem   wasm.read/elemsec'
                  wasm.bin/section-id-code   wasm.read/codesec'
                  wasm.bin/section-id-data   wasm.read/datasec'
                  ;(fn [ctx _view]
                  ;  ctx)
                  )
               ctx-2
               (binf/view view
                          start
                          n-byte))))
          ctx
          (get-in ctx
                  [:wasm/source
                   :wasm.source/section+])))


;;;;;;;;;;


(defn init

  ""

  [ctx view]

  (wasm.read/magic' view)
  (section-find+ (assoc ctx
                        :wasm/version
                        (wasm.read/version' view))
                 view))



(defn main

  ""

  ([source]

   (main ctx-default
         source))


  ([ctx source]

   (let [view (source->view source)]
     (-> (init ctx
               view)
         (section-read+ view)
         wasm.read/codesec'-2))))
