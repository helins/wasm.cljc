;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.decompile

  ""

  {:author "Adam Helinski"}

  (:require [helins.binf          :as binf]
            [helins.wasm.bin      :as wasm.bin]
            [helins.wasm.bin.read :as wasm.bin.read]
            [helins.wasm.bin.wat  :as wasm.bin.wat]))

;;;;;;;;;


(def ctx-default

  ""

  {
   :wasm/funcidx   0
   :wasm/globalidx 0
   :wasm/memoryidx 0
   :wasm/tableidx  0
   :wasm/funcsec   (sorted-map)
   :wasm/globalsec (sorted-map)
   :wasm/memorysec (sorted-map)
   :wasm/tablesec  (sorted-map)
   :wasm/typesec   []
   }
  )


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
                             (wasm.bin.read/section' view)))
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
                      (wasm.bin.read/customsec' (binf/view view
                                                           start
                                                           n-byte)))
              (if-some [f (condp =
                                 id
                             wasm.bin/section-id-type      wasm.bin.read/typesec'
                             wasm.bin/section-id-import    wasm.bin.read/importsec'
                             wasm.bin/section-id-function  wasm.bin.read/funcsec'
                            ; wasm.bin/section-id-table    [:wasm.bin/tablesec
                            ;                               wasm.bin.read/tablesec']
                            ; wasm.bin/section-id-memory   [:wasm.bin/memsec
                            ;                               wasm.bin.read/memsec']
                            ; wasm.bin/section-id-global   [:wasm.bin/globalsec
                            ;                               wasm.bin.read/globalsec']
                            ; wasm.bin/section-id-export   [:wasm.bin/exportsec
                            ;                               wasm.bin.read/exportsec']
                            ; wasm.bin/section-id-start    [:wasm.bin/startsec
                            ;                               wasm.bin.read/startsec']
                            ; wasm.bin/section-id-element  [:wasm.bin/elemsec
                            ;                               wasm.bin.read/elemsec']
                            ; wasm.bin/section-id-code     [:wasm.bin/codesec
                            ;                               wasm.bin.read/codesec']
                            ; wasm.bin/section-id-data     [:wasm.bin/datasec
                            ;                               wasm.bin.read/datasec']
                             nil)]
                (f ctx-2
                   (binf/view view
                              start
                              n-byte))
                ctx-2)))
          ctx
          (get-in ctx
                  [:wasm/source
                   :wasm.source/section+])))



(defn to-wat

  ""

  [ctx]

  (-> ctx
      wasm.bin.wat/importsec'
      wasm.bin.wat/exportsec'
      wasm.bin.wat/funcsec'
      wasm.bin.wat/tablesec'
      wasm.bin.wat/memsec'
      wasm.bin.wat/globalsec'
      wasm.bin.wat/elemsec'
      wasm.bin.wat/startsec'
      wasm.bin.wat/datasec'
      wasm.bin.wat/codesec'))


;;;;;;;;;;


(defn codesec

  ""

  [view+]
  
  (mapv wasm.bin.read/func'
        view+))


;;;;;;;;;;


(defn init

  ""

  [ctx view]

  (wasm.bin.read/magic' view)
  (section-find+ (assoc ctx
                        :wasm/version
                        (wasm.bin.read/version' view))
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
         (section-read+ view)))))
