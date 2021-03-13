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
  #?(:clj (:import (java.io File
                            RandomAccessFile)
                   java.nio.channels.FileChannel$MapMode))
  (:refer-clojure :exclude [compile]))


(declare prepare-view)


;;;;;;;;;;


(def ctx

  ""

  {:wasm/codesec       (sorted-map)
   :wasm/dataidx       0
   :wasm/exportsec     {:wasm.export/func   (sorted-map)
                        :wasm.export/global (sorted-map)
                        :wasm.export/mem    (sorted-map)
                        :wasm.export/table  (sorted-map)}
   :wasm/elemidx       0
   :wasm/elemsec       (sorted-map)
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


;;;;;;;;;; BinF views containing the source of a WASM module


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


;;;;;;;;;; Compilation


(defn- -compile

  ;;

  [view ctx]

  (wasm.write/module' (prepare-view view)
                      ctx))



(defn compile

  ""

  [ctx]

  (let [ctx-2 (wasm.count/module' ctx)]
    (-compile (-> (get-in ctx-2
                          [:wasm/write
                           :wasm.count/module])
                  binf.buffer/alloc
                  binf/view)
              ctx-2)))



#?(:clj (defn compile-file

  ""

  [ctx ^String path]

  (let [ctx-2  (wasm.count/module' ctx)
        n-byte (get-in ctx-2
                      [:wasm/write
                        :wasm.count/module])
        file   (File. path)]
    (.createNewFile file)
    (with-open [raf (RandomAccessFile. file
                                       "rw")]
      (let [channel (.getChannel raf)
            mmap    (.map channel
                          FileChannel$MapMode/READ_WRITE
                          0
                          n-byte)]
        (.setLength raf
                    n-byte)
        (-compile mmap
                  ctx-2)
        (.force mmap))))
   ctx))


;;;;;;;;;; Decompilation


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



#?(:clj (defn decompile-file

  ""


  ([path]

   (decompile-file ctx
                   path))


  ([ctx ^String path]

   (with-open [raf (RandomAccessFile. path
                                      "r")]
     (let [channel (.getChannel raf)]
       (decompile ctx
                  (-> (.map channel
                            FileChannel$MapMode/READ_ONLY
                            0
                            (.size channel))
                      .load
                      prepare-view)))))))
