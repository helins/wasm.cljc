;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.write

  ""

  {:author "Adam Helinski"}

  (:require [helins.binf        :as binf]
            [helins.binf.buffer :as binf.buffer]
            [helins.binf.int64  :as binf.int64]
            [helins.binf.leb128 :as binf.leb128]
            [helins.wasm.bin    :as wasm.bin]
            [helins.wasm.count  :as wasm.count]
            [helins.wasm.ir     :as wasm.ir]))


;;;;;;;;;


(defn writer

  ""

  []

  (volatile! (-> (binf.buffer/alloc (* 500
                                       1024))
                 binf/view
                 (binf/endian-set :little-endian))))



(defn at-least

  ""

  [v*view n-byte]

  (let [view @v*view]
    (if (>= (binf/remaining view)
            n-byte)
      view
      (let [n-byte-view   (binf/limit view)
            n-byte-min    (+ n-byte-view
                             n-byte)
            n-byte-view-2 (loop [n-byte-view-2 n-byte-view]
                          (let [n-byte-view-3 (Math/ceil (* n-byte-view-2
                                                            1.5))]
                            (if (>= n-byte-view-3
                                    n-byte-min)
                              n-byte-view-3
                              (recur n-byte-view-3))))]
        (vreset! v*view
                 (binf/grow view
                            (- n-byte-view-2
                               n-byte-view)))))))


;;;;;;;;;;


(defn u32

  [view u32]

  (binf.leb128/wr-u32 view
                      u32))


;;;;;;;;;;


(defn magic

  ""

  [ctx view]

  (binf/wr-b32 view
               wasm.bin/magic)
  ctx)
               


(defn version

  ""

  [ctx view]

  (binf/wr-b32 view
               wasm.bin/version-1)
  ctx)


;;;;;;;;;;


(defn n-byte

  ""

  [view n-byte]

  (u32 view
       n-byte))



(defn section-id

  [view section-id]

  (binf/wr-b8 view
              section-id))


;;;;;;;;;; Types


(defn valtype

  ""

  [ctx view vt]

  (binf/wr-b8 view
              vt)
  ctx)



(defn resulttype

  ""

  [ctx view valtype+]

  (binf.leb128/wr-u32 view
                      (count valtype+))
  (reduce #(valtype %1
                    view
                    %2)
          ctx
          valtype+))



(defn functype

  [ctx view [param+ result+]]

  (binf/wr-b8 view
              0x60)
  (-> ctx
      (resulttype view
                  param+)
      (resulttype view
                  result+)))


;;;;;;;;;; Type section


(defn typesec

  ""

  [{:as        ctx
    :wasm/keys [typesec]}
   view]

  (if (seq typesec)
    (do
      (-> view
          (section-id wasm.bin/section-id-type)
          (n-byte (get-in ctx
                          [:wasm/write
                           :wasm.count/typesec]))
          (u32 (count typesec)))
      (reduce #(functype %1
                         view
                         (%2 :wasm/signature))
              ctx
              (vals typesec)))
    ctx))


;;;;;;;;;;


(defn main

  ""

  [ctx]

  (let [view (-> (get-in ctx
                         [:wasm/write
                          :wasm.count/module])
                 binf.buffer/alloc
                 binf/view
                 (binf/endian-set :little-endian))]
    (-> ctx
        (magic view)
        (version view)
        (typesec view))
    view))


;;;;;;;;;;





(comment



  (binf/limit (main {}))



  )
