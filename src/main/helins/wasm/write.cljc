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


(defn magic'

  ""

  [ctx w]

  (binf/wr-b32 (at-least w
                         4)
               wasm.bin/magic)
  ctx)
               


(defn version'

  ""

  [ctx w]

  (binf/wr-b32 (at-least w
                         4)
               wasm.bin/version-1)
  ctx)


;;;;;;;;;;


(defn main

  ""

  [ctx]

  (let [w (writer)]
    (-> ctx
        (magic' w)
        (version' w))
    (let [view @w]
      (binf/view view
                 0
                 (binf/position view)))))


;;;;;;;;;;





(comment



  (binf/limit (main {}))



  )
