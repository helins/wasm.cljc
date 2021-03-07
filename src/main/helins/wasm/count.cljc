;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.count

  ""

  {:author "Adam Helinski"}

  (:require [helins.binf        :as binf]
            [helins.binf.buffer :as binf.buffer]
            [helins.binf.int64  :as binf.int64]
            [helins.binf.leb128 :as binf.leb128]
            [helins.wasm.bin    :as wasm.bin]
            [helins.wasm.ir     :as wasm.ir]))


;;;;;;;;;;


(defn u32

  [u32]

  (binf.leb128/n-byte-u32 u32))


;;;;;;;;;;


(def magic
     4)


(def version
     4)


;;;;;;;;;;


(def section-id
     1)



(defn section

  ""

  [n-byte]

  (+ section-id
     (u32 n-byte)
     n-byte))



(defn section+

  ""

  [{{:wasm.count/keys [typesec]} :wasm/write}]

  (or (some-> typesec
              section)
      0))
  

;;;;;;;;;; Types


(def valtype
     1)



(defn resulttype

  ""

  [valtype+]

  (let [n-valtype (count valtype+)]
    (+ (u32 n-valtype)
       (* n-valtype
          valtype))))



(defn functype

  ""

  [[param+ result+]]

  (+ 1 ;; 0x60 functype
     (resulttype param+)
     (resulttype result+)))


;;;;;;;;;;


(defn typesec

  [{:as        ctx
    :wasm/keys [typesec]}]

  (let [n (count typesec)]
    (if (pos? n)
      (loop [idx-real                    0
             idx-resolve                 {}
             n-byte                      0
             [[idx
               {:wasm/keys [signature]}]
              & typesec-2]               typesec]
        (let [idx-resolve-2 (assoc idx-resolve
                                   idx
                                   idx-real)
              n-byte-2      (+ n-byte
                               (functype signature))]
          (if typesec-2
            (recur (inc idx-real)
                   idx-resolve-2
                   n-byte-2
                   typesec-2)
            (update ctx
                    :wasm/write
                    #(assoc %
                            :wasm.count/typesec (+ (u32 n)
                                                   n-byte-2)
                            :wasm.symid/typesec idx-resolve-2)))))
      ctx)))


;;;;;;;;;;


(defn module

  ""

  [ctx]

  (let [ctx-2 (-> ctx
                  typesec
                  )]
    (assoc-in ctx-2
              [:wasm/write
               :wasm.count/module]
              (+ magic
                 version
                 (section+ ctx-2)))))
