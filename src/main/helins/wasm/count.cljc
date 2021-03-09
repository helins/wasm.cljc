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
            [helins.binf.string :as binf.string]
            [helins.wasm.bin    :as wasm.bin]
            [helins.wasm.ir     :as wasm.ir]))


;;;;;;;;;;


(defn u32

  [u32]

  (binf.leb128/n-byte-u32 u32))


;;;;;;;;;;


(defn idx

  [idx]

  (u32 idx))



(def typeidx'
  
  ""

  idx)


;;;;;;;;;;


(def magic
     4)


(def version
     4)


;;;;;;;;;;


(def section-id
     1)



(defn section'

  ""

  [n-byte]

  (if n-byte
    (+ section-id
       (u32 n-byte)
       n-byte)
    0))



(defn section+

  ""

  [{{:wasm.count/keys [importsec
                       typesec]} :wasm/write}]

  (reduce (fn [n-byte section]
            (+ n-byte
               (section' section)))
          0
          [importsec
           typesec]))
  

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



(defn func

  ""

  [{:wasm/keys [typeidx]}]
  
  (typeidx' typeidx))



(def elemtype'
     1)



(defn globaltype'

  [_global]

  2)



(defn limits

  ""

  [{min- :wasm.limit/min
    max- :wasm.limit/max}]

  (let [n-byte (+ 1             ;; Byte specifying if there is a maximum or not
                  (u32 min-))]
    (if max-
      (+ n-byte
         (u32 max-))
      n-byte)))



(defn memtype'

  ""

  [mem]

  (limits mem))



(defn tabletype'

  ""

  [table]

  (+ elemtype'
     (limits table)))


;;;;;;;;;; Import section


; (defn importdesc
; 
;   [ctx [k-section idx :as externval]]
; 
;   (let [f (case k-section
;             :wasm/funcsec   importdesc-func
;             :wasm/globalsec importdesc-global
;             :wasm/memsec    importdesc-mem
;             :wasm/tablesec  importdesc-table)]
;     (f (get-in ctx
;                externval)
;        )))



(defn importsec'

  [{:as        ctx
    :wasm/keys [importsec]}]


  (if (seq importsec)
    (with-local-vars [n-byte 0]
      (let [import+ (reduce-kv (fn [import+ str-module str-name->importdesc]
                                 (let [buffer-module     (binf.string/encode str-module)
                                       n-byte-module     (count buffer-module)
                                       n-byte-vec-module (+ (u32 n-byte-module)
                                                            n-byte-module)]
                                   (reduce-kv (fn [import-2+ str-name [k-section :as path-externval]]
                                                (let [buffer-name (binf.string/encode str-name)
                                                      externval   (get-in ctx
                                                                          path-externval)
                                                      n-byte-name (count buffer-name)]
                                                  (var-set n-byte
                                                           (+ @n-byte
                                                              n-byte-vec-module
                                                              (+ (u32 n-byte-name)
                                                                 n-byte-name)
                                                              1                   ;; importdesc type
                                                              (let [f (case k-section
                                                                        :wasm/funcsec   func
                                                                        :wasm/globalsec globaltype'
                                                                        :wasm/memsec    memtype'
                                                                        :wasm/tablesec  tabletype')]
                                                                (f externval))))
                                                  (conj import-2+
                                                        [buffer-module
                                                         buffer-name
                                                         externval])))
                                              import+
                                              str-name->importdesc)))
                               []
                               importsec)]
        (update ctx
                :wasm/write
                #(assoc %
                        :wasm.count/importsec (+ (u32 (count import+))
                                                 @n-byte)
                        :wasm.write/importsec import+))))
    ctx))


;;;;;;;;;; Type section


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
