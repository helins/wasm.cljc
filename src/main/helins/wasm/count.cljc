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


(defn name'

  [buffer]

  (let [n-byte (count buffer)]
    (+ (u32 n-byte)
       n-byte)))


;;;;;;;;;;


(defn idx

  [idx]

  (u32 idx))


(def funcidx'

  ""

  idx)

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

  [{{:wasm.count/keys [funcsec
                       importsec
                       memsec
                       startsec
                       tablesec
                       typesec]} :wasm/write}]

  (reduce (fn [n-byte-total n-byte-section]
            (if n-byte-section
              (+ n-byte-total
                 (section' n-byte-section))
              n-byte-total))
          0
          [funcsec
           memsec
           importsec
           startsec
           tablesec
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

  [flatidx-type {:wasm/keys [typeidx]}]

  (typeidx' (flatidx-type typeidx)))
  


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


;;;;;;;;;; Sections / Helpers


(defn section-externval

  ""

  [ctx k-section k-flatidx k-count count-item]

  (if-some [sec (not-empty (ctx k-section))]
    (update ctx
            :wasm/write
            (fn [ctx-write]
              (as-> ctx-write
                    ctx-write-2
                (assoc ctx-write-2
                       k-count
                       0)
                (reduce-kv (fn [ctx-write-3 idx item]
                             (-> ctx-write-3
                                 (update k-flatidx
                                         #(assoc %
                                                 idx
                                                 (count %)))
                                 (update k-count
                                         #(+ %
                                             (count-item item)))))
                           ctx-write-2
                           sec)
                (update ctx-write-2
                        k-count
                        #(+ %
                            (u32 (count sec)))))))
    ctx))


;;;;;;;;;; Sections / Func


(defn funcsec'

  ""

  [ctx]

  (section-externval ctx
                     :wasm/funcsec
                     :wasm.flatidx/func
                     :wasm.count/funcsec
                     (partial func
                              (get-in ctx
                                      [:wasm/write
                                       :wasm.flatidx/type]))))


;;;;;;;;;; Sections / Import


(defn importdesc

  ""

  [ctx-write space k-flatidx f-item]

  (if (seq space)
    (-> (reduce-kv (fn [ctx-write-2 idx hmap]
                     (-> ctx-write-2
                         (update k-flatidx
                                 (fn [flatidx]
                                   (assoc flatidx
                                          idx
                                          (count flatidx))))
                         (update :wasm.count/importsec
                                 #(+ %
                                     (name' (hmap :wasm.import/module))
                                     (name' (hmap :wasm.import/name))
                                     1  ;; byte specifying importdesc type
                                     (f-item hmap)))))
                   ctx-write
                   space)
        (update :wasm.import/n
                #(+ %
                    (count space))))
    ctx-write))



(defn importsec'

  [{:as        ctx
    :wasm/keys [importsec]}]

  (let [ctx-write    (ctx :wasm/write)
        ctx-write-2  (-> ctx-write
                         (assoc :wasm.count/importsec 0
                                :wasm.import/n        0)
                         (importdesc (importsec :wasm.import/func)
                                     :wasm.flatidx/func
                                     (partial func
                                              (ctx-write :wasm.flatidx/type)))
                         (importdesc (importsec :wasm.import/global)
                                     :wasm.flatidx/global
                                     globaltype')
                         (importdesc (importsec :wasm.import/mem)
                                     :wasm.flatidx/mem
                                     memtype')
                         (importdesc (importsec :wasm.import/table)
                                     :wasm.flatidx/table
                                     tabletype'))]
    (assoc ctx
           :wasm/write
           (update ctx-write-2
                   :wasm.count/importsec
                   (fn [n-byte]
                     (if (zero? n-byte)
                       0
                       (+ (u32 (ctx-write-2 :wasm.import/n))
                          n-byte)))))))


;;;;;;;;;; Sections / Mem


(defn memsec'

  [ctx]

  (section-externval ctx
                     :wasm/memsec
                     :wasm.flatidx/tablesec
                     :wasm.count/memsec
                     memtype'))


;;;;;;;;;; Sections / Start


(defn startsec'

  [{:as        ctx
    :wasm/keys [startsec]}]

  (if startsec
    (update ctx
            :wasm/write
            (fn [ctx-write]
              (assoc ctx-write
                     :wasm.count/startsec
                     (funcidx' (get-in ctx-write
                                       [:wasm.flatidx/func
                                        (startsec :wasm/funcidx)])))))
    ctx))


;;;;;;;;;; Sections / Table


(defn tablesec'

  [ctx]

  (section-externval ctx
                     :wasm/tablesec
                     :wasm.flatidx/table
                     :wasm.count/tablesec
                     tabletype'))


;;;;;;;;;; Sections / Type


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
                            :wasm.flatidx/type  idx-resolve-2)))))
      ctx)))


;;;;;;;;;;


(defn module

  ""

  [ctx]

  (let [ctx-2 (-> ctx
                  typesec
                  importsec'
                  funcsec'
                  tablesec'
                  memsec'
                  startsec'
                  )]
    (assoc-in ctx-2
              [:wasm/write
               :wasm.count/module]
              (+ magic
                 version
                 (section+ ctx-2)))))
