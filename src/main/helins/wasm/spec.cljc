;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.spec

  ""

  {:author "Adam Helinski"}
  
  (:require [clojure.spec.alpha     :as s]
            [clojure.spec.gen.alpha :as sgen]
            [helins.binf.string     :as binf.string]
            [helins.wasm.bin        :as wasm.bin]
            [helins.wasm.decompile  :as wasm.decompile]
            [malli.core             :as malli]
            [malli.util]))


;;;;;;;;;; Miscellaneous


(s/def :wasm/mutable?
       boolean?)


(s/def :wasm.externval/key
       #{:wasm/funcsec
         :wasm/globalsec
         :wasm/memsec
         :wasm/tablesec})


(s/def :wasm.externval/path
       (s/tuple :wasm.externval/key
                :wasm/idx))


;;;;;;;;;; Values


(s/def :wasm/u32
       (s/int-in  0
                  4294967295))



(s/def :wasm/name
       (s/with-gen #?(:clj  #(instance? (class (byte-array 0))
                                        %)
                      :cljs #(or (instance? js/ArrayBuffer
                                            %)
                                 (when (exists? js/SharedArrayBuffer)
                                   (instance? js/SharedArrayBuffer
                                              %))))
                   #(sgen/fmap binf.string/encode
                               (s/gen string?))))


;;;;;;;;;; Indices


(s/def :wasm/idx
       :wasm/u32)


(s/def :wasm/funcidx
       :wasm/idx)


(s/def :wasm/globalidx
       :wasm/idx)


(s/def :wasm/memidx
       :wasm/idx)


(s/def :wasm/tableidx
       :wasm/idx)


(s/def :wasm/typeidx
       :wasm/idx)


;;;;;;;;;; Types


(s/def :wasm/valtype
       #{wasm.bin/valtype-i32
         wasm.bin/valtype-i64
         wasm.bin/valtype-f32
         wasm.bin/valtype-f64})



(s/def :wasm/resulttype
       (s/or :nil      nil?
             :valtype+ (s/coll-of :wasm/valtype
                                  :kind       vector?
                                  :min-count  1)))



(s/def :wasm.limit/min
       :wasm/u32)



(s/def :wasm.limit/max
       :wasm/u32)



(s/def :wasm/limits
       (s/and (s/keys :req [:wasm.limit/min]
                      :opt [:wasm.limit/max])
              #(if-some [max- (% :wasm.limit/max)]
                 (>= max-
                     (% :wasm.limit/min))
                 true)))
     

(s/def :wasm/elemtype
       #{wasm.bin/elemtype})


;;;;;;;;;; Base external types


(s/def :wasm.externval/func
       (s/keys :req [:wasm/typeidx]))



(s/def :wasm.externval/global
       (s/keys :req [:wasm/mutable?
                     :wasm/valtype]))



(s/def :wasm.externval/mem
       :wasm/limits)



(s/def :wasm.externval/table
       (s/merge :wasm/limits
                (s/keys :req [:wasm/elemtype])))
              

;;;;;;;;;; Sections / Import


(s/def :wasm.import/module
       :wasm/name)



(s/def :wasm.import/name
       :wasm/name)



(s/def :wasm/import
       (s/keys :req [:wasm.import/module
                     :wasm.import/name]))



(s/def :wasm.import.item/func
       (s/merge :wasm/import
                :wasm.externval/func))



(s/def :wasm.import/func
       (s/map-of :wasm/funcidx
                 :wasm.import.item/func))
       


(s/def :wasm.import.item/global
       (s/merge :wasm/import
                :wasm.externval/global))



(s/def :wasm.import/global
       (s/map-of :wasm/globalidx
                 :wasm.import.item/global))



(s/def :wasm.import.item/mem
       (s/merge :wasm/import
                :wasm.externval/mem))



(s/def :wasm.import/mem
       (s/map-of :wasm/memidx
                 :wasm.import.item/mem))



(s/def :wasm.import.item/table
       (s/merge :wasm/import
                :wasm.externval/table))



(s/def :wasm.import/table
       (s/map-of :wasm/tableidx
                 :wasm.import.item/table))



(s/def :wasm/importsec
       (s/keys :opt [:wasm.import/func
                     :wasm.import/global
                     :wasm.import/mem
                     :wasm.import/table]))


;;;;;;;;;; Sections / Type


(s/def :wasm/signature
       (s/tuple :wasm/resulttype
                :wasm/resulttype))



(s/def :wasm.typesec/item
       (s/keys :req [:wasm/signature]))



(s/def :wasm/typesec
       (s/with-gen (s/map-of :wasm/typeidx
                             :wasm.typesec/item)
                   (fn []
                     (sgen/fmap (fn [item+]
                                  (into (sorted-map)
                                        (map vec)
                                        (partition 2
                                                   (interleave (range)
                                                               item+))))
                                (s/gen (s/coll-of :wasm.typesec/item))))))


;;;;;;;;;; Sections / TODO


(s/def :wasm/funcsec
       (s/map-of any?
                 any?))


(s/def :wasm/globalsec
       (s/map-of any?
                 any?))


(s/def :wasm/memsec
       (s/map-of any?
                 any?))


(s/def :wasm/tablesec
       (s/map-of any?
                 any?))


;;;;;;;;;;


(s/def :wasm/version
       #{1})


;;;;;;;;;;


(defn flatten-idx+

  [space]

  (into (sorted-map)
        (map vec)
        (partition 2
                   (interleave (range)
                               (vals space)))))



(defn fmap-importsec

  ""

  [ctx {:wasm/keys [importsec]}]

  (assoc ctx
         :wasm/importsec
         (reduce #(update %1
                          %2
                          flatten-idx+)
                 importsec
                 [:wasm.import/func
                  :wasm.import/global
                  :wasm.import/mem
                  :wasm.import/table])))



(defn fmap-typesec

  ""

  [ctx {:wasm/keys [typesec]}]

  (assoc ctx
         :wasm/typeidx (count typesec)
         :wasm/typesec typesec))




(s/def :wasm/module
       (s/with-gen (s/keys :req [
                                ;:wasm/funcidx
                                ;:wasm/funcsec
                                ;:wasm/globalidx
                                ;:wasm/globalsec
                                ;:wasm/memidx
                                ;:wasm/memidx
                                ;:wasm/tableidx
                                :wasm/typeidx
                                :wasm/typesec
                                :wasm/version
                                ])
                   (fn []
                     (sgen/fmap (fn [generated]
                                  (-> (assoc wasm.decompile/ctx-default
                                             :wasm/version
                                             1)
                                      (fmap-typesec generated)
                                      (fmap-importsec generated)
                                      ))
                                (sgen/hash-map :wasm/importsec (s/gen :wasm/importsec)
                                               :wasm/typesec   (s/gen :wasm/typesec))))))










(comment
  )
