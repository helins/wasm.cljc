;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.spec

  ""

  {:author "Adam Helinski"}
  
  (:require [clojure.spec.alpha     :as s]
            [clojure.spec.gen.alpha :as sgen]
            [helins.wasm.bin        :as wasm.bin]
            [helins.wasm.decompile  :as wasm.decompile]))


;;;;;;;;;;


(s/def :wasm/u32
       (s/int-in  0
                  4294967295))


;;;;;;;;;;


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


;;;;;;;;;;


(s/def :wasm/valtype
       #{wasm.bin/valtype-i32
         wasm.bin/valtype-i64
         wasm.bin/valtype-f32
         wasm.bin/valtype-f64})


;;;;;;;;;;


(s/def :wasm/resulttype
       (s/or :nil      nil?
             :valtype+ (s/coll-of :wasm/valtype
                                  :kind       vector?
                                  :min-count  1)))


;;;;;;;;;;


(s/def :wasm/signature
       (s/tuple :wasm/resulttype
                :wasm/resulttype))



(s/def :wasm.typesec/item
       (s/keys :req [:wasm/signature]))



(s/def :wasm/typesec
       (s/map-of :wasm/typeidx
                 :wasm.typesec/item
                 :into (sorted-map)))


;;;;;;;;;;


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
                     (sgen/fmap (fn [typesec]
                                  (merge (assoc wasm.decompile/ctx-default
                                                :wasm/version
                                                1)
                                         (when (not-empty typesec)
                                           {:wasm/typeidx (count typesec)
                                            :wasm/typesec (into (sorted-map)
                                                                (map vec)
                                                                (partition 2
                                                                           (interleave (range)
                                                                                       (vals typesec))))})))
                                (s/gen :wasm/typesec)))))
