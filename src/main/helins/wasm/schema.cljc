;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.schema

  ""

  {:author "Adam Helinski"}

  (:require [helins.wasm.bin :as wasm.bin]
            [malli.core      :as malli]
            [malli.generator :as malli.gen]))


;;;;;;;;;; Values / Byte


(def byte'
     [:int
      {:max 255
       :min 0}])


;;;;;;;;;; Values / Integers


(def u32'
     [:int
      {:max 4294967295
       :min 0}])


;;;;;;;;;; Types / Number Types


(def numtype'
     [:enum
      wasm.bin/numtype-i32
      wasm.bin/numtype-i64
      wasm.bin/numtype-f32
      wasm.bin/numtype-f64])


;;;;;;;;;; Types / Reference Types


(def reftype'
     [:enum
      wasm.bin/funcref
      wasm.bin/externref])


;;;;;;;;;; Types / Value Types


(def valtype'
     [:or
      :wasm/numtype
      :wasm/reftype])


;;;;;;;;;; Types / Result Types


(def resulttype'
     [:maybe
      [:vector
       {:min 1}
       :wasm/valtype]])


;;;;;;;;;; Types / Function Types


(def functype'
     [:tuple
      :wasm/resulttype
      :wasm/resulttype])


;;;;;;;;;; Modules / Indices


(def idx
  
  ""

  :wasm/u32)



(def funcidx'
     idx)


;;;;;;;;;; Modules / Type Section


(def typesec'
     [:map-of
      :wasm/funcidx
      :wasm/type])



(def type-
  
  ""

  [:map
   :wasm/signature])



(def signature
     :wasm/functype)


;;;;;;;;;; Registry


(def registry

  "Registry gathering all schemas related to WASM."

  {
   :wasm/byte       byte'
   :wasm/funcidx    funcidx'
   :wasm/functype   functype'
   :wasm/idx        idx
   :wasm/numtype    numtype'
   :wasm/reftype    reftype'
   :wasm/resulttype resulttype'
   :wasm/signature  signature
   :wasm/type       type-
   :wasm/typesec    typesec'
   :wasm/u32        u32'
   :wasm/valtype    valtype'
   })





(comment


  (malli/validate [:and
                   {:registry registry}
                   :wasm/funcidx]
                  420
                  )


  (malli.gen/sample [:and
                     {:registry registry}
                     :wasm/typesec])

  (malli.gen/generator [:and
                        {:registry registry}
                        :wasm/functype])
  )
