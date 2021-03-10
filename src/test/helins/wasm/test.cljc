;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.test

  "Testing core features."

  {:author "Adam"}

  (:require [clojure.spec.alpha              :as s]
            [clojure.spec.gen.alpha          :as sgen]
            [clojure.test                    :as t]
            [clojure.test.check.clojure-test :as tc.ct]
            [clojure.test.check.properties   :as tc.prop]
            [helins.binf                     :as binf]
            [helins.wasm.count               :as wasm.count]
            [helins.wasm.decompile           :as wasm.decompile]
            [helins.wasm.spec]
            [helins.wasm.write               :as wasm.write]))


;;;;;;;;;;


(defn compile-cycle

  ""

  [ctx]

  (-> ctx
      wasm.count/module
      wasm.write/main
      binf/backing-buffer
      wasm.decompile/main
      (dissoc :wasm/source)))


;;;;;;;;;;


#_(tc.ct/defspec module
  
  (tc.prop/for-all [m (s/gen :wasm/module)]
    (wasm.count/module m)
    #_(= m
       (compile-cycle m))))
