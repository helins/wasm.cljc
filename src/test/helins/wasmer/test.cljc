;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasmer.test

  "Testing core features."

  {:author "Adam"}

  (:require [clojure.test           :as t]
            [helins.wasmer.fn       :as wasmer.fn]
            [helins.wasmer.instance :as wasmer.instance]
            [helins.wasmer.mem      :as wasmer.mem]
            [helins.wasmer.module   :as wasmer.module]
            [helins.wasmeta         :as wasmeta]))


;;;;;;;;;;


(def arg-0
     (int 2))


(def arg-1
     (int 3))


(def arg-array
     (wasmer.fn/arg-array arg-0
                          arg-1))


(def source
     (wasmer.module/load-source "src/wasm/simple.wasm"))


(def module
     (wasmer.module/from-source source))


(def instance
     (wasmer.instance/from-module module))


(def mem
     (wasmer.mem/from-instance instance))


(def sum
     (wasmer.fn/find instance
                     "sum"))


;;;;;;;;;;


(t/deftest function

  (t/is (= 5
           (first (sum arg-array))
           (first ((wasmer.fn/normal-call sum) arg-0
                                               arg-1))
           ((wasmer.fn/return-1 sum) arg-array)))

  (t/is (= [5]
           ((wasmer.fn/return-vec sum) arg-array))))



(t/deftest memory

  (let [n-byte (.capacity (wasmer.mem/buffer mem))]

    (t/is (pos? n-byte))

    (let [n-page (wasmer.mem/grow mem
                                  2)]
      (t/is (pos? n-page))
      (t/is (= (+ n-page
                  2)
               (wasmer.mem/grow mem
                                0))))))
