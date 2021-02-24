;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasmer.fn

  ""

  {:author "Adam Helinski"}

  (:import org.wasmer.Instance
           org.wasmer.exports.Function))


;;;;;;;;;;


(defn function

  ""

  [^Instance instance function-name]

  (let [f (-> instance
              .-exports
              (.getFunction function-name))]
    (fn wasm-fn [^"[Ljava.lang.Object;" arg-array]
      (.apply f
              arg-array))))


;;;;;


(defn arg-array

  ""

  ^"[Ljava.lang.Object;"

  [& arg+]

  (into-array Object
              arg+))



(defn normal-call

  ""

  [function]

  (fn wasm-fn-normal-call [& arg+]
    (function ^"[Ljava.lang.Object;"
              (into-array Object
                          arg+))))



(defn return-1

  ""

  [function]

  (fn wasm-fn-ret-1 [arg-array]
    (aget ^"[Ljava.lang.Object;"
          (function arg-array)
          0)))



(defn return-vec


  ""

  [function]

  (fn wasm-fn-ret-vec [arg-array]
    (vec (function arg-array))))
