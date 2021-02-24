;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasmer.fn

  ""

  {:author "Adam Helinski"}

  (:import org.wasmer.Instance
           org.wasmer.exports.Function))


;;;;;;;;;; Retrieving and calling functions


(defn arg-array

  ""

  [& arg+]

  (into-array Object
              arg+))



(defn- -function

  ;;

  ^Function

  [^Instance instance function-name]

  (-> instance
      .-exports
      (.getFunction function-name)))



(defn function

  ""

  [instance function-name]

  (let [f (-function instance
                     function-name)]
    (fn wasm-function-2 [arg-array]
      (.apply f
              arg-array))))
