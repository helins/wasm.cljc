;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasmeta.wasmer

  ""

  {:author "Adam Helinski"}

  (:import java.io.File
           java.nio.file.Files
           (org.wasmer Instance
                       Memory)
           org.wasmer.exports.Function))


;;;;;;;;;; Acquiring and discarding an instance


(defn close

  ""

  [^Instance instance]

  (.close instance))



(defn instance

  ""

  ^Instance

  [^String path-wasm]

  (Instance. (Files/readAllBytes (.toPath (File. path-wasm)))))


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
    (fn wasm-function [& arg+]
      (.apply f
              (into-array Object
                          arg+)))))



(defn function-2

  ""

  [instance function-name]

  (let [f (-function instance
                     function-name)]
    (fn wasm-function-2 [arg-array]
      (.apply f
              arg-array))))


;;;;;;;;;; Accessing instance memory


(defn mem

  ""

  (^Memory [instance]

   (mem instance
        "memory"))


  (^Memory [^Instance instance memory-name]

   (-> instance
       .-exports
       (.getMemory memory-name))))



(defn mem->buffer

  ""

  [^Memory mem]

  (.buffer mem))



(defn mem-grow

  ""

  [^Memory mem n-page]

  (.grow mem
         n-page))


;;;;;;;;;;


(comment


  (def i
       (instance "src/wasm/simple.wasm"))

  (let [f (function i
                    "sum")]
    (first (f (int 2)
              (int 3))))


  
  (-> i
      mem
      mem->buffer)


  )
