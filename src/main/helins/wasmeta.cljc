;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasmeta

  ""

  {:author "Adam"}

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


(defn call

  ""

  [^Function function args]

  (vec (.apply function
               (into-array Object
                           args))))



(defn function

  ""

  [^Instance instance function-name]

  (-> instance
      .-exports
      (.getFunction function-name)))


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

  
  (-> i
      mem
      mem->buffer)


  )
