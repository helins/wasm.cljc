;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasmer.instance

  ""

  {:author "Adam Helinski"}

  (:import java.io.File
           java.nio.file.Files
           org.wasmer.Instance))


;;;;;;;;;; Acquiring and discarding an instance


(defn discard

  ""

  [^Instance instance]

  (.close instance))



(defn from-path

  ""

  ^Instance

  [^String path-wasm]

  (Instance. (Files/readAllBytes (.toPath (File. path-wasm)))))
