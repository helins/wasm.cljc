;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasmer.module

  ""

  {:author "Adam Helinski"}

  (:import java.io.File
           java.nio.file.Files
           org.wasmer.Module))


;;;;;;;;;; 


(defn load-source

  ""

  ^bytes

  [^String path]

  (Files/readAllBytes (.toPath (File. path))))



(defn validate

  ""

  [^bytes source]

  (Module/validate source))


;;;;;


(defn discard

  ""

  [^Module module]

  (.close module)
  nil)



(defn from-source

  ""

  ^Module

  [^bytes source]

  (Module. source))


;;;;;


(defn deserialize

  ""

  ^Module

  [^bytes serialized-module]

  (Module/deserialize serialized-module))



(defn serialize

  ""

  ^bytes

  [^Module module]

  (.serialize module))
