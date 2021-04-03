;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasmer.instance

  ""

  {:author "Adam Helinski"}

  (:import (org.wasmer Instance
                       Module)))


;;;;;;;;;; Acquiring and discarding an instance


(defn discard

  ""

  [^Instance instance]

  (.close instance))



(defn from-module

  ""

  ^Instance

  [^Module module]

  (.instantiate module))



(defn from-source

  ""

  ^Instance

  [^bytes source]

  (Instance. source))
