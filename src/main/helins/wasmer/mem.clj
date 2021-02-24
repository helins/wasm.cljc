;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasmer.mem

  ""

  {:author "Adam Helinski"}

  (:import (org.wasmer Instance
                       Memory)))


;;;;;;;;;;


(defn from-instance

  ""

  (^Memory [instance]

   (from-instance instance
                  "memory"))


  (^Memory [^Instance instance memory-name]

   (-> instance
       .-exports
       (.getMemory memory-name))))


;;;;;


(defn buffer

  ""

  [^Memory mem]

  (.buffer mem))



(defn grow

  ""

  [^Memory mem n-page]

  (.grow mem
         n-page))
