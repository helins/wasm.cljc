;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasmeta

  ""

  {:author "Adam Helinski"})


;;;;;;;;;;


(defn splice

  ""


  ([coll]

   (splice coll
           []))


  ([coll acc]

   (reduce (fn [acc-2 item]
             (if (list? item)
               (splice item
                       acc-2)
               (conj acc-2
                     item)))
           acc
           coll)))
