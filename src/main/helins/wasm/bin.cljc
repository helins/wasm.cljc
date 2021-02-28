;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.bin

  ""

  {:author "Adam Helinski"})


;;;;;;;;;; Types


(def elemtype
     0x70)

(def functype
     0x60)

(def limits-min
     0x00)

(def limits-minmax
     0x01)

(def mut-const
     0x00)

(def mut-var
     0x01)

(def valtype-i32
     0x7f)

(def valtype-i64
     0x7e)

(def valtype-f32
     0x7d)

(def valtype-f64
     0x7c)


;;;;;;;;;; Instructions


(def end
     0x0b)


;;;;;;;;; Sections


(def section-id-custom
     0)
     
(def section-id-type
     1)
     
(def section-id-import
     2)
     
(def section-id-function
     3)
     
(def section-id-table
     4)
     
(def section-id-memory
     5)
     
(def section-id-global
     6)
     
(def section-id-export
     7)
     
(def section-id-start
     8)
     
(def section-id-element
     9)
     
(def section-id-code
     10)
     
(def section-id-data
     11)


;;;;; Import section


(def importdesc-func
     0x00)

(def importdesc-table
     0x01)

(def importdesc-mem
     0x02)

(def importdesc-global
     0x03)


;;;;;;;;;; Predicates for validation


(defn section-id?

  ""

  [section-id]

  (<= 0
      section-id
      12))
