;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.compile.type

  ""

  {:author "Adam Helinski"})


;;;;;;;;;; Types


(def i32
     0x7f)

(def i64
     0x7e)

(def f32
     0x7d)

(def f64
     0x7c)


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
