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


;;;;; Variable instructions


(def local-get
     0x20)

(def local-set
     0x21)

(def local-tee
     0x22)

(def global-get
     0x23)

(def global-set
     0x24)


;;;;; Memory instructions


(def i32-load
     0x28)

(def i64-load
     0x29)

(def f32-load
     0x2a)

(def f64-load
     0x2b)

(def i32-load8_s
     0x2c)

(def i32-load8_u
     0x2d)

(def i32-load16_s
     0x2e)

(def i32-load16_u
     0x2f)

(def i64-load8_s
     0x30)

(def i64-load8_u
     0x31)

(def i64-load16_s
     0x32)

(def i64-load16_u
     0x33)

(def i64-load32_s
     0x34)

(def i64-load32_u
     0x35)

(def i32-store
     0x36)

(def i64-store
     0x37)

(def f32-store
     0x38)

(def f64-store
     0x39)

(def i32-store8
     0x3a)

(def i32-store16
     0x3b)

(def i64-store8
     0x3c)

(def i64-store16
     0x3d)

(def i64-store32
     0x3e)

(def memory-size
     0x3f)

(def memory-grow
     0x40)


;;;;; Numeric instructions


;;; Constants


(def const-i32
     0x41)

(def const-i64
     0x42)

(def const-f32
     0x43)

(def const-f64
     0x44)


;;;;;


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


;;;;; Export section


(def exportdesc-func
     0x00)

(def exportdesc-table
     0x01)

(def exportdesc-mem
     0x02)

(def exportdesc-global
     0x03)


;;;;;;;;;; Predicates for validation


(defn section-id?

  ""

  [section-id]

  (<= 0
      section-id
      12))
