;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.bin

  ""

  {:author "Adam Helinski"}

  (:refer-clojure :exclude [drop]))


;;;;;;;;;;


(def opcode->opsym

  ""

  
  {0x00 'unreachable
   0x01 'nop
   0x02 'block
   0x03 'loop
   0x04 'if
   0x05 'else

   ;;;; [0x06, 0x0a] is reserved

   0x0b 'end
   0x0c 'br
   0x0d 'br_if
   0x0e 'br_table
   0x0f 'return
   0x10 'call
   0x11 'call_indirect

   ;;;; [0x12, 0x19] is reserved

   0x1a 'drop
   0x1b 'select

   ;;;; [0x1c, 0x1f] is reserved

   0x20 'local.get
   0x21 'local.set
   0x22 'local.tee
   0x23 'global.get
   0x24 'global.set

   ;;;; [0x25, 0x27] is reserved

   0x28 'i32.load
   0x29 'i64.load
   0x2a 'f32.load
   0x2b 'f64.load

   0x2c 'i32.load8_s
   0x2d 'i32.load8_u
   0x2e 'i32.load16_s
   0x2f 'i32.load16_u
   0x30 'i64.load8_s
   0x31 'i64.load8_u
   0x32 'i64.load16_s
   0x33 'i64.load16_u
   0x34 'i64.load32_s
   0x35 'i64.load32_u

   0x36 'i32.store
   0x37 'i64.store
   0x38 'f32.store
   0x39 'f64.store
   0x3a 'i32.store8
   0x3b 'i32.store16
   0x3c 'i64.store8
   0x3d 'i64.store16
   0x3e 'i64.store32

   0x3f 'memory.size
   0x40 'memory.grow

   0x41 'i32.const
   0x42 'i64.const
   0x43 'f32.const
   0x44 'f64.const

   0x45 'i32.eqz
   0x46 'i32.eq
   0x47 'i32.ne
   0x48 'i32.lt_s
   0x49 'i32.lt_u
   0x4a 'i32.gt_s
   0x4b 'i32.gt_u
   0x4c 'i32.le_s
   0x4d 'i32.le_u
   0x4e 'i32.ge_s
   0x4f 'i32.ge_u

   0x50 'i64.eqz
   0x51 'i64.eq
   0x52 'i64.ne
   0x53 'i64.lt_s
   0x54 'i64.lt_u
   0x55 'i64.gt_s
   0x56 'i64.gt_u
   0x57 'i64.le_s
   0x58 'i64.le_u
   0x59 'i64.ge_s
   0x5a 'i64.ge_u

   0x5b 'f32.eq
   0x5c 'f32.be
   0x5d 'f32.lt
   0x5e 'f32.gt
   0x5f 'f32.le
   0x60 'f32.ge

   0x61 'f64.eq
   0x62 'f64.be
   0x63 'f64.lt
   0x64 'f64.gt
   0x65 'f64.le
   0x66 'f64.ge

   0x67 'i32.clz
   0x68 'i32.ctz
   0x69 'i32.popcnt
   0x6a 'i32.and
   0x6b 'i32.sub
   0x6c 'i32.mul
   0x6d 'i32.div_s
   0x6e 'i32.div_u
   0x6f 'i32.rem_s
   0x70 'i32.rem_u
   0x71 'i32.and
   0x72 'i32.or
   0x73 'i32.xor
   0x74 'i32.shl
   0x75 'i32.shr_s
   0x76 'i32.shr_u
   0x77 'i32.rotl
   0x78 'i32.rotr

   0x79 'i64.clz
   0x7a 'i64.ctz
   0x7b 'i64.popcnt
   0x7c 'i64.and
   0x7d 'i64.sub
   0x7e 'i64.mul
   0x7f 'i64.div_s
   0x80 'i64.div_u
   0x81 'i64.rem_s
   0x82 'i64.rem_u
   0x83 'i64.and
   0x84 'i64.or
   0x85 'i64.xor
   0x86 'i64.shl
   0x87 'i64.shr_s
   0x88 'i64.shr_u
   0x89 'i64.rotl
   0x8a 'i64.rotr

   0x8b 'f32.abs
   0x8c 'f32.neg
   0x8d 'f32.ceil
   0x8e 'f32.floor
   0x8f 'f32.trunc
   0x90 'f32.nearest
   0x91 'f32.sqrt
   0x92 'f32.add
   0x93 'f32.sub
   0x94 'f32.mul
   0x95 'f32.div
   0x96 'f32.min
   0x97 'f32.max
   0x98 'f32.copysign

   0x99 'f64.abs
   0x9a 'f64.neg
   0x9b 'f64.ceil
   0x9c 'f64.floor
   0x9d 'f64.trunc
   0x9e 'f64.nearest
   0x9f 'f64.sqrt
   0xa0 'f64.add
   0xa1 'f64.sub
   0xa2 'f64.mul
   0xa3 'f64.div
   0xa4 'f64.min
   0xa5 'f64.max
   0xa6 'f64.copysign

   0xa7 'i32.wrap_i64
   0xa8 'i32.trunc_f32_s
   0xa9 'i32.trunc_f32_u
   0xaa 'i32.trunc_f64_s
   0xab 'i32.trunc_f64_u

   0xac 'i64.extend_i32_s
   0xad 'i64.extend_i32_u
   0xae 'i64.trunc_f32_s
   0xaf 'i64.trunc_f32_u
   0xb0 'i64.trunc_f64_s
   0xb1 'i64.trunc_f64_u

   0xb2 'f32.convert_i32_s
   0xb3 'f32.convert_i32_u
   0xb4 'f32.convert_i64_s
   0xb5 'f32.convert_i64_u
   0xb6 'f32.demote_f64

   0xb7 'f64.convert_i32_s
   0xb8 'f64.convert_i32_u
   0xb9 'f64.convert_i64_s
   0xba 'f64.convert_i64_u
   0xbb 'f64.promote_f32

   0xbc 'i32.reinterpret_f32
   0xbd 'i64.reinterpret_f64
   0xbe 'f32.reinterpret_i32
   0xbf 'f32.reinterpret_i64

   0xc0 'i32.extend8_s
   0xc1 'i32.extend16_s
   0xc2 'i64.extend8_s
   0xc3 'i64.extend16_s
   0xc4 'i64.extend32_s

   ;;;; [0xC5, 0xFB] is reserved

   0xFC {0 'i32.trunc_sat_f32_s
         1 'i32.trunc_sat_f32_u
         2 'i32.trunc_sat_f64_s
         3 'i32.trunc_sat_f64_u
         4 'i64.trunc_sat_f32_s
         5 'i64.trunc_sat_f32_u
         6 'i64.trunc_sat_f64_s
         7 'i64.trunc_sat_f64_u}

   ;;;; [0xFD, 0xFF] is undefined
   })



(defn -trunc_sat?

  ""

  [opcode]

  (= opcode
     0xFC))


;;;;;;;;;; Types / Number Types


(def numtype-i32
     0x7f)

(def numtype-i64
     0x7e)

(def numtype-f32
     0x7d)

(def numtype-f64
     0x7c)


;;;;;;;;;; Types / Reference Types


(def funcref
     0x70)

(def externref
     0x6F)


;;;;;;;;;; Types / Function Types


(def functype
     0x60)

;;;;;;;;;; Types / Limits


(def limits-min
     0x00)

(def limits-minmax
     0x01)

;;;;;;;;;; Types / Global Types


(def mut-const
     0x00)

(def mut-var
     0x01)


;;;;;;;;;; Instructions / Control instructions


(def blocktype-nil
     0x40)

(def unreachable
     0x00)

(def nop
     0x01)

(def block
     0x02)

(def loop-
     0x03)

(def if-
     0x04)

(def else
     0x05)

(def end
     0x0b)

(def br
     0x0c)

(def br_if
     0x0d)

(def br_table
     0x0e)

(def return
     0x0f)

(def call
     0x10)

(def call_indirect
     0x11)


;;;;;;;;;; Instructions / Parametric Instructions


(def drop
     0x1a)

(def select
     0x1b)

;;;;;;;;;; Instructions / Variable instructions


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


;;;;;;;;;; Instructions / Memory Instructions


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


;;;;;;;;;; Instructions / Numeric Instructions


(def i32-const
     0x41)

(def i64-const
     0x42)

(def f32-const
     0x43)

(def f64-const
     0x44)



(def i32-eqz
     0x45)

(def i32-eq
     0x46)

(def i32-ne
     0x47)

(def i32-lt_s
     0x48)

(def i32-lt_u
     0x49)

(def i32-gt_s
     0x4a)

(def i32-gt_u
     0x4b)

(def i32-le_s
     0x4c)

(def i32-le_u
     0x4d)

(def i32-ge_s
     0x4e)

(def i32-ge_u
     0x4f)



(def i64-eqz
     0x50)

(def i64-eq
     0x51)

(def i64-ne
     0x52)

(def i64-lt_s
     0x53)

(def i64-lt_u
     0x54)

(def i64-gt_s
     0x55)

(def i64-gt_u
     0x56)

(def i64-le_s
     0x57)

(def i64-le_u
     0x58)

(def i64-ge_s
     0x59)

(def i64-ge_u
     0x5a)



(def f32-eq
     0x5b)

(def f32-be
     0x5c)

(def f32-lt
     0x5d)

(def f32-gt
     0x5e)

(def f32-le
     0x5f)

(def f32-ge
     0x60)



(def f64-eq
     0x61)

(def f64-be
     0x62)

(def f64-lt
     0x63)

(def f64-gt
     0x64)

(def f64-le
     0x65)

(def f64-ge
     0x66)



(def i32-clz
     0x67)

(def i32-ctz
     0x68)

(def i32-popcnt
     0x69)

(def i32-and
     0x6a)

(def i32-sub
     0x6b)

(def i32-mul
     0x6c)

(def i32-div_s
     0x6d)

(def i32-div_u
     0x6e)

(def i32-rem_s
     0x6f)

(def i32-rem_u
     0x70)

(def i32-and
     0x71)

(def i32-or
     0x72)

(def i32-xor
     0x73)

(def i32-shl
     0x74)

(def i32-shr_s
     0x75)

(def i32-shr_u
     0x76)

(def i32-rotl
     0x77)

(def i32-rotr
     0x78)



(def i64-clz
     0x79)

(def i64-ctz
     0x7a)

(def i64-popcnt
     0x7b)

(def i64-and
     0x7c)

(def i64-sub
     0x7d)

(def i64-mul
     0x7e)

(def i64-div_s
     0x7f)

(def i64-div_u
     0x80)

(def i64-rem_s
     0x81)

(def i64-rem_u
     0x82)

(def i64-and
     0x83)

(def i64-or
     0x84)

(def i64-xor
     0x85)

(def i64-shl
     0x86)

(def i64-shr_s
     0x87)

(def i64-shr_u
     0x88)

(def i64-rotl
     0x89)

(def i64-rotr
     0x8a)



(def f32-abs
     0x8B)

(def f32-neg
     0x8C)

(def f32-ceil
     0x8D)

(def f32-floor
     0x8E)

(def f32-trunc
     0x8F)

(def f32-nearest
     0x90)

(def f32-sqrt
     0x91)

(def f32-add
     0x92)

(def f32-sub
     0x93)

(def f32-mul
     0x94)

(def f32-div
     0x95)

(def f32-min
     0x96)

(def f32-max
     0x97)

(def f32-copysign
     0x98)



(def f64-abs
     0x99)

(def f64-neg
     0x9A)

(def f64-ceil
     0x9B)

(def f64-floor
     0x9C)

(def f64-trunc
     0x9D)

(def f64-nearest
     0x9E)

(def f64-sqrt
     0x9F)

(def f64-add
     0xA0)

(def f64-sub
     0xA1)

(def f64-mul
     0xA2)

(def f64-div
     0xA3)

(def f64-min
     0xA4)

(def f64-max
     0xA5)

(def f64-copysign
     0xA6)



(def i32-wrap_i64
     0xA7)

(def i32-trunc_f32_s
     0xA8)

(def i32-trunc_f32_u
     0xA9)

(def i32-trunc_f64_s
     0xAA)

(def i32-trunc_f64_u
     0xAB)



(def i64-extend_i32_s
     0xAC)

(def i64-extend_i32_u
     0xAD)

(def i64-trunc_f32_s
     0xAE)

(def i64-trunc_f32_u
     0xAF)

(def i64-trunc_f64_s
     0xB0)

(def i64-trunc_f64_u
     0xB1)



(def f32-convert_i32_s
     0xB2)

(def f32-convert_i32_u
     0xB3)

(def f32-convert_i64_s
     0xB4)

(def f32-convert_i64_u
     0xB5)

(def f32-demote_f64
     0xB6)



(def f64-convert_i32_s
     0xB7)

(def f64-convert_i32_u
     0xB8)

(def f64-convert_i64_s
     0xB9)

(def f64-convert_i64_u
     0xBA)

(def f64-promote_f32
     0xBB)



(def i32-reinterpret_f32
     0xBC)

(def i64-reinterpret_f64
     0xBD)

(def f32-reinterpret_i32
     0xBE)

(def f32-reinterpret_i64
     0xBF)



(def i32-extend8_s
     0xC0)

(def i32-extend16_s
     0xC1)

(def i64-extend8_s
     0xC2)

(def i64-extend16_s
     0xC3)

(def i64-extend32_s
     0xC4)



(def trunc_sat

  ""

  0xFC)


(def i32-trunc_sat_f32_s
     0)

(def i32-trunc_sat_f32_u
     1)

(def i32-trunc_sat_f64_s
     2)

(def i32-trunc_sat_f64_u
     3)

(def i64-trunc_sat_f32_s
     4)

(def i64-trunc_sat_f32_u
     5)

(def i64-trunc_sat_f64_s
     6)

(def i64-trunc_sat_f64_u
     7)


;;;;;;;;; Modules / Sections


(def section-id-custom
     0)
     
(def section-id-type
     1)
     
(def section-id-import
     2)
     
(def section-id-func
     3)
     
(def section-id-table
     4)
     
(def section-id-mem
     5)
     
(def section-id-global
     6)
     
(def section-id-export
     7)
     
(def section-id-start
     8)
     
(def section-id-elem
     9)
     
(def section-id-code
     10)
     
(def section-id-data
     11)



(defn section-id?

  ""

  [section-id]

  (<= 0
      section-id
      12))


;;;;;;;;;; Modules / Import Section


(def importdesc-func
     0x00)

(def importdesc-table
     0x01)

(def importdesc-mem
     0x02)

(def importdesc-global
     0x03)


;;;;;;;;;; Modules / Export Section


(def exportdesc-func
     0x00)

(def exportdesc-table
     0x01)

(def exportdesc-mem
     0x02)

(def exportdesc-global
     0x03)


;;;;;;;;;; Modules / Modules


(def magic

  ""

  0x6d736100)



(def version-1

  ""

  1)
