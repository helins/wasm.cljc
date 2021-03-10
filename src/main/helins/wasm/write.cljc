;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.write

  ""

  {:author "Adam Helinski"}

  (:require [helins.binf        :as binf]
            [helins.binf.buffer :as binf.buffer]
            [helins.binf.int64  :as binf.int64]
            [helins.binf.leb128 :as binf.leb128]
            [helins.wasm.bin    :as wasm.bin]
            [helins.wasm.count  :as wasm.count]
            [helins.wasm.ir     :as wasm.ir]))


;;;;;;;;;


(defn writer

  ""

  []

  (volatile! (-> (binf.buffer/alloc (* 500
                                       1024))
                 binf/view
                 (binf/endian-set :little-endian))))



(defn at-least

  ""

  [v*view n-byte]

  (let [view @v*view]
    (if (>= (binf/remaining view)
            n-byte)
      view
      (let [n-byte-view   (binf/limit view)
            n-byte-min    (+ n-byte-view
                             n-byte)
            n-byte-view-2 (loop [n-byte-view-2 n-byte-view]
                          (let [n-byte-view-3 (Math/ceil (* n-byte-view-2
                                                            1.5))]
                            (if (>= n-byte-view-3
                                    n-byte-min)
                              n-byte-view-3
                              (recur n-byte-view-3))))]
        (vreset! v*view
                 (binf/grow view
                            (- n-byte-view-2
                               n-byte-view)))))))


;;;;;;;;;;


(defn u32

  [view u32]

  (binf.leb128/wr-u32 view
                      u32))


;;;;;;;;;;


(defn idx

  ""

  [view idx]

  (u32 view
       idx))



(def funcidx'

  ""

  idx)



(def typeidx'

  ""

  idx)


;;;;;;;;;;


(defn magic

  ""

  [view]

  (binf/wr-b32 view
               wasm.bin/magic))



(defn version

  ""

  [view {:wasm/keys [version]}]

  (binf/wr-b32 view
               (condp =
                      version
                 wasm.bin/version-1 wasm.bin/version-1)))


;;;;;;;;;;


(defn n-byte

  ""

  [view n-byte]

  (u32 view
       n-byte))



(defn section-id

  [view section-id]

  (binf/wr-b8 view
              section-id))


;;;;;;;;;; Types


(defn func

  ""

  [view flatidx-type {:wasm/keys [typeidx]}]

  (typeidx' view
            (flatidx-type typeidx)))



(defn valtype'

  ""

  [view vt]

  (binf/wr-b8 view
              vt))



(defn resulttype

  ""

  [view valtype+]

  (binf.leb128/wr-u32 view
                      (count valtype+))
  (doseq [vt valtype+]
    (valtype' view
              vt))
  view)



(defn functype

  [view [param+ result+]]

  (-> view
      (binf/wr-b8 0x60)
      (resulttype param+)
      (resulttype result+)))



(defn mut'

  [view mutable?]

  (binf/wr-b8 view
              (if mutable?
                wasm.bin/mut-var
                wasm.bin/mut-const)))



(defn globaltype'

  [view {:wasm/keys [mutable?
                     valtype]}]

  (-> view
      (valtype' valtype)
      (mut' mutable?)))



(defn limits'

  ""

  [view {min- :wasm.limit/min
         max- :wasm.limit/max}]

  (if max-
    (-> view
        (binf/wr-b8 wasm.bin/limits-minmax)
        (u32 min-)
        (u32 max-))
    (-> view
        (binf/wr-b8 wasm.bin/limits-min)
        (u32 min-))))



(defn memtype'

  [view hmap]

  (limits' view
           hmap))



(defn elemtype'

  [view elemtype]

  (binf/wr-b8 view
              elemtype))



(defn tabletype'

  [view hmap]

  (-> view
      (elemtype' (hmap :wasm/elemtype))
      (limits' hmap)))


;;;;;;;;;; Sections / Helper


(defn section-externval

  ""

  [view ctx k-section bin-section-id k-count compile-item]

  (when-some [section (not-empty (ctx k-section))]
    (-> view
        (section-id bin-section-id)
        (n-byte (get-in ctx
                        [:wasm/write
                         k-count]))
        (u32 (count section)))
    (doseq [item (vals section)]
      (compile-item view
                    item)))
  view)


;;;;;;;;;; Sections / Func


(defn funcsec'

  [view ctx]

  (section-externval view
                     ctx
                     :wasm/funcsec
                     wasm.bin/section-id-func
                     :wasm.count/funcsec
                     (let [flatidx-type (get-in ctx
                                                [:wasm/write
                                                 :wasm.flatidx/type])]
                       (fn compile-item [view item]
                         (func view
                               flatidx-type
                               item)))))


;;;;;;;;;; Sections / Import


(defn import'+

  ""

  [view space import-type f]

  (doseq [{:as           hmap
           buffer-module :wasm.import/module
           buffer-name   :wasm.import/name}  (vals space)]
    (-> view
        (u32 (count buffer-module))
        (binf/wr-buffer buffer-module)
        (u32 (count buffer-name))
        (binf/wr-buffer buffer-name)
        (binf/wr-b8 import-type)
        (f hmap)))
  view)



(defn importsec'

  ""

  [view {:wasm/keys                          [importsec]
         {flatidx-type :wasm.flatidx/type
          n-byte-      :wasm.count/importsec
          n-import     :wasm.import/n}       :wasm/write}]

  (when (pos? n-byte-)
    (-> view
        (section-id wasm.bin/section-id-import)
        (n-byte n-byte-)
        (u32 n-import)
        (import'+ (importsec :wasm.import/func)
                  wasm.bin/importdesc-func
                  (fn [view hmap]
                    (func view
                          flatidx-type
                          hmap)))
        (import'+ (importsec :wasm.import/global)
                  wasm.bin/importdesc-global
                  globaltype')
        (import'+ (importsec :wasm.import/mem)
                  wasm.bin/importdesc-mem
                  memtype')
        (import'+ (importsec :wasm.import/table)
                  wasm.bin/importdesc-table
                  tabletype')))
  view)


;;;;;;;;;; Sections / mem


(defn memsec'

  [view ctx]

  (section-externval view
                     ctx
                     :wasm/memsec
                     wasm.bin/section-id-mem
                     :wasm.count/memsec
                     memtype'))


;;;;;;;;;; Sections / Start


(defn startsec'

  [view {:as        ctx
         :wasm/keys [startsec]}]

  (when startsec
    (let [ctx-write (ctx :wasm/write)]
      (-> view
          (section-id wasm.bin/section-id-start)
          (n-byte (ctx-write :wasm.count/startsec))
          (funcidx' (get-in ctx-write
                            [:wasm.flatidx/func
                             (startsec :wasm/funcidx)])))))
  view)


;;;;;;;;;; Sections / Table


(defn tablesec'

  [view ctx]

  (section-externval view
                     ctx
                     :wasm/tablesec
                     wasm.bin/section-id-table
                     :wasm.count/tablesec
                     tabletype'))


;;;;;;;;;; Sections / Type


(defn typesec

  ""

  [view {:as        ctx
         :wasm/keys [typesec]}]

  (when (seq typesec)
    (-> view
        (section-id wasm.bin/section-id-type)
        (n-byte (get-in ctx
                        [:wasm/write
                         :wasm.count/typesec]))
        (u32 (count typesec)))
    (doseq [signature (map :wasm/signature
                           (vals typesec))]
      (functype view
                signature)))
  view)


;;;;;;;;;;


(defn main

  ""

  [ctx]

  (-> (get-in ctx
              [:wasm/write
               :wasm.count/module])
      binf.buffer/alloc
      binf/view
      (binf/endian-set :little-endian)
      magic
      (version ctx)
      (typesec ctx)
      (importsec' ctx)
      (funcsec' ctx)
      (tablesec' ctx)
      (memsec' ctx)
      (startsec' ctx)
      ))
