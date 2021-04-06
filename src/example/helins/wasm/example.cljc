;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasm.example

  "Simple examples of compilation / decompilation and Malli schemas."

  {:author "Adam Helinski"}

  (:require [clojure.pprint]
            [helins.binf        :as binf]
            [helins.wasm        :as wasm]
            [helins.wasm.schema :as wasm.schema]
            [malli.core         :as malli]
            [malli.generator    :as malli.gen]
            [malli.util]))


;;;;;;;;;; Malli schemas


(def registry
       (-> (merge (malli/default-schemas)
                  (malli.util/schemas))
           wasm.schema/registry))



(comment


  ;; Generating a single WASM instruction at random
  ;;
  (clojure.pprint/pprint (malli.gen/generate :wasm/instr
                                             {:registry registry})))


;;;;;;;;;; Compilation / Decompilation - Files (JVM)


(comment


  (def decompiled
       (wasm/decompile-file "src/wasm/test.wasm"))


  (clojure.pprint/pprint decompiled)


  (def compiled
       (wasm/compile-file decompiled
                          "/tmp/test2.wasm"))


  ;; Of course, it is valid
  ;;
  (malli/validate :wasm/module
                  decompiled
                  {:registry registry}))


;;;;;;;;;; Compilation / Decompilation - BinF views (JVM + JS)


(comment

  ;; Anything that can be represented as a BinF view can be decompiled
  ;;
  ;; Eg. JVM - Wrapping a byte array in a view
  ;;     JS  - Wrapping a ArrayBuffer in a view
  ;;
  ;; See https://github.com/helins/binf.cljc

  ;; This is a realistic Clojurescript example using `js/fetch`.
  ;; Starting the :dev Shadow-CLJS profile serves the follow test files.
  ;; See "Development" section in README


  (def *decompiled
       (atom nil))


  ;; Fetching the WASM source and decompiling into above atom.
  ;;
  (-> (js/fetch "test.wasm")
      (.then (fn [resp]
               (.arrayBuffer resp)))
      (.then (fn [array-buffer]
               (reset! *decompiled
                       (-> array-buffer
                           ;; Converting to BinF view and preparing it (will set the right endianess)
                           binf/view
                           wasm/prepare-view
                           ;; Decompiling
                           wasm/decompile)))))


  ;; Pretty printing
  ;;
  (clojure.pprint/pprint @*decompiled)


  (malli/validate :wasm/module
                  @*decompiled
                  {:registry registry})


  ;; Recompiling
  ;;
  (def compiled
       (wasm/compile @*decompiled))


  ;; Of course, you can decompile again for fun
  ;;
  (-> compiled
      (binf/seek 0)
      wasm/decompile
      clojure.pprint/pprint)

  )
