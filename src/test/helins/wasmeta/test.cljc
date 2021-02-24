;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.wasmeta.test

  "Testing core features."

  {:author "Adam"}

  (:require [clojure.test   :as t]
            [helins.wasmeta :as wasmeta]))


;;;;;;;;;;


(t/deftest splice

  (t/is (= [1 2 3]
           (wasmeta/splice [1 2 3]))
        "Without splicing")

  (t/is (= [1 2 3]
           (wasmeta/splice [1 (list 2) 3]))
        "Nested 1")

  (t/is (= [1 2 3]
           (wasmeta/splice [1 (list (list 2)) 3]))
        "Nested several times"))
