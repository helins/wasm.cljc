;; Decompiling and pretty printing a WASM program such as "my_program.wasm":
;;
;; $ bb decompile.clj my_program.wasm
;;
;; Following WASM specs 1.1, compatible with Babashka >= 0.3.5

(require '[babashka.deps :as deps])
(deps/add-deps '{:deps {io.helins/wasm {:mvn/version "0.0.0-alpha2"}}})


(require 'clojure.pprint
         '[helins.wasm :as wasm])


(-> *command-line-args*
    first
    wasm/decompile-file
    clojure.pprint/pprint)
