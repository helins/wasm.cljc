# WASM 1.1 compiler / decompiler

[![Clojars](https://img.shields.io/clojars/v/io.helins/wasm.svg)](https://clojars.org/io.helins/wasm)

[![Cljdoc](https://cljdoc.org/badge/io.helins/wasm)](https://cljdoc.org/d/io.helins/wasm)

![CircleCI](https://circleci.com/gh/helins/wasm.cljc.svg?style=shield)

Compatible with Clojurescript.

A novel library for the WebAssembly (WASM) ecosystem:

- WASM programs as simple immutable Clojure data structures
- Decompiling and compiling WASM binaries
- JVM and browser, no compromise
- Allowing all sorts of crazy WASM analysis and metaprogramming 
- Working interactively as opposed to using the command-line
- Generating random WASM programs for runtime testing
- Fully described using [Malli](https://github.com/metosin/malli)
- Robust, backed up by generative testing

All binary processing relies on the powerful
[BinF](https://github.com/helins/binf.cljc) library.

## Status

The implementation of this library follows the WASM specification very closely.

Even the order of the definitions in the namespaces is identical to the order of
the definitions in the [WASM binary specification](https://webassembly.github.io/spec/core/binary/index.html)
so that they can be read alongside. Indeed, there is no better documentation than
the specification itself.

This design also reduces the chance of creating breaking changes. However, WASM
development is very active and new proposals are being built. There should never
be a breaking change within WASM itself. However, since this library is novel,
current status is **alpha** for the time bing in spite of the fact that the design
is robust and well-tested.

The goal is to remain up-to-date with [all **stable** WASM
proposals](https://github.com/WebAssembly/proposals).


## Documentation

The [full API is available on Cljdoc](https://cljdoc.org/d/helins/wasm).

Namespaces follow one naming scheme. In the [WASM binary specification](https://webassembly.github.io/spec/core/binary/index.html),
any item is defined by a so-called "non-terminal symbol". For instance, the
function section is designated by **funcsec**.

Names that refer to those non-terminal symbols end with the `'` character. For
instance, the `helins.wasm.read` namespace for decompiling WASM code has a
`funcsec'` function which decompiles a function section. Those names do not have
docstrings in Cljdoc since it is best to read and follow the WASM specification.
Namespaces mimick exactly that specification for that reason.

All other names, such as higher-level abstractions, are fully described on Cljdoc.


## Usage, brief overview

Compilation / decompilation is easy as demonstrated in the example below.

The rest of the document is about understanding and modifying a decompiled program.

In very, very short:

```clojure
(require 'clojure.pprint
         '[helins.wasm :as wasm])


;; Reading an example file from this repo:
;;
(def decompiled
     (wasm/decompile-file "src/wasm/test.wasm"))


;; Pretty printing decompiled form (Clojure data):
;;
(clojure.pprint/pprint decompiled)


;; Of course, we can recompile it:
;;
(def compiled
     (wasm/compile-file decompiled))
```

Working with files is the only JVM-exclusive utility in this library.

WASM binaries are represented as [BinF
views](https://github.com/helins/binf.cljc#buffers-and-views). For instance,
from Clojurescript:

```clojure
(require '[helins.binf :as binf])


(.then (js/fetch "")
       (fn [array-buffer]
         (-> array-buffer
             binf/view
             wasm/decompile)))
```


## Installation

After adding [this library](https://clojars.org/io.thelins/wasm) to dependencies,
one must also manually add Malli. As of today, an unreleased version is needed:

```clojure
{metosin/malli {:git/url "https://github.com/metosin/malli"
                :sha     "0e5e3f1ee9bc8d6ea60dc16e59abf9cc295ab510"}}
```


## Namespaces

In summary:

| Namespace | About |
|---|---|
| helins.wasm | Compiling and decompiling WASM modules |
| helins.wasm.bin | Defines all simple binary values such as opcodes |
| helins.wasm.ir | Simple manipulations of WASM programs in Clojure |
| helins.wasm.read | Implementing decompilation (for "experts") |
| helins.wasm.schema | Using Malli, describes the WASM binary format in Clojure |
| helins.wasm.write | Implementing compilation (for "experts") |


## Schema

The Clojure data structures representing WASM programs are almost a direct
translation of the WASM binary specification. Very little abstraction has been
added on purpose. The goal is to leverage those wonderful data structures while
having the illusion of working directly with the binary representation.

The registry of Malli schemas describes everything:

```clojure
(require '[helins.wasm.schema :as wasm.schema]
         '[malli.core         :as malli]
         '[malli.generator    :as malli.gen]
         '[malli.util])


;; Merging all needed registries.
;;
(def registry
     (merge (malli/default-schemas)
            (malli.util/schemas)
            (wasm.schema/registry)))


;; What is a `funcsec`?
;;
(get registry
     :wasm/funcsec)


;; Let us generate a random WASM program.
;;
(malli.gen/generate :wasm/module
                    {:registry registry})
```


## Overall shape

A WASM program is a map referred in the namespaces as a `ctx` (context). It
holds the program itself (WASM sections) as well as a few extra things
(akin to the context described in other sections of the WASM specification).

Almost everything is a map but WASM instructions which are vectors. All simples
values, such as opcodes, remain as binary values (see "Instructions" section for
an example).


## Sections

In the binary format, most WASM sections format are essentially a list of items,
such as the data section being a list of data segments. Other parts of the program,
such as instructions operating on such a data segment, refer to an item by
its index in that list.

Howewer, working with lists of items and addressing those items by index is hard work,
especially maintaining those references when things are removed, added, and
move around. Hence, those sections are described by sorted maps of `index` ->
`item`. They can be sparse and during compilation, indices (references) will be
transparently recomputed into a dense list. 

See `helins.wasm.ir` namespace for a few functions showing how to handle things
like adding a data segment.


## Instructions

Instructions are expressed as vectors where the first item is an opcode and the
rest might be so-called "immediates" (ie. mandatory arguments). Once again, they
look almost exactly like the binary format and the official specification is the
best documentation.

For example, here is a WASM `block` which adds 42 to a value from the WASM stack:

```clojure
(require '[helins.wasm.bin :as wasm.bin])

[wasm.bin/block
 nil
 [wasm.bin/i32-const 42]
 [wasm.bin/i32-add]]
```


## Modifying a WASM program

Since everything is described in the `helins.wasm.schema` namespace and since
those definitions are well documented in the WASM binary specification, it is
fairly easy to create or modify WASM programs. Once one understands the format,
it is just common Clojure programming without much surprise.

The `helins.wasm.ir` namespace ("ir" standing for "Intermediary
Representation"), proposes a few utilities for doing basic things such as adding
a function. It is not very well featured because usually, doing almost anything
is very straightforward and do not require special helpers.


## Novel WASM tools

The vast majority of existing WASM tools are implemented in Rust or C++. Doing
things such as dead code elimination of WASM if a tedious process performed from
the command-line. Building new tools in that ecosystem means abiding by that fact
and working excusively with those native languages.

Hence, this library is one of its kind by offering a powerful interactive
environment, on the JVM as well as in the browser, and leveraging Clojure
idioms which are excellent for analyzing WASM code.


## Running tests

Depending on hardware, tests usually takes a few minutes to run.

On the JVM, using [Kaocha](https://github.com/lambdaisland/kaocha):

```bash
$ ./bin/test/jvm/run
```
On NodeJS, using [Shadow-CLJS](https://github.com/thheller/shadow-cljs):

```bash
$ ./bin/test/node/run

# Or testing an advanced build:
$ ./bin/test/node/advanced
```


## Development

Starting in Clojure JVM mode, mentioning an additional Deps alias (here, a local
setup of NREPL):
```bash
$ ./bin/dev/clojure :nrepl
```

Starting in CLJS mode using [Shadow-CLJS](https://github.com/thheller/shadow-cljs):
```bash
$ ./bin/dev/cljs
# Then open ./cljs/index.html
```

## License

Copyright © 2021 Adam Helinski

Licensed under the term of the Mozilla Public License 2.0, see LICENSE.
