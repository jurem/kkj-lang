# kkj-lang

Compositional / concatenative programming language

## Prerequsites

You need Haskell and make. Tested on macOS with ghc 8.4.3 and make 3.81. 

## To compile and run

```bash
cd compiler
make
cd ..
out/kkj demos/hello.kkj
```

## Using prelude

To run most of the demos you should include prelude. For example

```bash
out/kkj demos/prelude.kkj demos/factorial.kkj
```
