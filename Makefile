.SUFFIXES:
.PHONY: all clean day01 day02 day03

# day02 is compiled
all: day02/_build/.db

clean:
	cd day02 && opam exec -- dune clean

## DAY 1
# written in gleam!
day01:
	cd day01 && gleam run

## DAY 2
# written in ocaml!
day02:
	cd day02 && opam exec -- dune exec day02

# set up compilation with make as well
day02/_build/.db: day02/bin/main.ml
	cd day02 && opam exec -- dune build

## DAY 3
# written in racket!
day03:
	cd day03 && racket day03.rkt
