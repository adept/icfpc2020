#!/bin/sh

eval "$(opam env)"
dune build --build-dir $(pwd)/app/_build --release app/bin/main.exe
