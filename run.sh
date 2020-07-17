#!/bin/sh

eval "$(opam env)"
app/_build/default/app/bin/main.exe run "$@" || echo "run error code: $?"
