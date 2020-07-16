#!/bin/sh

eval "$(opam env)"
app/_build/default/app/bin/main.exe "$@" || echo "run error code: $?"
