#!/bin/sh

eval "$(opam env)"
app/_build/default/app/bin/main.exe battle -api-key 7ec9764077154f0bb95eecdf09e31eed "$@" || echo "run error code: $?"
