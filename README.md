# icfpc2020 - team Just No Pathfinding

## Useful links

* [Competition website](https://icfpcontest2020.github.io/)
* [Discord chat](https://discord.com/invite/xvMJbas)
* [Twitter](https://twitter.com/icfpcontest2020)
* [Backstory](https://pegovka.space/)
* [The docs???](https://message-from-space.readthedocs.io/en/latest/)
* [Backstory Twitter](https://twitter.com/ivanzaitsev85)

## Build setup

It's [dune](dune.build).

Only the `app/` directory is writable during the build, so the `build.sh` script
actually sets up dune so that it builds in `app/_build`.

## Test setup

```
$ opam install ppx_expect
$ opam install ppx_inline_test
$ dune test
$ dune promote # accept the .corrected file
```

## Ocamlformat setup

Follow the [usual
instructions](https://github.com/ocaml-ppx/ocamlformat#emacs-setup) to get
ocamlformat setup.
