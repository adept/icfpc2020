# icfpc2020 - team Just No Pathfinding

## Useful links

* [Competition website](https://icfpcontest2020.github.io/)
* [API](https://icfpc2020-api.testkontur.ru/swagger/index.html)
* [Discord chat](https://discord.com/invite/xvMJbas)
* [Twitter](https://twitter.com/icfpcontest2020)
* [Backstory](https://pegovka.space/)
* [Ocaml base dockerfile](https://github.com/icfpcontest2020/dockerfiles/blob/master/dockerfiles/ocaml/Dockerfile.base)
* [Docs](https://message-from-space.readthedocs.io/en/latest/)
* [Docs repo](https://github.com/zaitsev85/message-from-space)
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
