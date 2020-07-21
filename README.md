
## setup

```bash
$ opam switch create . --deps-only --locked
$ opam install . --deps-only --with-test --locked
```

## dev

```bash
$ dune build
$ dune runtest
```

### format

```bash
$ dune build @fmt --auto-promote
```

### update deps

Firstly edit dune-project's `depends` and dune file's `libraries` then run `opam lock` command.

```bash
$ ls dune-project */dune
$ opam lock
```
