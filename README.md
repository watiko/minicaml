
## setup

```bash
$ opam switch create . --deps-only --locked
$ opam install . --deps-only --with-test --locked
```

### inside Docker without privileged

```bash
$ opam init --bare --no-setup --disable-sandboxing --reinit
$ opam switch create . --deps-only --locked
$ opam install . --deps-only --with-test
```

## dev

```bash
$ dune build
$ dune runtest
```

### try REPL

```bash
$ dune exec minicaml
$ rlwrap dune exec minicaml
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
