# Octo-syn

Octo's programs synthetiser and shellcode generator.

## What is Octo-syn ?

[Octo](https://github.com/codeanonorg/Octo) is a tool designed to assist the construction of [shellcodes](https://www.wikiwand.com/en/Shellcode).
Our goal is to embed a **program synthetiser** inside Octo, allowing users to automatically generate shellcodes fiting their needs.
In particular, shellcodes needs to satisfy strong constraints in order to be efficient (no null bytes, only alpha numerical characters, ...).

**Octo-syn** is a first experimental implementation of Octo's programs synthetiser. It is a work in progress and we absolutely don't know where this experiment will lead us.

## Ideas behind Octo-syn

1. Using reasearch algorithms and solvers
  + We try to generate programs satisfying constraints
2. Formal Methods
  + we use formal semantics to guide the research algorithm(s)
  + we'd like to prove that generated programs are correct with respect to a functional specification


## Dependencies

**Octo-syn** requires [keystone](https://github.com/keystone-engine/keystone) and [ocaml bindings](https://github.com/keystone-engine/keystone/tree/master/bindings/ocaml) to be installed.

## Compilation

```
dune build
```