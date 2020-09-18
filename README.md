# Octo-syn

Theoretical framework for Octo's programs synthetiser

## What is Octo-syn ?

[Octo](https://github.com/codeanonorg/Octo) is a tool designed to assist the construction of [shellcodes](https://www.wikiwand.com/en/Shellcode).
Our goal is to embed a **program synthetiser** inside Octo, allowing users to automatically generate shellcodes fiting their needs.
In particular, shellcodes needs to satisfy strong constraints in order to be efficient (no null bytes, only alpha numerical characters, ...).

**Octo-syn** is a first experimental implementation of Octo's programs synthetiser. It is a work in progress and we absolutely don't know where this experiment will lead us.


## Ideas behind Octo-syn

Octo-syn relies strongly on two domains

1. Reasearch algorithms and solvers
  + We try to generate programs satisfying constraints
2. Formal Methods
  + we use formal semantics to guide the research algorithm
  + We'd like to prove that generated programs are correct 
