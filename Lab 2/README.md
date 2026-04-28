# Lab 2 : Model Checking Flow Graphs
*Testing if a flow graph meets specification*
---

## Our setup and versions

```bash
% ocaml --version
The OCaml toplevel, version 5.2.1
```

```bash
% opam --version
2.3.0
% eval $(opam env)
```

```bash
% dune --version
3.19.1
```

Extension VSCode : [https://marketplace.visualstudio.com/items?itemName=ocamllabs.ocaml-platform](OCaml Platform)

## How to execute the code ?

### Compilation

```bash
dune build
```

## Overall structure

```bash
% tree code
code
├── dune
├── dune-project
├── main.ml
├── testcases
│   └── EvenOdd
│   └── Simple
│   └── Vote
└── 
```