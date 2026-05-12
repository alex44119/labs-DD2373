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

### The main code (the one that solves the lab)

```bash
dune exec ./main.exe "testcases/Simple/simple.spec" "testcases/Simple/simple.cfg"
```
```bash
dune exec ./main.exe "testcases/EvenOdd/EvenOdd1b.spec" "testcases/EvenOdd/EvenOdd.cfg"
```
```bash
dune exec ./main.exe "testcases/EvenOdd/EvenOdd1a.spec" "testcases/EvenOdd/EvenOdd.cfg"
```

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
├── testcases/[...]
├── utils
│   ├── cfg_cons.ml
│   ├── cfg_emptiness.ml
│   ├── cfg.ml
│   ├── dfa_parser.ml
│   ├── dfa.ml
│   ├── dune
│   ├── flowgraph_parser.ml
│   ├── flowgraph.ml
│   ├── server.ml
│   └── viewer.ml
└── viewer
    ├── dfa.json
    ├── flowgraph.json
    ├── index.html
    └── style.css
```