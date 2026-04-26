# Lab 1 : Regular Expressions and Text Search
*Searching for a pattern in a text file*
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

### The main code (the one that solves the lab)

```bash
dune exec ./main.exe < testcases/testcase1.txt
```

### The homework 2

```bash
dune exec ./homework.exe
```

### The parser tester

```bash
dune exec ./test_parser.exe
```

## Overall structure

```bash
% tree code
code
├── dune
├── dune-project
├── homework.ml
├── main.ml
├── test_parser.ml
├── testcases
│   └── testcase1.txt - ... - testcase5.txt
├── utils
│   ├── alphabet.ml
│   ├── automata.ml
│   ├── basics.ml
│   ├── dfa_minimization.ml
│   ├── dune
│   ├── nfa_cons.ml
│   ├── nfa_dfa.ml
│   ├── parser.ml
│   ├── reg.ml
│   ├── simulation.ml
│   └── viewer.ml
└── viewer/
    ├── dfa.json
    ├── index.html
    ├── nfa.json
    ├── style.css
    └── temp_dfa.json
```
