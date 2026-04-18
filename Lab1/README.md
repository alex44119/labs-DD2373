# Lab 1 : Regular Expressions and Text Search
*Searching for a pattern in a text file*
---

## Comment exécuter les executables ?

```bash
dune exec ./main.exe
```

```bash
dune exec ./test_parser.exe
```

## Setup

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

## Structure du dossier `code`

```bash
% tree code
code
├── dune
├── dune-project
├── main.ml
├── test_parser.ml
├── testcases
│   ├── testcase1.txt
│   ├── testcase2.txt
│   ├── testcase3.txt
│   ├── testcase4.txt
│   └── testcase5.txt
└── utils
    ├── dune
    ├── alphabet.ml
    ├── automata.ml
    ├── basics.ml
    ├── dfa_minimization.ml
    ├── nfa_dfa.ml
    ├── parser.ml
    └── reg.ml
```

## Comment afficher un shell pour debugger/tester rapidement du ocaml ?

Fais :
```bash
dune utop
```

Ça va t'ouvrir un shell, dans lequel tu peux exécuter des lignes ocaml de ton choix. Tu peux ensuite ouvrir n'importe quel fichier pour tester que tout va bien :

```ocaml
open Utils.Reg;;
let exemple : char reg = Concat(Star(Lit('a')), Or(Lit('b'), Lit('c')));;
```

ce qui équivaut à :

```ocaml
open Utils.Parser;;
let exemple2 : char reg error_option = parser "a*(b|c)";;
```

Ou encore tu peux directement exécuter `main.ml` pour mieux débugger :

```ocaml
#use "main.ml";;
```

## Pour s'y retrouver :

### 3.1 Reading and parsing the regular expression

- [parser.ml](code/utils/parser.ml)
- [reg.ml](code/utils/reg.ml)

### 3.2 Construction of the ϵ-NFA

### 3.3 Converting the ϵ-NFA into an equivalent DFA

- [nfa_dfa.ml](code/utils/nfa_dfa.ml)
- [automata.ml](code/utils/automata.ml)

### 3.4 Minimizing the DFA

- [dfa_minimization.ml](code/utils/dfa_minimization.ml)

### 3.5 Simulating the DFA