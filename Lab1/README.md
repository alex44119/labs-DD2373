# Lab 1 : Regular Expressions and Text Search
*Searching for a pattern in a text file*
---

## Setup Alex

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
├── testcases
│   ├── testcase1.txt
│   ├── testcase2.txt
│   ├── testcase3.txt
│   ├── testcase4.txt
│   └── testcase5.txt
└── utils
    ├── dune
    ├── parser.ml
    └── reg.ml
```

## Comment exécuter `main.ml` ?

Depuis la racine :
```bash
dune build
```

Et ensuite :
```bash
dune exec ./main.exe
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

## TODO (avant de continuer)

Changer `state` par `int` et faire en sorte que ce soit toujours un int en utilisant "Z.arith"

Trier bazar.ml dans `utils`