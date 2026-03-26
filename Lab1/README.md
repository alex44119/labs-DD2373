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

Voici mon output généré (Si t'as tout pareil c'est que ça fonctionne !):
```
input  : ""                         
raw : Eps
output : ε
-----------------------------
input  : "a"
raw : Lit('a')
output : a
-----------------------------
input  : "ε"
raw : Eps
output : ε
-----------------------------
input  : "."
raw : Any
output : .
-----------------------------
input  : "ab"
raw : Concat(Lit('a'), Lit('b'))
output : ab
-----------------------------
input  : "a|b"
raw : Or(Lit('a'), Lit('b'))
output : a|b
-----------------------------
input  : "(a|b)*"
raw : Star(Or(Lit('a'), Lit('b')))
output : ((a|b))*
-----------------------------
input  : "a+"
raw : Concat(Lit('a'), Star(Lit('a')))
output : aa*
-----------------------------
input  : "a?"
raw : Or(Lit('a'), Eps)
output : a|ε
-----------------------------
input  : "a*?"
raw : Or(Star(Lit('a')), Eps)
output : a*|ε
-----------------------------
input  : "a+*"
raw : Star(Concat(Lit('a'), Star(Lit('a'))))
output : ((aa*))*
-----------------------------
input  : "(ab)?*"
raw : Star(Or(Concat(Lit('a'), Lit('b')), Eps))
output : ((ab|ε))*
-----------------------------
input  : "a(b|c)"
raw : Concat(Lit('a'), Or(Lit('b'), Lit('c')))
output : a(b|c)
-----------------------------
input  : "ab|c"
raw : Or(Concat(Lit('a'), Lit('b')), Lit('c'))
output : ab|c
-----------------------------
input  : "a|bc"
raw : Or(Lit('a'), Concat(Lit('b'), Lit('c')))
output : a|bc
-----------------------------
input  : "()"
raw : empty parentheses are not allowed
output : Error: empty parentheses are not allowed
-----------------------------
input  : "|"
raw : expected an expression
output : Error: expected an expression
-----------------------------
input  : "a|"
raw : expected an expression after '|'
output : Error: expected an expression after '|'
-----------------------------
input  : "|a"
raw : expected an expression
output : Error: expected an expression
-----------------------------
input  : "*a"
raw : expected an expression
output : Error: expected an expression
-----------------------------
input  : "(a"
raw : missing closing ')'
output : Error: missing closing ')'
-----------------------------
input  : "a)"
raw : unexpected ')' at top level
output : Error: unexpected ')' at top level
-----------------------------
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