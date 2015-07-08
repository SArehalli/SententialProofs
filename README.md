# SententialProofs
Haskell implementation of a proof checker for Formal Sentential-Logic Proofs. Inspired by UCSD's PHIL 10 Course.

## Usage

To compile, simply run
```
ghc proof.hs
```

Given a properly formatted file `file.txt`, run
```
./proof file.txt
```

## Proof Format

Operators:

name | symbol
--- | ---
and  | ^
or   | v
conditional/implication | >
biconditional | =
not | ~
Parentheses | ()

Implemented Logical Rules:

rule | abbreviation
----- | -------------
Conditional Exchange | CE
Biconditional Exchange | BE
Duplication | Dup
Contraposition | Contra
Distribution | Dist
Commutation | Commute
Association | Assoc
DeMorgan's Law | DeM
Double Negation | DNeg

Basic Format:
```
[Assumption]
[Statement 1];[Rule for Assumption to 1]
[Statement 2];[Rule for 1 to 2]
...
[Statement n];[Rule for n-1 to n]
```

See the test* files for further examples.

_Note that as of right now, no multiline/nonlinear rules have been implemented, so parsing is purely sequential; i.e., line n must imply line n+1. In future versions with Modus Ponens et al, this will change._
