# mini-gcc


Compiler of mini-c language, a subset of C language.

## Authors

Cauim de Souza Lima  
Victor Hugo Vianna

## Requirements

ocamlbuild

## Instructions

To compile:
```
make
```
To run:
```
./mini-c <input mini-C file>
```
An output assembly file will be generated with the same name and in the same directory as the input.  
To run the tests:
```
cd tests
./run -all ../mini-c
```

## Language

- All declarations must be done in the beggining of each function (first declare, then assign).
-  Variables can be either ints or pointers to structs.
- More information can be found [here](https://www.enseignement.polytechnique.fr/informatique/INF564/projet/sujet-v1.pdf).
