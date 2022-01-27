# VM & Compilateur en Clisp

## Description du projet

Ce projet est consacré à l'implémentation d'une machine virtuelle (VM) ainsi que d'un compilateur en Clisp.
La VM doit pouvoir charger du code assembleur ayant été généré par le compilateur à partir d'un programme Clisp et ainsi l'exécuter.


### Guide d'exécution 

Dans le dossier courant, il faut effectuer les instructions ci-dessous depuis votre terminal:

```clisp 
(load "VM.lisp")
(load "Compilateur.lisp")
```

Pour pouvoir compiler un programme Clisp contenu dans un fichier et ainsi l'exécuter:
```clisp
(file-exec "<nom-du-fichier>")
```

Un fichier de tests "tests.lisp" est fourni pour traiter différents programmes Clisp.
```clisp
(load "tests.lisp")
```

Exemple d'exécution pour un fichier:
```clisp
(file-exec "fact.lisp")
```
