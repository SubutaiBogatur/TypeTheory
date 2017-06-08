# TypeTheory
Homeworks for Type Theory course as continuation of course on [Mathematical Logic](https://github.com/SubutaiBogatur/MathematicalLogic), Spring 2017, ITMO University. 

During the Type Theory course we have been studying a range of different type systems including [*typed primitive lambda calculus*](https://en.wikipedia.org/wiki/Church_encoding), [*System F*](https://en.wikipedia.org/wiki/System_F), [*Hindley-Milner type system*](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system) and systems with [dependent types](https://en.wikipedia.org/wiki/Dependent_type). Presented homeworks mostly cover mentioned topics. Homeworks were done in two languages: *OCaml* and *Idris*.  

Homeworks done in *OCaml* are:

* Introduction to functional programming and *OCaml* language: basic operations in *Peano arithmetics* including `add`, `mul`, `pow`, `sub`, etc and basic operations with lists: `reverse` and `mergesort`. 

* Operations in *primitive lambda calculus*: parser of lambda terms, *alpha-equivalence* check and step-by-step *beta-reduction* of a lambda term. 

* Then [Robinson's unification algorithm](https://en.wikipedia.org/wiki/Unification_(computer_science)#A_unification_algorithm) was written to be able to solve systems of equations with *algebraic terms*. Moreover, some other operations with *algebraic terms* are presented.

* The final task was to implement famous [algorithm W](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Algorithm_W) for type inference in *Hindley-Milner type system*. Moreover, algorithm for type inference in *primitive lambda calculus* was also done as part of the homework. 

*Idris* is known as the language with dependent types, where types can depend on terms. As a result, its type system is extremely powerful and allows to prove statements and theorems. Commutativity of addition was proved in *Idris* language.

Further reading: [conspect of our course covering preceding topics](https://github.com/artemohanjanyan/tt-conspect). 
