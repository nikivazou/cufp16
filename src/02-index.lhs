<br>
<br>
<br>
<br>

<h1 style="text-align:center">Liquid Haskell: 
<br> Verification of Haskell Code</b>

<h4 style="text-align:center"><i>Niki Vazou   </i></h4>
<h5 style="text-align:center"><i>(University of Maryland)</i></h5>

<br>
<br>
<br>
<br>
<h6 style="text-align:center">
[http://goto.ucsd.edu/~nvazou/presentations/shonan17/02-index.html](http://goto.ucsd.edu/~nvazou/presentations/shonan17/02-index.html) </h6>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Resourses
----------

<br>

[github @ https://github.com/ucsd-progsys/liquidhaskell](https://github.com/ucsd-progsys/liquidhaskell)

<br>

[book @ `http://www.refinement-types.org`](http://www.refinement-types.org)

<br>

[online demo @ http://goto.ucsd.edu/liquidhaskell](http://goto.ucsd.edu/liquidhaskell)

<br>

`cabal install liquidhaskell`
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


Plan
----

<br>
**DONE** 

0. [**Liquid Haskell 101**](http://goto.ucsd.edu/~nvazou/presentations/shonan17/LiquidHaskell101.pdf)
1. [**Refinements Types:**](02-refinements.html) Types + Predicates + SMT
2. [**Data Types:**](03-datatypes.html) Measures for Structural Properties
3. [**Termination**](04-termination.html) Semantic Termination Checking
4. [**Refinement Reflection**](05-refinement-reflection.html) Expressiveness 
<br>
<br>

**Coming up** 

5. [**Proof Automation: **](06-structural-induction.html) Lists are Monads
6. [**Case Study: **](07-mapReduce.html) Verification MapReduce
<br>
<br>
7. [**Case Study: **](09-insert-sort.html) List Sorting
8. [**Abstract Refinements**](10-abstract-refinements.html)
9. [**Bounded Refinements**](11-abstract-refinements.html)
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

<div class="hidden">


Recap
-----

<br>
<br>

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Evaluation
----------

<br>

+ Diverse Code Bases

+ 20KLoc

+ Memory Safety, Termination, Functional Correctness, Program Equivalence

+ **Specifications:** 1 / 10 LOC  

<br>

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


<div class="hidden">

Evaluation
----------


**Library**                     **LOC**     **Specs**      **Time**
---------------------------   ---------   -----------    ----------
`XMonad.StackSet`                   256            74          27s
`Data.List`                         814            46          26s
`Data.Set.Splay`                    149            27          27s
`Data.Vector.Algorithms`           1219            76          89s
`HsColour`                         1047            19         196s
`Data.Map.Base`                    1396           125         174s
`Data.Text`                        3128           305         499s
`Data.Bytestring`                  3505           307         294s
**Total**                     **11512**       **977**    **1336s**
---------------------------   ---------   -----------    ----------

</div>





Conclusion
----------

<br>

**Liquid Types:** Automated Verification via SMT

<br>

<div class="fragment">

-------------------       ------------------------------------------------
**Properties:**           Predicates  *+ Types*
**Proofs:**               SMT Solvers *+ Subtyping*
-------------------       ------------------------------------------------

</div>


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>







</div>