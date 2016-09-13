<br>
<br>
<br>
<br>

<h1 style="border-bottom:none">LiquidHaskell: Liquid Types for Haskell</b>

<h4 style="text-align:center"><i>Niki Vazou   </i></h4>
<h5 style="text-align:center"><i>(UC San Diego)</i></h5>
<h5 style="text-align:center"><i>(Awake Networks)</i></h5>

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


Well-Typed Programs *Can* Go Wrong
==================================


<div class="hidden">

\begin{code}
main = putStrLn "Easter Egg: to force Makefile"
\end{code}

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



The Heartbleed Bug.
------------------
[Heartbleed](https://en.wikipedia.org/wiki/Heartbleed) is a security bug in the OpenSSL cryptography library (April 2014).
<p align="center">
<img src="http://heartbleed.com/heartbleed.png" alt="The HEartbleed Bug." style="width: 300px;" align="middle" />
</p>
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
<br>
<br>
<br>
<br>
<br>
<br>



The Heartbleed Bug in Haskell
------------------------------

<div class="fragment">
\begin{spec}
λ> :m +Data.Text Data.Text.Unsafe
λ> let text = pack "Niki"
λ> :t takeWord16
    takeWord16 :: Int -> Text -> Text
\end{spec}
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
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

`True` is a bad argument 
-------------------------

<div class="fragment">
\begin{spec}
λ> takeWord16 True text

<interactive>:5:12:
    Couldn't match expected type ‘Int’ with actual type ‘Bool’
    In the first argument of ‘takeWord16’, namely ‘True’
    In the expression: takeWord16 True text
\end{spec}
</div>

<br>
<br>
<br>
<br>
<br>

But, `10` is a good argument 
-------------------------

Reveal `6` extra characters...
<br>
<div class="fragment">
\begin{spec}
λ>  takeWord16 10 text
"Niki\33624\5479\SOH\NUL\60480\5115"
\end{spec}
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



Partial Functions
------------

<div class="fragment">
\begin{spec}
λ> :t head
head :: [a] -> a

λ> head "Facebook"
'F'
\end{spec}
</div>

<br>

<div class="fragment">
\begin{spec}
λ> head []
*** Exception: Prelude.head: empty list
\end{spec}
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



Functional Correctness
---------------------

<div class="fragment">
<br>
\begin{spec}
λ> fib 1 <= fib 42
False
\end{spec}
</div>

<div class="fragment">
\begin{spec}
λ> mapReduce 42 sum (+) [1..20]
0 
\end{spec}
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



Goal: Extend Type System
------------------------

<br>

<br>

+ To prevent **wider class** of errors

+ To enforce **program specific** properties

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







Plan
----

<br>

1. [**Refinements Types**](02-refinements.html)
2. [**Data Types**](03-datatypes.html)
3. [**Termination**](04-termination.html)
4. [**Reflection**](05-refinement-reflection.html)
5. [**Case Study: MapReduce**](06-mapReduce.html)


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

+ 10KLoc / 56 Modules

+ Memory Safety, Termination, Functional Correctness

<br>

<div class="fragment">
**Inference is Crucial**
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

Evaluation
----------


<img src="img/code-spec-indiv.png" height=250px>


+ **Specifications:** 1 / 10 LOC  (*ok*)

+ **Compile Time:**  1s / 10 LOC  (*not ok!*)

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

**Refinement Types:** Automated Dependent Typing via SMT

<br>

<div class="fragment">

-------------------       ------------------------------------------------
**Properties:**           Predicates  *+ Types*
**Proofs:**               SMT Solvers *+ Subtyping*
**Inference:**            Abstract Interpretation *+ Hindley-Milner*
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




Thank You!
----------

<br>
<br>

`cabal install liquidhaskell`
<br>
[`http://www.refinement-types.org`](http://www.refinement-types.org)
<br>
[online demo @ http://goto.ucsd.edu/liquidhaskell](http://goto.ucsd.edu/liquidhaskell)

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
