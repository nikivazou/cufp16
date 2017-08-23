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
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Liquid Haskell: Verification of Haskell Code
=======================================================
<br>
<br>
<br>
<p align="center">
**Motivation:** _Why_ verification?
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


Software bugs are Everywhere
=============================
<p align="center">
“Airbus A400M crashed due to a software bug.”
</p>
<p align="center">
— May 2015
</p>
<p align="center">
<img src="https://si.wsj.net/public/resources/images/BN-NP763_aircut_P_20160419072444.jpg" alt="Plane" style="width: 300px;" align="middle" />
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
<br>
<br>

Software bugs are Everywhere
=============================
<p align="center">
“[Heartbleed](https://en.wikipedia.org/wiki/Heartbleed): a security bug in the OpenSSL cryptography library.”
</p>
<p align="center">
— April 2014
</p>
<p align="center">
<img src="http://heartbleed.com/heartbleed.png" alt="The Heartbleed Bug." style="height: 260px;" align="middle" />
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
<br>
<br>

How The Heartbleed Bug Works
---------------------------------
<p align="center">
<img src="http://imgs.xkcd.com/comics/heartbleed_explanation.png" alt="How The Heartbleed Bug Works" style="width: 500px;" align="middle" />
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


Goal: Make Bugs Difficult to Express
==================================

<br>
<br>

Using Modern Programming Languages (e.g., Haskell, Scala, Ocaml, F#).

<br>
<br>

Because of Strong Types & Lambda Calculus.

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

Via compile-time sanity checks
=====================================
<p align="center">
<img src="http://goto.ucsd.edu/~nvazou/images/lambda-man.png" alt="Lambda Man." style="height: 260px;" align="middle" />
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
<br>
<br>

Fact Check: Haskell VS. Heartbleed
---------------------------------
<p align="center">
<img src="http://goto.ucsd.edu/~nvazou/images/haskellbleed.png" alt="Haskell vs Heartbleed" style="height: 260px;" align="middle" />

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




The Heartbleed Bug in Haskell
------------------------------

<div class="fragment">
\begin{spec}
λ> :m +Data.Text Data.Text.Unsafe
λ> let text = pack "hat"
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

<br>
Reveal `7` extra characters...
<br>
<div class="fragment">
\begin{spec}
λ>  takeWord16 10 text
"hat\33624\5479\SOH\NUL\60480\5115\5479"
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



More Bugs: Program Equivalence
------------

<br>
<div class="fragment">
\begin{spec}
λ> sum  [1..1000]
500500

λ> psum [1..1000]
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
4. [**Refinement Reflection**](05-refinement-reflection.html)
5. [**Case Study: Verification MapReduce**](07-mapReduce.html)
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

1. **Refinements:** Types + Predicates
2. **Subtyping:** SMT Implication
3. **Measures:** Specify Properties of Data
4. **Termination:** Use Logic to Prove Termination
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





Thank You!
----------

<br>

[https://github.com/ucsd-progsys/liquidhaskell](https://github.com/ucsd-progsys/liquidhaskell)

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


</div>