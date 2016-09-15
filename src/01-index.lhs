<br>
<br>
<br>
<br>

<h1 style="text-align:center">LiquidHaskell @ Awake Networks</b>

<h4 style="text-align:center"><i>Niki Vazou   </i></h4>
<h5 style="text-align:center"><i>(UC San Diego)</i></h5>
<h5 style="text-align:center"><i>(Awake Networks)</i></h5>
<br> <br>
<h5 style="text-align:center"><i>(Summer 2016)</i></h5>

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

LiquidHaskell at Awake Networks
===============================
<br>

- [Awake Networks](http://www.awakenetworks.com/) builds a network security and analytics platform, 

- using Haskell to parse raw data into appropriate data structures. 

- Liquid Haskell is used to _verify_ correctness of Awake's Haskell code
    - All functions are total and terminating. 
    - No String indexing violations  

<br>

**Motivation:** _Why_ verification?

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
<br>

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

λ> head "Awake"
'A'
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
4. [**Awake Experience**](07-awake.html)


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
