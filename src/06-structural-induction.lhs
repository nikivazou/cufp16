<div class="hidden">

\begin{code}
{-# LANGUAGE TupleSections    #-}
{-@ LIQUID "--no-warnings"    @-}
{-@ LIQUID "--short-names"    @-}
{- LIQUID "--diff"           @-}


-- Hidden code 
{-@ LIQUID "--higherorder"     @-}
{-@ LIQUID "--totality"        @-}

module RefinementReflection where
import Language.Haskell.Liquid.ProofCombinators

fib :: Int -> Int
propPlusAccum :: Int -> Int -> Proof 
propOnePlusOne :: () -> Proof 
onePlusOne :: () -> Proof 
fibOne :: () -> Proof 
fibTwo :: () -> Proof
fibEq  :: () -> Proof  
fibCongr :: Int -> Int -> Proof
fibUp :: Int -> Proof 
fibThree :: () -> Proof 
fMono :: (Int -> Int)
      -> (Int -> Proof)
      -> Int
      -> Int 
      -> Proof 
fibMono :: Int -> Int -> Proof 
fibMonotonic :: Int -> Int -> Proof 

\end{code}

</div>

<br>
<br>
<br>
<br>
<br>



Refinement Reflection
=====================
<br>
<br>
Allow terminating **Haskell** functions into the logic! 
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Theorems about Haskell functions
--------------------------------

<br>
<p align="center">
A. Farmer *et al*: Reasoning with the HERMIT 
<br><br>
<img src="http://goto.ucsd.edu/~nvazou/images/hermit_laws.png" alt="Hermit Laws" style="width: 350px;" align="middle" />
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


Theorems about Haskell functions
--------------------------------

<br>
<br>
<br>
Can we express the above theorems in Liquid Haskell?
<br>
<br>

  - Express & Prove Theorems **in** Haskell ...
  - ... **for** Haskell functions. 
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


Types As Theorems 
-----------------
<br>

+ Liquid Types express theorems, and 

+ Haskell functions express proofs.

<br>
\begin{code}
{-@ onePlusOne :: () -> {v:() | 1 + 1 == 2 } @-}
onePlusOne _ = ()
\end{code}
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Make the theorems pretty!
--------------------------

<br>

[`ProofCombinators`](https://github.com/ucsd-progsys/liquidhaskell/blob/develop/include/Language/Haskell/Liquid/ProofCombinators.hs) comes with Liquid Haskell 
and allows for pretty proofs! 

<br>
\begin{code}
-- import Language.Haskell.Liquid.ProofCombinators 

{-@ propOnePlusOne :: () ->  {v: Proof | 1 + 1 == 2} @-}
propOnePlusOne _ = trivial
\end{code}

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Make the theorems even prettier!
--------------------------------

<br>

[`ProofCombinators`](https://github.com/ucsd-progsys/liquidhaskell/blob/develop/include/Language/Haskell/Liquid/ProofCombinators.hs) comes with Liquid Haskell 
and allows for pretty proofs! 

<br>
\begin{code}
{-@ propOnePlusOne' :: _ ->  { 1 + 1 == 2 } @-}
propOnePlusOne' _ = trivial *** QED 
\end{code}

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Use more SMT knowledge
--------------------------------

<br>

[`ProofCombinators`](https://github.com/ucsd-progsys/liquidhaskell/blob/develop/include/Language/Haskell/Liquid/ProofCombinators.hs) comes with Liquid Haskell 
and allows for pretty proofs! 

<br>
\begin{code}
{-@ propPlusAccum :: x:Int -> y:Int -> { x + y == y + x } @-}
propPlusAccum _ _ = trivial *** QED 
\end{code}

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Theorems about Haskell functions
--------------------------------

<br>
<br>
<br>
Can we express them in Liquid Haskell?
<br>
<br>

  - Express & Prove Theorems in Haskell...
  - ... **for Haskell functions.** 
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Refinement Reflection
--------------------------
<br>
Reflect terminating `fib` in the logic.
<br>

\begin{code}
{-@ reflect fib @-}
{-@ fib :: i:Nat -> Nat @-}
fib i | i == 0    = 0 
      | i == 1    = 1 
      | otherwise = fib (i-1) + fib (i-2)
\end{code}
<br>

Now `fib` can live in the Liquid Types!
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

`fib` is an uninterpreted function 
--------------------------
<br>
For which logic only knows the congruence axiom... 
<br>

\begin{code}
{-@ fibCongr :: i:Nat -> j:Nat -> {i == j => fib i == fib j} @-}
fibCongr _ _ = trivial *** QED 
\end{code}

<br>

... and nothing else 

<br>
\begin{code}
{-@ fibOne :: () ->  {fib 1 == 1 } @-}
fibOne _ = trivial *** QED 
\end{code}

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Reflection at Result Type
----------------------------------------------------------

<br>
The type of `fib` connects logic & Haskell implementation 
<br>

\begin{spec}
fib :: i:Nat -> {v:Nat | v == fib i && v == propFib i}

propFib i = if i == 0 then 0 else   
            if i == 1 then 1 else 
            fib (i-1) + fib (i-2)
\end{spec}

<br>
<br>

Calling `fib i` reveals its implementation into the logic!
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Reflection at Result Type
----------------------------------------------------------

<br>
<br>
\begin{code}
{-@ fibEq :: () ->  {fib 1 == 1 } @-}
fibEq _ = [fib 1] *** QED 
\end{code}
<br>
<br>

**Q:** Can you prove that `fib 2 == 1`?
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Structuring Proofs 
----------------------------------------------------------
<br>
<br>
Using combinators from [`ProofCombinators`](https://github.com/ucsd-progsys/liquidhaskell/blob/develop/include/Language/Haskell/Liquid/ProofCombinators.hs)! 

<br>
<br>
\begin{code}
{-@ fibTwo :: _ -> { fib 2 == 1 } @-}
fibTwo _ 
  =   fib 2 
  ==. fib 1 + fib 0 
  *** QED

\end{code}
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Reusing Proofs: The because operator
----------------------------------------------------------
<br>
<br>
Using combinators from [`ProofCombinators`](https://github.com/ucsd-progsys/liquidhaskell/blob/develop/include/Language/Haskell/Liquid/ProofCombinators.hs)! 

<br>
<br>
\begin{code}
{-@ fibThree :: _ -> { fib 3 == 2 } @-}
fibThree _ 
  =   fib 3 
  ==. fib 2 + fib 1
  ==. 1     + 1      ∵ fibTwo ()
  ==. 2 
  *** QED

\end{code}
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Pencil & Paper like Proofs
----------------------------------------------------------
`fib` is increasing 
<br>
<br>
\begin{code}
{-@ fibUp :: i:Nat -> {fib i <= fib (i+1)} @-}
fibUp i
  | i == 0
  =   fib 0 <. fib 1
  *** QED
  | i == 1
  =   fib 1 <=. fib 1 + fib 0 <=. fib 2
  *** QED
  | otherwise
  =   fib i
  ==. fib (i-1) + fib (i-2)
  <=. fib i     + fib (i-2) ∵ fibUp (i-1)
  <=. fib i     + fib (i-1) ∵ fibUp (i-2)
  <=. fib (i+1)
  *** QED

\end{code}
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>



Another Pencil & Paper like Proof
----------------------------------------------------------
Can you prove that `fib` is monotonic?
<br>
<br>
\begin{code}
{-@ fibMonotonic :: x:Nat -> y:{Nat | x < y }  -> {fib x <= fib y} / [y] @-}
fibMonotonic x y 
  = trivial 
\end{code}
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Hints

  - case analysis on `y == x + 1`
  - recursive call on `x` and `y-1`
  - invokation of `fibUp` on `x` and `y-1`

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>




Another Pencil & Paper like Proof
----------------------------------------------------------
Proof that `fib` is monotonic?
<br>
<br>
\begin{spec}
{-@ fibMonotonic :: x:Nat -> y:{Nat | x < y }  -> {fib x <= fib y} / [y] @-}
fibMonotonic x y 
  | y == x + 1 
  =   fib x 
  <=. fib (x+1) ∵ fibUp x 
  <=. fib y 
  *** QED  
  | x < y - 1 
  =   fib x 
  <=. fib (y-1) ? fibMonotonic x (y-1)
  <=. fib y     ? fibUp (y-1) 
  *** QED   
\end{spec}
<br>
<br>

**Note:** Proof is `fib` irrelevant! 
Abstract all `fib` info away!
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>



Higher Order Theorems
----------------------------------------------------------
Increasing implies monotonic!
<br>
<br>
\begin{code}
{-@ fMono :: f:(Nat -> Int)
          -> fUp:(z:Nat -> {f z <= f (z+1)})
          -> x:Nat
          -> y:{Nat|x < y}
          -> {f x <= f y} / [y] @-}
fMono f fUp x y  
  | x + 1 == y
  = f x   <=. f (x + 1) ∵ fUp x
          ==. f y       
          *** QED

  | x + 1 < y
  = f x   <=. f (y-1)   ∵ fMono f fUp x (y-1)
          <=. f y       ∵ fUp (y-1)
          *** QED
\end{code}
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Theorem Application
----------------------------------------------------------
<br>
<br>
`fib` is monotonic!
<br>
<br>
\begin{code}
{-@ fibMono :: n:Nat -> m:{Nat | n < m }  -> {fib n <= fib m} @-}
fibMono     = fMono fib fibUp
\end{code}
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Recap
-----

<br>
<br>

1. **Refinements:** Types + Predicates
2. **Subtyping:** SMT Implication
3. **Measures:** Specify Properties of Data
4. **Termination:** Use Logic to Prove Termination
5. **Reflection:** Allow Haskell functions in Logic.
6. <div class="fragment">**Structural Induction:**</div> Proving Theorems on Lists! 

<br>

<div class="fragment">
**Next:** [Case Study: MapReduce](07-mapReduce.html): Program Properties that matter!
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

