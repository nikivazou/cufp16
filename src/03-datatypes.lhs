<div class="hidden">

\begin{code}
{-# LANGUAGE TupleSections    #-}
{-@ LIQUID "--no-warnings"    @-}
{-@ LIQUID "--short-names"    @-}
{-@ LIQUID "--no-termination" @-}
{- LIQUID "--totality"       @-} -- totality does not play well with record selectors
{-@ LIQUID "--diff"           @-}

module DataTypes where
import qualified Data.Text        as T
import qualified Data.Text.Unsafe as T 

import Prelude hiding (length, sum, take)
import qualified Data.Set as S -- hiding (elems, insert)


head       :: List a -> a
tail       :: List a -> List a
impossible :: String -> a
avg        :: List Int -> Int


sum :: List Int -> Int 
sum Emp = 0 
sum (x ::: xs) = x + sum xs
infixr 9 :::

{-@ data List [length] a = Emp | (:::) {hd :: a, tl :: List a } @-}
{-@ invariant {v: List a | 0 <= length v} @-}

{-@ type Nat      = {v:Int | v >= 0} @-}
{-@ type Pos      = {v:Int | v >  0} @-}

{-@ impossible :: {v:_ | false} -> a @-}
impossible = error

{-@ safeDiv :: Int -> {v:Int | v /= 0} -> Int   @-}
safeDiv :: Int -> Int -> Int 
safeDiv _ 0 = impossible "divide-by-zero"
safeDiv x n = x `div` n

\end{code}

</div>

<br>
<br>
<br>
<br>
<br>



Data Types
==========

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


Example: Lists
--------------

<br>

<div class="fragment">
Lets define our own `List` data type:

<br>

\begin{code}
data List a = Emp               -- Nil
            | (:::) a (List a)  -- Cons
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



Specifying the Length of a List
-------------------------------

<br>

<div class="fragment">
**Measure**

Haskell function with *a single equation per constructor*
</div>

<br>

\begin{code}
{-@ measure length @-}
length :: List a -> Int
length Emp        = 0
length (_ ::: xs) = 1 + length xs
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



Specifying the Length of a List
-------------------------------


<br>

**Measure**

*Strengthens* type of data constructor

<br>

<div class="fragment">

\begin{spec} <div/>
data List a where

  Emp   :: {v:List a | length v = 0}

  (:::) :: x:a -> xs:List a
        -> {v:List a | length v = 1 + length xs}
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


Using Measures
==============

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>





Example: *Partial* Functions
-----------------------------

<br>

Fear `head` and `tail` no more!

<div class="fragment">
\begin{code}
{-@ head        :: List a -> a @-}
head (x ::: _)  = x
head _          = impossible "head"

{-@ tail        :: List a -> List a @-}
tail (_ ::: xs) = xs
tail _          = impossible "tail"
\end{code}

<br> <br>

**Q:** Write types for `head` and `tail` that verify `impossible`.
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


Totality Checking in Liquid Haskell 
------------------------------------

Naming Non-Empty Lists
----------------------

<br>

A convenient *type alias*

<br>

\begin{code}
{-@ type ListNE a = {v:List a| 0 < length v} @-}
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

<div class="slideonly">

`head` and `tail` are Safe
--------------------------

When called with *non-empty* lists:

<br>

\begin{spec}
{-@ head :: ListNE a -> a @-}
head (x ::: _)  = x
head _          = impossible "head"

{-@ tail :: ListNE a -> List a @-}
tail (_ ::: xs) = xs
tail _          = impossible "tail"
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

Back to `average`
---------------------------------

<br>

\begin{code}
{-@ avg    :: List Int -> Int @-}
avg xs     = safeDiv total n
  where
    total  = sum    xs
    n      = length xs         -- returns a Nat
\end{code}

<br> 
**Q:** Write type for `avg` that verifies safe division.


Safe List Indexing
---------------------------------
\begin{code}
take :: Int -> List a -> List a 
{-@ take :: i:Int -> xs:List a -> List a @-} 
take i (x ::: xs) = if i == 0 then Emp else x ::: (take (i-1) xs)
take i Emp        = impossible "Out of bounds indexing!" 
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

Totality Checking 

Example 
-------

HeartBleed
----------

\begin{code}
{-@ assume T.takeWord16 :: i:{Int | 0 <= i } -> {v:Data.Text.Internal.Text | i <= slen v} -> Data.Text.Internal.Text
@-}
safeTake   = T.takeWord16 2  (T.pack "Niki")
unsafeTake = T.takeWord16 10 (T.pack "Niki")
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


Recap
-----

<br>
<br>

1. **Refinements:** Types + Predicates
2. **Subtyping:** SMT Implication
3. <div class="fragment">**Measures:** Specify Properties of Data</div>

<br>

<div class="fragment">
**Next:** [Abstract Refinements](04-abstract-refinements.html)
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
