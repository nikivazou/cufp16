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


import           Data.Word
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.Bits


head       :: List a -> a
tail       :: List a -> List a
headt      :: List a -> a
tailt      :: List a -> List a
impossible :: String -> a
avg        :: List Int -> Int
take       :: Int -> List a -> List a 


sum :: List Int -> Int 
sum N = 0 
sum (C x xs) = x + sum xs

{-@ data List [length] a = N | C {hd :: a, tl :: List a } @-}
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
data List a = N             -- Nil
            | C a (List a)  -- Cons
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
length N        = 0
length (C _ xs) = 1 + length xs
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

  N :: {v:List a | length v = 0}

  C :: x:a -> xs:List a
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
{-@ head     :: List a -> a @-}
head (C x _) = x
head _       = impossible "head"

{-@ tail      :: List a -> List a @-}
tail (C _ xs) = xs
tail _        = impossible "tail"
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


Totality Checking in Liquid Haskell 
------------------------------------


<br>


<div class="fragment">
\begin{code}
{-@ LIQUID "--totality" @-}

{-@ headt      :: List a -> a @-}
headt (C x _)  = x

{-@ tailt      :: ListNE a -> List a @-}
tailt (C _ xs) = xs
\end{code}

<br> <br>

Partial Functions are _automatically_ detected when `totality` check is on!

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

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

In bounds `take`
---------------------------------

Use measures to specify `take`
<br>

\begin{code}

{-@ take :: i:Int -> xs:List a -> List a @-} 
take 0 N        = N 
take i (C x xs) = if i == 0 then N else x `C` (take (i-1) xs)
take i N        = impossible "Out of bounds indexing!" 

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


Catch the The Heartbleed Bug!
-----------------------------

Assuming the library `Text` types...

<br>

\begin{code}

{-@ measure tlen        :: T.Text -> Int                               @-}

{-@ assume T.pack       :: i:String -> {o:T.Text | len i == tlen o }   @-}
{-@ assume T.takeWord16 :: i:Nat -> {v:T.Text | i <= tlen v} -> T.Text @-}

\end{code}


<br>
... HeartBleed **cannot** happen!
<br>

\begin{code}

safeTake   = T.takeWord16 2  (T.pack "Niki")
unsafeTake = T.takeWord16 2 (T.pack "Niki")

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

Safe Indexing at Awake: Specifications
---------------------------------------

<br>

At the library, wrap unsafe String operators with Liquid Types ...

\begin{code}

-- | Get a 16-bit word from the start of a 'B.ByteString', unsafely.

{-@ getWord16 :: {bs:B.ByteString | 2 <= bslen bs } -> Word @-}

\end{code}

<div class="hidden">
\begin{code}
getWord16 :: B.ByteString -> Word
getWord16 bs = b1 .|. b0 where
    b1 = fromIntegral (B.head bs) `shiftL` 8
    b0 = fromIntegral (B.head $ B.drop 1 bs)
\end{code}
</div>

<br>
... Liquid Haskell proves users SAFE.
<br> 
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>



Specification Propagation I 
--------------------------------------------------
<br>

Restrict Valid Inputs with precoditions

<br>
\begin{code}

{-@ readDataA :: {bs:B.ByteString | 2 <= bslen bs } -> Data @-}
readDataA bs 
  = Data (getWord16 bs) (B.drop 2 bs)

{-@ readDataDef :: B.ByteString -> Data @-}
readDataDef bs 
  = Data (toEnum 0) bs  

\end{code}

<br>

Note: `readDataA bs` with `bslen bs < 2` leads to runtime error!

<br> 
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


Specification Propagation II
--------------------------------------------------
<br>

Restrict Valid Inputs with precoditions

<br>
\begin{code}

{-@ readRaw' :: {bs:B.ByteString | 4 <= bslen bs } -> Data @-}
readRaw' msg 
  | isCodeA code 
  = readDataA   rest
  | otherwise
  = readDataDef rest
  where
    code = getWord16 $ msg
    rest = B.drop 2 msg
\end{code}

<br>

Note: `readRaw' bs` with `bslen bs < 4` leads to runtime error!

<br> 
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Specification Validation
--------------------------------------------------
<br>

Check preconditions at run time _only once_!

<br>
\begin{code}

readRaw :: B.ByteString -> Maybe Data
readRaw = checkSize 4 Nothing (Just . readRaw')

\end{code}

<br>

Note: `readRaw bs` with `bslen bs < 4` returns `Nothing`:

- No run time errors
- Minimize run time checks

<br> 
<br>
<br>
<br>
<br>
<br>
<br>
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
\begin{code}
readDataA, readDataDef :: B.ByteString -> Data 

readRaw' :: B.ByteString -> Data

data Data = Data Word B.ByteString 

isCodeA :: Word -> Bool 
isCodeA _ = True        

{-@
checkSize
    :: sz : Int
    -> a
    -> ({ bs : B.ByteString | sz <= bslen bs } -> a)
    -> B.ByteString
    -> a
@-}
checkSize :: Int -> a -> (B.ByteString -> a) -> B.ByteString -> a
checkSize sz no yes bs
  | B.length bs >= sz = yes bs
  | otherwise = no
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

Recap
-----

<br>
<br>

1. **Refinements:** Types + Predicates
2. **Subtyping:** SMT Implication
3. <div class="fragment">**Measures:** Specify Properties of Data</div>

<br>
What properties can be expressed in the logic? 
<br> 

 - Linear Arithmetic, Booleans, Uninterpreted Functions, ... (SMT logic)
 
 - Terminating Haskell functions.

<br>

<div class="fragment">

**Next:** [Termination](04-termination.html)

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
