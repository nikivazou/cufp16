<div class="hidden">

\begin{code}
{-# LANGUAGE TupleSections    #-}
{-@ LIQUID "--no-warnings"    @-}
{-@ LIQUID "--short-names"    @-}
{-@ LIQUID "--diff"           @-}


-- Hidden code 
{-@ LIQUID "--higherorder"     @-}
{-@ LIQUID "--totality"        @-}

module StructuralInduction where

import Prelude hiding (map, id, length, return)
import Language.Haskell.Liquid.ProofCombinators


length :: L a -> Int

mapFusion :: (b -> c) -> (a -> b) -> L a -> Proof

mapId :: L a -> Proof 
emptyLeft :: L a -> Proof
emptyRight :: L a -> Proof
appendAssoc :: L a -> L a -> L a -> Proof 
leftIdentity :: a -> (a -> L b) -> Proof
rightIdentity :: L a -> Proof
associativity :: L a -> (a -> L b) -> (b -> L c) -> Proof
βequivalence :: (a -> L b) -> (b -> L c) -> a -> Proof

\end{code}

</div>

<br>
<br>
<br>
<br>
<br>



Structural Induction
=====================
<br>
<br>
Can we *express* and *prove* properties on ADTs?
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

The list data type 
--------------------------------

<br>
A user defined list, 
<br>

\begin{code}
data L a = N | C a (L a)

{-@ data L [length] a = 
      N | C { hd :: a, tl :: L a} 
  @-}
\end{code}

<br>

with its relevant size function. 
<br>
\begin{code}
{-@ measure length @-}
{-@ length :: L a -> Nat @-}
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
<br>
<br>
<br>
<br>



Reflection of ADTs into the logic
----------------------------------
<br>
The Liquid pragma
<br>

\begin{code}
{-@ LIQUID "--exact-data-cons" @-}
\end{code}
<br>
Automatically creates checker and selector *measures*:
<br>

\begin{spec}
isN :: L a -> Bool 
isC :: L a -> Bool 

select_C_1 :: L a -> a 
select_C_2 :: L a -> L a 
\end{spec}
<br>
**Q:** Suggest alternative names for the above selectors.
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Reflection of Structural Inductive Functions
---------------------------------------------
<br>
With the above measures, 
`map` reflects into logic!
<br>
\begin{code}
{-@ reflect map @-}
map :: (a -> b) -> L a -> L b 
map f N        = N 
map f (C x xs) = f x `C` map f xs 
\end{code}
<br>
The body of `map` reflects in its result type
<br>
\begin{spec}
map :: f:(a->b) -> xs:L a 
  -> {v = map f xs && v == mapPred f xs}

mapPred f xs = if isN xs then N else
               C (f (select_C_1 xs)
                 (map f (select_C_2))  
\end{spec}

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Reflection of Non Recursive Functions
---------------------------------------------
<br>
Non-recursive functions reflect too!
<br>

\begin{code}
{-@ reflect id @-}
id :: a -> a 
id x = x 

{-@ reflect compose @-}
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)
\end{code}

Get automatically the stronger Liquid Types:

\begin{spec}
id :: x:a -> {v:a | v == id x && v == x}

compose :: f:(b -> c) -> g:(a -> b) -> x:a 
        -> {v:c | v = compose f g x && v == f (g x)}
\end{spec}

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Functor Laws: Identity
---------------------------------------------
<br>
Lets prove the identity functor law!
<br>

\begin{code}
{-@ mapId :: xs:L a -> { map id xs == id xs } @-}
mapId N 
  =   map id N 
  ==. N 
  ==. id N 
  *** QED 
mapId (C x xs)
  =   map id (C x xs)
  ==. id x `C` map id xs 
  ==. x `C` map id xs 
  ==. x `C` id xs         ∵ mapId xs
  ==. x `C` xs 
  ==. id (x `C` xs)
  *** QED    
\end{code}

<br>
<br>
<br>
<br>
<br>
<br>
<br>
Hint 

  - Case splitting, and 
  - Recursive call!

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Functor Laws: Identity, Solution
---------------------------------------------
<br>
Here is the proof:
<br>

\begin{spec}
mapId N 
  =   map id N 
  ==. N 
  ==. id N 
  *** QED 
mapId (C x xs)
  =   map id (C x xs)
  ==. id x `C` map id xs 
  ==. x `C` map id xs 
  ==. x `C` id xs         ∵ mapId xs
  ==. x `C` xs 
  ==. id (x `C` xs)
  *** QED    
\end{spec}

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Functor Laws: Distribution
---------------------------------------------
<br>

Lets prove the distribution functor law!
<br>

Or, `mapFusion`!

\begin{code}
{-@ mapFusion :: f:(b -> c) -> g:(a -> b) -> xs:L a
  -> { map  (compose f g) xs == (compose (map f) (map g)) (xs) } @-}
mapFusion f g N 
  =   map (compose f g) N 
  ==. N 
  ==. map f N 
  ==. map f (map g N)
  ==. (compose (map f) (map g)) N
  *** QED 
mapFusion f g (C x xs)
   =   map (compose f g) (C x xs)
   ==. (compose f g) x `C` map (compose f g) xs
   ==. compose f g x   `C` map (compose f g) xs
   ==. f (g x) `C` (compose (map f) (map g)) xs  ∵ mapFusion f g xs 
   ==. f (g x) `C` compose (map f) (map g) xs  
   ==. f (g x) `C` (map f) ((map g) xs)
   ==. f (g x) `C` map f (map g xs)
   ==. map f (g x `C` map g xs)
   ==. map f (map g (C x xs))
   ==. compose (map f) (map g) (C x xs)
   *** QED 
\end{code}

<br>
<br>
<br>
<br>
<br>
<br>
<br>
Hint 

  - Case splitting, and 
  - Recursive call!

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Functor Laws: Distribution, Solution
---------------------------------------------
<br>
Here is the proof:
<br>

\begin{spec}
mapFusion f g N 
  =   map (compose f g) N 
  ==. N 
  ==. map f N 
  ==. map f (map g N)
  ==. (compose (map f) (map g)) N
  *** QED 
mapFusion f g (C x xs)
   =   map (compose f g) (C x xs)
   ==. (compose f g) x `C` map (compose f g) xs
   ==. compose f g x   `C` map (compose f g) xs
   ==. f (g x) `C` (compose (map f) (map g)) xs  ∵ mapFusion f g xs 
   ==. f (g x) `C` compose (map f) (map g) xs  
   ==. f (g x) `C` (map f) ((map g) xs)
   ==. f (g x) `C` map f (map g xs)
   ==. map f (g x `C` map g xs)
   ==. map f (map g (C x xs))
   ==. compose (map f) (map g) (C x xs)
   *** QED 
\end{spec}

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


Onto Monoid Laws
----------------
<br>

Reflect the monoid list operators

<br>

\begin{code}
{-@ reflect append @-}
append :: L a -> L a -> L a 
append N ys        = ys 
append (C x xs) ys = x `C` append xs ys 

{-@ reflect empty @-}
empty :: L a 
empty = N 
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

Monoid Laws: Left Identity
---------------------------------------------
<br>
Lets prove the left identity monoid law!
<br>

\begin{code}
{-@ emptyLeft :: x:L a 
  -> { append empty x == x }  @-}
emptyLeft x 
  = append empty x ==. append N x ==. x  *** QED 
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

Monoid Laws: Left Identity, Solution
---------------------------------------------
<br>
Here is the proof:
<br>
\begin{spec}
emptyLeft x 
  = append empty x ==. append N x ==. x  *** QED 
\end{spec}
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


Monoid Laws: Right Identity
---------------------------------------------
<br>
Lets prove the right identity monoid law!
<br>

\begin{code}
{-@ emptyRight :: x:L a 
  -> { append x empty == x }  @-}
emptyRight N 
  =   append N empty
  ==. append N N 
  ==. N 
  *** QED 

emptyRight (C x xs)
  =   append (C x xs) empty
  ==. x `C` append  xs empty
  ==. x `C` xs                ∵ emptyRight xs 
  *** QED 
\end{code}

<br>
<br>
<br>
<br>
<br>
<br>
<br>
Hint (surprise, surprise!)

  - Case splitting, and 
  - Recursive call!

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Monoid Laws: Right Identity, Solution
---------------------------------------------
<br>
Here is the proof:
<br>
\begin{spec}
emptyRight N 
  =   append N empty
  ==. append N N 
  ==. N 
  *** QED 

emptyRight (C x xs)
  =   append (C x xs) empty
  ==. x `C` append  xs empty
  ==. x `C` xs                ∵ emptyRight xs 
  *** QED 
\end{spec}
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Monoid Laws: Right Identity
---------------------------------------------
<br>
Lets prove the right identity functor monoid!
<br>

\begin{code}
{-@ emptyRight :: x:L a 
  -> { append x empty == x }  @-}
emptyRight xs = trivial 
\end{code}

<br>
<br>
<br>
<br>
<br>
<br>
<br>
Hint (surprise, surprise!)

  - Case splitting, and 
  - Recursive call!

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Monoid Laws: Right Identity, Solution
---------------------------------------------
<br>
Here is the proof:
<br>
\begin{spec}
emptyRight N 
  =   append N empty
  ==. append N N 
  ==. N 
  *** QED 

emptyRight (C x xs)
  =   append (C x xs) empty
  ==. x `C` append  xs empty
  ==. x `C` xs                ∵ emptyRight xs 
  *** QED 
\end{spec}
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Monoid Laws: Associativity
---------------------------------------------
<br>
Lets prove the associativity monoid law!
<br>

\begin{code}
{-@ appendAssoc :: xs:L a -> ys:L a -> zs:L a 
  -> {append xs (append ys zs) == append (append xs ys) zs } @-}
appendAssoc N ys zs 
  =   append N (append ys zs)
  ==. append ys zs                   ∵ emptyLeft (append ys zs)
  ==. append (append N ys) zs        ∵ emptyLeft ys 
  *** QED 

appendAssoc (C x xs) ys zs
  =   append (C x xs) (append ys zs)
  ==. x `C` append xs (append ys zs) 
  ==. x `C` append (append xs ys) zs ∵ appendAssoc xs ys zs
  ==. append (x `C` append xs ys) zs
  ==. append (append (C x xs) ys) zs
  *** QED 
\end{code}

<br>
<br>
<br>
<br>
<br>
<br>
<br>
Hints

  - Case splitting, and 
  - Recursive call!
  - `emptyLeft` on `ys` and `append ys zs`

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Monoid Laws: Associativity, Solution
---------------------------------------------
<br>
Here is the proof:
<br>
\begin{spec}
appendAssoc N ys zs 
  =   append N (append ys zs)
  ==. append ys zs                   ∵ emptyLeft (append ys zs)
  ==. append (append N ys) zs        ∵ emptyLeft ys 
  *** QED 

appendAssoc (C x xs) ys zs
  =   append (C x xs) (append ys zs)
  ==. x `C` append xs (append ys zs) 
  ==. x `C` append (append xs ys) zs ∵ appendAssoc xs ys zs
  ==. append (x `C` append xs ys) zs
  ==. append (append (C x xs) ys) zs
  *** QED 
\end{spec}
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


Onto Monad Laws! 
----------------
<br> 

Define monad list operators 

<br>

\begin{code}
{-@ reflect return @-}
return :: a -> L a
return x = C x N

{-@ reflect bind @-}
bind :: L a -> (a -> L b) -> L b
bind N _ = N 
bind (C x xs) f = append (f x) (bind xs f)
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

Monoid Laws: Left Identity
---------------------------------------------
<br>
Lets prove the left identity monad law!
<br>

\begin{code}
{-@ leftIdentity :: x:a -> f:(a -> L b) -> { bind (return x) f == f x } @-}
leftIdentity x f 
  =   bind (return x) f 
  ==. bind (C x N) f 
  ==. append (f x) (bind N f)
  ==. append (f x) N 
  ==. append (f x) empty 
  ==. f x                    ∵ emptyRight (f x)   
  *** QED 
\end{code}

<br>
<br>
<br>
<br>
<br>
<br>
<br>
Hint

  - `emptyRight` on `f x`

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Monad Laws: Left Identity, Solution
---------------------------------------------
<br>
Here is the proof:
<br>
\begin{spec}
leftIdentity x f 
  =   bind (return x) f 
  ==. bind (C x N) f 
  ==. append (f x) (bind N f)
  ==. append (f x) N 
  ==. f x                    ∵ emptyRight (f x)   
  *** QED 
\end{spec}
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Monoid Laws: Right Identity
---------------------------------------------
<br>
Lets prove the right identity monad law!
<br>

\begin{code}
{-@ rightIdentity :: x:L a -> { bind x return == x } @-}
rightIdentity N 
  =   bind N return 
  ==. N 
  *** QED 
rightIdentity (C x xs)
  =   bind (C x xs) return 
  ==. return x `append` bind xs return
  ==. (C x N)  `append` bind xs return 
  ==. (C x N)  `append` xs              ∵ rightIdentity xs 
  ==. C x (append N xs)                 ∵ emptyLeft xs 
  ==. C x xs 
  *** QED   
\end{code}

<br>
<br>
<br>
<br>
<br>
<br>
<br>
Hint

  - Case splitting, 
  - Recursive call, and 
  - `emptyLeft` on `xs`.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Monad Laws: Right Identity, Solution
---------------------------------------------
<br>
Here is the proof:
<br>
\begin{spec}
rightIdentity N 
  =   bind N return 
  ==. N 
  *** QED 
rightIdentity (C x xs)
  =   bind (C x xs) return 
  ==. return x `append` bind xs return
  ==. (C x N)  `append` bind xs return 
  ==. (C x N)  `append` xs              ∵ rightIdentity xs 
  ==. C x (append N xs)                 ∵ emptyLeft xs 
  ==. C x xs 
  *** QED   
\end{spec}
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Monoid Laws: Associativity 
---------------------------------------------
<br>
To prove associativity, lets assume two helper lemmata!
<br>

- Beta Equivalence on bind 

\begin{spec}
βequivalence :: f:(a -> L b) -> g:(b -> L c) -> x:a -> 
     {bind (f x) g == (\y:a -> bind (f y) g) (x)} 
\end{spec}

- Bind distribution 

\begin{spec}
bindAppend :: xs:L a -> ys:L a -> f:(a -> L b)
     -> { bind (append xs ys) f == append (bind xs f) (bind ys f) }
\end{spec}
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


Monoid Laws: Associativity 
---------------------------------------------
<br>
Lets prove the associativity monad law!
<br>

\begin{code}
{-@ associativity :: m:L a -> f: (a -> L b) -> g:(b -> L c)
  -> {bind (bind m f) g == bind m (\x:a -> (bind (f x) g)) } @-}
associativity N f g 
  =   bind (bind N f) g
  ==. bind N g 
  ==. N 
  ==. bind N (\x -> (bind (f x) g))
  *** QED 
associativity (C x xs) f g 
  =   bind (bind (C x xs) f) g 
  ==. bind (append (f x) (bind xs f)) g 
  ==. append (bind (f x) g) (bind (bind xs f) g)            
       ∵ bindAppend (f x) (bind xs f) g 
  ==. append (bind (f x) g) (bind xs (\x -> (bind (f x) g))) 
       ∵ associativity xs f g 
  ==. append ((\x -> bind (f x) g) x) (bind xs (\x -> (bind (f x) g))) 
       ∵ βequivalence f g x 
  ==. bind (C x xs) (\x -> (bind (f x) g))
  *** QED 

\end{code}

<br>
<br>
<br>
<br>
<br>
<br>
<br>
Hint

  - Case splitting, 
  - Recursive call, and 
  - use of two lemmata.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Monad Laws: Associativity, Solution
---------------------------------------------
<br>
Here is the proof:
<br>
\begin{spec}
associativity N f g 
  =   bind (bind N f) g
  ==. bind N g 
  ==. N 
  ==. bind N (\x -> (bind (f x) g))
  *** QED 
associativity (C x xs) f g 
  =   bind (bind (C x xs) f) g 
  ==. bind (append (f x) (bind xs f)) g 
  ==. append (bind (f x) g) (bind (bind xs f) g)            
       ∵ bindAppend (f x) (bind xs f) g 
  ==. append (bind (f x) g) (bind xs (\x -> (bind (f x) g))) 
       ∵ associativity xs f g 
  ==. append ((\x -> bind (f x) g) x) (bind xs (\x -> (bind (f x) g))) 
       ∵ βequivalence f g x 
  ==. bind (C x xs) (\x -> (bind (f x) g))
  *** QED 
\end{spec}
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>



Proving the Beta Equivalence Lemma
-----------------------------------

- Proof requires assumptions for β-equivalence
- Teach SMT β-equivalence via Liquid pragma. 

\begin{code}
{-@ LIQUID "--betaequivalence"  @-}

{-@ βequivalence :: f:(a -> L b) -> g:(b -> L c) -> x:a -> 
     {bind (f x) g == (\y:a -> bind (f y) g) (x)}  @-}
βequivalence f g x = trivial 
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


Proving the bind function Lemma
--------------------------------

<br>
Another boring inductive proof... 
<br>

\begin{code}
bindAppend :: L a -> L a -> (a -> L b) -> Proof
{-@ bindAppend :: xs:L a -> ys:L a -> f:(a -> L b)
     -> { bind (append xs ys) f == append (bind xs f) (bind ys f) }
  @-}
bindAppend N ys f 
  =   bind (append N ys) f 
  ==. bind ys f 
  ==. append N (bind ys f)
  ==. append (bind N f) (bind ys f)
  *** QED 
bindAppend (C x xs) ys f 
  =   bind (append (C x xs) ys) f 
  ==. bind (C x (append xs ys)) f 
  ==. append (f x) (bind (append xs ys) f)
  ==. append (f x) (append (bind xs f) (bind ys f)) 
       ∵ bindAppend xs ys f 
  ==. append (append (f x) (bind xs f)) (bind ys f)
       ∵  appendAssoc (f x) (bind xs f) (bind ys f)
  ==. append (bind (C x xs) f) (bind ys f)
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


Summing up
--------------------------------

We proved many equivalences on lists

- Functor Laws
  - Identity:     `map id == id`
  - Distribution: `map (compose f g) == compose (map f) (map g)`

- Monoid Laws
  - Left Identity: `append empty x == x`
  - Right Identity: `append x empty == x`
  - Associativity: `append xs (append ys zs) == append (append xs ys) zs`

- Monad Laws
  - Left Identity: `bind (return x) f == f x`
  - Right Identity: `bind x return == x`
  - Associativity: `bind (bind m f) g == bind m (\x:a -> (bind (f x) g))`


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

