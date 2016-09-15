
LiquidHaskell @ Awake Networks
===============================
<br>

- Verification of 74 modules string processing code base 
    - all functions are terminating & total 
    - safe ByteString indexing

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


Who runs Liquid Haskell?
--------------------------------
<div class="hidden">

\begin{code}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}


import Prelude hiding (take)


import           Data.Word
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.Bits
import           Data.Proxy

main = putStrLn "Easter Egg: to force Makefile"
\end{code}

</div>

<br>

Run `liquid-check-all` script automatically at pull requests. 

<br>

*Challenge* Running time for all code is `~80 min`.


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


Experience
--------------------------------
<br>

- Verification is generally easy, apart from 

      - Some code rewriting (e.g., monadic to maybe code)

      - Some known issues (e.g., use [filter's precise type](https://github.mv.awakenetworks.net/awakenetworks/packet-analysis/blob/671d4f858b7f5f77e6017d357b9da73151037948/src/Awake/Parser/Multiplier.hs#L170))

- Some (fancy) Haskell features were not supported (e.g., `TypeLits`) 

- Some invariants are unknown how to be proven 
      - [Dependent Type invariants](https://github.mv.awakenetworks.net/awakenetworks/packet-analysis/blob/master/src/Awake/Protocol/ICMP.hs#L263)
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>



Code Rewritting: Monad to Maybe 
--------------------------------


\begin{code}
{-@ monadicStyle :: Nat -> [a] -> Maybe [a] @-}
monadicStyle i xs =
  do checkSizeMaybe i head xs
     return (take i xs)
\end{code}


\begin{code}
{-@ maybeStyle :: Pos -> [a] -> Maybe [a] @-}
maybeStyle i xs =
  case checkSizeMaybe i head xs of 
    Just _  -> Just $ take i xs 
    Nothing -> Nothing 
\end{code}


\begin{code}
{-@ type Pos = {v:Int | 0 < v } @-}

{-@
checkSizeMaybe :: 
       n:Nat
    -> (bs:{[a] | n <= len bs } -> b)
    -> bs:[a]
    -> {v:Maybe b | isJust v => n <= len bs}
@-}
\end{code}

\begin{code}
checkSizeMaybe :: Int -> ([a] -> b) -> [a] -> Maybe b
checkSizeMaybe sz f bs
  | length bs >= sz = Just (f bs)
  | otherwise       = Nothing

{-@ take :: i:Nat -> xs:{[a] | i <= len xs} -> [a] @-} 
take :: Int -> [a] -> [a]
take 0 []        = [] 
take i (x:xs) = if i == 0 then [] else x:(take (i-1) xs)
\end{code}




Code Rerwitting: Filter & ADTS
------------------------------

Liquid Haskell in good is linear arithmetic ...

\begin{code}
{-@ filterPosSnd :: [(a, Int)] -> [(a, Pos)] @-}
filterPosSnd :: [(a, Int)] -> [(a, Int)]
filterPosSnd = filter ((>0) . snd)
\end{code}

\begin{code}
{-@ assume filter :: forall <p :: a -> Prop, w :: a -> Bool -> Prop>.
                 {y::a, b::{b:Bool<w y> | Prop b} |- {v:a | v == y} <: a<p>} 
                 (x:a -> Bool<w x>) -> [a] -> [a<p>]
  @-}
\end{code}

\begin{code}
{-@ filterPosSndAxiom :: [(a, Int)] -> [(a, Pos)] @-}
filterPosSndAxiom :: [(a, Int)] -> [(a, Int)]
filterPosSndAxiom = axiomPosPair . filter ((>0) . snd)


{-@ assume snd :: p:(a, b) -> {v:b | v == snd p} @-}

{-@ axiomPosPair :: [{x:(a, Int) | 0 < (snd x)}] -> [(a, Pos)] @-}
axiomPosPair :: [(a, Int)] -> [(a, Int)]
axiomPosPair [] = []
axiomPosPair ((x1, x2):xs) = (x1, x2):axiomPosPair xs 
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
<br>
<br>


Extend fix Liquid Haskell: Sucess
--------------------------------------

Verification required extending Liquid Haskell to support

- `TypeLits`, 
- (Refining `newtype`)[https://github.com/ucsd-progsys/liquidhaskell/pull/798], and 
- (DataKinds)[https://github.com/ucsd-progsys/liquidhaskell/pull/796].


Verification required fixing bugs in Liquid Haskell
- (Code explosion)[https://github.com/ucsd-progsys/liquidhaskell/issues/792], 
- (Unsoundness)[https://github.com/ucsd-progsys/liquidhaskell/issues/743], and
- (Parsing)[https://github.com/ucsd-progsys/liquidhaskell/issues/813].




Extend/fix Liquid Haskell: Failure
--------------------------------------




Code 


`Jenkis` (used to) [run](https://github.mv.awakenetworks.net/awakenetworks/packet-analysis/blob/master/jenkins.hs#L138) `liquid` on all but 4 files 

Files known to fails 

  - `Data.Struct.[Checked/Unchecked]`: John's dependent types magic
  - `Awake/Data/[Chunks/Buffer]`: Deep list invariants (known but time consuming)


Chunk Invariant :: All elements of the list have _the same size!_


Bug examples
----------------------
<br> 

  - Indexing: [Parser invariants](https://github.mv.awakenetworks.net/awakenetworks/packet-analysis/pull/372/files#diff-31ff57893eef02fe0f8575893cf90968R206)

  - Termination: [Flush consecutive elements](https://github.mv.awakenetworks.net/awakenetworks/packet-analysis/pull/279/files#diff-a1a70534ab13cef151dba674e1be6a3dR435)
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Annotations in Numbers
-------------------------
<br>
Annotations are 2.34&#37; of the code!
<br>
Time = 77 minutes :(
<br>


     --      4644.69 real      4634.93 user       301.72 sys

\begin{spec}
           LOC   Annotations  Termination+  Assumes 

74 files  20984   491          28(40)-13      19    
\end{spec}

+Termination = annotated functions (loc for annotations) - lazy functions 
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Annotations in Numbers per file
----------------------------------

\begin{spec}
                                   LOC   LLOC Termination+ Assumes Pragma


│── Awake                           
│   │   Data
│   │   ├── AppConfig.hs             24   0    0             0       0       
│   │   ├── Bounds.hs                42   0    0             0       0  
│   │   ├── Buffer.hs               423   1    1(1)          0       0
│   │   ├── Chunks.hs               100   0    0             0       0 
│   │   ├── Endian.hs               123   0    0             0       0 
│   │   ├── Extract.hs              305   0    0             0       0   
│   │   ├── Forgettable.hs          137   1    0             0     1 (prune) 
│   │   ├── Internalizeable.hs      14    0    0             0       0 
│   │   ├── Maybe
│   │   │   └── Type.hs             55    0    0             0       0 
│   │   ├── Packet
│   │   │   ├── DPDK.hs             337   4    1(2)          0       0          
│   │   │   ├── Linux.hs            323   4    2(2)          0     1 (scrape)
│   │   │   └── PCAP.hs             219   3    1             1     1 (scrape)
│   │   ├── Packet.hs                23   0    0             0       0          
│   │   ├── Sessions.hs             125   0    0             0       0 
│   │   ├── Struct
│   │   │   ├── Checked.hs          155   7    0             0       0 
│   │   │   ├── Examples.hs          51   0    0             0       0   
│   │   │   └── Unchecked.hs        465   8    0             0       0 
│   │   └── Struct.hs               258   4    0             1       0   
│   ├── Fused.hs                    674   3    0             0     1 (prune)
│   ├── Generator.hs               1014  18    3(5)-1        1     1 (prune)
│   ├── Parser
│   │   └── Multiplier.hs           172  21    0             1+    1 (prune)
│   ├── Protocol
│   │   ├── ASN1.hs                1024  16    3(3)-1        0     2 (case,trust)          
│   │   ├── DNS.hs                  285   2    0             0     2 (case,trust)          
│   │   ├── Ethernet.hs             168   5    0             0     1 (trust)
│   │   ├── FTP.hs                  536   0    0             0       0                          
│   │   ├── HTTP.hs                 835  24*   2(2)-3        2     4 (case,trust,prune,scrape)
│   │   ├── ICMP.hs                 384   6    0             2     2 (case,trust)
│   │   ├── IP.hs                   477  14    0             2     2 (case,trust) 
│   │   ├── IPv4.hs                 174   7    0             0     2 (case,trust) 
│   │   ├── IPv6.hs                 120   7    0             0     2 (case,trust)    
│   │   ├── Kerberos.hs             872   2    0             0     2 (case,trust)         
│   │   ├── LDAP.hs                 909   3    0             0     3 (case,trust,prune) 
│   │   ├── NTLM.hs                 328   0    0             0        0
│   │   ├── SMB.hs                 1531   4    1(1)          0     2 (case,trust)
│   │   ├── SPNEGO.hs               117   0    0             0        0 
│   │   ├── TCP.hs                 1349   9    0             0     3 (case,trust,prune)
│   │   ├── TFTP.hs                 548   5    0             0        0 
│   │   ├── TLS.hs                  685  10    1(1)          0        0 
│   │   └── UDP.hs                  270   1    0             1        0 
│   ├── Protocol.hs                  38   0    0             0        0 
│   ├── ProtocolBuffers (19 fs)    1914   0    0             0        0          
│   ├── ProtocolBuffers.hs            9   0    0             0        0            
│   ├── Streaming.hs                250   2                  0     2 (case,trust)
│   ├── System
│   │   └── Testimony.hs            564   7    0-1           2**   1 (prune) 
│   ├── Text             (2 fs)     103   0    0             0        0  
│   └── Util.hs                     347   6    4(5)-1        0        0
├── Data
│   ├── ByteString
│   │   ├── Extras.hs               610 179    2(10)         6        0 
│   │   └── Parser.hs                86   0    0             0        0 
│   ├── ListOr.hs                   139   1    0             0     1 (trust)
│   ├── Map
│   │   └── Lazy
│   │       └── Extras.hs            26   0    0             0        0  
│   ├── Monoid
│   │   └── Extras.hs               549  43   1(1)           0     2 (linear,scrape)
│   └── StrictPair.hs                22   0    0             0        0  
├── Network
│   └── Kafka
│       └── Extras.hs                45   0    0             0        0  
├── Parser
    └── Bidirectional.hs            631  64   7(7)-6         0     2 (trust,prune)



* 20 lines of measures, ghc complained for unsued 

Note: reasons for assumes 
  - dependent type invariants 
  - constants (bytestring length)
  - import functions 
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
<br>
<br>
<br>
<br>
<br>
<br>

LiquidHaskell at Awake Networks
===============================
<br>

- Verification of [packet-analysis](https://github.mv.awakenetworks.net/awakenetworks/packet-analysis)
    - all functions are terminating & total 
    - [safe](https://github.mv.awakenetworks.net/awakenetworks/packet-analysis/blob/master/src/Data/ByteString/Extras.hs) ByteString indexing

- Verification of Jeff's [parallel string matcher](https://github.com/ucsd-progsys/liquidhaskell/blob/develop/tests/strings/pos/StringIndexing.hs#L41)

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>



Dependent Types Example
-------------------------



\begin{code}
class Member a where
  sizeOfMember :: Proxy a -> Int 
  readMember   :: ReadPtr a -> a 

type ReadPtrN t = ReadPtr (t 'Nothing)

newtype ReadPtr a = ReadPtr B.ByteString

instance Member Word32 where
  sizeOfMember _ = 4
  readMember (ReadPtr bs) = getWord32 bs


{-@
getWord32
    :: { bs : B.ByteString | 4 <= bslen bs }
    -> Word32
@-}
getWord32 :: B.ByteString -> Word32
getWord32 bs = b3 .|. b2 .|. b1 .|. b0 where
    b3 = fromIntegral (B.head bs) `shiftL` 24
    b2 = fromIntegral (B.head $ B.drop 1 bs) `shiftL` 16
    b1 = fromIntegral (B.head $ B.drop 2 bs) `shiftL` 8
    b0 = fromIntegral (B.head $ B.drop 3 bs)

\end{code}



Parser Invariants
-----------------





let n = max 0 n' in inp {llen = llen inp - n, bs = drop n (bs inp)}
with {llen = len bs}




Termination bug
----------------


Flush consecutive 

consecutive :: Buffer a -> ([a], Buffer a)
consecutive (Buffer k m) = 
    case IntMap.lookup (fromIntegral k) m of
        Nothing -> (  [], Buffer k                   m )
        Just v  -> (v:vs, Buffer k' (IntMap.delete (fromIntegral k) m'))


{-| Flush as many consecutive elements as possible from the `Buffer`, starting
    at the expected `next` key

>>> consecutive (Buffer 0 (IntMap.fromList [(0, 'A'), (1, 'B'), (3, 'C')]))
("AB",Buffer {_next = 2, elems = fromList [(3,'C')]})
>>> consecutive (Buffer 0 IntMap.empty)
([],Buffer {_next = 0, elems = fromList []})

-}

consecutive :: Buffer a -> ([a], Buffer a)
consecutive (Buffer k0 m0) = go (fromIntegral (maxBound :: Word16)) k0 m0 
  where 
    go :: Int -> Word16 -> IntMap a -> ([a], Buffer a)
    go i k m
      | i == 0 = ([], Buffer k m)
    go i k m 
      = case IntMap.lookup (fromIntegral k) m of
             Nothing -> (  [], Buffer k                   m )
             Just v  -> (v:vs, Buffer k' (IntMap.delete (fromIntegral k) m'))
          where
            ~(vs, ~(Buffer k' m')) = go (i-1) (k + 1) m 



Thank You!
----------



<br>

[http://goto.ucsd.edu/~nvazou/presentations/awake2016/01-index.html](http://goto.ucsd.edu/~nvazou/presentations/awake2016/01-index.html)
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
