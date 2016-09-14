
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


Verification  of Packet Analysis
--------------------------------
<div class="hidden">

\begin{code}
main = putStrLn "Easter Egg: to force Makefile"
\end{code}

</div>

<br>

`Jenkis` (used to) [run](https://github.mv.awakenetworks.net/awakenetworks/packet-analysis/blob/master/jenkins.hs#L138) `liquid` on all but 4 files 

Files known to fails 

  - `Data.Struct.[Checked/Unchecked]`: John's dependent types magic
  - `Awake/Data/[Chunks/Buffer]`: Deep list invariants (known but time consuming)

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
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

      - Some code rewriting (e.g., [monadic to maybe code](https://github.mv.awakenetworks.net/awakenetworks/packet-analysis/pull/317/files#diff-27908f4a5fd211b5356de3a4ad0a7442R124) )

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

\begin{spec}
           LOC   Annotations  Termination+  Assumes 

74 files  20984   491          28(40)-13      19    
\end{spec}

+Termination = annotated functions (loc for annotations) - lazy functions 


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
