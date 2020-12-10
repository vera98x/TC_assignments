module Algebra where

import Model


-- Exercise 5
type Algebra r = ([r]      -> r, --Program
                String -> r -> r, -- Rule
                r,           -- Cmds empty
                r -> r -> r, -- Cmds
                r,           -- GO
                r,           -- TAKE
                r,           -- MARK
                r,           -- NOTHING       
                r -> r,    -- TURN
                r -> r -> r, -- CASE
                String -> r, -- CMD with string
                r,           -- LEFT
                r,           -- RIGHT
                r,           -- FRONT
                r,           -- Alts empty
                r -> r -> r, -- Alts
                r -> r -> r, -- Alt
                r,           -- EMPTY   
                r,           -- LAMBDA
                r,           -- DEBRIS
                r,           -- ASTEROID
                r,           -- BOUNDARY
                r           -- UNDERSCORE
                )

fold :: Algebra r-> Program -> r
fold (aProgram, aRule, aCmdsE, aCmds, aGO, aTAKE, aMARK, aNOTHING, aTURN, aCASE, aCMD, aLEFT, 
      aRIGHT, aFRONT, aAltsE, aAlts, aAlt, aEMPTY, aLAMBDA, aDEBRIS, aASTEROID, aBOUNDARY, aUNDERSCORE) = f
      where f (Program rs) = aProgram (map f rs)
            f (Rule s c) =  aRule s (f c)
            f (Cmds_) = aCmdsE
            f (Cmds c cs) = aCmds (f c) (map f cs)
            f (GO) = aGO
            f (TAKE) = aTAKE
            f (MARK) = aMARK
            f (NOTHING) = aNOTHING
            f (TURN d) = aTURN (f d)
            f (CASE d a) = aCASE (f d) (f a)
            f (CMD s) = aCMD s
            f (LEFT) = aLEFT
            f (RIGHT) = aRIGHT
            f (FRONT) = aFRONT
            f (Alts_) = aAltsE
            f (Alts a as) = aAlts (f a) (map f as)
            f (Alt p cmds) = aAlt (f p) (f cmds)
            f (EMPTY) = aEMPTY
            f (LAMBDA) = aLAMBDA
            f (DEBRIS) = aDEBRIS
            f (ASTEROID) = aASTEROID
            f (BOUNDARY) = aBOUNDARY
            f (UNDERSCORE) = aUNDERSCORE


             




-- Exercise 6

checkProgram :: Program -> Bool
checkProgram = undefined