module Algebra where

import Model


-- Exercise 5
type Algebra r = ([r]      -> r, --Program
                String -> r -> r, -- Rule
                r,           -- Cmds empty
                [r] -> r,      -- Cmds
                r,           -- GO
                r,           -- TAKE
                r,           -- MARK
                r,           -- NOTHING       
                r -> r,      -- TURN
                r -> r -> r, -- CASE
                String -> r, -- CMD with string
                r,           -- LEFT
                r,           -- RIGHT
                r,           -- FRONT
                r,           -- Alts empty
                [r] -> r,    -- Alts
                r -> r -> r, -- Alt
                r,           -- EMPTY   
                r,           -- LAMBDA
                r,           -- DEBRIS
                r,           -- ASTEROID
                r,           -- BOUNDARY
                r           -- UNDERSCORE
                )

fold :: Algebra r -> Program -> r
fold (aProgram, aRule, aCmdsE, aCmds, aGO, aTAKE, aMARK, aNOTHING, aTURN, aCASE, aCMD, aLEFT, 
      aRIGHT, aFRONT, aAltsE, aAlts, aAlt, aEMPTY, aLAMBDA, aDEBRIS, aASTEROID, aBOUNDARY, aUNDERSCORE) p = f p
      where f (Program rs) = aProgram (map fr rs)
            fr (Rule s c) =  aRule s (fcs c)
            fcs (Cmds_) = aCmdsE
            fcs (Cmds cs) = aCmds (map fc cs)
            fc (GO) = aGO
            fc (TAKE) = aTAKE
            fc (MARK) = aMARK
            fc (NOTHING) = aNOTHING
            fc (TURN d) = aTURN (fd d)
            fc (CASE d a) = aCASE (fd d) (fas a)
            fc (CMD s) = aCMD s
            fd (LEFT) = aLEFT
            fd (RIGHT) = aRIGHT
            fd (FRONT) = aFRONT
            fas (Alts_) = aAltsE
            fas (Alts as) = aAlts (map fa as)
            fa (Alt p cmds) = aAlt (fp p) (fcs cmds)
            fp (EMPTY) = aEMPTY
            fp (LAMBDA) = aLAMBDA
            fp (DEBRIS) = aDEBRIS
            fp (ASTEROID) = aASTEROID
            fp (BOUNDARY) = aBOUNDARY
            fp (UNDERSCORE) = aUNDERSCORE


-- Exercise 6

checkProgram :: Program -> Bool
checkProgram = undefined