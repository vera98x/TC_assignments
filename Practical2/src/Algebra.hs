module Algebra where

import Model

-- Exercise 5
type Algebra p a as d c cs r pr = 
                ([r]      -> pr, --Program
                String -> cs -> r, -- Rule
                [c] -> cs,      -- Cmds
                c,           -- GO
                c,           -- TAKE
                c,           -- MARK
                c,           -- NOTHING       
                d -> c,      -- TURN
                d -> as -> c, -- CASE
                String -> c, -- CMD with string
                d,           -- LEFT
                d,           -- RIGHT
                d,           -- FRONT
                [a] -> as,    -- Alts
                p -> cs -> a, -- Alt
                p,           -- EMPTY   
                p,           -- LAMBDA
                p,           -- DEBRIS
                p,           -- ASTEROID
                p,           -- BOUNDARY
                p           -- UNDERSCORE
                )

fold :: Algebra p a as d c cs r pr -> Program -> pr 
fold (aProgram, aRule, aCmds, aGO, aTAKE, aMARK, aNOTHING, aTURN, aCASE, aCMD, aLEFT, 
      aRIGHT, aFRONT, aAlts, aAlt, aEMPTY, aLAMBDA, aDEBRIS, aASTEROID, aBOUNDARY, aUNDERSCORE) p = f p
      where f (Program rs) = aProgram (map fr rs)
            fr (Rule s c) =  aRule s (fcs c)
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
            fas (Alts as) = aAlts (map fa as)
            fa (Alt p cmds) = aAlt (fp p) (fcs cmds)
            fp (EMPTY) = aEMPTY
            fp (LAMBDA) = aLAMBDA
            fp (DEBRIS) = aDEBRIS
            fp (ASTEROID) = aASTEROID
            fp (BOUNDARY) = aBOUNDARY
            fp (UNDERSCORE) = aUNDERSCORE


-- Exercise 6

-- check if there are no undefined rules
callsInNames :: [String] -> [String] -> Bool
callsInNames names calls = all (==True) (foldr (\x r -> (x `elem` names) : r) [] calls )

checkNoUndefinedRules :: Algebra String [String] [String] String [String] [String] (String, [String]) Bool 
checkNoUndefinedRules = (
                (\rs -> let (names, calls) = (unzip rs) in 
                        callsInNames names ((concat calls))), --Program
                (\s cs -> (s, cs)), -- Rule
                (\cs -> filter (/= " ") (concat cs)), -- Cmds
                [],           -- GO
                [],           -- TAKE
                [],           -- MARK
                [],           -- NOTHING       
                (\d -> []),      -- TURN
                (\d as -> as), -- CASE
                (\s -> [s]), -- CMD with string
                " ",           -- LEFT
                " ",           -- RIGHT
                " ",           -- FRONT
                (\as -> concat as),    -- Alts
                (\p cs -> cs), -- Alt
                " ",           -- EMPTY   
                " ",           -- LAMBDA
                " ",           -- DEBRIS
                " ",           -- ASTEROID
                " ",           -- BOUNDARY
                " "           -- UNDERSCORE
                )

-- check if command has start
checkStart :: Algebra String String String String String String String Bool 
checkStart = (
            (\rs -> (length (filter(== "start") rs)) == 1), --Program
            (\s cs -> s), -- Rule
            (\cs -> " "),      -- Cmds
            " ",           -- GO
            " ",           -- TAKE
            " ",           -- MARK
            " ",           -- NOTHING       
            (\d -> " "),      -- TURN
            (\d as -> " "), -- CASE
            (\s -> " "), -- CMD with string
            " ",           -- LEFT
            " ",           -- RIGHT
            " ",           -- FRONT
            (\as -> " "),    -- Alts
            (\p cs -> " "), -- Alt
            " ",           -- EMPTY   
            " ",           -- LAMBDA
            " ",           -- DEBRIS
            " ",           -- ASTEROID
            " ",           -- BOUNDARY
            " "           -- UNDERSCORE
            )

-- check if functions are double
checkNoRepeat :: Algebra String String String String String String String Bool 
checkNoRepeat = (
            (\rs -> let (_, result) = foldr (\x (l,b) -> if x `elem` l then (l, False) else (x:l, b)) ([], True) rs in
                        result), --Program
            (\s cs -> s), -- Rule
            (\cs -> " "),      -- Cmds
            " ",           -- GO
            " ",           -- TAKE
            " ",           -- MARK
            " ",           -- NOTHING       
            (\d -> " "),      -- TURN
            (\d as -> " "), -- CASE
            (\s -> " "), -- CMD with string
            " ",           -- LEFT
            " ",           -- RIGHT
            " ",           -- FRONT
            (\as -> " "),    -- Alts
            (\p cs -> " "), -- Alt
            " ",           -- EMPTY   
            " ",           -- LAMBDA
            " ",           -- DEBRIS
            " ",           -- ASTEROID
            " ",           -- BOUNDARY
            " "           -- UNDERSCORE
            )


-- algebra for pattern match check
isComplete :: [Pat] -> Bool
isComplete pats = UNDERSCORE `elem` pats || (EMPTY `elem` pats && LAMBDA `elem` pats && DEBRIS `elem` pats 
                                                  && ASTEROID `elem` pats && BOUNDARY `elem` pats)

checkMatchComplete :: Algebra Pat Pat Bool Bool Bool Bool Bool Bool
checkMatchComplete = (
                (\rs -> all (==True) rs), --Program
                (\s cs -> cs), -- Rule
                (\cs -> all (==True) cs ),      -- Cmds
                True,           -- GO
                True,           -- TAKE
                True,           -- MARK
                True,           -- NOTHING       
                (\d -> True),      -- TURN
                (\d as -> as), -- CASE
                (\s -> const True s ), -- CMD with string
                True,           -- LEFT
                True,           -- RIGHT
                True,           -- FRONT
                (\alt -> isComplete alt),    -- Alts
                (\pat cs -> pat), -- Alt
                EMPTY,           -- EMPTY   
                LAMBDA,           -- LAMBDA
                DEBRIS,           -- DEBRIS
                ASTEROID,           -- ASTEROID
                BOUNDARY,           -- BOUNDARY
                UNDERSCORE           -- UNDERSCORE
                )
    

checkProgram :: Program -> Bool
checkProgram p = (fold checkNoUndefinedRules p) && (fold checkStart p) && (fold checkNoRepeat p) && (fold checkMatchComplete p)