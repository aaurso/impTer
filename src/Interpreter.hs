{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Interpreter where
import Parsers ( getFromEnv, program, parse )
import Prelude hiding (read, write, empty)
import Utils ( Env )


--------------------------------------------------------------
--------------------------------------------------------------
-- EXECUTION OF THE PROGRAM
--------------------------------------------------------------
h3 :: (a,b,c) -- ^ 
  -> a
h3 (x,_,_) = x

run :: [(Env, String, String)] -- ^ 
  -> ([Env],String)

-- case: empty list
run [] = ([],"\n[ERROR] Invalid input!\n")

-- case: entire input string consumed
run [(env, parsedString, "")] = ([newEnv],
    "\nParsed input: \n\n " ++ parsedString ++ "\n\n" ++
    "\nMemory contains: \n\n" ++ getFromEnv parsed)
    where
        parsed = parse program env parsedString
        newEnv = if not (null parsed) then h3 (head parsed)
                 else env

-- case: input string not entirely consumed
run [(env, parsedString, leftString)] = ([env],
    "\nParsed input: \n\n" ++ parsedString ++ "\n\n" ++
    "\nMemory contains: \n\n" ++ getFromEnv (parse program [] parsedString) ++
    "\nError: \n\n Discarded input '" ++ leftString ++ "'\n\n")
