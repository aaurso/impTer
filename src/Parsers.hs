{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use fewer imports" #-}
--------------------------------------------------------------
-- INCLUDES
--------------------------------------------------------------
module Parsers where
import Control.Applicative ( Alternative(..) )
import Data.Char
    ( isAlpha, isAlphaNum, isDigit, isLower, isSpace, isUpper )
import Data.List (isPrefixOf)
import Data.Type.Coercion (sym)
import System.Posix (SystemID(systemName), Semaphore)
import Utils (Variable (name, value, vtype, Variable), Env, ArrayType, mulAllElemArr, sumAllElemArr, mulArr, addArr, dot)
 -- ENVIRONMENT FUNCTIONS
--------------------------------------------------------------
getFromEnv :: [(Env, String, String)] -> String
getFromEnv [] = "Invalid input\n"
getFromEnv [(x:xs, parsedString, "")] =
    "Integer: " ++ name x ++ " = " ++ show (value x) ++ "\n" ++
    getFromEnv    [(xs,parsedString,"")]
getFromEnv [(env, parsedString, leftString)] = case leftString of
    "" -> ""
    _ -> "Error (unused input '" ++ leftString ++ "')\n\n" ++ getFromEnv [(env,parsedString, "")]

updateEnv :: Variable -> Parser String
updateEnv var = P (\env input -> case input of
                     xs -> [(pushVarVal env var,"",xs)])

pushVarVal :: Env -- ^ 
  -> Variable -- ^ 
  -> Env
pushVarVal [] var = [var]
pushVarVal (x:xs) newVar = if name x == name newVar then newVar : xs
                          else x : pushVarVal xs newVar

pullVarVal :: String -> Parser Int
pullVarVal name = P (\env input -> case lookupVarVal env name of
    [] -> []
    [value] -> [(env, value, input)])

lookupVarVal :: Env -> String -> ArrayType
lookupVarVal []     queryname = []
lookupVarVal (x:xs) queryname = if name x == queryname then [value x]
                                  else lookupVarVal xs queryname

executeBlock :: String -> Parser String
executeBlock c = P(\env input -> [(env, "", c ++ input)])
newtype Parser a = P (Env -> String -> [(Env, a, String)])

--------------------------------------------------------------
-- PARSER INSTANCES
--------------------------------------------------------------

instance Functor Parser where
    fmap g p = P (\env input -> case parse p env input of
                [] -> []
                [(env, v, out)] -> [(env, g v, out)])

instance Applicative Parser where
    pure v = P (\env input -> [(env, v, input)])

    pg <*> px = P (\env input -> case parse pg env input of
                     []        -> []
                     [(env, g,out)] -> parse (fmap g px) env out)

instance Monad Parser where
    p >>= f = P (\env input -> case parse p env input of
                              []        -> []
                              [(env, v,out)] -> parse (f v) env out)


instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\env input -> [])

    p <|> q = P (\env input -> case parse p env input of
                              []        -> parse q env input
                              [(env,v,out)] -> [(env, v,out)])
parse :: Parser a -- ^ 
  -> Env -- ^ 
  -> String -- ^ 
  -> [(Env, a, String)]
parse (P p) = p


--------------------------------------------------------------
-- MINI PARSERS
--------------------------------------------------------------

item :: Parser Char
item = P (\env input -> case input of
            [] -> []
            (x:xs) -> [(env, x,xs)])


satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do x <- item
               if p x then return x else empty

-- Using satisfy for appropriate predicates from the lib. Data.Char, we can 
--  now define parsers for single digits, lower-case letters, upper-case letters,
--  arbitrary letters, alphanumeric characters, and specific characters

digit :: Parser Char
digit = satisfy isDigit

lower :: Parser Char
lower = satisfy isLower

upper :: Parser Char
upper = satisfy isUpper

letter :: Parser Char
letter = satisfy isAlpha

alphanum :: Parser Char
alphanum = satisfy isAlphaNum

char :: Char -> Parser Char
char x = satisfy (== x)

string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

-- Using many and some, we can now define parsers for identifiers (variable names)
--  comprising a lower-case letter followed by zero or more alphanumeric characters,
--  natural numbers comprising one or more digits, and spacing comprising zero or more space,
--  tab, and newline characters

ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

space :: Parser ()
space = do many (satisfy isSpace)
           return ()

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
       <|> nat

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

-- Using token, we cna now define parsers that ignore spacing around identifiers,
--  natural numbers, integers, and special symbols:

identifier :: Parser String
identifier =  token identArr <|> token ident


identArr :: Parser [Char]
identArr = do
           x <- lower
           xs <- many alphanum
           aq <- symbol "["
           q <- some digit
           cq <-symbol "]"
           return (x: xs ++ aq ++ q ++ cq)
             <|>
            do
           x <- lower
           xs <- many alphanum
           aq <- symbol "["
           q <- ident
           cq <-symbol "]"
           return (x: xs ++ aq ++ q ++ cq)


natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

--------------------------------------------------------------
-- ARTIHMETIC EXPRESSIONS EVALUATION
--------------------------------------------------------------

aexp :: Parser Int
aexp = (do t <- aterm
           symbol "+"
           a <- aexp
           return (t+a))
        <|>
       (do t <- aterm
           symbol "-"
           a <- aexp
           return (t-a))
        <|>
       aterm

aterm1 :: Int -> Parser Int;
aterm1 t1 =
 do {
  symbol "*";
  f <- afactor;
  t <- aterm1 (t1 * f);
  return (t);
 }
 <|>
 do {
  symbol "/";
  f <- afactor;
  t <- aterm1 (div t1 f);
  return (t);
 }
 <|>
 return t1;

aterm :: Parser Int
aterm =
 do {
  f <- afactor;
  t <- aterm1 f;
  return (t);
 }

afactor :: Parser Int
afactor =  do
            i <- identifier;
            pullVarVal i;
            <|>
             do
              symbol "("
              a <- aexp
              symbol ")"
              return a
            <|>
           do
            i <- identifier;
            symbol "[";
            index <- aexp
            symbol "]";
            pullVarVal $ i ++ "[" ++ show index ++ "]";
            <|>
          integer


--------------------------------------------------------------
-- BOOLEAN EXPRESSIONS EVALUATION
--------------------------------------------------------------

bexp :: Parser Bool
bexp =  (do b0 <- bterm
            symbol "||"
            b1 <- bexp
            return (b0 || b1))
        <|>
        bterm

bterm :: Parser Bool
bterm = (do f0 <- bfactor
            symbol "&&"
            f1 <- bterm
            return (f0 && f1))
        <|>
        bfactor

bfactor :: Parser Bool
bfactor = (do symbol "True"
              return True)
          <|>
          (do symbol "False"
              return False)
          <|>
          (do symbol "!"
              not <$> bfactor)
          <|>
          (do symbol "("
              b <- bexp
              symbol ")"
              return b)
         <|>
          bcomparison


bcomparison :: Parser Bool
bcomparison =  do a0 <- aexp
                  symbol "!="
                  a1 <- aexp
                  return (a0 /= a1)
             <|>
              (do a0 <- aexp
                  symbol "=="
                  a1 <- aexp
                  return (a0 == a1))
             <|>

              (do a0 <- aexp
                  symbol "<="
                  a1 <- aexp
                  return (a0 <= a1))
                <|>
              (do a0 <- aexp
                  symbol ">="
                  a1 <- aexp
                  return (a0 >= a1))
                  <|>
              (do a0 <- aexp
                  symbol "<"
                  a1 <- aexp
                  return (a0 < a1))
                     <|>
              (do a0 <- aexp
                  symbol ">"
                  a1 <- aexp
                  return (a0 > a1))


--------------------------------------------------------------
-- COMMAND EXPRESSIONS EVALUATION
--------------------------------------------------------------


program :: Parser String
program = (do command
              symbol ";"
              program)  <|> (do
                                command
                                symbol ";")
                                             <|>  command

command :: Parser String
command = assignment <|> arrayAssignment <|>ifThenElse <|> forLoop <|> while  <|> repeatUntil <|> symbol "skip" <|> specialFuns



assignment :: Parser String
assignment =
            do
            x <- identifier
            symbol ":="
            v <- aexp
            updateEnv Variable{name=x, vtype="", value=v}




repeatUntil :: Parser String
repeatUntil = do {
                    w <- consumeRepeatUntil;
                    executeBlock w;
                    symbol "repeat";
                    symbol "{";
                    program;
                    symbol "}";
                    symbol "until";
                    b <- bexp;
                    if (not b) then do{
                        executeBlock w;
                        repeatUntil;
                    } else do{
                        return "";
                    }
                }



specialFuns :: Parser [Char]
specialFuns =
            do
            id <- identifier
            symbol "@("
            symbol "sumall"
            arId <- identifier
            symbol ")"
            arrayVal <- pullArray arId
            updateEnv Variable{name=id, vtype="Integer", value= sumAllElemArr arrayVal}
            <|>
            do
            id <- identifier
            symbol "@("
            symbol "mulall"
            arId <- identifier
            symbol ")"
            arrayVal <- pullArray arId
            updateEnv Variable{name=id, vtype="Integer", value= mulAllElemArr arrayVal}
            <|>
            do
            id <- identifier
            symbol "@("
            a2 <- array
            symbol "+."
            a3 <- array
            symbol ")"
            pushArray id (addArr a2 a3)
            <|>
            do
            id <- identifier
            symbol "@("
            a2 <- array
            symbol "*."
            a3 <- array
            symbol ")"
            pushArray id ( mulArr a2 a3)
            <|>
            do
            id<-identifier
            symbol "@("
            ar1 <- array
            symbol "."
            ar2 <- array
            symbol ")"
            updateEnv Variable{name=id, vtype="Integer", value=dot ar1 ar2}
            <|>
            do
            id <- identifier
            symbol "@("
            ar2 <- array
            symbol "++"
            ar3 <- array
            symbol ")"
            pushArray id (ar2 ++ ar3)




forLoop :: Parser String
forLoop = do
    f <- consumeFor
    executeBlock f
    program
    return ""



ifThenElse :: Parser String
ifThenElse = do symbol "if"
                symbol "("
                b <- bexp
                symbol ")"
                symbol "then"
                symbol "{"
                if b then
                    (do program
                        symbol "}"
                        (do symbol "else"
                            symbol "{"
                            consumeProgram;
                            symbol "}"
                            return "")
                           <|>
                           return "")
                else
                    (do consumeProgram
                        symbol "}"
                        (do symbol "else"
                            symbol "{"
                            program
                            symbol "}"
                            return "")
                         <|>
                         return "")

while :: Parser String
while = do
           w <- consumeWhile
           executeBlock w
           symbol "while"
           symbol "("
           b <- bexp
           symbol")"
           symbol "do"
           symbol "{"
           if b then
               do  program
                   symbol "}"
                   executeBlock w
                   while
           else
               (do consumeProgram
                   symbol "}")








--------------------------------------------------------------
--  PARSING OF INPUT STRINGS
--------------------------------------------------------------


consumeAexp :: Parser String
consumeAexp = (do t <- consumeAterm
                  symbol "+"
                  a <- consumeAexp
                  return (t ++ "+" ++ a))
                <|>
              (do t <- consumeAterm
                  symbol "-"
                  a <- consumeAexp
                  return (t ++ "-" ++ a))
                <|>
                consumeAterm

consumeAterm :: Parser String
consumeAterm =  do {
                    f <- consumeAfactor;
                    do {
                    symbol "*";
                    t <- consumeAterm;
                    return (f ++ "*" ++ t);
                    }
                    <|>
                    do {
                    symbol "/";
                    t <- consumeAterm;
                    return (f ++ "/" ++ t);
                    }
                    <|>
                    return f
                    }


consumeAfactor :: Parser String
consumeAfactor = do symbol "("
                    a <- consumeAexp
                    symbol ")"
                    return ("(" ++ a ++ ")")
                <|>
                 (do symbol "-"
                     f <- consumeAfactor
                     return ("-" ++ f))
                <|>
                 identifier
                <|>
                 (do show <$> integer)
                <|>
                  do
                     i <- identifier;
                     symbol "[";
                     index <- consumeAexp;
                     symbol "]";
                     return $ i ++ "[" ++ index ++ "]";


consumeBexp :: Parser String
consumeBexp = do
                t <- consumeBterm
                symbol "||"
                b <- consumeBexp
                return (t ++ " || " ++ b)
              <|>
              consumeBterm


consumeBterm :: Parser String
consumeBterm = (do f <- consumeBfactor
                   symbol "&&"
                   t <- consumeBterm
                   return (f ++ " && " ++ t))
                <|>
               consumeBfactor

consumeBfactor :: Parser String
consumeBfactor = (do symbol "True"
                     return "True")
                 <|>
                 (do symbol "False"
                     return "False")
                 <|>
                 (do symbol "!"
                     f <- consumeBfactor
                     return ("!" ++ f))
                 <|>
                 (do symbol "("
                     b <- consumeBexp
                     symbol ")"
                     return ("(" ++ b ++ ")"))
                 <|>
                 consumeBcomparison

consumeBcomparison :: Parser String
consumeBcomparison = (do a0 <- consumeAexp
                         symbol "=="
                         a1 <- consumeAexp
                         return (a0 ++ "==" ++ a1))
                     <|>
                     (do a0 <- consumeAexp
                         symbol "<="
                         a1 <- consumeAexp
                         return (a0 ++ "<=" ++ a1))
                       <|>
                     (do a0 <- consumeAexp
                         symbol ">="
                         a1 <- consumeAexp
                         return (a0 ++ ">=" ++ a1))

                     <|>
                     (do a0 <- consumeAexp
                         symbol "<"
                         a1 <- consumeAexp
                         return (a0 ++ "<" ++ a1))
                        <|>
                     (do a0 <- consumeAexp
                         symbol ">"
                         a1 <- consumeAexp
                         return (a0 ++ ">" ++ a1))
                        <|>
                     (do a0 <- consumeAexp
                         symbol "!="
                         a1 <- consumeAexp
                         return (a0 ++ "!=" ++ a1))



consumeCommand :: Parser String
consumeCommand = consumeAssignment <|> consumeArrayAssignment <|>consumeIfThenElse <|> consumeWhile <|> consumeDoWhile <|> consumeFor <|> consumeRepeatUntil
          <|> symbol "skip" <|> consumespecialFuns

consumeProgram :: Parser String
consumeProgram = (do
                   x <- consumeCommand
                   symbol ";"
                   y <- consumeProgram
                   return $ x ++ ";" ++ y) <|>
          (do x <- consumeCommand
              symbol ";"
              return $ x ++ ";")
          <|> consumeCommand


consumeIfThenElse :: Parser String
consumeIfThenElse = do {
                        symbol "if";
                        symbol "(";
                        b <- consumeBexp;
                        symbol ")";
                        symbol "then";
                        symbol "{";
                        p0 <- consumeProgram;
                        symbol "}";
                        do {
                           symbol "else";
                           symbol "{";
                           p1 <- consumeProgram;
                           symbol "}";
                           return ("if (" ++ b ++ ") then {" ++ p0 ++ "} else {" ++ p1 ++ "}"); }
                        <|>
                        return ("if (" ++ b ++ ") then {" ++ p0 ++ "}");
                        }

consumeWhile :: Parser String
consumeWhile  = do
                symbol "while"
                symbol "("
                b <- consumeBexp
                symbol ")"
                symbol "do"
                symbol "{"
                x <- consumeProgram
                symbol "}"
                return $ "while (" ++ b ++ ") do {" ++ x ++ "}"
consumeAssignment :: Parser String
consumeAssignment =
              (do  x <- identifier
                   symbol ":="
                   a <- consumeAexp
                   return (x ++ ":=" ++ a ))



consumespecialFuns :: Parser String
consumespecialFuns =
                   do
                   id <- identifier
                   symbol "@("                 --dot product
                   ar1 <- consumeArray
                   symbol "."
                   ar2 <- consumeArray
                   symbol ")"
                   return $ id ++ "@(" ++ ar1 ++ "." ++ ar2 ++ ")"
               <|>
                   do
                   id <- identifier
                   symbol "@("                --concat array
                   ar1 <- consumeArray
                   symbol "++"
                   ar2 <-consumeArray
                   symbol ")"
                   return $ id ++ "@(" ++ ar1 ++ "++" ++ ar2 ++")"
              <|>
            do
            id <- identifier
            symbol "@("                 --sums all elements in an array
            symbol "sumall"
            arrayID <- consumeArray
            symbol ")"
            return $ id ++ "@(" ++ "sumall" ++ arrayID ++ ")"
            <|>
            do
            id <- identifier
            symbol "@("                      --multiplies all elements in an array
            symbol "mulall"
            arrayID <- consumeArray
            symbol ")"
            return $ id ++ "@(" ++ "mulall" ++ arrayID ++ ")"
            <|>
            do
            id <- identifier
            symbol "@("                     --sums all elements from two array (pairwise)
            arrayID <- consumeArray
            symbol "+."
            arrayID2 <- consumeArray
            symbol ")"
            return $ id ++ "@(" ++ arrayID ++ "+." ++ arrayID2 ++ ")"
           <|>
            do
            id <- identifier
            symbol "@("
            arrayID <- consumeArray             --multiplies all elements from two array (pairwise)
            symbol "*."
            arrayID2 <- consumeArray
            symbol ")"
            return $ id ++ "@(" ++ arrayID ++ "*." ++ arrayID2 ++ ")"


consumeRepeatUntil :: Parser String
consumeRepeatUntil = do{
                        symbol "repeat";
                        symbol "{";
                        p <- consumeProgram;
                        symbol "}";
                        symbol "until";
                        symbol "(";
                        b <- consumeBexp;
                        symbol ")";
                        return("repeat {" ++ p ++ "} until " ++ "( " ++ b ++ " ) " );
                        }



consumeDoWhile :: Parser String
consumeDoWhile =
 do {
  symbol "do";
  symbol "{";
  p <- consumeProgram;
  symbol "}";
  symbol "while";
  symbol "(";
  b <- consumeBexp;
  symbol ")";
  symbol ";";
  return ( p ++ " while(" ++ b ++ ") do {" ++ p ++ "};");
 }


consumeFor :: Parser String
consumeFor = do
                symbol "for";
                symbol "(";
                a <- consumeAssignment;
                symbol ";";
                b <- consumeBexp;
                symbol ";";
                c <- consumeAssignment;
                symbol ";";
                symbol ")";
                symbol "{";
                p <- consumeProgram;
                symbol "}";
                return (a ++ "; while (" ++  b ++ ") do {" ++ p ++ c ++ ";}" );
                <|>
                do
                symbol "for(";
                a <- consumeAssignment;
                symbol ";"
                b <- consumeBexp;
                symbol ";"
                x <- identifier;
                symbol "--";
                symbol ")";
                symbol "{";
                p <- consumeProgram;
                symbol "}";
                return (a ++ "; while (" ++ b ++ ") do {" ++ p ++ x ++ ":=" ++ x ++ "-1;}");
                <|>
                do
                symbol "for"
                symbol "("
                a <- consumeAssignment
                symbol ";"
                b <- consumeBexp
                symbol ";"
                x <- identifier
                symbol "++"
                symbol ")"
                symbol "{"
                p <- consumeProgram
                symbol "}"
                return (a ++ "; while (" ++  b ++ ") do {" ++ p ++ x ++ ":=" ++ x ++ "+1;}")




--------------------------------------------------------------
-- ARRAY IMPLEMENTATION
--------------------------------------------------------------



consumeArrayItems :: Parser String
consumeArrayItems = (do
                      a <- consumeAexp
                      symbol ","
                      b <- consumeArrayItems
                      return (a ++ "," ++ b))
                <|> consumeAexp

consumeArray :: Parser String
consumeArray = (do
                 symbol "["
                 a <- consumeArrayItems
                 symbol "]"
                 return ("[" ++ a ++ "]"))
              <|> identifier


arrayItems :: Parser ArrayType
arrayItems = (do a <- aexp
                 symbol ","
                 as <- arrayItems
                 return (a : as))
             <|>
             (do a <- aexp
                 return [a])



array :: Parser ArrayType
array =  (do symbol "["
             a <- arrayItems
             symbol "]"
             return a)
          <|>
          do i <- identifier
             pullArray i


pushArray :: String -> ArrayType -> Parser String
pushArray var val = P(\env inp -> [(updateArray env var val, "", inp)])

updateArray :: Env -> String -> ArrayType -> Env
updateArray env var val = foldl pushVarVal env x
                          where x = zipWith (\v i ->
                                   Variable { name=var ++ "[" ++ show i ++ "]", vtype="Array", value= v}) val [0..]

searchArray :: Env -> String -> ArrayType
searchArray env array =
     case lookupVarVal env x of
          []    -> []
          value -> concat(value : map (lookupVarVal env) xs)
     where (x:xs) = map (\i -> array ++ "[" ++ show i ++ "]") [0..l]
           l = countElem env
           countElem [] = 0
           countElem (x:xs) = if (array ++ "[") `isPrefixOf` name x
                                then 1 + countElem xs
                                else countElem xs

pullArray :: String -> Parser ArrayType
pullArray name = P(\env inp -> case searchArray env name of
                    [] -> []
                    value -> [(env, value, inp)])



consumeArrayAssignment :: Parser String
consumeArrayAssignment =
              (do id <- identifier
                  symbol ":="
                  id2 <- identifier
                  symbol "["
                  index <- consumeAexp
                  symbol "]"
                  return $ id ++ ":=" ++ id2 ++ "[" ++ index ++ "]")
              <|>

              (do id <- identifier
                  symbol "["
                  index <- consumeAexp
                  symbol "]"
                  symbol ":="
                  id2 <- identifier
                  symbol "["
                  index2 <- consumeAexp
                  symbol "]"
                  return $ id ++ "[" ++ index ++ "]:=" ++ id2 ++ "[" ++ index2 ++ "]" )
              <|>

              (do id <- identifier
                  symbol "["
                  index <- consumeAexp
                  symbol "]"
                  symbol ":="
                  val <- consumeAexp
                  array <- pullArray id
                  return $ id ++ "[" ++ index ++ "]:=" ++ val )
            <|>

              (do id <- identifier
                  symbol ":="
                  arr <- consumeArray
                  return $ id ++ ":=" ++ arr )



arrayAssignment :: Parser String
arrayAssignment =
                  (do
                  id <- identifier
                  symbol "["
                  index <- aexp
                  symbol "]"
                  symbol ":="
                  val <- aexp
                  array <- pullArray id
                  if length array <= index
                  then empty
                  else updateEnv Variable{ name= (id ++ "[" ++ (show index) ++ "]"), vtype="array", value=val})
            <|>

              (do id <- identifier
                  symbol ":="
                  id2 <- identifier
                  symbol "["
                  index <- aexp
                  symbol "]"
                  val <- pullVarVal (id2 ++ "[" ++ (show index) ++ "]")
                  updateEnv Variable{name = id, vtype="int", value=val})
            <|>

              (do id <- identifier
                  symbol "["
                  index <- aexp
                  symbol "]"
                  symbol ":="
                  val <- aexp
                  array <- pullArray id
                  if length array <= index
                  then empty
                  else updateEnv Variable{ name= (id ++ "[" ++ (show index) ++ "]"), vtype="Array", value=val})
                <|>

              (do id <- identifier
                  symbol "["
                  index <- aexp
                  symbol "]"
                  symbol ":="
                  id2 <- identifier
                  symbol "["
                  index2 <- aexp
                  symbol "]"
                  val <- pullVarVal (id2 ++ "[" ++ (show index2) ++ "]")
                  updateEnv Variable{name = (id ++ "[" ++ (show index) ++ "]"), vtype="Array", value=val})
              <|>

              (do id <- identifier
                  symbol ":="
                  arr <- array
                  pushArray id arr)





        