{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Applicative ( Alternative(..) )
import System.IO ( hFlush, stdout )
import Interpreter ( run )
import Parsers ( parse, consumeProgram ) 
import Utils ( Env,trim )


logo :: IO String
logo = do
    putStrLn ""
    putStrLn "8888888.888b.....d888.8888888b........88888888888.8888888888.8888888b..."
    putStrLn "..888...8888b...d8888.888...Y88b..........888.....888........888...Y88b."
    putStrLn "..888...88888b.d88888.888....888..........888.....888........888....888."
    putStrLn "..888...888Y88888P888.888...d88P..........888.....8888888....888...d88P."
    putStrLn "..888...888.Y888P.888.8888888P............888.....888........8888888P.."
    putStrLn "..888...888..Y8P..888.888.................888.....888........888.T88b..."
    putStrLn "..888...888.......888.888.................888.....888........888..T88b.."
    putStrLn "8888888.888.......888.888.................888.....8888888888.888...T88b."
    showhelp;
    menu [[]];

menu :: [Env] -> IO String
menu [] = do logo;
menu [env] = do {
             putStr "IMP::TER> ";
             hFlush stdout;
             input <- getLine;
             case input of
                ":c" -> do
                        putStr "\nClearing the memory..\n\n"
                        menu [[]];
                ":q" -> return "Thank you for using IMP_ter! Bye!";
                ":h:"-> showhelp;
                ":g" -> showgrammar;
                ":e" -> showexamples;
                ":f" -> do
                        putStr "\nInserisci il nome del file\n"
                        fileName <- getLine 
                        contents <- trim <$> readFile fileName  
                        putStr contents
                        let res = run (parse consumeProgram env contents )
                        putStrLn (snd res)
                        menu (fst res)
                _-> do
                        let res = run (parse consumeProgram env input)
                        putStrLn (snd res)
                        menu (fst res)
                }

main :: IO String
main = logo;

showhelp :: IO String
showhelp =      do
                putStrLn "\nHello there, these are your options:\n\n"
                putStrLn ":c -> clear the screen, erase MEMORY!\n"
                putStrLn ":g -> show grammar rules\n"
                putStrLn ":e -> example programs to cut and paste!\n"
                putStrLn ":f -> load your file containing a program written for IMP::TER!\n";
                putStrLn ":h -> show again this help..\n";
                putStrLn "OR write your program here in the terminal!\n";
                return ""


showgrammar :: IO String
showgrammar =   do
                putStrLn "Arithmetic expression grammar:";
                putStrLn "";
                putStrLn "<aexp> ::= <aterm> + <aexp>";
                putStrLn "              | <aterm>  - <aexp>";
                putStrLn "              | <aterm>";
                putStrLn "";
                putStrLn "<aterm> ::= <afactor> * <aterm> | <afactor>";
                putStrLn "";
                putStrLn "<afactor> ::= (<aexp>) * <integer> | <identifier>";
                putStrLn "";
                putStrLn "Boolean expression grammar:";
                putStrLn "";
                putStrLn "<bexp> ::= <bterm> || <bexp> | <bterm>";
                putStrLn "";
                putStrLn "<bterm> ::= <bfactor> && <bterm> | <bfactor>";
                putStrLn "";
                putStrLn "<bfactor> ::= true | false ";
                putStrLn "     | !<bfactor> ";
                putStrLn "     | (boolExp)";
                putStrLn "     | <bcomparison>";
                putStrLn "";
                putStrLn "< bcomparison> ::=            <aexp> ==   <aexp>   | ";
                putStrLn "                              <aexp> !=    <aexp>   | ";
                putStrLn "                              <aexp> <=   <aexp>   |";
                putStrLn "                              <aexp> >=   <aexp>   |";
                putStrLn "                              <aexp>  >    <aexp>   |";
                putStrLn "                              <aexp>  <    <aexp>   |";
                putStrLn "";
                putStrLn "Command grammar:";
                putStrLn "";
                putStrLn "<program>  ::= <command> | <command> <program>";
                putStrLn "";
                putStrLn "";
                putStrLn "<command > ::= <assignment> | <ifThenElse> | <while> | skip;";
                putStrLn "";
                putStrLn "";
                putStrLn "";
                putStrLn "<assignment> ::= <identifier>  :=  <aexp>";
                putStrLn "                                      | := <array> ++ <array> ";
                putStrLn "                                      | @ <array> . <array>";
                putStrLn "                                      | := <array>";
                putStrLn "                                      | := <identifier> <aexp>";
                putStrLn "                                      | <aexp> := <identifier> <aexp>";
                putStrLn "";
                putStrLn "                                      | <identifier> <aexp> := <aexp>";
                putStrLn "                                      | <specialfun>";
                putStrLn "";
                putStrLn "";
                putStrLn "<ifThenElse> ::=      if (<boolExp>) then { <program> }";
                putStrLn "                     |if (<boolExp>)  then {<program>} else {<program>}";
                putStrLn "";
                putStrLn "";
                putStrLn "<for>::= for (<assignment> <boolExp> <assignment>) {<program>}";
                putStrLn "< while>::= while (<boolExp>) do {<program>}";
                putStrLn "";
                putStrLn "Identifier:";
                putStrLn "";
                putStrLn "<lower> ::= a-z";
                putStrLn "<upper ::= A-Z";
                putStrLn "<integer> ::= [-]<nat>";
                putStrLn "<identifier> ::= <lower>|<lower><alphanum>";
                putStrLn "<alphanum>::=<upper><alphanum>";
                putStrLn "     | <lower> <alphanum> ";
                putStrLn "     |<natural> <alphanum>";
                putStrLn "     | <upper> |<lower> | <natural>";
                putStrLn "<lower> ::= a-z";
                putStrLn "<upper>::= A-Z";
                putStrLn "\n"
                menu [[]]



showexamples :: IO String
showexamples =  do{
                putStrLn "\nHere is a list of test programs, feel free to copy and paste!\n";

                putStrLn " x:= 1; b:=0;\n";

                putStrLn " z:=(b*1+2*x);\n";

                putStrLn " b:=0; for (j:=9; j>0; j--) {b:=b+1;};\n";

                putStrLn " h:=15; b:=1; while (h>10) do {b:=b+1; h:=h-1;};\n";

                putStrLn " m:=5; if (m >= 6) then {k:= 10} else {k:= 6};\n";

                putStrLn " u:=[0,1]; v:=[2,3]; p@u.v; n:=u++v;\n";

                putStrLn " v2:=[3,5];v1[1]:=10;c:=v2[1];v2[1]:=v1[4];v1[1]:=10;w1:= (v1 +. v2); aa:= (v1 *. v1);\n";

                putStrLn " if (s1 > 0) then {i:=3;} else {i:=7;};\n";

                putStrLn " if (s1 > 0 && s2 < 0) then {i:=3;} else {i:=7;}\n;";

                putStrLn " if (s1 > 0 || False ) then {k:=3;} else {k:=7;};\n\n";
                menu[[]]               
                 }

