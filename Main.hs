-- Advanced Programming, HW 5
-- by <NAME1> <PENNKEY1>
--    <NAME2> <PENNKEY2>

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
module Main where 

import Control.Monad
import Test.QuickCheck

import Text.PrettyPrint (Doc, (<+>),(<>))
import qualified Text.PrettyPrint as PP

import Parser
import ParserCombinators

import Test.HUnit

type Variable = String
 
data Value =
    IntVal Int
  | BoolVal Bool
  deriving (Show, Eq)
 
data Expression =
    Var Variable
  | Val Value  
  | Op  Bop Expression Expression
  deriving (Show, Eq)
 
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt        
  | Ge       
  | Lt       
  | Le       
  deriving (Show, Eq)

data Statement =
    Assign Variable Expression          
  | If Expression Statement Statement
  | While Expression Statement       
  | Sequence Statement Statement        
  | Skip
  deriving (Show, Eq)

-- Problem 0
---------------------------------------------

wFact :: Statement
wFact = Sequence (Assign "N" (Val (IntVal 2))) (Sequence (Assign "F" (Val (IntVal 1))) (While (Op Gt (Var "N") (Val (IntVal 0))) (Sequence (Assign "X" (Var "N")) (Sequence (Assign "Z" (Var "F")) (Sequence (While (Op Gt (Var "X") (Val (IntVal 1))) (Sequence (Assign "F" (Op Plus (Var "Z") (Var "F"))) (Assign "X" (Op Minus (Var "X") (Val (IntVal 1)))))) (Assign "N" (Op Minus (Var "N") (Val (IntVal 1)))))))))

class PP a where
  pp :: a -> Doc

oneLine :: PP a => a -> IO ()
oneLine = putStrLn . PP.renderStyle (PP.style {PP.mode=PP.OneLineMode}) . pp

indented :: PP a => a -> IO ()
indented = putStrLn . PP.render . pp

instance PP Bop where
  pp Plus   = PP.char '+'
  pp Minus  = PP.char '-'
  pp Times  = PP.char '*'
  pp Divide = PP.char '/'
  pp Gt     = PP.char '>'
  pp Ge     = PP.text ">="
  pp Lt     = PP.char '<'
  pp Le     = PP.text "<="

instance PP Value where
  pp (IntVal     v)  = PP.int v
  pp (BoolVal True)  = PP.text "true"
  pp (BoolVal False) = PP.text "false"


instance PP Expression where
  pp (Var va)           = PP.text va
  pp (Val va)           = pp va
  pp (Op op e1 e2)      = case op of
      Plus   -> pp e1   <+> pp op <+> fpp' e2
      Minus  -> pp e1   <+> pp op <+> fpp' e2
      Times  -> fpp' e1 <+> pp op <+> fpp e2
      Divide -> fpp' e1 <+> pp op <+> fpp e2
      _      -> pp e1   <+> pp op <+> pp e2
      where fpp e  = case e of
                    (Op _ _ _)       -> PP.parens (pp e)
                    _                -> pp e
            fpp' e = case e of
                    (Op Plus   _ _)  -> PP.parens (pp e)
                    (Op Minus  _ _)  -> PP.parens (pp e) 
                    _                -> pp e
      

instance PP Statement where
  pp (Assign v e)     = PP.text v <+> PP.colon <> PP.equals <+> pp e
  pp (If e st1 st2)   = PP.vcat [ PP.text "if" <+> pp e <+> PP.text "then"
                                , PP.nest 2 (pp st1)
                                , PP.text "else " <+> pp st2
                                , PP.text "endif"]

  pp (While e  st)   = PP.vcat [ PP.text "while" <+> pp e <+> PP.text "do"
                               , PP.nest 2 (pp st)
                               , PP.text "endwhile"]

  pp sq@(Sequence _ _) = PP.vcat $ PP.punctuate PP.semi (map pp (toList sq))
  pp (Skip)             = PP.text "skip"

toList :: Statement -> [Statement]
toList (Sequence s1 s2) = s1 : toList s2
toList st                   = [st]

display :: PP a => a -> String
display = show . pp

-- Simple tests 

zeroV,oneV,twoV,threeV :: Expression
zeroV  = Val (IntVal 0)
oneV   = Val (IntVal 1)
twoV   = Val (IntVal 2)
threeV = Val (IntVal 3)

t0 :: Test
t0 = TestList [display oneV ~?= "1",
      display (BoolVal True) ~?= "true",        
      display (Var "X") ~?= "X",
      display (Op Plus oneV twoV) ~?= "1 + 2",
      display (Op Plus oneV (Op Plus twoV threeV)) ~?= "1 + (2 + 3)", 
      display (Op Plus (Op Plus oneV twoV) threeV) ~?= "1 + 2 + 3",
      display (Assign "X" threeV) ~?= "X := 3",
      display Skip ~?= "skip"  ]



--- (Your own test cases go here...)

------------ statement instances

while_instance :: Statement
while_instance = (While (Op Lt (Var "X") (Op Times (Val (IntVal 3)) (Val (IntVal 6)))) s)
  where
    s = Sequence (Assign "Z" (Op Plus (Var "Z") (Var "Y")))
                 (Assign "X" (Op Minus (Var "X") oneV))

if_instance :: Statement
if_instance  = (If (Op Ge (Var "X") zeroV) (Assign "V" (Op Plus zeroV oneV)) Skip)

assign_instance :: Statement
assign_instance  = Sequence (Assign "X" oneV)
                            (Sequence (Assign "Y" twoV)
                                      (Assign "Z" threeV))

-------- test cases

t0_more :: Test
t0_more  = TestList [testtoList, t0b', t0_while, t0_if , t0_ass, t0_seq, t0_paren]

testtoList :: Test
testtoList  = toList (Sequence (Assign "Z" (Op Plus (Var "Z") (Var "Y")))
                               (Assign "X" (Op Minus (Var "X") oneV)))
                           ~?= [Assign "Z" (Op Plus (Var "Z") (Var "Y")),Assign "X" (Op Minus (Var "X") oneV)]

t0b' :: Test
t0b' = display (If (Val (BoolVal True)) Skip Skip) ~?=
      "if true then\n  skip\nelse  skip\nendif"

t0_while :: Test
t0_while = display while_instance ~?=
            "while X < 3 * 6 do\n  Z := Z + Y;\n  X := X - 1\nendwhile"

t0_if :: Test
t0_if = display if_instance ~?=
          "if X >= 0 then\n  V := 0 + 1\nelse  skip\nendif"

t0_ass :: Test
t0_ass = display (Assign "X" (Op Plus (Op Minus (Op Plus oneV twoV) threeV)
                                    (Op Plus oneV threeV)) ) ~?=
           "X := 1 + 2 - 3 + (1 + 3)"
t0_seq :: Test
t0_seq = display (Sequence assign_instance while_instance) ~?=
           "X := 1;\nY := 2;\nZ := 3;\nwhile X < 3 * 6 do\n  Z := Z + Y;\n  X := X - 1\nendwhile"


--test if parentness are added corrected according to the precedence of the opreators

t0_paren :: Test
t0_paren =  display(Assign "Y" (Op Times (Op Plus twoV threeV) oneV)) ~?=
              "Y := (2 + 3) * 1"

------------------------------------------------------------------------
-- Problem 1

valueP :: Parser Value
valueP = intP <|> boolP

intP :: Parser Value
intP = liftM IntVal int

constP :: String -> a -> Parser a
constP s x = (string s) >> return x

boolP :: Parser Value
boolP = liftM BoolVal (constP "true" True <|> constP "false" False)

opP :: Parser Bop 
opP = choice [ constP "+"  Plus
             , constP "-"  Minus
             , constP "*"  Times
             , constP "/"  Divide
             , constP ">=" Ge
             , constP ">"  Gt
             , constP "<=" Le
             , constP "<"  Lt]

varP :: Parser Variable
varP = many1 upper

wsP :: Parser a -> Parser a
wsP p = p >>= \a -> many space >> return a

exprP :: Parser Expression
exprP = wsP (choice [bopP, liftM Val valueP, liftM Var varP])

parenP :: Parser a -> Parser a
parenP p = between (wsP (string "(")) p (wsP (string ")"))

comparitorP :: Parser Bop
comparitorP = choice [ constP ">=" Ge
                     , constP ">" Gt
                     , constP "<=" Le
                     , constP "<" Lt ]
bopP :: Parser Expression
bopP  = liftM3 (flip Op) factorE (wsP $ comparitorP) factorE <|> factorE where
   factorE =  chainl1 (wsP $ prodE) (wsP $ addOp)
   prodE   =  chainl1 (wsP $ compE)  (wsP $ mulOp)
   compE   =  wsP (choice [parenP bopP, liftM Var varP, liftM Val intP])
   addOp   = opP >>= \bop -> case bop of
                              Plus     -> return $ Op Plus
                              Minus    -> return $ Op Minus
                              _        -> fail ""
   mulOp   = opP >>= \bop -> case bop of
                              Times    -> return $ Op Times
                              Divide   -> return $ Op Divide
                              _        -> fail ""

t11 :: Test
t11 = TestList ["s1" ~: succeed (parse exprP "1 "),
                "s2" ~: succeed (parse exprP "1  + 2") ] where
  succeed (Left _)  = assert False
  succeed (Right _) = assert True


-- Test cases for simple expression----

t11_more :: Test
t11_more = TestList [t11_precedence,t11_comp]

t11_precedence :: Test
t11_precedence  = parse exprP "(1 + 2) * (4 / 5)" ~?=
                   Right (Op Times (Op Plus (Val (IntVal 1)) (Val (IntVal 2))) (Op Divide (Val (IntVal 4)) (Val (IntVal 5))))

t11_comp :: Test
t11_comp  = parse exprP "L <= 3 + 4 * 5" ~?= 
             (Right (Op Le (Var "L") (Op Plus (Val (IntVal 3)) (Op Times (Val (IntVal 4)) (Val (IntVal 5))))))
---------------------------------------


statementP :: Parser Statement
statementP = wsP (choice [sequenceP, skipP, assignP, ifP, whileP])

assignP :: Parser Statement
assignP  = do
     v   <-  wsP (varP)
     _   <-  wsP (string ":=")
     val <- exprP
     return $ Assign v val

ifP :: Parser Statement
ifP  = do 
    e    <-  between (wsP (string "if")) (wsP (exprP)) (wsP (string "then"))
    st1  <-  wsP (statementP)
    st2  <-  between (wsP (string "else")) (wsP (statementP)) (wsP (string "endif"))
    return (If e st1 st2)


whileP :: Parser Statement
whileP  = do 
    e  <- between (wsP (string "while")) (wsP (exprP)) (wsP (string "do"))
    st <- wsP (statementP)
    _  <- wsP (string "endwhile")
    return $ While e st

skipP :: Parser Statement
skipP  = wsP (constP "skip" Skip)

sequenceP :: Parser Statement
sequenceP  = do 
    st1 <- choice [skipP, assignP, ifP, whileP]
    _   <- wsP (string ";")
    st2 <- statementP
    return $ Sequence st1 st2


t12 :: Test
t12 = TestList ["s1" ~: p "fact.imp",
                "s2" ~: p "test.imp", 
                "s3" ~: p "abs.imp" ,
                "s4" ~: p "times.imp" ] where
  p s = parseFromFile statementP s >>= succeed
    -- Or: p = succeed <=< parseFromFile statementP
  succeed (Left _)  = assert False
  succeed (Right _) = assert True



-- Test cases for simple statements ---
t12_more :: Test
t12_more  = TestList[t12_seq, t12_skip, t12_ass, t12_if, t12_while]

t12_seq :: Test
t12_seq  = parse statementP "F := 1 + 2 * 3; X := 3" ~?=
            (Right (Sequence (Assign "F" (Op Plus (Val (IntVal 1)) (Op Times (Val (IntVal 2)) (Val (IntVal 3))))) (Assign "X" (Val (IntVal 3)))))
   
t12_skip :: Test
t12_skip  = parse statementP "skip" ~?=
              (Right Skip)

t12_ass :: Test
t12_ass  = parse statementP "X := (1 + 2) * 3" ~?= 
            (Right (Assign "X" (Op Times (Op Plus (Val (IntVal 1)) (Val (IntVal 2))) (Val (IntVal 3)))))

t12_if :: Test
t12_if = parse statementP "if L > 2 then\n  X := 1 else X := 2\n endif" ~?=
           (Right (If (Op Gt (Var "L") (Val (IntVal 2))) (Assign "X" (Val (IntVal 1))) (Assign "X" (Val (IntVal 2)))))

t12_while :: Test
t12_while  = parse statementP "while X < 3 * 6 do\n  skip\nendwhile" ~?= 
              (Right (While (Op Lt (Var "X") (Op Times (Val (IntVal 3)) (Val (IntVal 6)))) Skip))
----------------------------------

testRT :: String -> Assertion
testRT filename = do 
   x <- parseFromFile statementP filename 
   case x of 
     Right ast -> case parse statementP (display ast) of
       Right ast' -> assert (ast == ast')
       Left _ -> assert False
     Left _ -> assert False                             

t13 :: Test
t13 = TestList ["s1" ~: testRT "fact.imp",
                "s2" ~: testRT "test.imp", 
                "s3" ~: testRT "abs.imp" ,
                "s4" ~: testRT "times.imp" ]

-- Generators
genBop :: Gen Bop
genBop = elements [ Plus, Minus, Times, Divide ]

genCmp :: Gen Bop
genCmp = elements [ Gt, Ge, Lt, Le ]

genVar :: Gen Variable
genVar = listOf1 (elements ['A'..'Z'])

genOp :: Gen Expression
genOp = oneof [ genCmpOp, genArithOp ]

genExpr :: Gen Expression
genExpr = oneof [ liftM Var genVar, liftM (Val . IntVal) arbitrary ]

genArithOp :: Gen Expression
genArithOp = liftM3 Op genBop arith arith where
  arith = frequency [ (2, genExpr)
                    , (1, genArithOp) ]

genCmpOp :: Gen Expression
genCmpOp = liftM3 Op genCmp genExpr genExpr

instance Arbitrary Value where
  arbitrary = frequency [ (6, liftM IntVal arbitrary)
                        , (2, return $ BoolVal True)
                        , (2, return $ BoolVal False) ]

instance Arbitrary Expression where
  arbitrary = frequency [ (1, genOp)
                        , (2, liftM Val arbitrary)
                        , (2, liftM Var genVar) ]

instance Arbitrary Statement where
  arbitrary = frequency [ (3, liftM2 Assign genVar arbitrary)
                        , (1, liftM3 If arbitrary arbitrary arbitrary)
                        , (1, liftM2 While arbitrary arbitrary)
                        , (1, liftM2 Sequence arbitrary arbitrary)
                        , (2, return Skip) ]

prop_roundtrip :: Statement -> Bool
prop_roundtrip stm = case parse statementP (display stm) of
                          Right stm'  -> (toList stm) == (toList stm')
                          Left _      -> False


------------------------------------------------------------------------
-- Problem 2

data Token = 
     TokVar String     -- variables
   | TokVal Value      -- primitive values
   | TokBop Bop        -- binary operators
   | Keyword String    -- keywords        
      deriving (Eq, Show)

keywords :: [ Parser Token ]
keywords = map (\x -> constP x (Keyword x)) 
             [ "(", ")", ":=", ";", "if", "then", "else",
             "endif", "while", "do", "endwhile", "skip" ]

type Lexer = Parser [Token]

lexer :: Lexer
lexer = sepBy1
        (liftM TokVal valueP <|>
         liftM TokVar varP   <|>
         liftM TokBop opP    <|>
         choice keywords)
        (many space)

t2 :: Test
t2 = parse lexer "X := 3" ~?= 
        Right [TokVar "X", Keyword ":=", TokVal (IntVal 3)]

-- methods for parsing statement on tokens


-- token parser for expression, simple convert the parser type using TknParser,
-- and use monad State instead of string concatenation and split.

valueTP :: TknParser Token Value
valueTP =  do tkn <- getE
              case tkn of
                   TokVal v -> return v
                   _        -> fail ""

intTP :: TknParser Token Value
intTP  = do tkn <- getE
            case tkn of
                 TokVal (IntVal v) -> return $ IntVal v
                 _                 -> fail ""

boolTP :: TknParser Token Value
boolTP = do tkn <- getE
            case tkn of
                 TokVal (BoolVal bv) -> return $ BoolVal bv
                 _                   -> fail ""


-- don't need since it's tokens, there are no whitespace here
-- wsTP  :: TknParser Token a -> TknParser Token a
-- wsTP p = p >>= \a -> many space >> return a

-- don't need since operator are opTP here
{-
opP :: Parser Bop 
opP = choice [ constP "+"  Plus
             , constP "-"  Minus
             , constP "*"  Times
             , constP "/"  Divide
             , constP ">=" Ge
             , constP ">"  Gt
             , constP "<=" Le
             , constP "<"  Lt]
-}

opTP :: TknParser Token Bop
opTP  = do tkn <- getE
           case tkn of
                TokBop bop  -> return bop
                _           -> fail ""
              

varTP :: TknParser Token Variable
varTP  = do tkn <- getE
            case tkn of
                 TokVar v -> return v
                 _        -> fail ""

-- may not be used, instead we only need to parse keyword here
-- constTP :: String -> a -> TknParser char a
-- constTP s x = (string s) >> return x

-- new addition : like constP, check if the string s is within keyword set, and parse it accordingly.

keywordTP  :: String ->  TknParser Token Token
keywordTP s = do tkn <- getE
                 case tkn of
                      Keyword k -> if s == k then return tkn else fail ""
                      _         -> fail ""

parenTP  :: TknParser Token a -> TknParser Token a
parenTP p = between (keywordTP "(") p (keywordTP ")")

comparitorTP :: TknParser Token Bop
comparitorTP =  do tkn <- opTP
                   case tkn of
                        Ge  -> return Ge
                        Gt  -> return Gt
                        Le  -> return Le
                        Lt  -> return Lt
                        _   -> fail ""

bopTP :: TknParser Token Expression
bopTP  = liftM3 (flip Op) factorE (comparitorTP) factorE <|> factorE where
   factorE =  chainl1 (prodE) (addOp)
   prodE   =  chainl1 (compE) (mulOp)
   compE   =  choice [parenTP bopTP, liftM Var varTP, liftM Val intTP]
   addOp   = opTP >>= \bop -> case bop of
                                   Plus     -> return $ Op Plus
                                   Minus    -> return $ Op Minus
                                   _        -> fail ""
   mulOp   = opTP >>= \bop -> case bop of
                                   Times    -> return $ Op Times
                                   Divide   -> return $ Op Divide
                                   _        -> fail ""

expTP :: TknParser Token Expression
expTP  = choice [bopTP, liftM Val valueTP, liftM Var varTP]

-------- test cases for parsing token statements into expression

t21_bsc :: Test
t21_bsc  = TestList ["s1" ~: succeed (parse expTP [TokVal (IntVal 1)])
                    ,"s2" ~: succeed (parse expTP [TokVal (IntVal 1),TokBop Plus,TokVal (IntVal 2)])]
 where   succeed (Left _)  = assert False
         succeed (Right _) = assert True

t21_more :: Test
t21_more  = TestList [t21_precedence,t21_comp]

t21_precedence :: Test
t21_precedence  = parse expTP [(Keyword "("),TokVal (IntVal 1),TokBop Plus,TokVal (IntVal 2),Keyword ")",TokBop Times,Keyword "(",TokVal (IntVal 4),TokBop Divide,TokVal (IntVal 5),Keyword ")"]  ~?=
                   Right (Op Times (Op Plus (Val (IntVal 1)) (Val (IntVal 2))) (Op Divide (Val (IntVal 4)) (Val (IntVal 5)))) 

t21_comp :: Test
t21_comp  = parse expTP  [TokVar "L",TokBop Le,TokVal (IntVal 3),TokBop Plus,TokVal (IntVal 4),TokBop Times,TokVal (IntVal 5)] ~?= Right (Op Le (Var "L") (Op Plus (Val (IntVal 3)) (Op Times (Val (IntVal 4)) (Val (IntVal 5)))))

-------- prasing token statements into statement

statementTP :: TknParser Token Statement
statementTP  = choice [sequenceTP, skipTP, assignTP, ifTP, whileTP]

assignTP :: TknParser Token Statement
assignTP  = do
     v   <-  varTP
     _   <-  keywordTP ":="
     val <-  expTP
     return $ Assign v val

ifTP :: TknParser Token Statement
ifTP  = do 
    e    <-  between (keywordTP "if") expTP (keywordTP "then")
    st1  <-  statementTP
    st2  <-  between (keywordTP "else") statementTP (keywordTP "endif")
    return (If e st1 st2)


whileTP :: TknParser Token Statement
whileTP  = do 
    e  <- between (keywordTP "while") expTP (keywordTP "do")
    st <- statementTP
    _  <- keywordTP "endwhile"
    return $ While e st

skipTP :: TknParser Token Statement
skipTP  = liftM (\_ -> Skip) (keywordTP "skip")

sequenceTP :: TknParser Token Statement
sequenceTP  = do 
    st1 <- choice [skipTP, assignTP, ifTP, whileTP]
    _   <- keywordTP ";"
    st2 <- statementTP
    return $ Sequence st1 st2

-------- test cases for tokens statement parsing

t22_more :: Test
t22_more  = TestList[t22_seq, t22_skip, t22_ass, t22_if, t22_while]

t22_seq :: Test
t22_seq  = parse statementTP [TokVar "F",Keyword ":=",TokVal (IntVal 1),TokBop Plus,TokVal (IntVal 2),TokBop Times,TokVal (IntVal 3),Keyword ";",TokVar "X",Keyword ":=",TokVal (IntVal 3)] ~?=
            (Right (Sequence (Assign "F" (Op Plus (Val (IntVal 1)) (Op Times (Val (IntVal 2)) (Val (IntVal 3))))) (Assign "X" (Val (IntVal 3)))))
   
t22_skip :: Test
t22_skip  = parse statementTP [Keyword "skip"] ~?=
              (Right Skip)

t22_ass :: Test
t22_ass  = parse statementTP [TokVar "X",Keyword ":=",Keyword "(",TokVal (IntVal 1),TokBop Plus,TokVal (IntVal 2),Keyword ")",TokBop Times,TokVal (IntVal 3)] ~?= 
            (Right (Assign "X" (Op Times (Op Plus (Val (IntVal 1)) (Val (IntVal 2))) (Val (IntVal 3)))))

t22_if :: Test
t22_if = parse statementTP [Keyword "if",TokVar "L",TokBop Gt,TokVal (IntVal 2),Keyword "then",TokVar "X",Keyword ":=",TokVal (IntVal 1),Keyword "else",TokVar "X",Keyword ":=",TokVal (IntVal 2),Keyword "endif"] ~?=
           (Right (If (Op Gt (Var "L") (Val (IntVal 2))) (Assign "X" (Val (IntVal 1))) (Assign "X" (Val (IntVal 2)))))

t22_while :: Test
t22_while  = parse statementTP [Keyword "while",TokVar "X",TokBop Lt,TokVal (IntVal 3),TokBop Times,TokVal (IntVal 6),Keyword "do",Keyword "skip",Keyword "endwhile"] ~?= 
              (Right (While (Op Lt (Var "X") (Op Times (Val (IntVal 3)) (Val (IntVal 6)))) Skip))




prop_groundtrip    :: Statement -> Bool
prop_groundtrip stm = case parse lexer (display stm) of 
                          Right tokens -> case parse statementTP tokens of
                                               Right stm'    -> (toList stm) == (toList stm')
                                               Left  _      -> False
                          Left  _      -> False
-----------------------------------------------------------------
-- A main action to run all the tests...

main :: IO () 
main = do _ <- runTestTT (TestList [ t0, t0_more,                          -- Prob 0
                                     t11, t11_more,  t12, t12_more, t13,   -- Prob 1
                                     t2, t21_bsc, t21_more,t22_more])  -- Prob 2
          return ()

