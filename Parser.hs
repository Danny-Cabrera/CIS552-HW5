-- Advanced Programming, HW #7
-- 
-- This file defines a new type TknParser that can parse either a list of chars or a list of tokens
-- according to the given parameter. The functionalities here are quite simple to what it is in 
-- "Parser.hs". Here we simple do a rewrite on the functions so that they now accept either char or
-- tokens.

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}


module      Parser ( TknParser
                   , Parser
                   , doParse
                   , getE
                   , choose
                   , (<|>)
                   , satisfy
                   ) where

import Control.Monad.State

-- the new type TknParser can parse either list of chars or list of tokens, according to 
-- the first parameter 'e'

newtype TknParser e a = P (StateT [e] [] a)


-- generalize the typical parser into a TknParser Char type.

type Parser a = TknParser Char a


-- to make the new type as a Monad

instance Monad (TknParser tp) where
  p1 >>= f = P $ StateT (\st -> do (x, st') <- doParse p1 st
                                   doParse (f x) st')
  return x = P $ return x
  fail _   = P $ StateT (\_ -> [])



-- execute the parsing on the b element. The intermediate result are kept using Monad state.

doParse         :: TknParser b a -> [b] -> [(a, [b])]
doParse (P p) st = runStateT p st



-- returns the next element (could either be char or token)

getE :: TknParser e e 
getE = P ( do (c:cs) <- get
              put cs
              return c)


-- returns the next element if the current predicate is satisfied

satisfy   :: (e -> Bool) -> TknParser e e 
satisfy p = do c <- getE
               if (p c) then return c 
                        else fail "" 


-- | Combine two parsers together in parallel
                 
choose :: TknParser e a -> TknParser e a -> TknParser e a
p1 `choose` p2 = P $ StateT (\st -> doParse p1 st ++ doParse p2 st)


-- | Combine two parsers together in parallel, but only use the 
-- first result.

(<|>)    :: TknParser e a -> TknParser e a -> TknParser e a
p1 <|> p2 = P $ StateT (\st -> case doParse (p1 `choose` p2) st of
                                 []  -> []
                                 x:_ -> [x])