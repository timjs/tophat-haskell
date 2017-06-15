implementation module Util

import StdEnv
import Text.Parsers.CParsers.ParserCombinators
import Data.List

import itasks

pEvent :: CParser Char Event Event
pEvent = symbol 'r' &> yield Refresh
     <|> symbol 'c' &> pSpace &> pCommit
     <|> symbol 'u' &> pSpace &> pUpdate

pCommit = Commit @> pTaskId
pUpdate = Update @> pTaskId +&+ pSpace -&+ pDynamic

pSpace = symbol ' '
pTaskId = <+> digit
pNumber = (foldl (\sum dig -> sum*10+dig) 0) @> (<+> digit)
pBool = token (fromString "True") &> yield True
    <|> token (fromString "False") &> yield False
pString = symbol '"' &> (toString @> (<*> (satisfy (\x = x =!= '"')))) <& symbol '"'
pDynamic = pNumber <&> yieldDynamic
       <|> pBool <&> yieldDynamic
       <|> pString <&> yieldDynamic

yieldDynamic val = yield (dynamic val)

parseEvent :: String -> Event
parseEvent input = snd (head ((begin pEvent) (fromString input)))
