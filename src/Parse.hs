{-# LANGUAGE FlexibleContexts #-}

module Parse where

import Data.Char

import Text.Parsec

-- Count occurrences
num x = length <$> many x

nonspace :: Stream s m Char => ParsecT s u m Char
nonspace = satisfy $ not . isSpace

notStr = notFollowedBy . string

mdSection :: Parsec String () String
mdSection = do
  between (string "(**") (string "*)") $ do
    -- n <- num (spaces >> char '*')
    spaces
    n <- num (notStr "*)" >> char '*')
    content <- many (notStr "*)" >> anyChar)
    pure $ replicate n '#' ++ content

-- TODO: should coq section be ended prematurely if the only characters left are spaces?
coqSection = try $ do 
  padding <- many space
  c <- notStr "(**" >> nonspace
  ontent <- many (notStr "(**" >> anyChar)
  spaces
  pure $ unlines
    ["{% highlight Coq %}"
    , padding ++ c:ontent ++ "{% endhighlight %}" ]

coqToMd = do
  spaces
  content <- concat <$> many (
    many1 (newline <|> crlf) <|>
    mdSection <|>
    coqSection <|>
    many1 space)
  eof 
  pure content

parseCoqToMd =
  parse coqToMd ""