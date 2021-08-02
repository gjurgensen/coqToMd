{-# LANGUAGE FlexibleContexts #-}

module Parse where

import Data.Char

import Text.Parsec

-- Count occurrences
num x = length <$> many x

nonspace :: Stream s m Char => ParsecT s u m Char
nonspace = satisfy $ not . isSpace

notStr = notFollowedBy . string

blankLine :: Stream s m Char => ParsecT s u m String
blankLine = try $ do 
  s <- many $ oneOf " \t\f\v"
  nl <- newline <|> crlf
  pure $ s ++ [nl]

blankLines :: Stream s m Char => ParsecT s u m ()
blankLines = skipMany blankLine

mdSection :: Parsec String () String
mdSection = do
  between (string "(**") (string "*)") $ do
    n <- num $ try ( spaces >> notStr "*)" >> char '*')
    content <- many (notStr "*)" >> anyChar)
    pure $ replicate n '#' ++ content

-- TODO: should coq section be ended prematurely if the only characters left are spaces?
coqSection = try $ do 
  padding <- many space
  c <- notStr "(**" >> nonspace
  ontent <- many (
    notFollowedBy (spaces >> string "(**") >>
    anyChar)
  pure $ unlines
    ["{% highlight Coq %}"
    , padding ++ c:ontent,
    "{% endhighlight %}" ]

coqToMd = do
  spaces
  content <- many (
    blankLine <|>
    mdSection <|>
    coqSection <|>
    many1 space)
  eof 
  pure $ concat content

parseCoqToMd =
  parse coqToMd ""