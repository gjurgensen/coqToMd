{-# LANGUAGE FlexibleContexts #-}

module Parse where

import Text.Parsec

-- Count occurrences
num x = length <$> many x

unless p end = 
  (end >> unexpected "Unexpected") <|> p

many1Till p end = do
  x <- unless p end
  y <- manyTill p end
  pure $ x : y

seeStr :: Stream s m Char => String -> ParsecT s u m String
seeStr = try . lookAhead . string

mdSection :: Parsec String () String
mdSection = do
  content <- between (string "(**") (string "*)") $ do
    -- n <- num (spaces >> char '*')
    spaces
    n <- num $ char '*'
    content <- manyTill (anyChar <|> newline) $ seeStr "*)"
    pure $ replicate n '#' ++ content
  spaces
  pure content

coqSection = do 
  content <- many1Till anyChar $ seeStr "(**"
  spaces
  pure $ unlines
    [ "{% highlight Coq %}"
    , content ++ "{% endhighlight %}" ]
  -- pure $ unlines
  --   [ "{% highlight Coq %}"
  --   , content
  --   , "{% endhighlight %}" ]

coqToMd = do
  spaces
  content <- unlines <$> many1 (mdSection <|> coqSection)
  spaces
  eof
  pure content

parseCoqToMd =
  parse coqToMd ""