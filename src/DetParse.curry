module DetParse where

type Parser a = String -> [(a, String)]

parse :: Parser a -> String -> Maybe a
parse p s = case filter (null . snd) $ p s of
  ((x, _):_) -> Just x
  _          -> Nothing

failure :: Parser a
failure = \_ -> []

yield :: a -> Parser a
yield x = \s -> [(x, s)]

empty :: Parser ()
empty = yield ()

anyChar :: Parser Char
anyChar = \s -> case s of
                  []      -> []
                  (c:cs)  -> [(c,cs)]

check :: (a -> Bool) -> Parser a -> Parser a
check ok p = filter (ok . fst) . p

char :: Char -> Parser ()
char c = check (c==) anyChar *> yield ()

word :: String -> Parser ()
word []     = empty
word (c:cs) = char c *> word cs

infixl 3 <|>, <!>

(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = \s -> p s ++ q s

(<!>) :: Parser a -> Parser a -> Parser a
p <!> q = \s -> case p s of
                  [] -> q s
                  xs -> xs

infixl 4 <$>

(<$>) :: (a -> b) -> Parser a -> Parser b
f <$> p = map (\(x, s) -> (f x, s)) . p

infixl 4 <*>, <*, *>

(<*>) :: Parser (a -> b) -> Parser a -> Parser b
p <*> q = \s -> [ (f x, s2) | (f, s1) <- p s,
                              (x, s2) <- q s1 ]

(<*) :: Parser a -> Parser b -> Parser a
p <* q = (\x _ -> x) <$> p <*> q

(*>) :: Parser a -> Parser b -> Parser b
p *> q = (\_ y -> y) <$> p <*> q

infixl 1 *>=

(*>=) :: Parser a -> (a -> Parser b) -> Parser b
p *>= f = \s -> [ (y, s2) | (x, s1) <- p s,
                            (y, s2) <- (f x) s1 ]

many :: Parser a -> Parser [a]
many p = some p <|> yield []

some :: Parser a -> Parser [a]
some p = (:) <$> p <*> many p
