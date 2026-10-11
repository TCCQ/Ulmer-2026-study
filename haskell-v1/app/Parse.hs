{-# LANGUAGE FlexibleContexts #-}

-- | A parser for the level-2 language, producing 'DProgram', 'DDecl', 'DExp'
-- and 'SExp' values.
--
-- Two rules hold throughout, and together they give the grammar its shape.
--
-- * /Whitespace, linebreaks and comments carry no meaning./  Every token goes
--   through 'tok', which skips the whitespace and any comments before and after
--   it, so no parser has to remember to skip any and no token can be
--   accidentally fused to its neighbour.  A comment runs from @#@ to the end of
--   the line.
--
-- * /A token either matches or leaves the input exactly where it was./  'tok'
--   wraps the match in 'try' so that a token which fails to match also rolls
--   back the whitespace it skipped on the way in.  That is what lets tokens
--   sit inside a '<|>' alternative without consuming input the alternatives
--   behind them need to see.
--
-- The parser runs in the language's own monad, 'TM', with a counter as its
-- state; that counter is where the tag on each implementation comes from.  The
-- environment is the same one the rest of the language uses, though nothing here
-- reads it: it is there so that the parser and the expander share a type and the
-- tag counter is threaded the same way as everything else.
module Parse where

import Control.Monad (void, unless)
import Data.Char (digitToInt, isAlphaNum)
import Data.Functor.Identity (Identity)
import Data.List (nub)
import Data.Maybe (catMaybes, fromMaybe)

import Text.Parsec

import Ast


-- | The parser's state: the counter that hands out implementation tags and the
-- static variables currently in scope.  Static variables are picked out by the
-- enclosing parameter lists rather than by a tick, so the scope has to travel
-- with the parser.
--
-- This is Parsec's own user state, which 'runP' seeds and Parsec threads through
-- the combinators, and not the 'TM' the rest of the language runs in.
data PState = PState
  { psTag    :: Int
  , psStatic :: [SName]
  }

-- | A parser, polymorphic in the character stream as the worked example is.
type P s a = ParsecT s PState Identity a

-- | The next implementation tag, taken from the parser's state.  Tags only have
-- to be distinct, so nothing is reclaimed when a parse fails.
freshTag :: P s Int
freshTag = do
  st <- getState
  putState st { psTag = psTag st + 1 }
  pure (psTag st)

-- | Run a parser with some extra static variables in scope, restoring the
-- previous scope afterwards.  Used for the parameter lists of @dec@ and @imp@.
withStatic :: [SName] -> P s a -> P s a
withStatic names p = do
  st <- getState
  putState st { psStatic = names ++ psStatic st }
  result <- p
  modifyState (\st' -> st' { psStatic = psStatic st })
  pure result

-- | Whether a name denotes a static variable in the current scope.  Names not
-- in scope are type constructors.
inStatic :: SName -> P s Bool
inStatic n = elem n . psStatic <$> getState


-- TOKENS
-- ======

{- | Whitespace, linebreaks and comments are insignificant and may appear, in any
amount, between any two tokens.
-}
wt :: Stream s Identity Char => P s ()
wt = skipMany (void space <|> comment)

{- | A comment, running from @#@ to the end of the line.  It is not nested.
-}
comment :: Stream s Identity Char => P s ()
comment = char '#' *> skipMany (satisfy (\c -> c /= '\n' && c /= '\r'))

{- | A single token, surrounded by insignificant whitespace.

Skipping on the way in matters as much as skipping on the way out, and the 'try'
matters just as much: a token that fails to match must leave the input exactly
where it was, whitespace included, so that '<|>' can try the alternatives behind
it.
-}
tok :: Stream s Identity Char => P s a -> P s a
tok p = try (wt >> p <* wt)

{- | Punctuation.  No word boundary is wanted, because punctuation is never part
of a name.
-}
sym :: Stream s Identity Char => String -> P s String
sym = tok . string

-- | Punctuation whose text is of no interest.
pun :: Stream s Identity Char => String -> P s ()
pun w = sym w >> pure ()

{- | A reserved word, which must not be the start of a longer name, so that
@iffy@ is one name and not @if@ followed by @fy@.
-}
kw :: Stream s Identity Char => String -> P s String
kw w = tok (string w >> notFollowedBy identTail >> pure w)

-- | A reserved word whose name is of no interest.
kwOnly :: Stream s Identity Char => String -> P s ()
kwOnly w = kw w >> pure ()

{- | One operator, matched longest first, so that @<=@ is preferred over @<@ and
a comparison never comes apart into two tokens.
-}
op :: Stream s Identity Char => [String] -> P s String
op = choice . map sym . longestFirst
  where longestFirst = foldr insertBy []
        insertBy s [] = [s]
        insertBy s (r:rs)
          | length s > length r = s : r : rs
          | otherwise = r : insertBy s rs

-- | A character that may appear in a name after its first.
identTail :: Stream s Identity Char => P s Char
identTail = satisfy (\c -> isAlphaNum c || c == '_' || c == '$')

-- | The reserved words, which may not be used as names.
reserved :: [String]
reserved =
  [ "lam", "fix", "if", "then", "else", "let", "in", "end"
  , "val", "dec", "imp", "local", "true", "false"
  ]

-- | A name: a letter or underscore followed by letters, digits, @_@ or @$@.
-- Reserved words are rejected.
name :: Stream s Identity Char => P s String
name = tok $ do
  c <- letter <|> char '_'
  cs <- many identTail
  let n = c : cs
  if n `elem` reserved then fail ("reserved word " ++ n) else pure n

-- | A non-negative integer literal, folded rather than accumulated as a string.
natural :: Stream s Identity Char => P s Int
natural = tok $ do
  d <- digit
  ds <- many digit
  pure $ foldl (\acc c -> 10 * acc + digitToInt c) 0 (d : ds)

{- | A string literal.  A backslash escapes the character after it, and the
abbreviations @\\n@, @\\r@, @\\t@, @\\\\@ and @\\"@ are recognised.  Unknown
escapes and unescaped linebreaks are syntax errors.
-}
stringLit :: Stream s Identity Char => P s String
stringLit = tok $ char '"' *> many stringChar <* char '"'
  where
    stringChar =
      (char '\\' *> escaped) <|>
      satisfy (\c -> c /= '"' && c /= '\n' && c /= '\r')
    escaped = choice
      [ char 'n'  >> pure '\n'
      , char 'r'  >> pure '\r'
      , char 't'  >> pure '\t'
      , char '\\' >> pure '\\'
      , char '"'  >> pure '"'
      ]


-- STATIC EXPRESSIONS
-- ==================

-- | A comma-separated list between two pieces of punctuation.  Empty lists and
-- trailing commas are allowed, so @[]@, @[a]@ and @[a, b,]@ are all lists.
commaList :: Stream s Identity Char => String -> String -> P s a -> P s [a]
commaList open close item = do
  pun open
  xs <- loop
  pun close
  pure xs
  where
    loop = do
      mFirst <- optionMaybe item
      case mFirst of
        Nothing -> pure []
        Just x  -> (x :) <$> rest
    rest = do
      mComma <- optionMaybe (sym ",")
      case mComma of
        Nothing -> pure []
        Just _  -> do
          mNext <- optionMaybe item
          case mNext of
            Nothing -> pure []
            Just y  -> (y :) <$> rest

{- | A parenthesised list.  With no comma it is grouping and yields the one item;
otherwise it is the tuple built by the supplied constructor.  Empty lists and
trailing commas are allowed.
-}
parenthesized :: Stream s Identity Char => P s a -> ([a] -> a) -> P s a
parenthesized item mkTuple = do
  pun "("
  mFirst <- optionMaybe item
  case mFirst of
    Nothing -> do pun ")"; pure (mkTuple [])
    Just x  -> do
      mComma <- optionMaybe (sym ",")
      case mComma of
        Nothing -> do pun ")"; pure x
        Just _  -> do
          mNext <- optionMaybe item
          case mNext of
            Nothing -> do pun ")"; pure (mkTuple [x])
            Just y  -> do
              rest <- restItems
              pun ")"
              pure (mkTuple (x : y : rest))
  where
    restItems = do
      mComma <- optionMaybe (sym ",")
      case mComma of
        Nothing -> pure []
        Just _  -> do
          mNext <- optionMaybe item
          case mNext of
            Nothing -> pure []
            Just y  -> (y :) <$> restItems

-- | A bracketed list of static arguments.
staticArgs :: Stream s Identity Char => P s [SExp]
staticArgs = commaList "[" "]" parseType

-- | A bracketed or braced list of parameter names.  Parameters must be unique.
parameters :: Stream s Identity Char => String -> String -> P s [SName]
parameters open close = do
  xs <- commaList open close name
  unless (nub xs == xs) $ fail "duplicate static parameter"
  pure xs

-- | A parenthesised type, which is a tuple unless it holds a single type.
parenType :: Stream s Identity Char => P s SExp
parenType = parenthesized parseType STuple

{- | A type atom: a name, with static arguments unless it is a bound static
variable, or a parenthesised type.
-}
sAtom :: Stream s Identity Char => P s SExp
sAtom = nameType <|> parenType
  where
    nameType = do
      n <- name
      bound <- inStatic n
      if bound
        then do
          mArgs <- optionMaybe (pun "[")
          case mArgs of
            Just _  -> fail ("static variable " ++ n ++ " cannot take static arguments")
            Nothing -> pure (SVar n)
        else SCon n . fromMaybe [] <$> optionMaybe staticArgs

-- | A type @a -> b@, which associates to the right.
parseType :: Stream s Identity Char => P s SExp
parseType = do
  l <- sAtom
  mR <- optionMaybe (op ["->"])
  case mR of
    Nothing -> pure l
    Just _  -> SFun l <$> parseType


-- DECLARATIONS
-- ============

{- | A binding declaration @val name = body@.
-}
parseBind :: Stream s Identity Char => P s DDecl
parseBind = do
  kwOnly "val"
  n <- name
  pun "="
  DBind n <$> parseExpr

-- | Everything in a signature declaration up to and including the colon.
decHead :: Stream s Identity Char => P s (TName, [SName])
decHead = do
  kwOnly "dec"
  n <- name
  ps <- fromMaybe [] <$> optionMaybe (parameters "[" "]")
  pun ":"
  pure (n, ps)

{- | A signature declaration @dec name[params] : type@.  The parameters scope
over the signature and are 'SVar' wherever they appear.
-}
parseDec :: Stream s Identity Char => (TName, [SName]) -> P s DDecl
parseDec (n, ps) = do
  sig <- withStatic ps parseType
  pure $ DTDec (TDecl (n, ps, sig))

-- | Everything in an implementation up to and including the name.
impHead :: Stream s Identity Char => P s (TName, [SName])
impHead = do
  kwOnly "imp"
  ps <- fromMaybe [] <$> optionMaybe (parameters "{" "}")
  n <- name
  pure (n, ps)

{- | An implementation @imp{params} name[pats] = body@.  The parameters scope
over the patterns and the body.  The result type is left to 'Typecheck', which
derives it from the signature this implementation is declared against.
-}
parseImp :: Stream s Identity Char => (TName, [SName]) -> P s DDecl
parseImp (n, ps) = withStatic ps $ do
  pats <- fromMaybe [] <$> optionMaybe staticArgs
  pun "="
  body <- parseExpr
  tag <- freshTag
  pure $ DImpl (TImpl (n, body, pats, tag))

-- | A local block, whose private declarations do not escape.
parseLocal :: Stream s Identity Char => P s DDecl
parseLocal = do
  kwOnly "local"
  priv <- parseDecls
  kwOnly "in"
  pub <- parseDecls
  kwOnly "end"
  pure $ DLocal priv pub

{- | A declaration of any kind.  Each head is read behind a 'try' so that a
mistake in a body or a signature is reported where it happened.
-}
parseDecl :: Stream s Identity Char => P s DDecl
parseDecl =
  try parseLocal
    <|> (try decHead >>= parseDec)
    <|> (try impHead >>= parseImp)
    <|> try parseBind

{- | Declarations separated by semicolons.

The separator is read first and the declaration after it may come back empty, so
that a trailing @;@ is absorbed by the list rather than reported as a missing
declaration.
-}
parseDecls :: Stream s Identity Char => P s [DDecl]
parseDecls = do
  mFirst <- optionMaybe parseDecl
  case mFirst of
    Nothing    -> pure []
    Just first -> do
      rest <- many (sym ";" >> optionMaybe parseDecl)
      pure (first : catMaybes rest)


-- DYNAMIC EXPRESSIONS
-- ===================
-- From strongest to weakest: postfix operations (application, projection and
-- static application); unary signs; multiplication, division and modulo;
-- addition and subtraction; comparisons.

-- | A literal, a parenthesised expression, or a name.
dAtom :: Stream s Identity Char => P s DExp
dAtom = dLit <|> parenExpr <|> dName

-- | The argument of an application: one expression, or several as a tuple.
parenExpr :: Stream s Identity Char => P s DExp
parenExpr = parenthesized parseExpr DTuple

-- | An integer, boolean or string literal.
dLit :: Stream s Identity Char => P s DExp
dLit = DInt <$> natural
    <|> (DBool True <$ kw "true")
    <|> (DBool False <$ kw "false")
    <|> (DStr <$> stringLit)

-- | A dynamic variable, or a constant reference @\@name[args]@.
dName :: Stream s Identity Char => P s DExp
dName = do
  at <- optionMaybe (sym "@")
  n <- name
  case at of
    Just _  -> DTapp n . fromMaybe [] <$> optionMaybe staticArgs
    Nothing -> pure $ DVar n

{- | A primary followed by any number of applications and projections.  The
argument list is not behind a 'try': once one has been opened, a failure inside
it is a real error and should be reported as one.
-}
dPostfix :: Stream s Identity Char => P s DExp
dPostfix = dAtom >>= go
  where
    go e =
      (do arg <- parenExpr; go (DApp e arg))
      <|> (do i <- try (sym "." >> natural); go (DProj i e))
      <|> pure e

-- | An operand, with a unary sign kept as @DOp1@.
dUnary :: Stream s Identity Char => P s DExp
dUnary = (DOp1 "-" <$ sym "-" <*> dUnary)
     <|> (DOp1 "+" <$ sym "+" <*> dUnary)
     <|> dPostfix

{- | A left-associative layer of binary operators.  Each operator is matched by
'op', which either matches or consumes nothing, so a layer can be left without
moving the input and the layer above it can take over.
-}
binops :: Stream s Identity Char
       => [String] -> (DName -> DExp -> DExp -> DExp)
       -> P s DExp -> P s DExp
binops ops build operand = do
  first <- operand
  rest first
  where
    rest acc = do
      mOp <- optionMaybe (op ops)
      case mOp of
        Nothing -> pure acc
        Just o  -> do
          r <- operand
          rest (build o acc r)

-- | A product of @*@, @/@ or @%@.
dProduct :: Stream s Identity Char => P s DExp
dProduct = binops ["*", "/", "%"] DOp2 dUnary

-- | A sum or difference of products.
dSum :: Stream s Identity Char => P s DExp
dSum = binops ["+", "-"] DOp2 dProduct

-- | A comparison of sums.  Comparisons do not chain, so at most one operator
-- is read here.
dCompare :: Stream s Identity Char => P s DExp
dCompare = do
  l <- dSum
  mOp <- optionMaybe (op ["==", "!=", "<=", ">=", "<", ">"])
  case mOp of
    Nothing -> pure l
    Just o  -> DOp2 o l <$> dSum

-- | A lambda @lam (x : type) => body@.
dFun :: Stream s Identity Char => P s DExp
dFun = do
  kwOnly "lam"
  pun "("
  n <- name
  pun ":"
  t <- parseType
  pun ")"
  pun "=>"
  DLam (n, t) <$> parseExpr

-- | A fixpoint @fix f(x : type) : type => body@.
dFix :: Stream s Identity Char => P s DExp
dFix = do
  kwOnly "fix"
  self <- name
  pun "("
  n <- name
  pun ":"
  t <- parseType
  pun ")"
  pun ":"
  r <- parseType
  pun "=>"
  body <- parseExpr
  pure $ DFix self (n, t) body r

-- | A conditional @if c then a else b@.  The branches extend as far right as
-- the enclosing syntax permits.
dIf :: Stream s Identity Char => P s DExp
dIf = do
  kwOnly "if"
  c <- parseExpr
  kwOnly "then"
  a <- parseExpr
  kwOnly "else"
  DIf c a <$> parseExpr

-- | A let block @let decls [in body] end@.
dLet :: Stream s Identity Char => P s DExp
dLet = do
  kwOnly "let"
  ds <- parseDecls
  mBody <- optionMaybe (kwOnly "in" >> parseExpr)
  kwOnly "end"
  pure $ DLet ds mBody

-- | An expression of any kind.
parseExpr :: Stream s Identity Char => P s DExp
parseExpr = dFun <|> dFix <|> dIf <|> dLet <|> dCompare


-- WHOLE PROGRAMS
-- ==============

-- | A program: declarations, then a body.  A name that does not start a
-- declaration begins the body, so no trailing separator is needed.
parseProgram :: Stream s Identity Char => P s DProgram
parseProgram = do
  wt
  ds <- parseDecls
  body <- parseExpr
  pure $ DProgram ds body


-- ENTRY POINTS
-- ============

{- | Run a parser over a named source, starting the tag counter at zero and the
static scope empty.
-}
evalP :: String -> String -> P String a -> Either ParseError a
evalP name src p = runP p (PState 0 []) name src

-- | Parse a whole program, rejecting anything left over.
parseWhole :: String -> Either ParseError DProgram
parseWhole src = evalP "<program>" src (parseProgram <* eof)

-- | Parse a single expression, rejecting anything left over.
parseExprString :: String -> Either ParseError DExp
parseExprString src = evalP "<expr>" src (parseExpr <* eof)

-- | Parse a single type, rejecting anything left over.  With no enclosing
-- parameters every bare name is a type constructor.
parseTypeString :: String -> Either ParseError SExp
parseTypeString src = evalP "<type>" src (parseType <* eof)
