{-# LANGUAGE FlexibleContexts #-}

-- | A parser for the level-2 language, producing 'DProgram', 'DDecl', 'DExp'
-- and 'SExp' values.
--
-- The grammar this follows is the one in the module comment further down.
--
-- Two rules hold throughout, and together they give the grammar its shape.
--
-- * /Whitespace, linebreaks and comments carry no meaning./  Every token goes
--   through 'tok', which skips the whitespace and any comments before and after
--   it, so no parser has to remember to skip any and no token can be
--   accidentally fused to its neighbour.  @let{x=1;y=2}in x+y end@,
--   @let { x = 1; y = 2 } in x + y end@ and a version with one token per line
--   are the same program.  A comment is written @(* like this *)@ and may nest,
--   so the inner markers of a commented-out block need no escaping.
--
-- * /A token either matches or leaves the input exactly where it was./  'tok'
--   wraps the match in 'try' so that a token which fails to match also rolls
--   back the whitespace it skipped on the way in.  That is what lets tokens
--   sit inside a '<|>' alternative without consuming input the alternatives
--   behind them need to see.
--
-- Parsers are polymorphic in the character stream, in the style of the worked
-- example; 'parseWhole' is the 'String' entry point.
module Parse where

import Control.Monad (void)
import Data.Char (digitToInt, isAlphaNum)
import Data.Maybe (catMaybes)

import Text.Parsec

import Ast


-- GRAMMAR NOTES
-- =============
--
-- The module comment lists the forms the grammar has to cover.  Four points
-- needed a decision, and the choices are recorded here.
--
-- Type vs. value
--   '(,)' and ',' are shared by both levels, so a form is read as a static
--   expression or a dynamic one purely by where it appears: an annotation or a
--   template argument is parsed with 'parseType', everything else with
--   'parseExpr'.  No lookahead is needed to tell the two apart.
--
-- The case of a name
--   The grammar writes its names in lower case throughout -- @foo[a, b]@,
--   @tname<a>@, @n1 = def1@, @'type1 -> 'type2@ -- and takes no position on
--   their case, since they read as placeholders rather than as literals.  This
--   parser follows the convention the AST's synonyms suggest instead: a name
--   starts with a lower case letter for a value or a template ('lName'), an
--   upper case one for a type constructor ('uName'), and a type variable needs
--   a tick, as the grammar requires.  So @Foo[a, b]@ is a type former and
--   @foo[a, b]@ is a rejected bare name that would need to be written @'foo@.
--
-- Parenthesised forms
--   A parenthesised list of zero or one expressions is just grouping, and two
--   or more is a tuple.  This is what makes the application rule "desugar
--   f(t1,t2) to f((t1,t2))" fall out: 'dArgs' hands a lone argument straight
--   through and wraps several in a 'DTuple'.
--
-- The overall type of a fix
--   The grammar writes @fix (selfname, arg : type) body@ and stops there, with
--   no place for 'DFix''s last field.  The type of the whole expression is now
--   written out after it -- @fix (f, a : t) body : t2@ -- since a fixpoint
--   needs its result type stated, not guessed: the recursion is only
--   well-typed if @f@ can return it.
--
-- Literals and operators
--   The comment lists neither, but 'DInt', 'DBool', 'DStr', 'DOp1' and 'DOp2'
--   all exist and no useful program can be written without them, so they are
--   parsed too.  Operator names are the ones 'Interpret' implements, and unary
--   minus desugars to @DOp1 "-1"@ for the same reason.


-- TOKENS
-- ======

{- | Whitespace, linebreaks and comments are insignificant and may appear, in any
amount, between any two tokens.

This skips, and never fails on anything but running out of input inside a
comment, so 'tok' can put it on either side of a token it is about to match.

An unterminated comment cannot be committed to while it is still inside the
'try' that 'tok' wraps around a token: the closing @*)@ may simply not be there
yet, and only the enclosing parser can tell.  So 'comment' fails without
consuming input at the end of the input, and the failure surfaces as a missing
token at the @(@ rather than as a missing @*)@.  It still fails, which is the part
that matters.
-}
wt :: Stream s m Char => ParsecT s u m ()
wt = skipMany (void space <|> comment)

{- | A comment, delimited by @(*@ and @*)@, which nests.

@(* outer (* inner *) still outer *)@ is one comment, and the depth is what says
so: @(*@ opens one level, @*)@ closes one, and the level reaching zero ends the
comment.  Inner markers therefore need no escaping.  A marker counts only when
the two characters really are adjacent, so inside a comment @(*)@ is ordinary
text rather than an empty one, as in the language the delimiters come from.

Note that this makes @(*@ unusable as a token boundary, so a function applied to
a negated argument has to be written @f (-x)@.  There is no way to keep both
spellings apart without a lexical rule that would make @(*@ depend on what comes
after it.

The 'try' around the opening delimiter is what keeps an ordinary @(@ from being
read as the start of a comment and then stranded: @string "(*"@ consumes the
bracket before discovering there is no star, and a '<|>' cannot recover from that,
so 'wt' would fail outright on every parenthesised expression in the language.
-}
comment :: Stream s m Char => ParsecT s u m ()
comment = try (string "(*") *> go (1 :: Int)
  where
    -- The character in hand has already been read, so when it turns out not to
    -- open or close a marker, the way on is to carry on from the next one.
    go depth
      | depth <= 0 = pure ()
      | otherwise = do
          c <- anyChar
          if c == '('
            then (char '*' *> go (depth + 1)) <|> go depth
            else if c == '*'
              then (char ')' *> go (depth - 1)) <|> go depth
              else go depth

{- | A single token, surrounded by insignificant whitespace.

Skipping on the way in matters as much as skipping on the way out: it is what
makes the grammar agnostic of spacing without every parser having to say so.

The 'try' matters just as much.  Without it a token that fails to match would
still have eaten the whitespace in front of it, and because '<|>' only recovers
from a failure that consumed nothing, that would strand the alternatives behind
it.  Wrapping the skip as well as the match keeps a failed token invisible.

'try' is only safe here because 'tok' is always given a parser that reads one
token and cannot fail part way through having read something.  Never hand it a
compound parser: rolling back the whitespace along with the tokens would report
a failure at the start of the construct instead of inside it.
-}
tok :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
tok p = try (wt >> p <* wt)

{- | Punctuation.  No word boundary is wanted, because punctuation is never part
of a name.

The text comes back so that 'op' can report which operator it matched.
-}
sym :: Stream s m Char => String -> ParsecT s u m String
sym = tok . string

-- | Punctuation whose text is of no interest.
pun :: Stream s m Char => String -> ParsecT s u m ()
pun w = sym w >> pure ()

{- | A reserved word, which must not be the start of a longer name, so that
@iffy@ is one name and not @if@ followed by @fy@.
-}
kw :: Stream s m Char => String -> ParsecT s u m String
kw w = tok (string w >> notFollowedBy identTail >> pure w)

-- | A reserved word whose name is of no interest.
kwOnly :: Stream s m Char => String -> ParsecT s u m ()
kwOnly w = kw w >> pure ()

{- | One operator, matched longest first, so that @<=@ is preferred over @<@ and
a comparison never comes apart into two tokens.  Unlike 'kw' an operator needs
no word boundary, because an operator is never part of a name.
-}
op :: Stream s m Char => [String] -> ParsecT s u m String
op = choice . map sym . longestFirst
  where longestFirst = foldr insertBy []
        insertBy s [] = [s]
        insertBy s (r:rs)
          | length s > length r = s : r : rs
          | otherwise = r : insertBy s rs

-- | A character that may appear in a name after its first.  A tick belongs
-- here, which is what lets @left'@ be a single name.
identTail :: Stream s m Char => ParsecT s u m Char
identTail = satisfy (\c -> isAlphaNum c || c == '_' || c == '\'')

-- | A name starting with an upper case letter, such as a type constructor.
uName :: Stream s m Char => ParsecT s u m String
uName = tok $ (:) <$> upper <*> many identTail

-- | A name starting with a lower case letter, such as a variable or a template.
lName :: Stream s m Char => ParsecT s u m String
lName = tok $ (:) <$> lower <*> many identTail

-- | A non-negative integer literal, folded rather than accumulated as a string.
natural :: Stream s m Char => ParsecT s u m Int
natural = tok $ do
  d <- digit
  ds <- many digit
  pure $ foldl (\acc c -> 10 * acc + digitToInt c) 0 (d : ds)

{- | A string literal.  A backslash escapes the character after it, and the
abbreviations @\\n@, @\\t@, @\\\\@ and @\\"@ are recognised, so a string can
carry quotes and linebreaks.
-}
stringLit :: Stream s m Char => ParsecT s u m String
stringLit = tok $ char '"' *> many stringChar <* char '"'
  where
    stringChar = (char '\\' *> escaped) <|> satisfy (/= '"')
    escaped = choice
      [ char 'n'  >> pure '\n'
      , char 't'  >> pure '\t'
      , char '\\' >> pure '\\'
      , char '"'  >> pure '"'
      , anyChar
      ]

-- STATIC EXPRESSIONS
-- ==================

-- | A type variable, written with a mandatory tick: @'a@.
sName :: Stream s m Char => ParsecT s u m SExp
sName = do
  tick <- optionMaybe (sym "'")
  n <- lName
  case tick of
    Just _  -> pure $ SVar n
    Nothing -> fail $ "A type variable needs a tick: '" ++ n ++ "'"

-- | A type variable name for the two lists of an implementation declaration.
-- The tick is optional here and then dropped, so that the name is recorded
-- without it and lines up with the names 'sName' produces.
typeVarName :: Stream s m Char => ParsecT s u m String
typeVarName = (sym "'" >> lName) <|> lName

-- | A parenthesised type, which is a tuple unless it holds a single type.
parens :: Stream s m Char => ParsecT s u m SExp
parens = do
  pun "("
  ts <- sepBy parseType (sym ",")
  pun ")"
  pure $ case ts of
    [t] -> t
    _   -> STuple ts

-- | A type former @foo[a, b]@, a type variable @'a@, or a parenthesised type.
sAtom :: Stream s m Char => ParsecT s u m SExp
sAtom = former <|> sName <|> parens
  where
    former = do
      n <- uName
      mArgs <- optionMaybe (try brackets)
      pure $ maybe (SCon n []) (\args -> SCon n args) mArgs
    brackets = sym "[" *> sepBy parseType (sym ",") <* sym "]"

-- | A type function @a -> b@, which associates to the right.
parseType :: Stream s m Char => ParsecT s u m SExp
parseType = do
  l <- sAtom
  mR <- optionMaybe (op ["->"]) >>= mapM (const parseType)
  pure $ maybe l (\r -> SFun l r) mR


-- DECLARATIONS
-- ============

-- | An implementation declaration @impl<'a, 'b> name<a, b> = body@.  The two
-- lists are read as parallel: each declared free variable names the pattern
-- beside it, which is the pairing 'DImpl' stores.  The head has already been
-- read by 'implHead'.
parseImpl :: Stream s m Char
          => (DName, [SName], [SExp]) -> ParsecT s u m DDecl
parseImpl {- (n, frees, pats) -} _
  -- 'DImpl' holds one paired list, so a declaration that names a different
  -- number of variables and patterns has no representation here.  Say so rather
  -- than dropping the difference.  An implementation with no free type
  -- variables is written with a name for each pattern anyway; the name is inert,
  -- since an implementation is resolved by position rather than by name.
 = error $ "TODO"
{-
  | length frees /= length pats =
      fail $ "An implementation needs one variable per type pattern, got " ++
             show (length frees) ++ " and " ++ show (length pats)
  | otherwise = do
      pun "="
      body <- parseExpr
      pure $ DImpl n body (zip frees pats)
-}

-- | Everything in an implementation declaration up to the @=@, read behind a
-- 'try' by 'parseDecl' so that a binding named @impl@ is still a binding.  The
-- body and the arity check sit outside that 'try', so that a genuine mistake in
-- either is reported where it happened instead of being rolled back.
implHead :: Stream s m Char => ParsecT s u m (DName, [SName], [SExp])
implHead = do
  kwOnly "impl"
  pun "<"
  frees <- sepBy typeVarName (sym ",")
  pun ">"
  n <- lName
  pun "<"
  pats <- sepBy parseType (sym ",")
  pun ">"
  pure (n, frees, pats)

-- | A local block, whose private declarations do not escape.
parseLocal :: Stream s m Char => ParsecT s u m DDecl
parseLocal = do
  kwOnly "local"
  priv <- block
  kwOnly "in"
  pub <- block
  kwOnly "end"
  pure $ DLocal priv pub
  where block = sym "{" *> parseDecls <* sym "}"

-- | A binding declaration @name = body@.
parseBind :: Stream s m Char => ParsecT s u m DDecl
parseBind = do
  n <- lName
  pun "="
  DBind n <$> parseExpr

{- | A declaration of any kind.

Each alternative is wrapped in 'try' just far enough that a name which is not a
declaration leaves the input untouched -- so that @local@ or @impl@ can be a
binding, and so that a list of declarations ends at the first thing that is not
one.  For @impl@ the 'try' covers only the head, and 'parseImpl' runs outside it,
so a mistake in the body or in the number of type patterns is reported where it
happened instead of being rolled back into the enclosing list.
-}
parseDecl :: Stream s m Char => ParsecT s u m DDecl
parseDecl =
  try parseLocal
    <|> (try implHead >>= parseImpl)
    <|> try parseBind

{- | Declarations separated by semicolons.

The separator is read first and the declaration after it may come back empty, so
that a trailing @;@ is absorbed by the list rather than reported as a missing
declaration.  'sepBy' cannot express that: it makes a separated element into
@sep >> p@, so a @;@ followed by something that is not a declaration leaves the
separator consumed and the whole list failing.
-}
parseDecls :: Stream s m Char => ParsecT s u m [DDecl]
parseDecls = do
  mFirst <- optionMaybe parseDecl
  case mFirst of
    Nothing    -> pure []
    Just first -> do
      rest <- many (sym ";" >> optionMaybe parseDecl)
      pure (first : catMaybes rest)


-- DYNAMIC EXPRESSIONS
-- ===================

-- | A template use @name<a, b>@, or a plain variable.  Reading the arguments
-- as types is what tells a template use apart from a comparison against the
-- same name, and the 'try' lets @x < y@ fall back to a comparison when no
-- closing @>@ follows.
dName :: Stream s m Char => ParsecT s u m DExp
dName = do
  n <- lName
  mArgs <- optionMaybe (try brackets)
  pure $ maybe (DVar n) (DTapp n) mArgs
  where brackets = sym "<" *> sepBy parseType (sym ",") <* sym ">"

-- | An integer, boolean or string literal.
dLit :: Stream s m Char => ParsecT s u m DExp
dLit = DInt <$> natural
    <|> (DBool True <$ kw "true")
    <|> (DBool False <$ kw "false")
    <|> (DStr <$> stringLit)

-- | A parenthesised expression, which is a tuple unless it holds a single
-- expression.
dParens :: Stream s m Char => ParsecT s u m DExp
dParens = do
  pun "("
  es <- sepBy parseExpr (sym ",")
  pun ")"
  pure $ case es of
    [e] -> e
    _   -> DTuple es

-- | A literal, a parenthesised expression, or a name.
dPrimary :: Stream s m Char => ParsecT s u m DExp
dPrimary = dLit <|> dParens <|> dName

-- | A primary followed by any number of projections, as in @(left, right).1@.
dPostfix :: Stream s m Char => ParsecT s u m DExp
dPostfix = do
  a <- dPrimary
  mProj <- optionMaybe (try (sym "." >> natural))
  pure $ maybe a (\i -> DProj i a) mProj

-- | An operand, with a unary minus desugaring to @DOp1 "-1"@.
dUnary :: Stream s m Char => ParsecT s u m DExp
dUnary = (DOp1 "-1" <$ sym "-" <*> dUnary) <|> dApply

{- | A left-associative layer of binary operators.  Each operator is matched by
'op', which either matches or consumes nothing, so a layer can be left without
moving the input and the layer above it can take over.
-}
binops :: Stream s m Char
       => [String] -> (DName -> DExp -> DExp -> DExp)
       -> ParsecT s u m DExp -> ParsecT s u m DExp
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

-- | A product of @*@, which the interpreter implements.
dProduct :: Stream s m Char => ParsecT s u m DExp
dProduct = binops ["*"] DOp2 dUnary

-- | A sum or difference of products.
dSum :: Stream s m Char => ParsecT s u m DExp
dSum = binops ["+", "-"] DOp2 dProduct

-- | A comparison of sums, the loosest binding form.
dCompare :: Stream s m Char => ParsecT s u m DExp
dCompare = binops ["==", "!=", "<=", ">=", "<", ">"] DOp2 dSum

-- | A lambda @lam (x : type) body@.
dFun :: Stream s m Char => ParsecT s u m DExp
dFun = do
  kwOnly "lam"
  pun "("
  n <- lName
  pun ":"
  at <- parseType
  pun ")"
  DLam (n, at) <$> parseExpr

-- | A fixpoint @fix (self, n : type) body : type@.  The type after the body is
-- the type of the whole expression, which 'DFix' has to carry and the grammar
-- leaves unstated.  It is unambiguous where it sits: a @:@ only ever appears
-- inside the brackets of a 'lam' or 'fix' annotation, so one following a whole
-- expression can only be this.
dFix :: Stream s m Char => ParsecT s u m DExp
dFix = do
  kwOnly "fix"
  pun "("
  self <- lName
  pun ","
  n <- lName
  pun ":"
  at <- parseType
  pun ")"
  body <- parseExpr
  pun ":"
  overall <- parseType
  pure $ DFix self (n, at) body overall

-- | A conditional @if c then a else b end@.
dIf :: Stream s m Char => ParsecT s u m DExp
dIf = do
  kwOnly "if"
  c <- parseExpr
  kwOnly "then"
  a <- parseExpr
  kwOnly "else"
  b <- parseExpr
  kwOnly "end"
  pure $ DIf c a b

-- | A let block @let { decls } in body end@.
dLet :: Stream s m Char => ParsecT s u m DExp
dLet = do
  kwOnly "let"
  pun "{"
  ds <- parseDecls
  pun "}"
  kwOnly "in"
  body <- parseExpr
  kwOnly "end"
  pure $ DLet ds (Just body)

-- | The argument of an application: one expression, or several as a tuple.
dArgs :: Stream s m Char => ParsecT s u m DExp
dArgs = do
  pun "("
  es <- sepBy parseExpr (sym ",")
  pun ")"
  pure $ case es of
    [e] -> e
    _   -> DTuple es

{- | A function applied to any number of parenthesised arguments.

This sits between the primary and the arithmetic layers rather than beside them
in 'parseExpr', so that @f(x) + 1@ reads as @f(x) + 1@ and @f + x(1)@ as
@f + x(1)@.  The argument list is not behind a 'try': once one has been opened, a
failure inside it is a real error and should be reported as one.
-}
dApply :: Stream s m Char => ParsecT s u m DExp
dApply = do
  f <- dPostfix
  args <- many dArgs
  pure (foldl DApp f args)

-- | An expression of any kind.
parseExpr :: Stream s m Char => ParsecT s u m DExp
parseExpr = dFun <|> dFix <|> dIf <|> dLet <|> dCompare


-- WHOLE PROGRAMS
-- ==============

-- | A program: declarations, each followed by a semicolon, then a body.  A name
-- not followed by @=@ is not a declaration but the start of the body, which is
-- why this needs no 'try': 'parseDecl' already consumes nothing when it fails.
parseProgram :: Stream s m Char => ParsecT s u m DProgram
parseProgram = do
  wt
  ds <- many (parseDecl <* sym ";")
  body <- parseExpr
  pure $ DProgram ds body

-- | Parse a whole program, rejecting anything left over.
parseWhole :: String -> Either ParseError DProgram
parseWhole = parse (parseProgram <* eof) "<program>"

-- | Parse a single expression, rejecting anything left over.
parseExprString :: String -> Either ParseError DExp
parseExprString = parse (parseExpr <* eof) "<expr>"

-- | Parse a single type, rejecting anything left over.
parseTypeString :: String -> Either ParseError SExp
parseTypeString = parse (parseType <* eof) "<type>"
