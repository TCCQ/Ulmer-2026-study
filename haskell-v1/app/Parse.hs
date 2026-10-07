{-# LANGUAGE FlexibleContexts #-}

-- | A parser for the level-2 language, producing 'DProgram', 'DDecl', 'DExp'
-- and 'SExp' values.
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
-- The parser runs in the language's own monad, 'TM', with a counter as its
-- state; that counter is where the tag on each implementation comes from.  The
-- environment is the same one the rest of the language uses, though nothing here
-- reads it: it is there so that the parser and the expander share a type and the
-- tag counter is threaded the same way as everything else.
module Parse where

import Control.Monad (void)
import Data.Char (digitToInt, isAlphaNum)
import Data.Functor.Identity (Identity)
import Data.Maybe (catMaybes)

import Text.Parsec

import Ast


-- | The parser's state: the counter that hands out implementation tags.
--
-- This is Parsec's own user state, which 'runP' seeds and Parsec threads through
-- the combinators, and not the 'TM' the rest of the language runs in.  Parsec
-- pins both of its runners to 'Either ParseError' over 'Identity' and keeps
-- 'unParser' unexported, so a 'ParsecT' over any other monad cannot be run at
-- all -- the user state is the only state a parser in this library can carry.
type PState = Int

-- | A parser, polymorphic in the character stream as the worked example is.
type P s a = ParsecT s PState Identity a

-- | The next implementation tag, taken from the parser's state.  Tags only have
-- to be distinct, so nothing is reclaimed when a parse fails.
freshTag :: P s Int
freshTag = do
  n <- getState
  putState (n + 1)
  pure n


-- GRAMMAR NOTES
-- =============
--
-- Six points needed a decision, and the choices are recorded here.
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
--   The tick is required in the two bracketed name lists too, so that a name in
--   either of them is unambiguously a type variable and cannot be mistaken for
--   a template name.  Losing a tick is reported only as a syntax error rather
--   than by name, since the list is read behind a 'try' and has to be able to
--   backtrack when there are no brackets at all.
--
-- Parenthesised forms
--   A parenthesised list of zero or one expressions is just grouping, and two
--   or more is a tuple.  This is what makes the application rule "desugar
--   f(t1,t2) to f((t1,t2))" fall out: 'dArgs' hands a lone argument straight
--   through and wraps several in a 'DTuple'.
--
-- A type former's argument
--   The grammar writes @foo[a, b]@ with brackets, but the declaration syntax
--   this parser was asked for writes @Constructor 'c@ without them, so a former
--   takes either.  Only one argument unbracketed -- @Foo 'a 'b@ is not a form --
--   which keeps the rule from having to decide how far a run of atoms reaches.
--
-- The overall type of a fix
--   The grammar writes @fix (selfname, arg : type) body@ and stops there, with
--   no place for 'DFix''s last field.  The type of the whole expression is
--   written out after it -- @fix (f, a : t) body : t2@ -- since a fixpoint
--   needs its result type stated, not guessed: the recursion is only
--   well-typed if @f@ can return it.
--
-- Literals and operators
--   The comment lists neither, but 'DInt', 'DBool', 'DStr', 'DOp1' and 'DOp2'
--   all exist and no useful program can be written without them, so they are
--   parsed too.  Operator names are the ones 'Interpret' implements, and unary
--   minus desugars to @DOp1 "-1"@ for the same reason.
--
-- Templates
--   A declaration and its implementations are separate declarations:
--
--   > tdecl name<'a, 'b> (Constructor 'c)
--   > impl name<a, b> body : c
--
--   'TDecl' pairs each argument name with the template's output type, so the
--   argument list is the list of names only and the @Constructor 'c@ is read as
--   a type by 'parseType'.  The round brackets in the example are just grouping,
--   so @tdecl f<'a> 'a -> 'a@ means the same thing.
--
--   'TImpl' keeps the body, the argument patterns, the body's own type, and a
--   tag, and that is all it keeps: an implementation does not say which type
--   variables its body is generic in, because there is nowhere to put the list
--   and nothing to check it against.  The only place a type variable can be
--   bound is the argument list, which 'chooseImpl' matches positionally against
--   the arguments of a use, so a generic implementation is written
--   @impl id<'a> lam (x : 'a) x : 'a -> 'a@ and a concrete one
--   @impl zero<Nat> 0 : Nat@.  A type variable left free in the body or in the
--   result type is simply left free.
--
--   The tag comes from 'freshTag' rather than from anything written, so a
--   program is numbered from 0 upwards in the order its implementations appear
--   and two of them can never collide -- which is what 'blacklist' needs when
--   it marks an expansion as already being expanded.  It does not matter that
--   the number is not in the source: the tags name implementations inside a
--   run, not across runs.



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
wt :: Stream s Identity Char => P s ()
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
comment :: Stream s Identity Char => P s ()
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
tok :: Stream s Identity Char => P s a -> P s a
tok p = try (wt >> p <* wt)

{- | Punctuation.  No word boundary is wanted, because punctuation is never part
of a name.

The text comes back so that 'op' can report which operator it matched.
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
a comparison never comes apart into two tokens.  Unlike 'kw' an operator needs
no word boundary, because an operator is never part of a name.
-}
op :: Stream s Identity Char => [String] -> P s String
op = choice . map sym . longestFirst
  where longestFirst = foldr insertBy []
        insertBy s [] = [s]
        insertBy s (r:rs)
          | length s > length r = s : r : rs
          | otherwise = r : insertBy s rs

-- | A character that may appear in a name after its first.  A tick belongs
-- here, which is what lets @left'@ be a single name.
identTail :: Stream s Identity Char => P s Char
identTail = satisfy (\c -> isAlphaNum c || c == '_' || c == '\'')

-- | A name starting with an upper case letter, such as a type constructor.
uName :: Stream s Identity Char => P s String
uName = tok $ (:) <$> upper <*> many identTail

-- | A name starting with a lower case letter, such as a variable or a template.
lName :: Stream s Identity Char => P s String
lName = tok $ (:) <$> lower <*> many identTail

-- | A non-negative integer literal, folded rather than accumulated as a string.
natural :: Stream s Identity Char => P s Int
natural = tok $ do
  d <- digit
  ds <- many digit
  pure $ foldl (\acc c -> 10 * acc + digitToInt c) 0 (d : ds)

{- | A string literal.  A backslash escapes the character after it, and the
abbreviations @\\n@, @\\t@, @\\\\@ and @\\"@ are recognised, so a string can
carry quotes and linebreaks.
-}
stringLit :: Stream s Identity Char => P s String
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
sName :: Stream s Identity Char => P s SExp
sName = do
  tick <- optionMaybe (sym "'")
  n <- lName
  case tick of
    Just _  -> pure $ SVar n
    Nothing -> fail $ "A type variable needs a tick: '" ++ n ++ "'"

-- | A type variable name, as written in the bracketed list of a template
-- declaration or of an implementation.  The tick is mandatory, as it is
-- everywhere a type variable appears, and is then dropped: the lists hold names
-- rather than 'SExp's, so that a name lines up with the 'SVar' 'sName' builds.
typeVarName :: Stream s Identity Char => P s String
typeVarName = sym "'" >> lName

-- | A parenthesised type, which is a tuple unless it holds a single type.
parens :: Stream s Identity Char => P s SExp
parens = do
  pun "("
  ts <- sepBy parseType (sym ",")
  pun ")"
  pure $ case ts of
    [t] -> t
    _   -> STuple ts

{- | A type former @Foo[a, b]@, a type variable @'a@, or a parenthesised type.

A former may also be given a single argument without brackets -- @Foo 'a@ as well
as @Foo['a]@ -- because the declaration syntax is written that way.  Only one,
and only unbracketed: @Foo 'a 'b@ is not a form, so nothing here has to decide
how far the arguments reach.
-}
sAtom :: Stream s Identity Char => P s SExp
sAtom = former <|> sName <|> parens
  where
    former = do
      n <- uName
      mArgs <- optionMaybe (try brackets <|> try unbracketed)
      pure $ maybe (SCon n []) (\args -> SCon n args) mArgs
    brackets = sym "[" *> sepBy parseType (sym ",") <* sym "]"
    unbracketed = (: []) <$> sAtom

-- | A type function @a -> b@, which associates to the right.
parseType :: Stream s Identity Char => P s SExp
parseType = do
  l <- sAtom
  mR <- optionMaybe (op ["->"]) >>= mapM (const parseType)
  pure $ maybe l (\r -> SFun l r) mR


-- DECLARATIONS
-- ============

{- | A template declaration @tdecl name<'a, 'b> (Constructor 'c)@, which says
how many arguments the template takes and what it gives back.  Only the head,
up to the closing bracket, is read behind a 'try' by 'parseDecl', so that a
binding named @tdecl@ is still a binding while a mistake in the result type is
reported as one.
-}
parseTDec :: Stream s Identity Char => (TName, [SName]) -> P s DDecl
parseTDec (n, argNames) = do
  out <- parseType
  pure $ DTDec (TDecl (n, argNames, out))

-- | Everything in a template declaration up to and including the @>@.
tdeclHead :: Stream s Identity Char => P s (TName, [SName])
tdeclHead = do
  kwOnly "tdecl"
  n <- lName
  pun "<"
  argNames <- sepBy typeVarName (sym ",")
  pun ">"
  pure (n, argNames)

{- | An implementation @impl name<a, b> body : c@, where @<a, b>@ are the
argument patterns the template use is matched against and @c@ is the type the
expansion gives back.  A type variable in that list is the only thing that binds
one: the implementation says nothing else about the variables its body mentions,
and says nothing about the names the template's 'TDecl' introduces either.

The tag comes from the parser's state, so no two implementations in a program
share one and 'chooseImpl' can blacklist an expansion of an implementation from
re-entering itself.

The head is read behind a 'try' by 'parseDecl' so that a binding named @impl@ is
still a binding.  The body and the result type sit outside that 'try', so that a
genuine mistake in either is reported where it happened instead of being rolled
back into the enclosing list of declarations.
-}
parseImpl :: Stream s Identity Char => (TName, [SExp]) -> P s DDecl
parseImpl (n, pats) = do
  body <- parseExpr
  pun ":"
  ret <- parseType
  tag <- freshTag
  pure $ DImpl (TImpl (n, body, pats, ret, tag))

-- | Everything in an implementation up to and including the argument patterns.
implHead :: Stream s Identity Char => P s (TName, [SExp])
implHead = do
  kwOnly "impl"
  n <- lName
  pun "<"
  pats <- sepBy parseType (sym ",")
  pun ">"
  pure (n, pats)

-- | A local block, whose private declarations do not escape.
parseLocal :: Stream s Identity Char => P s DDecl
parseLocal = do
  kwOnly "local"
  priv <- block
  kwOnly "in"
  pub <- block
  kwOnly "end"
  pure $ DLocal priv pub
  where block = sym "{" *> parseDecls <* sym "}"

-- | A binding declaration @name = body@.
parseBind :: Stream s Identity Char => P s DDecl
parseBind = do
  n <- lName
  pun "="
  DBind n <$> parseExpr

{- | A declaration of any kind.

Each alternative is wrapped in 'try' just far enough that a name which is not a
declaration leaves the input untouched -- so that @local@, @impl@ or @tdecl@ can
be a binding, and so that a list of declarations ends at the first thing that is
not one.  For @impl@ and @tdecl@ the 'try' covers only the head and the rest runs
outside it, so that a mistake in a body or a result type is reported where it
happened instead of being rolled back.
-}
parseDecl :: Stream s Identity Char => P s DDecl
parseDecl =
  try parseLocal
    <|> (try implHead >>= parseImpl)
    <|> (try tdeclHead >>= parseTDec)
    <|> try parseBind


{- | Declarations separated by semicolons.

The separator is read first and the declaration after it may come back empty, so
that a trailing @;@ is absorbed by the list rather than reported as a missing
declaration.  'sepBy' cannot express that: it makes a separated element into
@sep >> p@, so a @;@ followed by something that is not a declaration leaves the
separator consumed and the whole list failing.
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

-- | A template use @name<a, b>@, or a plain variable.  Reading the arguments
-- as types is what tells a template use apart from a comparison against the
-- same name, and the 'try' lets @x < y@ fall back to a comparison when no
-- closing @>@ follows.
dName :: Stream s Identity Char => P s DExp
dName = do
  n <- lName
  mArgs <- optionMaybe (try brackets)
  pure $ maybe (DVar n) (DTapp n) mArgs
  where brackets = sym "<" *> sepBy parseType (sym ",") <* sym ">"

-- | An integer, boolean or string literal.
dLit :: Stream s Identity Char => P s DExp
dLit = DInt <$> natural
    <|> (DBool True <$ kw "true")
    <|> (DBool False <$ kw "false")
    <|> (DStr <$> stringLit)

-- | A parenthesised expression, which is a tuple unless it holds a single
-- expression.
dParens :: Stream s Identity Char => P s DExp
dParens = do
  pun "("
  es <- sepBy parseExpr (sym ",")
  pun ")"
  pure $ case es of
    [e] -> e
    _   -> DTuple es

-- | A literal, a parenthesised expression, or a name.
dPrimary :: Stream s Identity Char => P s DExp
dPrimary = dLit <|> dParens <|> dName

-- | A primary followed by any number of projections, as in @(left, right).1@.
dPostfix :: Stream s Identity Char => P s DExp
dPostfix = do
  a <- dPrimary
  mProj <- optionMaybe (try (sym "." >> natural))
  pure $ maybe a (\i -> DProj i a) mProj

-- | An operand, with a unary minus desugaring to @DOp1 "-1"@.
dUnary :: Stream s Identity Char => P s DExp
dUnary = (DOp1 "-" <$ sym "-" <*> dUnary) <|> dApply

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

-- | A product of @*@, which the interpreter implements.
dProduct :: Stream s Identity Char => P s DExp
dProduct = binops ["*"] DOp2 dUnary

-- | A sum or difference of products.
dSum :: Stream s Identity Char => P s DExp
dSum = binops ["+", "-"] DOp2 dProduct

-- | A comparison of sums, the loosest binding form.
dCompare :: Stream s Identity Char => P s DExp
dCompare = binops ["==", "!=", "<=", ">=", "<", ">"] DOp2 dSum

-- | A lambda @lam (x : type) body@.
dFun :: Stream s Identity Char => P s DExp
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
dFix :: Stream s Identity Char => P s DExp
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
dIf :: Stream s Identity Char => P s DExp
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
dLet :: Stream s Identity Char => P s DExp
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
dArgs :: Stream s Identity Char => P s DExp
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
dApply :: Stream s Identity Char => P s DExp
dApply = do
  f <- dPostfix
  args <- many dArgs
  pure (foldl DApp f args)

-- | An expression of any kind.
parseExpr :: Stream s Identity Char => P s DExp
parseExpr = dFun <|> dFix <|> dIf <|> dLet <|> dCompare


-- WHOLE PROGRAMS
-- ==============

-- | A program: declarations, each followed by a semicolon, then a body.  A name
-- not followed by @=@ is not a declaration but the start of the body, which is
-- why this needs no 'try': 'parseDecl' already consumes nothing when it fails.
parseProgram :: Stream s Identity Char => P s DProgram
parseProgram = do
  wt
  ds <- many (parseDecl <* sym ";")
  body <- parseExpr
  pure $ DProgram ds body


-- ENTRY POINTS
-- ============

{- | Run a parser over a named source, starting the tag counter at zero.

'parse' is 'runP' with those two things already supplied, so a program is tagged
from 0 upwards in the order its implementations are written.  The counter the
parser finishes on is dropped: it has nowhere to go, since the tags are already
in the tree.
-}
evalP :: String -> String -> P String a -> Either ParseError a
evalP name src p = runP p 0 name src

-- | Parse a whole program, rejecting anything left over.
parseWhole :: String -> Either ParseError DProgram
parseWhole src = evalP "<program>" src (parseProgram <* eof)

-- | Parse a single expression, rejecting anything left over.
parseExprString :: String -> Either ParseError DExp
parseExprString src = evalP "<expr>" src (parseExpr <* eof)

-- | Parse a single type, rejecting anything left over.
parseTypeString :: String -> Either ParseError SExp
parseTypeString src = evalP "<type>" src (parseType <* eof)
