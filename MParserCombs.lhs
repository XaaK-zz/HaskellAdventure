----------------------------------------------------------------------
CS457/557 Functional Languages, Winter 2012

Parsers Combinators in Haskell, Monadic version
----------------------------------------------------------------------

This file provides a library for combinator parsing in Haskell.  This
particular library is intended for the purposes of illustrating basic
ideas and concepts; for production use of parser combinators,
something like the standard Parsec library would probably be a more
appropriate choice.

> module MParserCombs where

We'll write some simple parsers using characters, so I'll import the
Char library:

> import Char

BASIC DEFINITIONS: ---------------------------------------------------
I'll define a parser to be a function that takes a list of characters
of type String as its input and returns a list of possible parses,
each of which is a pair of type (a, String).  In each such pair, the
first component is the result of the parse and the second component is
the list of remaining tokens (if any).  If there are no valid parses,
then we just return the empty list.

> data Parser a = P (String -> [(a, String)])

> unP      :: Parser a -> String -> [(a, String)]
> unP (P x) = x

We define a helper function for running a given parser on a particular
string and discard any results that do not consume all of the input.

> parse    :: Parser a -> String -> [a]
> parse p s = [ x | (x,"") <- unP p s ]

(zero) is a parser that never returns a successful parse:

> noparse :: Parser a
> noparse  = P (\s -> [])

(ok x) is a parser that returns a given value, x, without consuming
any input tokens:

> ok  :: a -> Parser a
> ok x = P (\s -> [(x,s)])

(sat p) is a parser that will parse a single character, so long as it
satisfies the predicate p:

> sat  :: (Char -> Bool) -> Parser Char
> sat p = P (\s -> case s of
>                    ""     -> []
>                    (c:cs) -> [(c,cs) | p c])

For example, here is a parser that will read a single digit:

> ex1 :: Parser Char
> ex1  = sat isDigit

ALTERNATIVES: --------------------------------------------------------

(p `orelse` q) returns all the ways that we can find to match the
input against p followed by all the ways that we can find to match the
input against q.

> infixr 4 `orelse`

> orelse       :: Parser a -> Parser a -> Parser a
> p `orelse` q  = P (\s -> unP p s ++ unP q s)

For example, here is a parser that will read a single digit,
adding a default '0' if no digit is found:

> ex2 :: Parser Char
> ex2  = ex1 `orelse` ok '0'

SEQUENCES: -----------------------------------------------------------

(p `andthen` q)  parses the input using p and then parses the
remaining input using q.  For each successful parse, we return a pair
that includes the values produced by the first and second parses,
respectively.

> infixr 6 `andthen`

> andthen      :: Parser a -> Parser b -> Parser (a,b)
> p `andthen` q = P (\s -> [ ((x,y), s'') | (x, s')  <- unP p s,
>                                           (y, s'') <- unP q s' ])

For example, here is a parser that will recognize a pair of adjacent
characters in which the first character is a digit and the second is
not:

> ex3 :: Parser (Char, Char)
> ex3  = sat isDigit `andthen` sat (not . isDigit)

MAPPING: -------------------------------------------------------------

We can apply a function to each of the results produced by a parser
using the following variant of the standard map function (the order of
the two arguments is reversed here so that there is a more natural
reading of an expression like p `pmap` f from left to right):

> infix 5 `pmap`

> pmap      :: Parser a -> (a -> b) -> Parser b
> p `pmap` f = P (\s -> [ (f x, s') | (x, s') <- unP p s ])

For example, the following parser behaves the same as ex3 except that
it repackages the pair produced by ex3 as a list:

> ex4       :: Parser String
> ex4        = ex3 `pmap` \(x,y) -> [x,y]

BIND OPERATIONS: -----------------------------------------------------

We can define the "bind" operators for parsers as a special case of
the standard >>= operator for the Monad class:

> instance Monad Parser where
>   return  = ok
>   p >>= f = P (\s -> [ r  | (x, s') <- unP p s, r <- unP (f x) s' ])

Now we can use >>= instead of >>>=; >> instead of >>>; and sequence
instead of the mapP operator defined below.  Of course, we can also
use do-notation with parsers now that we've defined the monad operations
for the Parser type.

REPETITION: ----------------------------------------------------------

(many p) parses a sequence of zero or more things, each of which
matches p.  (Think of this like a regular expression r*.)

> many   :: Parser a -> Parser [a]
> many p  = many1 p `orelse` return []

(many1 p) parses a sequence of one or more things, each of which
matches p.  (Think of this like a regular expression r+.)

> many1  :: Parser a -> Parser [a]
> many1 p = do x <- p; xs <- many p; return (x:xs)

LEXICAL ANALYSIS: ----------------------------------------------------

Now we can start to define some basic parsers for recognizing:

A single decimal digit:

> digit  :: Parser Int
> digit   = do d <- sat isDigit; return (ord d - ord '0')

A decimal integer comprising one or more decimal digits:

> number :: Parser Int
> number  = many1 digit `pmap` foldl1 (\a x -> 10*a+x)

A keyword/symbol comprising a specific sequence of characters:

> tok    :: String -> Parser ()
> tok     = sequence_ . map (sat . (==))

----------------------------------------------------------------------
An evaluator for arithmetic expressions:

Here's a grammar for simple integer-valued expressions:

  expr = term ("+" expr | "-" expr | )
  term = atom ("*" term | atom "/" | )
  atom = "-" atom
       | "(" expr ")"
       | number

And here is a parser that follows this grammar, and calculates the
integer value of a string containing an Integer expression:

> expr, term, atom :: Parser Int

> expr = do x <- term
>           (do tok "+"; y <- expr; return (x+y))
>             `orelse` (do tok "-"; y <- expr; return (x-y))
>             `orelse` return x

> term = do x <- atom
>           (do tok "*"; y <- term; return (x*y))
>             `orelse` (do tok "/"; y <- term; return (x`div`y))
>             `orelse` return x

> atom = (do tok "-"; p <- atom; return (negate p)) `orelse`
>        (do tok "("; n <- expr; tok ")"; return n) `orelse`
>        number

----------------------------------------------------------------------
What if we want to obtain a data structure that describes expressions?
Here is a suitable datatype that we can use to describe the "abstract
syntax" of simple arithmetic expressions:

> data Expr = Add Expr Expr
>           | Sub Expr Expr
>           | Mul Expr Expr
>           | Div Expr Expr
>           | Neg Expr
>           | Num Int
>             deriving Show

And now we can adapt the parser we saw above to generate abstract
syntax values from input strings:

> absyn :: Parser Expr
> absyn  = expr
>  where
>   expr    = do x <- term
>                (do tok "+"; y <- expr; return (Add x y))
>                 `orelse` (do tok "-"; y <- expr; return (Sub x y))
>                 `orelse` return x
> 
>   term    = do x <- atom
>                (do tok "*"; y <- term; return (Mul x y))
>                 `orelse` (do tok "/"; y <- term; return (Div x y))
>                 `orelse` return x
> 
>   atom    =  (do tok "-"; atom `pmap` Neg)              `orelse`
>              (do tok "("; n <- expr; tok ")"; return n) `orelse`
>              (number `pmap` Num)

----------------------------------------------------------------------
Does the code duplication in the two examples above offend you?  It
seems that there is a lot of common structure in our two expression
parsers, and that's not really surprising because the grammar is the
same in each case.  What differs is the "semantic actions" that we use
to combine results from intermediate parses.  We can capture this by
abstracting over those operators as follows:

> exprWith  :: (a -> a -> a)  -- used to add values
>           -> (a -> a -> a)  -- used to subtract values
>           -> (a -> a -> a)  -- used to multiply values
>           -> (a -> a -> a)  -- used to divide values
>           -> (a -> a)       -- used to negate values
>           -> (Int -> a)     -- used to turn numbers into values
>           -> Parser a
> exprWith add sub mul div neg num = expr
>  where
>   expr    = do x <- term
>                (do tok "+"; y <- expr; return (add x y))
>                 `orelse` (do tok "-"; y <- expr; return (sub x y))
>                 `orelse` return x
> 
>   term    = do x <- atom
>                (do tok "*"; y <- term; return (mul x y))
>                 `orelse` (do tok "/"; y <- term; return (div x y))
>                 `orelse` return x
> 
>   atom    =  (do tok "-"; atom `pmap` neg)              `orelse`
>              (do tok "("; n <- expr; tok ")"; return n) `orelse`
>              (number `pmap` num)

Now we can define one line versions of the two expression parsers that
we've seen previously:

> absyn' = exprWith Add Sub Mul Div Neg Num
> expr'  = exprWith (+) (-) (*) div negate id

One interesting observation: suppose that we've defined a fold
function on the Expr type, which would then have the same number and
types of arguments as exprWith.  In this case, we can prove that:

   exprWith add sub mul div neg num
 ==
   absyn' *** foldExpr add sub mul div neg num

What this tells us is that, in fact, we don't need to construct
multiple parsing functions for expressions!  Any parser on expressions
can be obtained as the combination of a parser that returns abstract
syntax and a fold function over that abstract syntax.

----------------------------------------------------------------------
We'll take a brief look to see why the left-factoring of the grammar
is so important in the examples above.  More specifically, here is a
different grammar for the same language that some people might think
is more natural than the grammar we used previously:

  expr = term "+" expr | term "-" expr | term
  term = atom "*" term | atom "/" term | atom
  atom = "-" atom      | "(" expr ")"  | number

And here's the corresonding parser following that grammar:

> slowAbsyn :: Parser Expr
> slowAbsyn  = expr
>  where
>   expr    = (do x <- term; tok "+"; y <- expr; return (Add x y))
>             `orelse`
>             (do x <- term; tok "-"; y <- expr; return (Sub x y))
>             `orelse`
>             term
> 
>   term    = (do x <- atom; tok "*"; y <- term; return (Mul x y))
>             `orelse`
>             (do x <- atom; tok "/"; y <- term; return (Div x y))
>             `orelse`
>             atom
> 
>   atom    = (do tok "-"; atom `pmap` Neg)              `orelse`
>             (do tok "("; n <- expr; tok ")"; return n) `orelse`
>             (number `pmap` Num)

We call this parser "slowAbsyn" because although it parses expressions,
it can be very slow.  This slowness will occur when there is excessive
backtracking.  For example, in the grammar above, we can only parse an
expression that is a single term (Line 5 in the definition of expr)
after having first:

- tried to parse it as a term and then found that there is no
  following tok "+"  (Line 1 in the definition of expr)

- tried to parse it as a term and then found that there is no
  following tok "-"  (Line 3 in the definition of expr)

This problem is compounded by the same pattern in the definition of
term and becomes much worse when the input string contains
parentheses.  For example, see how long it takes to parse
"(((((1)))))" using absyn and compare that with the results using
slowAbsyn:

  ParserCombs> :set +s

  ParserCombs> parse absyn "(((((1)))))"
  [Num 1]
  (1074 reductions, 1654 cells)

  ParserCombs> parse slowAbsyn "(((((1)))))"
  [Num 1]
  (59122750 reductions, 84698346 cells, 86 garbage collections)

  ParserCombs> 

The point here is that the choice of grammar makes an important
difference in the cost of parsing.  A simple functional parser can get
the job done very efficiently, but not if there are inherent problems
in the grammar (left factoring, and left recursion being the biggest
problems in practice).

----------------------------------------------------------------------
We can easily go beyond context-free parsing in this framework:

- Read a number from the user and then parse that many things from the
  rest of the list.  For example, csp  will parse "1a" or "2aa" or
  "3aaa" or "4aaaa" or ...

> csp  :: Parser String
> csp   = do n <- number; sequence (replicate n char)

> char :: Parser Char
> char  = sat (\c -> True)

- Read a string of characters, enclosed in a matching pair of
  characters.  For example, this parser matches inputs like +hello+ or
  @hello@ or "hello" or ...

> brack :: Parser String
> brack  = do c <- char
>             xs <- many (sat (c/=))
>             sat (c==)
>             return xs

----------------------------------------------------------------------
