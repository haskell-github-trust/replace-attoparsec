# replace-attoparsec

[![Hackage](https://img.shields.io/hackage/v/replace-attoparsec.svg?style=flat)](https://hackage.haskell.org/package/replace-attoparsec)
[![Stackage Nightly](http://stackage.org/package/replace-attoparsec/badge/nightly)](http://stackage.org/nightly/package/replace-attoparsec)
[![Stackage LTS](http://stackage.org/package/replace-attoparsec/badge/lts)](http://stackage.org/lts/package/replace-attoparsec)

* [Usage Examples](#usage-examples)
* [In the Shell](#in-the-shell)
* [Alternatives](#alternatives)
* [Benchmarks](#benchmarks)
* [Hypothetically Asked Questions](#hypothetically-asked-questions)

__replace-attoparsec__ is for finding text patterns, and also
replacing or splitting on the found patterns.
This activity is traditionally done with regular expressions,
but __replace-attoparsec__ uses
[__attoparsec__](http://hackage.haskell.org/package/attoparsec)
parsers instead for the pattern matching.

__replace-attoparsec__ can be used in the same sort of “pattern capture”
or “find all” situations in which one would use Python
[`re.findall`](https://docs.python.org/3/library/re.html#re.findall)
or
Perl [`m//`](https://perldoc.perl.org/functions/m.html),
or
Unix [`grep`](https://www.gnu.org/software/grep/).

__replace-attoparsec__ can be used in the same sort of “stream editing”
or “search-and-replace” situations in which one would use Python
[`re.sub`](https://docs.python.org/3/library/re.html#re.sub),
or
Perl [`s///`](https://perldoc.perl.org/functions/s.html),
or Unix
[`sed`](https://www.gnu.org/software/sed/manual/html_node/The-_0022s_0022-Command.html),
or
[`awk`](https://www.gnu.org/software/gawk/manual/gawk.html).

__replace-attoparsec__ can be used in the same sort of “string splitting”
situations in which one would use Python
[`re.split`](https://docs.python.org/3/library/re.html#re.split)
or Perl
[`split`](https://perldoc.perl.org/functions/split.html).

See [__replace-megaparsec__](https://hackage.haskell.org/package/replace-megaparsec)
for the
[__megaparsec__](http://hackage.haskell.org/package/megaparsec)
version.

## Why would we want to do pattern matching and substitution with parsers instead of regular expressions?

* Haskell parsers have a nicer syntax than
  [regular expressions](https://en.wikipedia.org/wiki/Regular_expression),
  which are notoriously
  [difficult to read](https://en.wikipedia.org/wiki/Write-only_language).

* Regular expressions can do “group capture” on sections of the matched
  pattern, but they can only return stringy lists of the capture groups. Parsers
  can construct typed data structures based on the capture groups, guaranteeing
  no disagreement between the pattern rules and the rules that we're using
  to build data structures based on the pattern matches.

  For example, consider
  scanning a string for numbers. A lot of different things can look like a number,
  and can have leading plus or minus signs, or be in scientific notation, or
  have commas, or whatever. If we try to parse all of the numbers out of a string
  using regular expressions, then we have to make sure that the regular expression
  and the string-to-number conversion function agree about exactly what is
  and what isn't a numeric string. We can get into an awkward situation in which
  the regular expression says it has found a numeric string but the
  string-to-number conversion function fails. A typed parser will perform both
  the pattern match and the conversion, so it will never be in that situation.
  [Parse, don't validate.](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/)

* Regular expressions are only able to pattern-match
  [regular](https://en.wikipedia.org/wiki/Chomsky_hierarchy#The_hierarchy)
  grammers.
  Attoparsec parsers are able pattern-match context-free grammers.

* The replacement expression for a traditional regular expression-based
  substitution command is usually just a string template in which
  the *Nth* “capture group” can be inserted with the syntax `\N`. With
  this library, instead of a template, we get
  an `editor` function which can perform any computation, including IO.

# Usage Examples

Try the examples in `ghci` by
running `cabal v2-repl` in the `replace-attoparsec/`
root directory.

The examples depend on these imports and `LANGUAGE OverloadedStrings`.

```haskell
:set -XOverloadedStrings
import Replace.Attoparsec.Text
import Data.Attoparsec.Text as AT
import qualified Data.Text as T
import Control.Applicative
import Data.Either
import Data.Char
```

## Split strings with `splitCap`

### Find all pattern matches, capture the matched text and the parsed result

Separate the input string into sections which can be parsed as a hexadecimal
number with a prefix `"0x"`, and sections which can't. Parse the numbers.

```haskell
let hexparser = string "0x" *> hexadecimal :: Parser Integer
splitCap (match hexparser) "0xA 000 0xFFFF"
```
```haskell
[Right ("0xA",10), Left " 000 ", Right ("0xFFFF",65535)]
```

### Pattern match balanced parentheses

Find groups of balanced nested parentheses. This is an example of a
“context-free” grammar, a pattern that can't be expressed by a regular
expression. We can express the pattern with a recursive parser.

```haskell
import Data.Functor (void)
import Data.Bifunctor (second)
let parens :: Parser ()
    parens = do
        char '('
        manyTill
            (void parens <|> void anyChar)
            (char ')')
        pure ()

second fst <$> splitCap (match parens) "(()) (()())"
```
```haskell
[Right "(())",Left " ",Right "(()())"]
```

## Edit text strings with `streamEdit`

The following examples show how to search for a pattern in a string of text
and then edit the string of text to substitute in some replacement text
for the matched patterns.

### Pattern match and replace with a constant

Replace all carriage-return-newline occurances with newline.

```haskell
streamEdit (string "\r\n") (const "\n") "1\r\n2\r\n"
```
```haskell
"1\n2\n"
```

### Pattern match and edit the matches

Replace alphabetic characters with the next character in the alphabet.

```haskell
streamEdit (AT.takeWhile isLetter) (T.map succ) "HAL 9000"
```
```haskell
"IBM 9000"
```

### Pattern match and maybe edit the matches, or maybe leave them alone

Find all of the string sections *`s`* which can be parsed as a
hexadecimal number *`r`*,
and if *`r≤16`*, then replace *`s`* with a decimal number. Uses the
[`match`](https://hackage.haskell.org/package/attoparsec/docs/Data-Attoparsec-Text.html#v:match)
combinator.

```haskell
let hexparser = string "0x" *> hexadecimal :: Parser Integer
streamEdit (match hexparser) (\(s,r) -> if r <= 16 then T.pack (show r) else s) "0xA 000 0xFFFF"
```
```haskell
"10 000 0xFFFF"
```

### Pattern match and edit the matches with IO with `streamEditT`

Find an environment variable in curly braces and replace it with its
value from the environment.

```haskell
import System.Environment (getEnv)
streamEditT (char '{' *> manyTill anyChar (char '}')) (fmap T.pack . getEnv) "- {HOME} -"
```
```haskell
"- /home/jbrock -"
```

### Pattern match, edit the matches, and count the edits with `streamEditT`

Find and capitalize no more than three letters in a string, and return the 
edited string along with the number of letters capitalized. To enable the
editor function to remember how many letters it has capitalized, we'll 
run `streamEditT` in the `State` monad from the `mtl` package. Use this
technique to get the same functionality as Python
[`re.subn`](https://docs.python.org/3/library/re.html#re.subn).

```haskell
import qualified Control.Monad.State.Strict as MTL
import Control.Monad.State.Strict (get, put, runState)
import Data.Char (toUpper)

let editThree :: Char -> MTL.State Int T.Text
    editThree x = do
        i <- get
        if i<3
            then do
                put $ i+1
                pure $ T.singleton $ toUpper x
            else pure $ T.singleton x

flip runState 0 $ streamEditT (satisfy isLetter) editThree "a a a a a"
```
```haskell
("A A A a a",3)
```


# In the Shell

If we're going to have a viable `sed` replacement then we want to be able
to use it easily from the command line. This
[Stack script interpreter](https://docs.haskellstack.org/en/stable/GUIDE/#script-interpreter)
script will find decimal numbers in a stream and replace them with their double.

```haskell
#!/usr/bin/env stack
{- stack
  script
  --resolver lts-16
  --package attoparsec
  --package text
  --package text-show
  --package replace-attoparsec
-}
-- https://docs.haskellstack.org/en/stable/GUIDE/#script-interpreter

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as T
import TextShow
import Data.Attoparsec.Text
import Replace.Attoparsec.Text

main = T.interact $ streamEdit decimal (showt . (* (2::Integer)))
```

If you have
[The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/)
installed then you can just copy-paste this into a file named `doubler.hs` and
run it. (On the first run Stack may need to download the dependencies.)

```bash
$ chmod u+x doubler.hs
$ echo "1 6 21 107" | ./doubler.hs
2 12 42 214
```


# Alternatives

Some libraries that one might consider instead of this one.

<http://hackage.haskell.org/package/regex-applicative>

<http://hackage.haskell.org/package/pcre-heavy>

<http://hackage.haskell.org/package/lens-regex-pcre>

<http://hackage.haskell.org/package/regex>

<http://hackage.haskell.org/package/pipes-parse>

<http://hackage.haskell.org/package/stringsearch>

<http://hackage.haskell.org/package/substring-parser>

<http://hackage.haskell.org/package/pcre-utils>

<http://hackage.haskell.org/package/template>

<https://github.com/RaminHAL9001/parser-sed-thing>

<http://hackage.haskell.org/package/attosplit>

# Benchmarks

These benchmarks are intended to measure the wall-clock speed
of *everything except the actual pattern-matching*. Speed of the
pattern-matching is the responsibility of the
[__megaparsec__](http://hackage.haskell.org/package/megaparsec) and
[__attoparsec__](http://hackage.haskell.org/package/attoparsec)
libraries.

The benchmark task is to find all of the one-character patterns `x` in a
text stream and replace them by a function which returns the constant
string `oo`. So, like the regex `s/x/oo/g`.

We have two benchmark input cases, which we call __dense__ and __sparse__.

The __dense__ case is ten megabytes of alternating spaces and `x`s
like

```
x x x x x x x x x x x x x x x x x x x x x x x x x x x x
```

The __sparse__ case is ten megabytes of spaces with a single `x` in the middle
like

```
                         x
```

Each benchmark program reads the input from `stdin`, replaces `x` with `oo`,
and writes the result to `stdout`. The time elapsed is measured by `perf stat`,
and the best observed time is recorded.

See [replace-benchmark](https://github.com/jamesdbrock/replace-benchmark)
for details.

| Program                                           | dense *ms*  | sparse *ms* |
| :---                                              |      ---: |     ---:  |
| Python 3.10.9 [`re.sub`](https://docs.python.org/3/library/re.html#re.sub) *repl* function | 557.22 | 35.47 |
| Perl  v5.36.0 [`s///ge`](https://perldoc.perl.org/functions/s.html) function | 1208.66 | 12.61 |
| [`Replace.Megaparsec.streamEdit`](https://hackage.haskell.org/package/replace-megaparsec/docs/Replace-Megaparsec.html#v:streamEdit) `String` | 2921.25 | 2911.81 |
| [`Replace.Megaparsec.streamEdit`](https://hackage.haskell.org/package/replace-megaparsec/docs/Replace-Megaparsec.html#v:streamEdit) `ByteString` | 3743.25 | 757.21 |
| [`Replace.Megaparsec.streamEdit`](https://hackage.haskell.org/package/replace-megaparsec/docs/Replace-Megaparsec.html#v:streamEdit) `Text` | 3818.47 | 881.69 |
| [`Replace.Attoparsec.ByteString.streamEdit`](https://hackage.haskell.org/package/replace-attoparsec/docs/Replace-Attoparsec-ByteString.html#v:streamEdit) | 3006.38 | 179.66 |
| [`Replace.Attoparsec.Text.streamEdit`](https://hackage.haskell.org/package/replace-attoparsec/docs/Replace-Attoparsec-Text.html#v:streamEdit) | 3062.43 | 300.13 |
| [`Replace.Attoparsec.Text.Lazy.streamEdit`](https://hackage.haskell.org/package/replace-attoparsec/docs/Replace-Attoparsec-Text-Lazy.html#v:streamEdit) | 3102.15 | 241.58 |
| [`Text.Regex.Applicative.replace`](http://hackage.haskell.org/package/regex-applicative/docs/Text-Regex-Applicative.html#v:replace) `String` | 13875.25 | 4330.52 |
| [`Text.Regex.PCRE.Heavy.gsub`](http://hackage.haskell.org/package/pcre-heavy/docs/Text-Regex-PCRE-Heavy.html#v:gsub) `Text` | ∞ | 113.27 |
| [`Control.Lens.Regex.ByteString.match`](https://hackage.haskell.org/package/lens-regex-pcre/docs/Control-Lens-Regex-ByteString.html#v:match) | ∞ | 117.05 |
| [`Control.Lens.Regex.Text.match`](https://hackage.haskell.org/package/lens-regex-pcre/docs/Control-Lens-Regex-Text.html#v:match) | ∞ | 35.97 |


# Hypothetically Asked Questions

1. *Could we write this library for __parsec__?*

   No, because the
   [`match`](https://hackage.haskell.org/package/attoparsec/docs/Data-Attoparsec-Text.html#v:match)
   combinator doesn't exist for __parsec__. (I can't find it anywhere.
   [Can it be written?](http://www.serpentine.com/blog/2014/05/31/attoparsec/#from-strings-to-buffers-and-cursors))

2. *Is this a good idea?*

   You may have
   [heard it suggested](https://stackoverflow.com/questions/57667534/how-can-i-use-a-parser-in-haskell-to-find-the-locations-of-some-substrings-in-a/57712672#comment101804063_57667534)
   that monadic parsers are better for pattern-matching when
   the input stream is mostly signal, and regular expressions are better
   when the input stream is mostly noise.

   The premise of this library is that monadic parsers are great for finding
   small signal patterns in a stream of otherwise noisy text.

   Our reluctance to forego the speedup opportunities afforded by restricting
   ourselves to regular grammars is an old superstition about
   opportunities which
   [remain mostly unexploited anyway](https://swtch.com/~rsc/regexp/regexp1.html).
   The performance compromise of allowing stack memory allocation (a.k.a. pushdown
   automata, a.k.a. context-free grammar) was once considered
   [controversial for *general-purpose* programming languages](https://vanemden.wordpress.com/2014/06/18/how-recursion-got-into-programming-a-comedy-of-errors-3/).
   I think we
   can now resolve that controversy the same way for pattern matching languages.

