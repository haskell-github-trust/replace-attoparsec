# replace-attoparsec

[![Hackage](https://img.shields.io/hackage/v/replace-attoparsec.svg?style=flat)](https://hackage.haskell.org/package/replace-attoparsec)
[![Stackage Nightly](http://stackage.org/package/replace-attoparsec/badge/nightly)](http://stackage.org/nightly/package/replace-attoparsec)
[![Stackage LTS](http://stackage.org/package/replace-attoparsec/badge/lts)](http://stackage.org/lts/package/replace-attoparsec)

* [Examples](#examples)
* [In the Shell](#in-the-shell)
* [Alternatives](#alternatives)
* [Hypothetically Asked Questions](#hypothetically-asked-questions)

__replace-attoparsec__ is for finding text patterns, and also editing and
replacing the found patterns.
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

* Regular expressions are only able to pattern-match
  [regular](https://en.wikipedia.org/wiki/Chomsky_hierarchy#The_hierarchy)
  grammers.
  Parsers are able pattern-match with context-free grammers, and
  even context-sensitive grammers, if needed. See below for
  an example of lifting a `Parser` into a `State` monad for context-sensitive
  pattern-matching.

* The replacement expression for a traditional regular expression-based
  substitution command is usually just a string template in which
  the *Nth* “capture group” can be inserted with the syntax `\N`. With
  this library, instead of a template, we get
  an `editor` function which can perform any computation, including IO.

# Examples

Try the examples in `ghci` by
running `cabal v2-repl` in the `replace-attoparsec/`
root directory.

The examples depend on these imports and `LANGUAGE OverloadedStrings`.

```haskell
:set -XOverloadedStrings
import Replace.Attoparsec.Text
import Data.Attoparsec.Text as AT
import qualified Data.Text as T
import Data.Either
import Data.Char
```

## Parsing with `sepCap` family of parser combinators

The following examples show how to match a pattern to a string of text
and deconstruct the string of text by separating it into sections
which match the pattern, and sections which don't match.

### Pattern match, capture only the parsed result with `sepCap`

Separate the input string into sections which can be parsed as a hexadecimal
number with a prefix `"0x"`, and sections which can't.

```haskell
let hexparser = string "0x" >> hexadecimal :: Parser Integer
fromRight [] $ parseOnly (sepCap hexparser) "0xA 000 0xFFFF"
```
```haskell
[Right 10,Left " 000 ",Right 65535]
```

### Pattern match, capture only the matched text with `findAll`

Just get the strings sections which match the hexadecimal parser, throw away
the parsed number.

```haskell
let hexparser = string "0x" >> hexadecimal :: Parser Integer
fromRight [] $ parseOnly (findAll hexparser) "0xA 000 0xFFFF"
```
```haskell
[Right "0xA",Left " 000 ",Right "0xFFFF"]
```

### Pattern match, capture the matched text and the parsed result with `findAllCap`

Capture the parsed hexadecimal number, as well as the string section which
parses as a hexadecimal number.

```haskell
let hexparser = chunk "0x" >> hexadecimal :: Parser Integer
fromRight [] $ parseOnly (findAllCap hexparser) "0xA 000 0xFFFF"
```
```haskell
[Right ("0xA",10),Left " 000 ",Right ("0xFFFF",65535)]
```

### Pattern match, capture only the locations of the matched patterns

Find all of the sections of the stream which match
a string of whitespace.
Print a list of the offsets of the beginning of every pattern match.

```haskell
import Data.Either
let spaceoffset = getOffset <* some space :: Parser Int
fromRight [] $ parseOnly (return . rights =<< sepCap spaceoffset) " a  b  "
```
```haskell
[0,2,5]
```

### Pattern match balanced parentheses

Find the outer parentheses of all balanced nested parentheses.
Here's an example of matching a pattern that can't be expressed by a regular
expression. We can express the pattern with a recursive parser.

```haskell
let parens :: Parser ()
    parens = do
        char '('
        manyTill
            (void (satisfy $ notInClass "()") <|> void parens)
            (char ')')
        return ()

fromRight [] $ parseOnly (findAll parens) "(()) (()())"
```
```haskell
[Right "(())",Left " ",Right "(()())"]
```

## Edit text strings by running parsers with `streamEdit`

The following examples show how to search for a pattern in a string of text
and then edit the string of text to substitute in some replacement text
for the matched patterns.

### Pattern match and replace with a constant

Replace all carriage-return-newline instances with newline.

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
let hexparser = string "0x" >> hexadecimal :: Parser Integer
streamEdit (match hexparser) (\(s,r) -> if r <= 16 then T.pack (show r) else s) "0xA 000 0xFFFF"
```
```haskell
"10 000 0xFFFF"
```

### Pattern match and edit the matches with IO

Find an environment variable in curly braces and replace it with its
value from the environment.

```haskell
import System.Environment
streamEditT (char '{' *> manyTill anyChar (char '}')) (fmap T.pack . getEnv) "- {HOME} -"
```
```haskell
"- /home/jbrock -"
```


# In the Shell

If we're going to have a viable `sed` replacement then we want to be able
to use it easily from the command line. This script uses the
[Stack script interpreter](https://docs.haskellstack.org/en/stable/GUIDE/#script-interpreter)
To find decimal numbers in a stream and replace them with their double.

```haskell
#!/usr/bin/env stack
{- stack
  script
  --resolver nightly-2019-09-13
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
installed then you can just copy-paste this into a file named `script.hs` and
run it. (On the first run Stack may need to download the dependencies.)

```bash
$ chmod u+x script.hs
$ echo "1 6 21 107" | ./script.hs
2 12 42 214
```


# Alternatives

<http://hackage.haskell.org/package/regex-applicative>

<http://hackage.haskell.org/package/regex>

<http://hackage.haskell.org/package/pipes-parse>

<http://hackage.haskell.org/package/stringsearch>

<http://hackage.haskell.org/package/substring-parser>

<http://hackage.haskell.org/package/pcre-utils>

<http://hackage.haskell.org/package/template>

<https://github.com/RaminHAL9001/parser-sed-thing>

<http://hackage.haskell.org/package/attosplit>

# Hypothetically Asked Questions

1. *Is it fast?*

   lol not really. `sepCap` is fundamentally about consuming the stream one
   token at a time while we try and fail to run a parser and then
   backtrack each time. That's
   [a slow activity](https://markkarpov.com/megaparsec/megaparsec.html#writing-efficient-parsers).

2. *Could we write this library for __parsec__?*

   No, because the
   [`match`](https://hackage.haskell.org/package/attoparsec/docs/Data-Attoparsec-Text.html#v:match)
   combinator doesn't exist for __parsec__. (I can't find it anywhere.
   [Can it be written?](http://www.serpentine.com/blog/2014/05/31/attoparsec/#from-strings-to-buffers-and-cursors))


