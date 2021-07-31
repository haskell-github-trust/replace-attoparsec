# Revision history for replace-attoparsec

## 1.4.5.0 -- 2021-07-29

Add Replace.Attoparsec.Text.Lazy

## 1.4.4.0 -- 2021-01-08

Deprecate `findAll` and `findAllCap`.

## 1.4.2.0 -- 2020-09-28

Bugfix sepCap backtracking when sep fails

See [replace-megaparsec/issues/33](https://github.com/jamesdbrock/replace-megaparsec/issues/33)

## 1.4.0.0 -- 2020-05-06

__Running Parsers__: Add `splitCap` and `breakCap`.

__Parser Combinators__: Add `anyTill`.

## 1.2.0.0 -- 2019-10-31

Benchmark improvements

Specializations of the `sepCap` function, guided by
[replace-benchmark](https://github.com/jamesdbrock/replace-benchmark).

### New benchmarks

| Program                                           | dense     | sparse   |
| :---                                              |      ---: |     ---: |
| `Replace.Attoparsec.ByteString.streamEdit`        | 394.12ms  | 41.13ms  |
| `Replace.Attoparsec.Text.streamEdit`              | 515.26ms  | 46.10ms  |

### Old benchmarks

| Program                                           | dense     | sparse   |
| :---                                              |      ---: |     ---: |
|  `Replace.Attoparsec.ByteString.streamEdit`       | 537.57ms  | 407.33ms |
|  `Replace.Attoparsec.Text.streamEdit`             | 549.62ms  | 280.96ms |

Also don't export `getOffset` anymore. It's too complicated to explain
what it means for `Text`. If users want to know positional parsing information
then they should use Megaparsec.

## 1.0.0.0 -- 2019-09-10

* First version.

