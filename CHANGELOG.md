# Revision history for replace-attoparsec

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

## 1.0.0.0 -- 2019-09-10

* First version.

