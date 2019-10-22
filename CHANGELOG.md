# Revision history for replace-attoparsec

## 1.0.4.0 -- 2019-10-22

A total re-write of the `sepCap` function, guided by
[replace-benchmark](https://github.com/jamesdbrock/replace-benchmark).

#### Before

| Program                                           | dense     | sparse   |
| :---                                              |      ---: |     ---: |
| [`Replace.Attoparsec.ByteString.streamEdit`][ab]  | 537.57ms  | 407.33ms |
| [`Replace.Attoparsec.Text.streamEdit`][at]        | 549.62ms  | 280.96ms |

#### After

| Program                                           | dense     | sparse   |
| :---                                              |      ---: |     ---: |
| [`Replace.Attoparsec.ByteString.streamEdit`][ab]  | 394.12ms  | 41.13ms  |
| [`Replace.Attoparsec.Text.streamEdit`][at]        | 495.49ms  | 38.39ms  |

[m]: https://hackage.haskell.org/package/replace-megaparsec/docs/Replace-Megaparsec.html#v:streamEdit
[ab]: https://hackage.haskell.org/package/replace-attoparsec/docs/Replace-Attoparsec-ByteString.html#v:streamEdit
[at]: https://hackage.haskell.org/package/replace-attoparsec/docs/Replace-Attoparsec-Text.html#v:streamEdit

## 1.0.0.0 -- 2019-09-10

* First version.

