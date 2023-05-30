# ddmf-scala

Sample code in scala, using ZIO, using the examples
of the book [Domain modeling made functional](https://www.ebooks.com/en-us/book/96028851/domain-modeling-made-functional/scott-wlaschin/) from Scott Wlaschin.

## Features

* Scala 3
* Hexagonal Architecture
* [ZIO 2](https://zio.dev/)
* New (refined) types: Application demonstrates the use of [ZIO Prelude New Types](https://zio.github.io/zio-prelude/docs/newtypes/) as a way to implement refined types for DDD `value objects`.
* Validation: Application uses [`Validation[E,A]`](https://zio.github.io/zio-prelude/docs/functionaldatatypes/validation) to back the validations of input models.
* Combining non-effectful & side-effecting validations: `Validation[E,A]` is a datastructure representing either a sucessful validation or some errors. In the application we demonstrate a way to combine this with `ZIO[R,E,A]`.
