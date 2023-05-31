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

### DDD Value objects

Project demonstrates the use of new types to enhance the type safety and clarity of simple DDD value objects.
Three approaches have been used:

* Using [New types](https://zio.github.io/zio-prelude/docs/newtypes/) from ZIO Prelude library
* Using [Scala opaue types](https://docs.scala-lang.org/scala3/book/types-opaque-types.html)
* Combination of both

The DDD Value object simple or composite can be found in [domain](orderTaking/domain/src/main/scala/io/gitlab/routis/dmmf/ordertaking/domain/package.scala)

An example of a simple DDD VO using new types

```scala
 type ZipCode       = ZipCode.Type
 object ZipCode extends Newtype[String]:
    override inline def assertion: Assertion[String] = matches("^\\d{5}$".r)
```

An example of a simple DDD VO that uses opaque types is the
[Money](orderTaking/domain/src/main/scala/io/gitlab/routis/dmmf/ordertaking/domain/Money.scala) which is implemented 
using `Joda Money`.

```scala

import org.joda.money.Money as JodaMoney
import org.joda.money.CurrencyUnit as JodaCurrencyUnit

opaque type Money = JodaMoney
object Money:

  def apply(amount: BigDecimal): Money = JodaMoney.of(JodaCurrencyUnit.EUR, amount.bigDecimal)

  def make(amount: BigDecimal): Validation[String, Money] =
    Validation
      .fromTry(Try(Money(amount)))
      .mapError(t => s"$amount is not a valid amount. ${t.getMessage}")
      
```

Finally, an example that uses both methods is
[BillingAmount](orderTaking/domain/src/main/scala/io/gitlab/routis/dmmf/ordertaking/domain/BillingAmount.scala)
which is based on [Money](orderTaking/domain/src/main/scala/io/gitlab/routis/dmmf/ordertaking/domain/Money.scala) 
which is constraint to a specific max value

```scala
import zio.prelude.{ Assertion, Identity, Subtype, Validation }
import Assertion.*
import zio.prelude.newtypes.Sum

object BillingAmount extends Subtype[Money]:

  override inline def assertion: Assertion[Money] = between(Money(0), Money(10000))

  def total(prices: Iterable[Price]): Validation[String, BillingAmount] =
    make(Money.total(prices))
```
