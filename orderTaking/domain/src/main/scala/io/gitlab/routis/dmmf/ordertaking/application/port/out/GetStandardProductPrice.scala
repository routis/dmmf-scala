package io.gitlab.routis.dmmf.ordertaking.application.port.out

import io.gitlab.routis.dmmf.ordertaking.domain.{ Price, ProductCode }
import zio.UIO

trait GetStandardProductPrice extends (ProductCode => UIO[Price])
