package io.gitlab.routis.dmmf.ordertaking.application.port.out

import io.gitlab.routis.dmmf.ordertaking.domain.{ Price, ProductCode, PromotionCode }
import zio.UIO

trait GetPromotionProductPrice extends ((PromotionCode, ProductCode) => UIO[Option[Price]])
