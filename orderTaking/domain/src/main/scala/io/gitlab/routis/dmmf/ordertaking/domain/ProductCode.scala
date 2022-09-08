package io.gitlab.routis.dmmf.ordertaking.domain

import zio.prelude.{ Assertion, Subtype, Validation }
import zio.prelude.Assertion.matches

sealed trait ProductCode

object ProductCode:

  type Gizmo  = Gizmo.Type
  type Widget = Widget.Type

  object Widget extends Subtype[String]:
    override inline def assertion: Assertion[String] =
      matches("\\AW\\d{4}\\z".r)

  object Gizmo extends Subtype[String]:
    override inline def assertion: Assertion[String] =
      matches("\\AG\\d{3}\\z".r)

  case class GizmoCode(gizmo: Gizmo) extends ProductCode

  case class WidgetCode(widget: Widget) extends ProductCode

  def value(productCode: ProductCode): String =
    productCode match
      case GizmoCode(gizmo)   => gizmo
      case WidgetCode(widget) => widget

  def make(value: String): Validation[String, ProductCode] =
    Widget
      .make(value)
      .map(WidgetCode.apply)
      .orElse(Gizmo.make(value).map(GizmoCode.apply))
      .mapError(_ => "Neither Widget nor Gizmo")
