package io.gitlab.routis.dmmf.ordertaking.domain

import zio.prelude.{ Assertion, Subtype, Validation }
import zio.prelude.Assertion.matches
import io.gitlab.routis.dmmf.ordertaking.domain.ProductCode.{ GizmoCode, WidgetCode }
enum ProductCode:
  self =>
  def value: String = self match
    case Gizmo(code)  => code
    case Widget(code) => code

  case Gizmo(code: GizmoCode)

  case Widget(code: WidgetCode)

object ProductCode:

  type GizmoCode  = GizmoCode.Type
  type WidgetCode = WidgetCode.Type

  object GizmoCode extends Subtype[String]:
    override inline def assertion: Assertion[String] = matches("\\AG\\d{3}\\z".r)

  object WidgetCode extends Subtype[String]:
    override inline def assertion: Assertion[String] = matches("\\AW\\d{4}\\z".r)

  def make(value: String): Validation[String, ProductCode] =
    def tryGizmo  = GizmoCode.make(value).map(Gizmo.apply)
    def tryWidget = WidgetCode.make(value).map(Widget.apply)
    tryWidget.orElse(tryGizmo).mapError(_ => "Neither Widget nor Gizmo")
