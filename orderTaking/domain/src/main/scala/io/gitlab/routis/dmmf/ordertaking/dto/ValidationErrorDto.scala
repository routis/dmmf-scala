package io.gitlab.routis.dmmf.ordertaking.dto

import io.gitlab.routis.dmmf.ordertaking.pub.PlaceOrder.PlaceOrderError.ValidationFailure
import io.gitlab.routis.dmmf.ordertaking.pub.internal.Validations.ValidationError
import io.gitlab.routis.dmmf.ordertaking.pub.internal.Validations.ValidationError.*

case class ValidationErrorDto(fieldName: String, description: String)

object ValidationErrorDto:

  def fromDomain(e: ValidationError): ValidationErrorDto =
    e match
      case Cause(description)                    => ValidationErrorDto("", description)
      case FieldError(fieldName, error)          =>
        val nested              = fromDomain(error)
        val suffix              = if nested.fieldName == "" then "" else s".${nested.fieldName}"
        val normalizedFieldName = s"$fieldName$suffix"
        ValidationErrorDto(normalizedFieldName, nested.description)
      case IndexedFieldError(list, index, error) =>
        val nested              = fromDomain(error)
        val suffix              = if nested.fieldName == "" then "" else s".${nested.fieldName}"
        val normalizedFieldName = s"$list[$index]$suffix"
        ValidationErrorDto(normalizedFieldName, nested.description)

  def fromDomain(f: ValidationFailure): List[ValidationErrorDto] =
    f.errors.map(fromDomain).toList
