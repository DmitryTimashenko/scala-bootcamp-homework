package com.evolutiongaming.bootcamp.error_handling

import cats.data.ValidatedNec
import cats.implicits._

// Homework. Place the solution under `error_handling` package in your homework repository.
//
// 1. Model `PaymentCard` class as an ADT (protect against invalid data as much as it makes sense).
// 2. Add `ValidationError` cases (at least 5, may be more).
// 3. Implement `validate` method to construct `PaymentCard` instance from the supplied raw data.

object ErrorHandling {


  case class PaymentCard(cardNumber: String,
                         expirationDate: String,
                         CardholderName: String,
                         securityCode: String)

  sealed trait ValidationError

  object ValidationError {
    final case object CardNumberIsInvalid extends ValidationError {
      override def toString: String = "Card number is invalid"
    }

    final case object ExpirationDateIsRequired extends ValidationError {
      override def toString: String = "Expiration date is required"
    }

    final case object ExpirationDateIsInvalid extends ValidationError {
      override def toString: String = "Card number is invalid"
    }

    final case object CardholderNameIsRequired extends ValidationError {
      override def toString: String = "Card holder name is required"
    }

    final case object SecurityCodeIsRequired extends ValidationError {
      override def toString: String = "Security code is required"
    }

    final case object SecurityCodeIsInvalid extends ValidationError {
      override def toString: String = "Security code is invalid"
    }
  }

  object PaymentCardValidator {
    import ValidationError._
    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    def validate(
                  name: String,
                  number: String,
                  expirationDate: String,
                  securityCode: String,
                ): AllErrorsOr[PaymentCard] = {

      def validateCardholderName(name: String): AllErrorsOr[String] = {
        if (name.nonEmpty) name.validNec
        else CardholderNameIsRequired.invalidNec
      }

      def validateCardNumber(number: String): AllErrorsOr[String] = {
        val validationRegex = "^(?:" +
          "(4[0-9]{12}(?:[0-9]{3})?)|" +          // Visa
          "(5[1-5][0-9]{14}) |" +                 // MasterCard
          "(6(?:011|5[0-9]{2})[0-9]{12}) |" +     // Discover
          "(3[47][0-9]{13}) |" +                  // AMEX
          "(3(?:0[0-5]|[68][0-9])[0-9]{11}) |" +  // Diners Club
          "((?:2131|1800|35[0-9]{3})[0-9]{11})" + // JCB
          ")$"
        if (expirationDate.matches(validationRegex)) number.validNec
        else CardNumberIsInvalid.invalidNec
      }

      def validateExpirationDate(expirationDate: String): AllErrorsOr[String] = {
        def validateIsExpirationDateIsEmpty: AllErrorsOr[String] =
          if (expirationDate.nonEmpty) expirationDate.validNec
          else ExpirationDateIsRequired.invalidNec

        def validateExpirationDateFormat: AllErrorsOr[String] =
          if (expirationDate.matches("^(0[1-9]|1[0-2])\\/?([0-9]{2})")) expirationDate.validNec
          else ExpirationDateIsInvalid.invalidNec

        validateIsExpirationDateIsEmpty *> validateExpirationDateFormat
      }

      def validateSecurityCode(securityCode: String): AllErrorsOr[String] = {
        def validateIsSecurityCodeIsEmpty: AllErrorsOr[String] =
          if (securityCode.nonEmpty) securityCode.validNec
          else SecurityCodeIsRequired.invalidNec

        def validateSecurityCodeFormat: AllErrorsOr[String] =
          if (securityCode.matches("^[0-9]{3}")) securityCode.validNec
          else SecurityCodeIsInvalid.invalidNec

        validateIsSecurityCodeIsEmpty *> validateSecurityCodeFormat
      }

      (validateCardholderName(name),
        validateCardNumber(number),
        validateExpirationDate(expirationDate),
        validateSecurityCode(securityCode)
        ).mapN(PaymentCard)

    }
  }

}
