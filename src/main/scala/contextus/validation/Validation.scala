package contextus.validation

import contextus.model.DomainError.ValidationError

case class ValidationErrorWithoutIdentifier(
	serializedValue: Option[String],
	reason: Option[String],
	underlyingErrors: List[ValidationError | ValidationErrorWithoutIdentifier] = Nil,
	underlyingException: Option[Throwable] = None,
):
	def withIdentifier(id: String): ValidationError = ValidationError(
		identifier = id,
		serializedValue = serializedValue,
		reason = reason,
		underlyingErrors = underlyingErrors.map {
			case err: ValidationError => err
			case err: ValidationErrorWithoutIdentifier =>
				err.withIdentifier(id)
		},
		underlyingException = underlyingException,
	)

trait Validation[A]:
	def identifier: String
	protected def validateImplementation(value: A): Option[ValidationErrorWithoutIdentifier]
	final def validate(value: A): Option[ValidationError] =
		validateImplementation(value).map(_.withIdentifier(identifier))

object Validation:
	def apply[A](using validation: Validation[A]): Validation[A] =
		summon[Validation[A]]

	def validate[A](value: A)(using validation: Validation[A]): Option[ValidationError] =
		validation.validate(value)
