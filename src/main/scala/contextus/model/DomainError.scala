package contextus.model

import izumi.reflect.macrortti.LightTypeTag
import zio.Tag

object DomainError:
	enum IOError:
		case FileIOError(path: String, problem: String, underlying: Option[Throwable])
		case HttpIOError(url: String, method: HttpMethod, problem: String, underlying: Option[Throwable])

	enum HttpMethod:
		case Get, Post, Put, Delete
	
	final case class SefariaApiError(
		url: String,
		method: HttpMethod,
		message: Option[String],
		status: Option[Int],
		underlying: Option[Throwable],
	)

	final case class DecodingError(
		typeId: Either[LightTypeTag, String],
		problem: String,
		underlying: Option[Throwable],
	)

	final case class ValidationError(
		identifier: String,
		serializedValue: Option[String],
		reason: Option[String],
		underlyingErrors: List[ValidationError] = Nil,
		underlyingException: Option[Throwable] = None,
	)
	
	enum ConfigError:
		case MissingValue(property: String, message: String)
