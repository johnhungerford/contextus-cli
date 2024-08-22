package contextus.model.sefaria

import contextus.json.*

/**
 * Error messages returned (sometimes) by Sefaria
 * @param error string message
 */
final case class SefariaError(
	error: String
)

object SefariaError:
	given Encoder[SefariaError] = deriveEncoder
	given Decoder[SefariaError] = deriveDecoder

	object IsError:
		def unapply(string: String): Option[String] =
			string.decodeJson[SefariaError].toOption.map(_.error)
