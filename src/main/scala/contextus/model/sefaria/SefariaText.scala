package contextus.model.sefaria

import contextus.json.*
import io.circe.Decoder.Result
import io.circe.{HCursor, Json}

/**
 * Represents Sefaria's JaggedArray. DAO for read/write text versions.
 * 
 * @param value list of text sections, each of which can be a string or a nested SefariaText
 */
final case class SefariaText(
	value: List[String | SefariaText]
)

object SefariaText:
	def empty: SefariaText = SefariaText(Nil)

	given Encoder[SefariaText] with
		override def apply(a: SefariaText): Json =
			Json.fromValues(a.value.map {
				case str: String => Json.fromString(str)
				case txt: SefariaText => apply(txt)
			})

	given Decoder[SefariaText] with
		override def apply(c: HCursor): Result[SefariaText] =
			c.as[List[Json]].flatMap(_.foldLeft(Right(Nil): Result[List[String | SefariaText]]) {
				(currentListRes, nextJson) =>
					for {
						nextValue <- apply(nextJson.hcursor).orElse(nextJson.as[String])
						currentList <- currentListRes
					} yield nextValue :: currentList
			}).map(list => SefariaText(list.reverse))

/**
 * DAO modeling a text submission request for Sefaria
 * {
 *   "versionTitle": "Sefaria Community Translation",
 * 	 "versionSource": "https://www.sefaria.org",
 *   "text": [
 * 	   "Test paragraph 1",
 * 	   "Test paragraph 2"
 *   ],
 * 	 "language": "en"
 * }
 */
final case class SefariaTextSubmission(
	versionTitle: String,
	versionSource: String,
	text: SefariaText,
	language: String,
)

object SefariaTextSubmission:
	given Encoder[SefariaTextSubmission] = deriveEncoder
	given Decoder[SefariaTextSubmission] = deriveDecoder
