package contextus.model.sefaria

import contextus.model.xml.*
import contextus.json.*
import io.circe.Decoder.Result
import io.circe.{HCursor, Json}

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

	def fromContextusSection(paragraphBreak: ParagraphBreak, parseIndent: ParseIndent)(section: Section): String | SefariaText =
		section.parsedText(paragraphBreak, parseIndent) match
			case Some(string: String) => string
			case Some(sections: List[String]) => SefariaText(sections)
			case None => fromContextusSections(section.subSections, paragraphBreak, parseIndent)

	def fromContextusSections(sections: List[Section], paragraphBreak: ParagraphBreak, parseIndent: ParseIndent): SefariaText =
		SefariaText(sections.map(fromContextusSection(paragraphBreak, parseIndent)))

	def fromContextusDoc(doc: XmlContextusDoc): SefariaText =
		val Schema(_, paragraphBreak, parseIndent) = doc.schema
		fromContextusSection(paragraphBreak, parseIndent)(doc.body.textAsSection) match
			case str: String => SefariaText(List(str))
			case sect: SefariaText => sect


/**
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

	def fromContextusDoc(document: XmlContextusDoc): SefariaTextSubmission =
		SefariaTextSubmission(
			versionTitle = document.version.title,
			versionSource = document.version.source,
			text = SefariaText.fromContextusDoc(document),
			language = document
			  .version
			  .language
			  .getOrElse("en"),
		)