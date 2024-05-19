package contextus.model.sefaria

import contextus.model.xml.XmlContextusDoc
import contextus.json.*
import contextus.model.xml.XmlContextusDocConversion.CATEGORY_SEPARATOR

/**
 * {
 * 	"title": "Test submission",
 * 	"heTitle": "Test submission hebrew",
 * 	"titleVariants": [
 * 	  "Test submission alternate title 1"
 * 	],
 * 	"heTitleVariants": [
 * 	  "Test submission alternate title 1 hebrew"
 * 	],
 * 	"categories": [
 * 	  "The Gilded Age"
 * 	],
 * 	"sectionNames": [
 * 	  "house",
 * 	  "room",
 * 	  "corner"
 * 	]
 * }
 */

final case class SefariaIndexSimpleEntry(
	title: String,
	heTitle: String,
	titleVariants: List[String],
	heTitleVariants: List[String],
	categories: List[String],
	sectionNames: List[String],
	enDesc: Option[String],
)

object SefariaIndexSimpleEntry:
	given Encoder[SefariaIndexSimpleEntry] = deriveEncoder
	given Decoder[SefariaIndexSimpleEntry] = deriveDecoder

	def fromContextusDoc(document: XmlContextusDoc): SefariaIndexSimpleEntry =
		val alternateTitles = document.alternateTitles.toList.flatten
		SefariaIndexSimpleEntry(
			title = document.title,
			heTitle = document.title + " HEBREW",
			titleVariants = alternateTitles,
			heTitleVariants = alternateTitles.map(_ + " HEBREW"),
			categories = document.category.split(CATEGORY_SEPARATOR).toList.map(_.trim),
			sectionNames = document.schema.levels,
			enDesc = document.description,
		)
