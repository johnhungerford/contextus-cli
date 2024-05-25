package contextus.model.sefaria

import cats.Traverse
import contextus.json.*
import contextus.model.DomainError.DecodingError
import io.circe.Decoder.Result
import io.circe.{CursorOp, DecodingFailure, HCursor, JsonObject}

case class SefariaIndexEntry(
	title: String,
	heTitle: String,
	titleVariants: List[String],
	heTitleVariants: List[String],
	categories: List[String],
	heCategories: List[String],
	schemaOrSections: Either[SefariaSchemaNode, List[String]],
	order: Option[List[Int]] = None,
	authors: Option[List[SefariaAuthor]] = None,
	enDesc: Option[String] = None,
	heDesc: Option[String] = None,
	enShortDesc: Option[String] = None,
	heShortDesc: Option[String] = None,
	hasErrorMargin: Option[Boolean] = None,
	compDate: Option[List[Int]] = None,
	is_cited: Option[Boolean] = None,
	compDateString: Option[SefariaCompDate] = None,
)

object SefariaIndexEntry:
	private val schemaOrSectionsKey = "schemaOrSections"
	private val schemaKey = "schema"
	private val sectionsKey = "sectionNames"

	import io.circe.syntax.*
	given Encoder[Either[SefariaSchemaNode, List[String]]] = {
		case Left(value) => value.asJson
		case Right(value) => value.asJson
	}

	given Decoder[Either[List[SefariaSchemaNode], List[String]]] =
		(c: HCursor) =>
			c.as[List[SefariaSchemaNode]]
				.map(Left.apply)
				.orElse(
					c.as[List[String]].map(Right.apply)
				)

	inline given Encoder.AsObject[SefariaIndexEntry] = io.circe.generic.semiauto.deriveEncoder[SefariaIndexEntry]
		.mapJsonObject { obj =>
			val key = schemaOrSectionsKey
			obj
				.get(key, "Right", "value")
				.map(innerJson => {
					obj.remove(key).add(sectionsKey, innerJson)
				})
				.orElse(
					obj
						.get(key, "Left", "value")
						.map(innerJson => {
							obj.remove(key).add(schemaKey, innerJson)
						})
				)
				.getOrElse(obj)
				.filter(v => !v._2.isNull)
		}

	inline given Decoder[SefariaIndexEntry] = deriveDecoder[SefariaIndexEntry]
		.contramapJsonObject { obj =>
			obj(schemaKey) match
				case None => obj(sectionsKey) match
					case None => Left(DecodingFailure("missing both schema and sections: must have one or the other", Nil))
					case Some(json) =>
						Right(obj.remove(sectionsKey).add(schemaOrSectionsKey, Json.fromJsonObject(JsonObject("Right" -> Json.fromJsonObject(JsonObject("value" -> json))))))
				case Some(json) =>
					obj(sectionsKey) match
						case None =>
							Right(obj.remove(schemaKey).add(schemaOrSectionsKey, Json.fromJsonObject(JsonObject("Left" -> Json.fromJsonObject(JsonObject("value" -> json))))))
						case Some(_) =>
							Left(DecodingFailure("contains both schema and sections: must have one or the but not both", Nil))
		}

final case class SefariaAuthor(
	en: String,
	he: String,
	slug: String,
)

object SefariaAuthor:
	given Encoder[SefariaAuthor] = deriveEncoder
	given Decoder[SefariaAuthor] = deriveDecoder

final case class SefariaCompDate(
	en: String,
	he: String,
)

object SefariaCompDate:
	given Encoder[SefariaCompDate] = deriveEncoder
	given Decoder[SefariaCompDate] = deriveDecoder

final case class SefariaTitle(
	text: String,
	lang: String,
	primary: Option[Boolean],
)

object SefariaTitle:
	given Encoder[SefariaTitle] = deriveEncoder
	given Decoder[SefariaTitle] = deriveDecoder

sealed trait SefariaSchemaNode
object SefariaSchemaNode:
	final case class Text(
		depth: Int,
		addressTypes: List[SefariaAddressType],
		sectionNames: List[String],
		titles: List[SefariaTitle],
		heSectionNames: List[String],
		title: String,
		heTitle: String,
		key: String,
	) extends SefariaSchemaNode

	object Text:
		private val NODE_TYPE = "JaggedArrayNode"

		given textEncoder: Encoder[Text] = Encoder.AsObject.derived[Text]
			.mapJsonObject(("nodeType" -> Json.fromString(NODE_TYPE)) +: _)

		given textDecoder: Decoder[Text] = deriveDecoder[Text]
			.contramapJsonObject { jsonObj =>
				for {
					nodeTypeJson <- jsonObj.apply("nodeType").toRight(DecodingFailure("missing field \"nodeType\"", List(CursorOp.DownField("nodeType"))))
					nodeType <- nodeTypeJson.as[String]
					_ <- if (nodeType == NODE_TYPE) Right(()) else Left(DecodingFailure("nodeType was not `JaggedArrayNode`", Nil))
				} yield (jsonObj.remove("nodeType"))
			}

	final case class Section(
		nodes: List[SefariaSchemaNode],
		titles: List[SefariaTitle],
		title: String,
		heTitle: String,
		key: String,
	) extends SefariaSchemaNode

	object Section:
		given sectionEncoder: Encoder[Section] = deriveEncoder
		given sectionDecoder: Decoder[Section] = deriveDecoder

	given nodeEncoder: Encoder[SefariaSchemaNode] = encodeAny {
		case text: Text => Text.textEncoder
		case section: Section => Section.sectionEncoder
	}
	given nodeDecoder: Decoder[SefariaSchemaNode] = decodeAny(
		Decoder[Section].wide,
		Decoder[Text].wide,
	)

enum SefariaAddressType:
	self =>
		override def toString = self match
			case SefariaAddressType.Integer => "Integer"
			case SefariaAddressType.Other(other) => other

	case Integer
		case Other(other: String)

object SefariaAddressType:
	def fromString(str: String): SefariaAddressType = str.toLowerCase match
		case "integer" => Integer
		case other => Other(str)

	given Encoder[SefariaAddressType] =
		(a: SefariaAddressType) => Json.fromString(a.toString)

	given Decoder[SefariaAddressType] =
		(c: HCursor) => c.as[String].map(SefariaAddressType.fromString)