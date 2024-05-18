package contextus.model.sefaria

import cats.Traverse
import contextus.json.*
import contextus.model.xml.{XmlContextusDoc, Schema}
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

	def fromContextusDoc(document: XmlContextusDoc): Either[DecodingError, SefariaIndexEntry] =
		val schemaOrSections: Either[DecodingError, Either[SefariaSchemaNode, List[String]]] =
			if document.body.sections.isEmpty && document.body.textIsEmpty then
				Left(DecodingError(Right(s"ContextusDoc"), "document body must either have subsections or text", None))
			else if document.body.sections.isEmpty then
				Right(Right(document.schema.levels))
			else for {
				nodeOptList <- Traverse[List]
				  .traverse(document.body.sections)(s => SefariaSchemaNode.fromContextusSection(document.schema, s))
				nodeList <- {
					val filteredNodes = nodeOptList collect { case Some(node) => node }
					if filteredNodes.size < nodeOptList.size then
						Left(DecodingError(Right(s"ContextusDoc"), "some sections have titles and some do not", None))
					else Right(filteredNodes)
				}
			} yield Left(SefariaSchemaNode.Section(
				nodes = nodeList,
				titles = List(SefariaTitle(document.title, "en", Some(true))),
				title = document.title,
				heTitle = document.title + " HEBREW",
				key = document.title
			))

		schemaOrSections.map { schOrSect =>
			SefariaIndexEntry(
				document.title,
				s"${document.title} HEBREW",
				Nil,
				Nil,
				document.category.split(",").toList.map(_.trim),
				document.category.split(",").toList.map(cat => s"${cat.trim} HEBREW"),
				schOrSect,
				None,
				Some(List(SefariaAuthor(document.author, s"${document.author} HEBREW", document.author))),
				document.description,
				document.description.map(_ + " HEBREW"),
				None,
				None,
				None,
				document.publicationYear.map(v => List(v)),
				None,
				None,
			)
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

	private def sectionsHaveTitles(sections: List[contextus.model.xml.Section]): Either[DecodingError, Boolean] =
		sections.foldLeft(Right(Set.empty): Either[DecodingError, Set[Boolean]]) { (currentList, nextSection) =>
			val hasTitle = nextSection.title.nonEmpty
			currentList.flatMap { set =>
				sectionsHaveTitles(nextSection.subSections).flatMap {subsectionsHaveTitles =>
					if !hasTitle && subsectionsHaveTitles then Left(DecodingError(Right("Section"), "section without title cannot have subsection with title!", None))
					else Right(set + hasTitle)
				}
			}
		}.flatMap { boolSet =>
			if boolSet.isEmpty then Right(false)
			else if boolSet.size == 1 && boolSet.headOption.contains(false) then Right(false)
			else if boolSet.size == 1 then Right(true)
			else Left(DecodingError(Right("Section"), "all sections at a given level must either have a title or have no title", None))
		}

	def fromContextusSection(defaultSchema: Schema, section: contextus.model.xml.Section): Either[DecodingError, Option[SefariaSchemaNode]] =
		(section.title -> section.schema) match
			case (None, Some(_)) =>
				Left(DecodingError(Right("SefariaSchemaNode"), "Cannot decode a sefaria schema node from a section with schema but no title", None))
			case (None, None) => Right(None)
			case (Some(title), schemaOpt) =>
				val schema = schemaOpt.getOrElse(defaultSchema)
				val heTitle = s"$title HEBREW"
				val titles = List(SefariaTitle(title, "en", Some(true)), SefariaTitle(heTitle, "he", Some(true)))
				sectionsHaveTitles(section.subSections).flatMap {subsectionsHaveTitles =>
					Traverse[List]
					  .traverse(section.subSections)(s => fromContextusSection(defaultSchema, s))

					if subsectionsHaveTitles then for {
						innerNodes <- Traverse[List]
						  .traverse(section
							.subSections
						  )(s => fromContextusSection(defaultSchema, s))
						nodes <- {
							val filteredNodes = innerNodes collect {
								case Some(node) => node
							}
							if filteredNodes.size < innerNodes
							  .size then Left(DecodingError(Right("Section"), "Some inner sections have titles and some don't", None))
							else Right(filteredNodes)
						}
					} yield Some(Section(
						nodes,
						titles,
						title,
						heTitle,
						title,
					)) else Right(Some(Text(
						schema.levels.length,
						schema.levels.map(_ => SefariaAddressType.Integer),
						schema.levels,
						titles,
						schema.levels.map(level => s"$level HEBREW"),
						title,
						s"$title HEBREW",
						title,
					)))
				}

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