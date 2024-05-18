package contextus.model.xml

import contextus.validation.{Validation, ValidationErrorWithoutIdentifier}
import contextus.model.DomainError.ValidationError
import ru.tinkoff.phobos.syntax.*
import ru.tinkoff.phobos.derivation.semiauto.*
import ru.tinkoff.phobos.encoding.*
import ru.tinkoff.phobos.decoding.*
import ParseIndent.{Auto, IgnoreIndent}
import contextus.util.HandleIndents


final case class XmlContextusDoc(
	title: String,
	@renamed("alternateTitle") alternateTitles: Option[List[String]],
	author: String,
	@renamed("composition-year") publicationYear: Option[Int],
	description: Option[String],
	@renamed("short-description") shortDescription: Option[String],
	@renamed("category") category: String,
	schema: Schema,
	version: Version,
	@renamed("body") body: Body,
)

object XmlContextusDoc:
	given XmlEncoder[XmlContextusDoc] = deriveXmlEncoder("document")
	given XmlDecoder[XmlContextusDoc] = deriveXmlDecoder("document")

	given Validation[XmlContextusDoc] with {
		override val identifier = "ContextusDoc"

		private def validateLevels(section: Section, levels: List[String], paragraphBreak: ParagraphBreak): Option[ValidationErrorWithoutIdentifier] =
			(section.parsedText(paragraphBreak), levels) match
				case (_, Nil) => Some(ValidationErrorWithoutIdentifier(Some(section.toString), Some("the number levels in the document exceed the number of levels specified in the schema")))
				case (Some(_: String), level :: Nil) =>
					if section.level.contains(level) then None
					else Some(ValidationErrorWithoutIdentifier(Some(section.toString), Some(s"section level ${section.level} does not match schema level $level")))
				case (Some(_: List[String]), level :: _ :: Nil) =>
					if section.level.contains(level) then None
					else Some(ValidationErrorWithoutIdentifier(Some(section.toString), Some(s"section level ${section.level} does not match schema level $level")))
				case (Some(_), _) =>
					Some(ValidationErrorWithoutIdentifier(Some(section.toString), Some(s"the number of levels in the schema exceeds the number of levels found in the document")))
				case (None, _ :: nextLevels) =>
					section
						.subSections
						.foldLeft(None: Option[ValidationErrorWithoutIdentifier]) {
							case (opt @ Some(_), _) => opt
							case (None, nextSection) => nextLevels match
								case Nil =>
									Some(ValidationErrorWithoutIdentifier(Some(nextSection.toString), Some(s"the number levels in the document exceed the number of levels specified in the schema")))
								case _ :: next =>
									validateLevels(nextSection, nextLevels, paragraphBreak)
						}

		override protected def validateImplementation(
			value: XmlContextusDoc,
		): Option[ValidationErrorWithoutIdentifier] =
			val bodyValidation = Validation.validate(value.body)
			val levelValidation =
				if value.body.textIsEmpty then
					value.body.sections.foldLeft(None: Option[ValidationErrorWithoutIdentifier]) {
						(lastValidationOpt, nextSection) => lastValidationOpt match
							case None => validateLevels(nextSection, value.schema.levels, value.schema.paragraphBreak)
							case fail @ Some(_) => fail
					}
				else validateLevels(
					Section(
						None,
						value.schema.levels.headOption,
						Nil,
						value.body.rawText,
					),
					value.schema.levels,
					value.schema.paragraphBreak,
				)
			val schemaValidation = Validation[Schema]
			  .validate(value.schema)

			(levelValidation.toList ++ schemaValidation.toList) match
				case Nil => None
				case (err: ValidationErrorWithoutIdentifier) :: Nil =>
					Some(err.copy(serializedValue = Some(value.toString)))
				case (err: ValidationError) :: Nil =>
					Some(ValidationErrorWithoutIdentifier(
						Some(value.toString),
						Some("Invalid schema"),
						List(err),
					))
				case both => Some(ValidationErrorWithoutIdentifier(
					Some(value.toString),
					Some("Invalid schema and invalid number of levels"),
					both,
				))
	}

final case class Version(
	title: String,
	source: String,
	language: Option[String]
)

object Version:
	given ElementEncoder[Version] = deriveElementEncoder
	given ElementDecoder[Version] = deriveElementDecoder

enum ParagraphBreak:
	self =>
		lazy val stringValue: Option[String] = self match
			case ParagraphBreak.IgnoreParagraphs =>
				None
			case ParagraphBreak.LineBreak =>
				Some("line-break")
			case ParagraphBreak.MultiLineBreak =>
				Some("multi-line-break")
			case ParagraphBreak.MultiLineBreakRemoveSingleLineBreaks =>
				Some("multi-line-break-remove-line-breaks")

		case IgnoreParagraphs
		case LineBreak
		case MultiLineBreak
		case MultiLineBreakRemoveSingleLineBreaks

object ParagraphBreak:
	def fromString(string: Option[String]): Option[ParagraphBreak] =
		string.map(_.trim.toLowerCase) match
			case None => Some(ParagraphBreak.IgnoreParagraphs)
			case Some("line-break") => Some(ParagraphBreak.LineBreak)
			case Some("multi-line-break") => Some(ParagraphBreak.MultiLineBreak)
			case Some("multi-line-break-remove-line-breaks") =>
				Some(ParagraphBreak.MultiLineBreakRemoveSingleLineBreaks)
			case _ => None

	given AttributeEncoder[ParagraphBreak] =
		AttributeEncoder[Option[String]].contramap(_.stringValue)
	given AttributeDecoder[ParagraphBreak] =
		AttributeDecoder[Option[String]].emap((history, v) => {
			ParagraphBreak.fromString(v).toRight(
				DecodingError(
					s"Value ${v.getOrElse("{}")} must either be omitted be one of the following: line-break, multi-line-break, multi-line-break-remove-line-breaks",
					history,
					None,
				)
			)
		})


sealed trait ParseIndent:
	self =>
		def stringValue: Option[String] = self match
			case ParseIndent.Auto => None
			case ParseIndent.IgnoreIndent => Some("ignore")

object ParseIndent:
	case object IgnoreIndent extends ParseIndent
	case object Auto extends ParseIndent

	def fromString(string: Option[String]): Option[ParseIndent] = string match
		case None => Some(ParseIndent.Auto)
		case Some("auto") => Some(ParseIndent.Auto)
		case Some("ignore" | "ignore-indent") => Some(ParseIndent.IgnoreIndent)
		case _ => None

	given AttributeEncoder[ParseIndent] =
		AttributeEncoder[Option[String]].contramap(_.stringValue)
	given AttributeDecoder[ParseIndent] =
		AttributeDecoder[Option[String]].emap((history, v) => {
			ParseIndent.fromString(v).toRight(
				DecodingError(
					s"Value ${v.getOrElse("{}")} must either be omitted be one of the following: auto, ignore",
					history,
					None,
				)
			)
		})

final case class Schema(
	@renamed("level") levels: List[String],
	@attr paragraphBreak: ParagraphBreak = ParagraphBreak.MultiLineBreak,
	@attr indent: ParseIndent = ParseIndent.Auto,
):
	def popLevel: Option[(String, Schema)] = levels match
		case Nil => None
		case level :: next => Some(level -> copy(levels = next))

object Schema:
	given ElementEncoder[Schema] = deriveElementEncoder
	given ElementDecoder[Schema] = deriveElementDecoder

	given Validation[Schema] with {
		override val identifier = "Schema"

		override protected def validateImplementation(
			value: Schema,
		): Option[ValidationErrorWithoutIdentifier] =
			val levels = value.levels
			if levels.toSet.size == levels.size then None
			else Some(ValidationErrorWithoutIdentifier(
				Some(value.toString),
				Some("Multiple section levels have the same name"),
			))
	}

final case class Body(
	@renamed("section") sections: List[Section],
	@text rawText: String,
):
	lazy val textIsEmpty: Boolean =
		rawText.isBlank

	def textAsSection: Section =
		Section(None, None, sections, rawText, None)

	def parsedText(
		paragraphBreak: ParagraphBreak,
		indent: ParseIndent = ParseIndent.Auto,
	): Option[String | List[String]] = Option.when(!textIsEmpty)(rawText).map {
		textStringWithIndents =>
			val textString = HandleIndents(textStringWithIndents, indent)
			paragraphBreak match
				case ParagraphBreak.IgnoreParagraphs =>
					textString
				case ParagraphBreak.LineBreak =>
					textString.split("\n+").toList.filter(!_.isBlank)
				case ParagraphBreak.MultiLineBreak =>
					textString.split("\n\n+").toList.filter(!_.isBlank)
				case ParagraphBreak.MultiLineBreakRemoveSingleLineBreaks =>
					textString.split("\n\n+").map(_.trim.split("\n").mkString(" ")).toList.filter(!_.isBlank)
	}


object Body:
	given ElementEncoder[Body] = deriveElementEncoder
	given ElementDecoder[Body] = deriveElementDecoder

	given Validation[Body] with
		override def identifier: String = "Section"

		override protected def validateImplementation(
			value: Body,
		): Option[ValidationErrorWithoutIdentifier] =
			if !value.textIsEmpty && value.sections.nonEmpty then
				Some(ValidationErrorWithoutIdentifier(
					Some(value.toString),
					Some("Section has both text and subsections"),
				))
			else if value.textIsEmpty && value.sections.isEmpty then
				Some(ValidationErrorWithoutIdentifier(
					Some(value.toString),
					Some("Section has neither text nor subsections"),
				))
			else None


final case class Section(
	title: Option[String],
	@attr level: Option[String],
	@renamed("section") subSections: List[Section],
	@text rawText: String,
	schema: Option[Schema] = None,
):
	lazy val textIsEmpty: Boolean =
		rawText.isBlank

	def parsedText(
		paragraphBreak: ParagraphBreak,
		indent: ParseIndent = ParseIndent.Auto,
	): Option[String | List[String]] = Option.when(!textIsEmpty)(rawText).map {
		textStringWithIndents =>
			val textString = HandleIndents(textStringWithIndents, indent)
			paragraphBreak match
				case ParagraphBreak.IgnoreParagraphs =>
					textString
				case ParagraphBreak.LineBreak =>
					textString.split("\n+").toList.filter(!_.isBlank)
				case ParagraphBreak.MultiLineBreak =>
					textString.split("\n\n+").toList.filter(!_.isBlank)
				case ParagraphBreak.MultiLineBreakRemoveSingleLineBreaks =>
					textString.split("\n\n+").map(_.trim.split("\n").mkString(" ")).toList.filter(!_.isBlank)
	}

object Section:
	given ElementEncoder[Section] = deriveElementEncoder
	given ElementDecoder[Section] = deriveElementDecoder

	given Validation[Section] with
		override def identifier: String = "Section"

		override protected def validateImplementation(
			value: Section,
		): Option[ValidationErrorWithoutIdentifier] =
			if !value.textIsEmpty && value.subSections.nonEmpty then
				Some(ValidationErrorWithoutIdentifier(
					Some(value.toString),
					Some("Section has both text and subsections"),
				))
			else if value.textIsEmpty && value.subSections.isEmpty then
				Some(ValidationErrorWithoutIdentifier(
					Some(value.toString),
					Some("Section has neither text nor subsections"),
				))
			else None
