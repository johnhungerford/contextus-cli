package contextus.model.xml

import contextus.validation.{Validation, ValidationErrorWithoutIdentifier}
import contextus.model.DomainError.ValidationError
import ru.tinkoff.phobos.syntax.*
import ru.tinkoff.phobos.derivation.semiauto.*
import ru.tinkoff.phobos.encoding.*
import ru.tinkoff.phobos.decoding.*
import ParseIndent.{Auto, IgnoreIndent}
import contextus.service.XmlTextProcessingService
import contextus.util.HandleIndents


final case class XmlContextusDoc(
	title: Option[String],
	@renamed("alternateTitle") alternateTitles: Option[List[String]],
	author: Option[String],
	@renamed("composition-year") publicationYear: Option[Int],
	description: Option[String],
	@renamed("short-description") shortDescription: Option[String],
	@renamed("category") category: Option[String],
	schema: Option[Schema],
	version: Option[Version],
	@renamed("body") body: Option[Body],
)

object XmlContextusDoc:
	given XmlEncoder[XmlContextusDoc] = deriveXmlEncoder("document")
	given XmlDecoder[XmlContextusDoc] = deriveXmlDecoder("document")

final case class Version(
	title: Option[String],
	source: Option[String],
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

final case class Body(
	@renamed("section") sections: List[Section],
	@text rawText: String,
):
	lazy val textIsEmpty: Boolean =
		rawText.isBlank

	def textAsSection: Section =
		Section(None, None, sections, rawText, None)

	def parsedText(
		textProcessingService: XmlTextProcessingService,
		paragraphBreak: ParagraphBreak,
		indent: ParseIndent = ParseIndent.Auto,
	): Option[String | List[String]] = Option.when(!textIsEmpty)(rawText).map {
		textStringWithIndents =>
			val processedText =
				textProcessingService.processPostIngest(
					textStringWithIndents,
				)
			val textString = HandleIndents(processedText, indent)
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
		textProcessingService: XmlTextProcessingService,
		paragraphBreak: ParagraphBreak,
		indent: ParseIndent = ParseIndent.Auto,
	): Option[String | List[String]] = Option.when(!textIsEmpty)(rawText).map {
		textStringWithIndents =>
			val processedText =
				textProcessingService.processPostIngest(textStringWithIndents)

			val textString = HandleIndents(processedText, indent)
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
