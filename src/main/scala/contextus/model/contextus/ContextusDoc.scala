package contextus.model.contextus

import contextus.model.types.NonEmptyList

/**
 * Core model of a contextus document for the purposes of this application.
 * Input document (XmlContextusDoc) should be converted to ContextusDoc,
 * and then ContextusDoc can be converted to an output document (SefariaDoc)
 * for submission.
 * 
 * Data validation (in the form of highly constrained types) should occur in this
 * model, not in the input and output.
 */
case class ContextusDoc(
	title: Title,
	author: String,
	description: Option[Description] = None,
	shortDescription: Option[ShortDescription] = None,
	compositionYear: Option[Year] = None,
	category: NonEmptyList[Category],
	version: DocVersion,
	content: NonEmptyList[ComplexDocContent] | SimpleDocContent,
)

type Title = Title.Type
object Title:
	val Pattern = """[^:.\-\\/\n]{1,1000}""".r

	opaque type Type <: String = String
	def parse(value: String): Either[String, Title] =
		val trimmed = value.replace('\n', ' ').trim
		if """^\p{ASCII}*$""".r.findFirstIn(trimmed).isEmpty then
			Left("Title must contain plain characters. Did you include non-English characters or curved quotes/apostrophes?")
		else if Pattern.matches(trimmed) then Right(trimmed)
		else Left(s"Title cannot contain colons, periods, hyphens, line breaks, or slashes")
	def unsafeWrap(value: String): Title =
		parse(value).left.foreach(v => s"INVALID TITLE: $v")
		value

type Author = Author.Type
object Author:
	opaque type Type <: String = String
	def parse(value: String): Either[String, Type] = Right(value)
	def unsafeWrap(value: String): Type =
		parse(value).left.foreach(v => s"INVALID AUTHOR: $v")
		value

type Description = Description.Type
object Description:
	opaque type Type <: String = String
	def parse(value: String): Either[String, Type] =
		if value.length <= 500 then Right(value)
		else Left(s"Description can be no more than 500 characters (${value.length})")
	def unsafeWrap(value: String): Type =
		parse(value).left.foreach(v => s"INVALID DESCRIPTION: $v")
		value

type ShortDescription = ShortDescription.Type
object ShortDescription:
	opaque type Type <: String = String
	def parse(value: String): Either[String, Type] =
		if value.length <= 100 then Right(value)
		else Left(s"Short description can be no more than 100 characters (${value.length})")
	def unsafeWrap(value: String): Type =
		parse(value).left.foreach(v => s"INVALID SHORT DESCRIPTION: $v")
		value

type Category = Category.Type
object Category:
	val Pattern = """[^.\-\n]{1,100}""".r

	opaque type Type <: String = String
	def parse(value: String): Either[String, Type] =
		if Pattern.matches(value) then Right(value)
		else Left(s"Illegal category value: '$value'. Category must be between 1 and 50 characters and cannot contain periods or hyphens")
	def unsafeWrap(value: String): Type =
		parse(value).left.foreach(v => s"INVALID CATEGORY: $v")
		value

type Year = Year.Type
object Year:
	opaque type Type <: Int = Int
	def parse(value: Int): Either[String, Type] =
		if value > -3200 && value < 2100 then Right(value)
		else Left(s"Year must be between -3200 and 2100")
	def unsafeWrap(value: Int): Type =
		parse(value).left.foreach(v => s"INVALID YEAR: $v")
		value

/**
 * Represents version of the document. Text and document are inseparable in this model. In 
 * Sefaria they are separate.
 * 
 * @param title
 * @param source
 * @param language
 */
final case class DocVersion(
	title: String,
	source: String,
	language: Option[String],
)

/**
 * Represents a document that has a consistent section structure. Cannot be used 
 * for documents that have individually titled sections or sections with different 
 * section headers or depths.
 * 
 * @param levels schema for sections, e.g., List("Chapter", "Section", "Paragraph")
 * @param sections simple document content in the form of a list of [[ContentSection]]
 */
final case class SimpleDocContent(
	levels: NonEmptyList[String],
	sections: NonEmptyList[ContentSection]
)

/**
 * Represents a complex document that has inconsistent section structure. This means
 * that a document either has sections with different depths or section headings, or 
 * its section headings must be given explicitly (e.g., Chapter One: Yada yada yada, as 
 * opposed to Chapter 1, Chapter 2...)
 */
sealed trait ComplexDocContent

object ComplexDocContent:
	/**
	 * Sub-document that has actual content
	 * 
	 * @param title sub-document (or section) title
	 * @param levels schema for sub-document, e.g., List("Chapter", "Section", "Paragraph")
	 * @param sections: sub-document content in the form of a list of [[ContentSection]]
	 */
	final case class Content(
		title: Title,
		levels: NonEmptyList[String],
		sections: NonEmptyList[ContentSection],
	) extends ComplexDocContent

	/**
	 * Sub-document composed of other sub-documents.
	 * 
	 * @param title sub-document title
	 * @param content nested [[ComplexDocContent]] sub-documents
	 */
	final case class ContentList(
		title: String,
		content: NonEmptyList[ComplexDocContent],
	) extends ComplexDocContent

/**
 * Text content for a simple document or a simple sub-document
 */
sealed trait ContentSection
object ContentSection:
	/**
	 * Simple text section
	 * @param text text string
	 */
	final case class Text(
		text: String,
	) extends ContentSection

	/**
	 * Text section with nested sections
	 * @param subSections inner sections
	 */
	final case class Nested(
		subSections: NonEmptyList[ContentSection]
	) extends ContentSection
