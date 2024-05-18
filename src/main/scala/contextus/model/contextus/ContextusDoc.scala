package contextus.model.contextus

import contextus.model.types.NonEmptyList

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
	val Pattern = """[^:.\-\\/]{1,100}""".r

	opaque type Type <: String = String
	def parse(value: String): Either[String, Title] =
		if Pattern.matches(value) then Right(value)
		else Left(s"Title cannot contain colons, periods, hyphens, or slashes")
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
	val Pattern = """[^.\-]{1,50}""".r

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

final case class DocVersion(
	title: String,
	source: String,
	language: Option[String],
)

final case class SimpleDocContent(
	levels: NonEmptyList[String],
	sections: NonEmptyList[ContentSection]
)

sealed trait ComplexDocContent
object ComplexDocContent:
	final case class Content(
		title: Title,
		levels: NonEmptyList[String],
		sections: NonEmptyList[ContentSection],
	) extends ComplexDocContent

	final case class ContentList(
		title: String,
		content: NonEmptyList[ComplexDocContent],
	) extends ComplexDocContent

sealed trait ContentSection
object ContentSection:
	final case class Text(
		text: String,
	) extends ContentSection

	final case class Nested(
		subSections: NonEmptyList[ContentSection]
	) extends ContentSection
