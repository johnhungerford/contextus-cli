package contextus.model.contextus

import contextus.model.types.NonEmptyList
import contextus.model.xml.{Body, Schema, XmlContextusDoc, XmlContextusDocConversion, XmlContextusDocSpec}
import zio.test.*

object ContextusDocTest extends ZIOSpecDefault:
	val simpleDoc = XmlContextusDocSpec.basicDoc.copy(category = "category1, category2")

	val complexDoc = XmlContextusDoc(
		"Title",
		Some(List("Alt Title 1", "Alt Title 2")),
		"Author",
		Some(1999),
		Some("Here is the long description"),
		Some("Short description"),
		"category1, category2",
		Schema(
			List("paragraph"),
		),
		contextus.model.xml.Version(
			"Version Title",
			"www.version.com",
			Some("en"),
		),
		Body(
			rawText = "",
			sections = List(
				contextus.model.xml.Section(
					Some("Preface"),
					None,
					Nil,
					"preface paragraph 1\n\npreface paragraph 2",
					None,
				),
				contextus.model.xml.Section(
					Some("Book 1"),
					None,
					List(
						contextus.model.xml.Section(
							None,
							Some("chapter"),
							Nil,
							"book 1, chapter 1, paragraph 1\n\nbook 1, chapter 1, paragraph 2",
							None,
						),
						contextus.model.xml.Section(
							None,
							Some("chapter"),
							Nil,
							"book 1, chapter 2, paragraph 1\n\nbook 1, chapter 2, paragraph 2",
							None,
						)
					),
					"",
					Some(Schema(List("chapter", "paragraph"))),
				),
				contextus.model.xml.Section(
					Some("Book 2"),
					None,
					List(
						contextus.model.xml.Section(
							None,
							Some("chapter"),
							Nil,
							"book 2, chapter 1, paragraph 1\n\nbook 2, chapter 1, paragraph 2",
							None,
						),
						contextus.model.xml.Section(
							None,
							Some("chapter"),
							Nil,
							"book 2, chapter 2, paragraph 1\n\nbook 2, chapter 2, paragraph 2",
							None,
						)
					),
					"",
					Some(Schema(List("chapter", "paragraph"))),
				),
			)
		)
	)

	val expectedComplexDoc = ContextusDoc(
		title = Title.unsafeWrap(complexDoc.title),
		author = Author.unsafeWrap(complexDoc.author),
		category = NonEmptyList(Category.unsafeWrap("category1"), Category.unsafeWrap("category2")),
		description = complexDoc.description.map(Description.unsafeWrap),
		shortDescription = complexDoc.shortDescription.map(ShortDescription.unsafeWrap),
		compositionYear = complexDoc.publicationYear.map(Year.unsafeWrap),
		version = DocVersion(
			complexDoc.version.title,
			complexDoc.version.source,
			complexDoc.version.language,
		),
		content = NonEmptyList(
			ComplexDocContent.Content(
				title = Title.unsafeWrap("Preface"),
				levels = NonEmptyList("paragraph"),
				sections = NonEmptyList(
					ContentSection.Text("preface paragraph 1"),
					ContentSection.Text("preface paragraph 2"),
				),
			),
			ComplexDocContent.Content(
				title = Title.unsafeWrap("Book 1"),
				levels = NonEmptyList("chapter", "paragraph"),
				sections = NonEmptyList(
					ContentSection.Nested(NonEmptyList(
						ContentSection.Text("book 1, chapter 1, paragraph 1"),
						ContentSection.Text("book 1, chapter 1, paragraph 2"),
					)),
					ContentSection.Nested(NonEmptyList(
						ContentSection.Text("book 1, chapter 2, paragraph 1"),
						ContentSection.Text("book 1, chapter 2, paragraph 2"),
					))
				),
			),
			ComplexDocContent.Content(
				title = Title.unsafeWrap("Book 2"),
				levels = NonEmptyList("chapter", "paragraph"),
				sections = NonEmptyList(
					ContentSection.Nested(NonEmptyList(
						ContentSection.Text("book 2, chapter 1, paragraph 1"),
						ContentSection.Text("book 2, chapter 1, paragraph 2"),
					)),
					ContentSection.Nested(NonEmptyList(
						ContentSection.Text("book 2, chapter 2, paragraph 1"),
						ContentSection.Text("book 2, chapter 2, paragraph 2"),
					))
				),
			),
		)
	)

	val expectedSimpleDoc = ContextusDoc(
		title = Title.unsafeWrap(simpleDoc.title),
		author = Author.unsafeWrap(simpleDoc.author),
		category = NonEmptyList(Category.unsafeWrap("category1"), Category.unsafeWrap("category2")),
		description = None,
		shortDescription = None,
		compositionYear = None,
		version = DocVersion(
			simpleDoc.version.title,
			simpleDoc.version.source,
			simpleDoc.version.language,
		),
		content = SimpleDocContent(
			levels = NonEmptyList("book", "chapter", "paragraph"),
			sections = NonEmptyList(
				ContentSection.Nested(NonEmptyList(
					ContentSection.Nested(NonEmptyList(
						ContentSection.Text(
							"text of book 1, chapter 1, paragraph 1",
						),
						ContentSection.Text(
							"text of book 1, chapter 1, paragraph 2",
						)
					)),
					ContentSection.Nested(NonEmptyList(
						ContentSection.Text(
							"text of book 1, chapter 2, paragraph 1",
						),
						ContentSection.Text(
							"text of book 1, chapter 2, paragraph 2",
						)
					)),
				)),
				ContentSection.Nested(NonEmptyList(
					ContentSection.Nested(NonEmptyList(
						ContentSection.Text(
							"text of book 2, chapter 1, paragraph 1",
						),
						ContentSection.Text(
							"text of book 2, chapter 1, paragraph 2",
						)
					)),
					ContentSection.Nested(NonEmptyList(
						ContentSection.Text(
							"text of book 2, chapter 2, paragraph 1",
						),
						ContentSection.Text(
							"text of book 2, chapter 2, paragraph 2",
						),
					)),
				))
			)
		)
	)

	override def spec = suite("ContextusDocTest")(
		suite("from XmlContextusDoc")(
			test("simple doc") {
				XmlContextusDocConversion
					.convert(simpleDoc)
					.map { doc =>
						assertTrue(
							doc == expectedSimpleDoc,
						)
					}
			},
			test("complex doc") {
				XmlContextusDocConversion
					.convert(complexDoc)
					.map { doc =>
						assertTrue(
							doc == expectedComplexDoc,
						)
					}
			}
		)
	)
