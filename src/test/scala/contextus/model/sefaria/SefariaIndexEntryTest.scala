package contextus.model.sefaria

import zio.*
import zio.test.*
import contextus.json.*


object SefariaIndexEntryTest extends ZIOSpecDefault:

	/**
	 * SefariaIndexSubmission(
	 * "Test submission",
	 * "Test submission hebrew",
	 * List("Test submission alternate title 1"),
	 * List("Test submission alternate title 1 hebrew"),
	 * List("The Gilded Age"),
	 * List("house", "room", "corner"),
	 * )
	 */

	val simpleEntry = SefariaIndexEntry(
		"Test submission",
		"Test submission hebrew",
		List("Test submission alternate title 1"),
		List("Test submission alternate title 1 hebrew"),
		List("The Gilded Age"),
		List("The Gilded Age hebrew"),
		Right(List("house", "room", "corner")),
	)

	val complexEntry = SefariaIndexEntry(
		"Test submission",
		"Test submission hebrew",
		List("Test submission alternate title 1"),
		List("Test submission alternate title 1 hebrew"),
		List("The Gilded Age"),
		List("The Gilded Age hebrew"),
		Left(List(
			SefariaSchemaNode.Section(
				List(
					SefariaSchemaNode.Text(
						1,
						List(SefariaAddressType.Integer),
						List("section"),
						List(SefariaTitle("title", "en", Some(true)), SefariaTitle("title HEBREW", "he", Some(true))),
						List("section HEBREW"),
						"title",
						"title HEBREW",
						"title",
					),
				),
				List(SefariaTitle("section-title", "en", Some(true)), SefariaTitle("section-title HEBREW", "he", Some(true))),
				"section-title",
				"section-title HEBREW",
				"section-title",
			),
			SefariaSchemaNode.Section(
				List(
					SefariaSchemaNode.Text(
						1,
						List(SefariaAddressType.Integer),
						List("paragraph"),
						List(SefariaTitle("second-title", "en", Some(true)), SefariaTitle("second-title HEBREW", "he", Some(true))),
						List("pragraph HEBREW"),
						"second-title",
						"second-title HEBREW",
						"second-title",
					),
					SefariaSchemaNode.Text(
						1,
						List(SefariaAddressType.Integer, SefariaAddressType.Integer),
						List("chapter", "paragraph"),
						List(SefariaTitle("third-title", "en", Some(true)), SefariaTitle("third-title HEBREW", "he", Some(true))),
						List("chapter HEBREW", "paragraph HEBREW"),
						"third-title",
						"third-title HEBREW",
						"third-title",
					),
				),
				List(SefariaTitle("second-section-title", "en", Some(true)), SefariaTitle("second-section-title HEBREW", "he", Some(true))),
				"second-section-title",
				"second-section-title HEBREW",
				"second-section-title",
			),
			SefariaSchemaNode.Text(
				1,
				List(SefariaAddressType.Integer),
				List("book", "chapter", "verse"),
				List(SefariaTitle("fourth-title", "en", Some(true)), SefariaTitle("fourth-title HEBREW", "he", Some(true))),
				List("book HEBREW", "chapter HEBREW", "verse HEBREW"),
				"fourth-title",
				"fourth-title HEBREW",
				"fourth-title",
			),
		)),
	)

	val simpleJson = """{"title":"Test submission","heTitle":"Test submission hebrew","titleVariants":["Test submission alternate title 1"],"heTitleVariants":["Test submission alternate title 1 hebrew"],"categories":["The Gilded Age"],"heCategories":["The Gilded Age hebrew"],"order":null,"authors":null,"enDesc":null,"heDesc":null,"enShortDesc":null,"heShortDesc":null,"hasErrorMargin":null,"compDate":null,"is_cited":null,"compDateString":null,"sectionNames":["house","room","corner"]}"""
	val complexJson = """{"title":"Test submission","heTitle":"Test submission hebrew","titleVariants":["Test submission alternate title 1"],"heTitleVariants":["Test submission alternate title 1 hebrew"],"categories":["The Gilded Age"],"heCategories":["The Gilded Age hebrew"],"order":null,"authors":null,"enDesc":null,"heDesc":null,"enShortDesc":null,"heShortDesc":null,"hasErrorMargin":null,"compDate":null,"is_cited":null,"compDateString":null,"schema":[{"nodes":[{"nodeType":"JaggedArrayNode","depth":1,"addressTypes":["Integer"],"sectionNames":["section"],"titles":[{"text":"title","lang":"en","primary":true},{"text":"title HEBREW","lang":"he","primary":true}],"heSectionNames":["section HEBREW"],"title":"title","heTitle":"title HEBREW","key":"title"}],"titles":[{"text":"section-title","lang":"en","primary":true},{"text":"section-title HEBREW","lang":"he","primary":true}],"title":"section-title","heTitle":"section-title HEBREW","key":"section-title"},{"nodes":[{"nodeType":"JaggedArrayNode","depth":1,"addressTypes":["Integer"],"sectionNames":["paragraph"],"titles":[{"text":"second-title","lang":"en","primary":true},{"text":"second-title HEBREW","lang":"he","primary":true}],"heSectionNames":["pragraph HEBREW"],"title":"second-title","heTitle":"second-title HEBREW","key":"second-title"},{"nodeType":"JaggedArrayNode","depth":1,"addressTypes":["Integer","Integer"],"sectionNames":["chapter","paragraph"],"titles":[{"text":"third-title","lang":"en","primary":true},{"text":"third-title HEBREW","lang":"he","primary":true}],"heSectionNames":["chapter HEBREW","paragraph HEBREW"],"title":"third-title","heTitle":"third-title HEBREW","key":"third-title"}],"titles":[{"text":"second-section-title","lang":"en","primary":true},{"text":"second-section-title HEBREW","lang":"he","primary":true}],"title":"second-section-title","heTitle":"second-section-title HEBREW","key":"second-section-title"},{"nodeType":"JaggedArrayNode","depth":1,"addressTypes":["Integer"],"sectionNames":["book","chapter","verse"],"titles":[{"text":"fourth-title","lang":"en","primary":true},{"text":"fourth-title HEBREW","lang":"he","primary":true}],"heSectionNames":["book HEBREW","chapter HEBREW","verse HEBREW"],"title":"fourth-title","heTitle":"fourth-title HEBREW","key":"fourth-title"}]}"""

	override def spec = suite("SefariaIndexEntry")(
		suite("serialization")(
			test("should serialize a simple entry") {
				val json = simpleEntry.encodeJson
				assertTrue(
					json == simpleJson,
				)
			},
			test("should serialize a complex entry") {
				val json = complexEntry.encodeJson
				assertTrue(
					json == complexJson,
				)
			}
		),
		suite("deserialization")(
			test("should deserialize a simple entry") {
				for {
					entry <- simpleJson.decodeJson[SefariaIndexEntry]
				} yield assertTrue(
					entry == simpleEntry,
				)
			},
			test("should deserialize a complex entry") {
				for {
					entry <- complexJson.decodeJson[SefariaIndexEntry]
				} yield assertTrue(
					entry == complexEntry,
				)
			},
		),
	)