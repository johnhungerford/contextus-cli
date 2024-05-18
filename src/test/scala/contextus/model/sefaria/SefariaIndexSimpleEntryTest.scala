package contextus.model.sefaria

import contextus.json.*
import contextus.model.xml.XmlContextusDoc
import contextus.model.sefaria.SefariaIndexSimpleEntry
import contextus.phobos.PhobosZIO.*
import zio.*
import zio.test.*

import scala.io.Source

object SefariaIndexSimpleEntryTest extends ZIOSpecDefault:
	val simpleIndexSubmission = SefariaIndexSimpleEntry(
		"Test submission",
		"Test submission hebrew",
		List("Test submission alternate title 1"),
		List("Test submission alternate title 1 hebrew"),
		List("The Gilded Age"),
		List("house", "room", "corner"),
		None,
	)
	val simpleIndexSubmissionJson =
		"""{"title":"Test submission","heTitle":"Test submission hebrew","titleVariants":["Test submission alternate title 1"],"heTitleVariants":["Test submission alternate title 1 hebrew"],"categories":["The Gilded Age"],"sectionNames":["house","room","corner"],"enDesc":null}"""

	override def spec = suite("SefariaIndexSubmission")(
		suite("serialization/deserialization")(
			test("should serialize as expected") {
				val jsonStr = simpleIndexSubmission.encodeJson
				assertTrue(jsonStr == simpleIndexSubmissionJson)
			},
			test("should deserialize as expected") {
				for {
					submission <- simpleIndexSubmissionJson.decodeJsonZIO[SefariaIndexSimpleEntry]
				} yield assertTrue(
					submission == simpleIndexSubmission
				)
			},
		),
		suite("from contextus doc")(
			test("should construct SefariaIndexSubmission from ContextusDoc") {
				for {
					xmlStr <- ZIO.attempt(Source.fromResource("test-document.xml").mkString)
					doc <- xmlStr.decodeXmlZIO[XmlContextusDoc]
					submission = SefariaIndexSimpleEntry.fromContextusDoc(doc)
				} yield assertTrue(
					submission == SefariaIndexSimpleEntry(
						"Test Title",
						"Test Title HEBREW",
						List("Test Alternate Title 1", "Test Alternate Title 2"),
						List("Test Alternate Title 1 HEBREW", "Test Alternate Title 2 HEBREW"),
						List("Test Category 1", "Test Category 2"),
						List("book", "chapter", "paragraph"),
						None,
					),
				)
			}
		)
	)
