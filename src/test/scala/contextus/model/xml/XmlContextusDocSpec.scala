package contextus.model.xml

import contextus.model.xml.*
import contextus.phobos.PhobosZIO.*
import contextus.validation.Validation
import zio.*
import zio.test.*

import java.nio.file.Files
import scala.io.Source

object XmlContextusDocSpec extends ZIOSpecDefault:
	val basicDoc =
		XmlContextusDoc(
			"Test Title",
			None,
			"Test Author",
			None,
			None,
			None,
			"",
			Schema(
				List("book", "chapter", "paragraph"),
				ParagraphBreak.LineBreak,
			),
			Version(
				"Original version",
				"http://johnhungerford.github.io",
				None,
			),
			Body(
				List(
					Section(
						None,
						Some("book"),
						List(
							Section(
								None,
								Some("chapter"),
								List(),
								"text of book 1, chapter 1, paragraph 1\ntext of book 1, chapter 1, paragraph 2"
							),
							Section(
								None,
								Some("chapter"),
								List(),
								"text of book 1, chapter 2, paragraph 1\ntext of book 1, chapter 2, paragraph 2"
							),
						),
						""
					),
					Section(
						None,
						Some("book"),
						List(
							Section(
								None,
								Some("chapter"),
								List(),
								"text of book 2, chapter 1, paragraph 1\ntext of book 2, chapter 1, paragraph 2"
							),
							Section(
								None,
								Some("chapter"),
								List(),
								"text of book 2, chapter 2, paragraph 1\ntext of book 2, chapter 2, paragraph 2"
							),
						),
						""
					)
				),
				"",
			)
		)

	override def spec = suite("ContextusDoc")(
		suite("serialization/deserialization")(
			test("should serialize document as expected") {
				for {
					text <- basicDoc.toXmlZIO
					_ <- ZIO.logInfo(text)
				} yield assertTrue(
					text.nonEmpty
				)
			},
			test("should deserialize document as expected") {
				val text =
					"""<document><title>Test Title</title><author>Test Author</author><category></category><schema paragraphBreak="line-break"><level>book</level><level>chapter</level><level>paragraph</level></schema><version><title>Original version</title><source>http://johnhungerford.github.io</source></version><body><section level="book"><title>Book 1</title><section level="chapter">text of book 1, chapter 1, paragraph 1
					  |text of book 1, chapter 1, paragraph 2</section><section level="chapter">text of book 1, chapter 2, paragraph 1
					  |text of book 1, chapter 2, paragraph 2</section></section><section level="book"><title>Book 2</title><section level="chapter">text of book 2, chapter 1, paragraph 1
					  |text of book 2, chapter 1, paragraph 2"</section><section level="chapter">text of book 2, chapter 2, paragraph 1
					  |text of book 2, chapter 2, paragraph 2"</section></section></body></document>""".stripMargin
				for {
					doc <- text.decodeXmlZIO[XmlContextusDoc]
					_ <- ZIO.logInfo(doc.toString)
				} yield assertTrue(
					doc.title == "Test Title"
				)
			},
			test("should deserialize from file") {
				for {
					xmlStr <- ZIO.attempt(Source.fromResource("test-document.xml").mkString)
					doc <- xmlStr.decodeXmlZIO[XmlContextusDoc]
					_ <- ZIO.logInfo(doc.toString)
				} yield assertTrue(
					true,
				)
			}
//			test("should serialize and deserialize properly") {
//				for {
//					docXml <- basicDoc.toXmlZIO
//					doc <- docXml.decodeXmlZIO[ContextusDoc]
//				} yield assertTrue(
//					doc == basicDoc,
//				)
//			},
		),
		suite("validation")(
			test("a valid document with sections specified by line breaks should be valid") {
				val validationResult = Validation.validate(basicDoc)
				assertTrue(validationResult.isEmpty)
			},
			test("a valid document without sections specified by line breaks should be valide") {
				val doc = basicDoc.copy(schema = Schema(
					levels = basicDoc.schema.levels.take(2),
					paragraphBreak = ParagraphBreak.IgnoreParagraphs,
				))
				val validationResult = Validation.validate(doc)
				assertTrue(validationResult.isEmpty)
			},
			test("a document with duplicate levels should be invalid") {
				val doc = basicDoc.copy(schema = Schema(levels = List("a", "b", "a"), paragraphBreak = ParagraphBreak.LineBreak))
				val validationResult = Validation.validate(doc)
				assertTrue(
					validationResult.nonEmpty,
					validationResult.exists(_.identifier == "ContextusDoc"),
					validationResult.exists(_.underlyingErrors.exists(_.identifier == "Schema")),
				)
			},
			test("a document without the defined levels should be invalid when parseParagraph is not IgnoreParagraphs") {
				val doc1 = basicDoc.copy(schema = Schema(levels = List("a", "b"), paragraphBreak = ParagraphBreak.LineBreak))
				val doc2 = doc1.copy(schema = doc1.schema.copy(paragraphBreak = ParagraphBreak.MultiLineBreak))
				val doc3 = doc1.copy(schema = doc1.schema.copy(paragraphBreak = ParagraphBreak.MultiLineBreakRemoveSingleLineBreaks))
				val validationResult1 = Validation.validate(doc1)
				val validationResult2 = Validation.validate(doc2)
				val validationResult3 = Validation.validate(doc3)
				assertTrue(
					validationResult1.map(_.copy(serializedValue = None)) == validationResult2.map(_.copy(serializedValue = None)),
					validationResult1.map(_.copy(serializedValue = None)) == validationResult3.map(_.copy(serializedValue = None)),
					validationResult1.nonEmpty,
					validationResult1.exists(_.identifier == "ContextusDoc"),
					validationResult1.exists(_.underlyingErrors.isEmpty),
				)
			},
			test("a document without the defined levels should be invalid when parseParagraph = IgnoreParagraphs") {
				val doc = basicDoc.copy(schema = Schema(levels = List("a"), paragraphBreak = ParagraphBreak.IgnoreParagraphs))
				val validationResult = Validation.validate(doc)
				assertTrue(
					validationResult.nonEmpty,
					validationResult.exists(_.identifier == "ContextusDoc"),
					validationResult.exists(_.underlyingErrors.isEmpty),
				)
			},
			test("a document with a section level that doesn't match the corresponding schema level should be invalid") {
				val doc = basicDoc.copy(schema = basicDoc.schema.copy(levels = List("book", "derp", "paragraph")))
				val validationResult = Validation.validate(doc)
				println(validationResult)
				assertTrue(
					validationResult.nonEmpty,
					validationResult.exists(_.identifier == "ContextusDoc"),
					validationResult.exists(_.underlyingErrors.isEmpty),
				)
			},
		),
	)