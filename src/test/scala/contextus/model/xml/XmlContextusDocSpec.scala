package contextus.model.xml

import contextus.model.xml.*
import contextus.phobos.PhobosZIO.*
import zio.*
import zio.test.*

import java.nio.file.Files
import scala.io.Source

object XmlContextusDocSpec extends ZIOSpecDefault:
	val basicDoc =
		XmlContextusDoc(
			Some("Test Title"),
			None,
			Some("Test Author"),
			None,
			None,
			None,
			Some(""),
			Some(Schema(
				List("book", "chapter", "paragraph"),
				ParagraphBreak.LineBreak,
			)),
			Some(Version(
				Some("Original version"),
				Some("http://johnhungerford.github.io"),
				None,
			)),
			Some(Body(
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
		))

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
					doc.title.contains("Test Title")
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
	)