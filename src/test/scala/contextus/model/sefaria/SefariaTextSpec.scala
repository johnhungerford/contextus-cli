package contextus.model.sefaria

import contextus.json.*
import contextus.model.xml.XmlContextusDoc
import contextus.model.sefaria.SefariaText
import contextus.phobos.PhobosZIO.*
import zio.*
import zio.test.*

import scala.io.Source

object SefariaTextSpec extends ZIOSpecDefault:
	val simpleText = SefariaText(List(
		"hello",
		SefariaText(List(
			SefariaText(List(
				"three",
				"simple",
				"sections"
			)),
			"world"
		)),
	))
	val simpleJson = """["hello",[["three","simple","sections"],"world"]]"""

	override def spec = suite("SefariaText")(
		suite("serialization/deserialization")(
			test("should serialize properly") {
				val json = simpleText.encodeJson
				assertTrue(
					json == simpleJson,
				)
			},
			test("should deserialize properly") {
				for {
					text <- simpleJson.decodeJson[SefariaText]
				} yield assertTrue(
					text == simpleText,
				)
			}
		),
		suite("from contextus") {
			test("should construct SefariaText from ContextusDoc") {
				for {
					xmlStr <- ZIO.attempt(Source.fromResource("test-document.xml").mkString)
					doc <- xmlStr.decodeXmlZIO[XmlContextusDoc]
					text = SefariaText.fromContextusDoc(doc)
				} yield assertTrue(
					text == SefariaText(List(SefariaText(List(SefariaText(List("text of book 1, chapter 1, paragraph 1", "text of book 1, chapter 1, paragraph 2")),SefariaText(List("text of book 1, chapter 2, paragraph 1", "text of book 1, chapter 2, paragraph 2")))), SefariaText(List(SefariaText(List("text of book 2, chapter 1, paragraph 1", "text of book 2, chapter 1, paragraph 2")),SefariaText(List("text of book 2, chapter 2, paragraph 1", "text of book 2, chapter 2, paragraph 2")))))),
				)
			}
		}
	)
