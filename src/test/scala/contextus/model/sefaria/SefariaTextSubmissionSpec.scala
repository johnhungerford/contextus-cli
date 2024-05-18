package contextus.model.sefaria

import contextus.json.*
import contextus.model.xml.XmlContextusDoc
import contextus.phobos.PhobosZIO.*
import zio.*
import zio.test.*

import scala.io.Source

object SefariaTextSubmissionSpec extends ZIOSpecDefault:
	val simpleSubmission: SefariaTextSubmission =
		SefariaTextSubmission(
			"version-title",
			"version-source",
			SefariaTextSpec.simpleText,
			"en",
		)

	override def spec = suite("SefariaTextSubmission")(
		suite("serialization/deserialization")(
			test("should serialize correctly") {
				val submissionJson = simpleSubmission.encodeJson
				assertTrue(
					submissionJson == """{"versionTitle":"version-title","versionSource":"version-source","text":["hello",[["three","simple","sections"],"world"]],"language":"en"}""",
				)
			},
			test("should deserialize correctly") {
				val submissionJson = """{"versionTitle":"version-title","versionSource":"version-source","text":["hello",[["three","simple","sections"],"world"]],"language":"en"}"""
				for {
					submission <- submissionJson.decodeJson[SefariaTextSubmission]
				} yield assertTrue(
					submission == simpleSubmission,
				)
			},
		),
		suite("from contextus")(
			test("should get a SefariaTextSubmission from a contextus doc") {
				for {
					xmlStr <- ZIO.attempt(Source.fromResource("test-document.xml").mkString)
					doc <- xmlStr.decodeXmlZIO[XmlContextusDoc]
					submission = SefariaTextSubmission.fromContextusDoc(doc)
				} yield assertTrue(
					submission == SefariaTextSubmission(
						versionTitle = "Original version",
						versionSource = "http://johnhungerford.github.io",
						text = SefariaText(List(SefariaText(List(SefariaText(List("text of book 1, chapter 1, paragraph 1", "text of book 1, chapter 1, paragraph 2")), SefariaText(List("text of book 1, chapter 2, paragraph 1", "text of book 1, chapter 2, paragraph 2")))), SefariaText(List(SefariaText(List("text of book 2, chapter 1, paragraph 1", "text of book 2, chapter 1, paragraph 2")), SefariaText(List("text of book 2, chapter 2, paragraph 1", "text of book 2, chapter 2, paragraph 2")))))),
						language = "en",
					),
				)
			},
		)
	)
