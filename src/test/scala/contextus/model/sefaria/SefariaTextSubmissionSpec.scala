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
	)
