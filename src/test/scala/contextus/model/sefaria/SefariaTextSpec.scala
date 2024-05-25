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
	)
