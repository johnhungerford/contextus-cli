package contextus.model.sefaria

import zio.*
import zio.test.*

import contextus.json.*

import scala.io.Source

object SefariaIndexTest extends ZIOSpecDefault:
	override def spec = suite("SefariaIndex")(
		suite("deserialization")(
			test("should deserialize index payload correctly") {
				for {
					jsonStr <- ZIO.attempt(Source.fromResource("test-index.json").mkString)
					index <- jsonStr.decodeJsonZIO[SefariaIndex]
				} yield assertTrue(
					index.contents.nonEmpty,
				)
			}
		)
	)
