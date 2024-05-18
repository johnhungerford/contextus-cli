package contextus.util

import zio.test.*

object HandleIndentsSpec extends ZIOSpecDefault:
	override def spec = suite("HandleIndents")(
		test("should detect common indent and strip on each line") {
			val text =
				"""   This is line 1
				  |   This is line 2
				  |   This is line 3""".stripMargin
			val handled = HandleIndents(text)
			assertTrue(
				handled ==
				  """This is line 1
					|This is line 2
					|This is line 3""".stripMargin
			)
		},
		test("should detect common indent with different whitespaces and strip on each line") {
			val text =
				"""	 	This is line 1
				  |	 	This is line 2
				  |	 	This is line 3""".stripMargin
			val handled = HandleIndents(text)
			assertTrue(
				handled ==
				  """This is line 1
					|This is line 2
					|This is line 3""".stripMargin
			)
		},
		test("should detect least common indent and strip on each line") {
			val text =
				"""	 	This is line 1
				  |	 	   This is line 2
				  |	 	This is line 3""".stripMargin
			val handled = HandleIndents(text)
			assertTrue(
				handled ==
				  """This is line 1
					|   This is line 2
					|This is line 3""".stripMargin
			)
		},
		test("should ignore empty lines") {
			val text =
				"""	 	This is line 1
				  |
				  |	 	This is line 2
				  |
				  |	 	This is line 3""".stripMargin
			val handled = HandleIndents(text)
			assertTrue(
				handled ==
				  """This is line 1
					|
					|This is line 2
					|
					|This is line 3""".stripMargin
			)
		},
	)