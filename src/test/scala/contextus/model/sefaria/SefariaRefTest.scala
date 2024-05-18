package contextus.model.sefaria

import zio.test.*

object SefariaRefTest extends ZIOSpecDefault:
	
	def spec = suite("SefariaRefTest")(
		test("should format") {
			val ref = SefariaRef("hello", "there", 1, 2)
			
			assertTrue(ref.refString == "hello there 1.2")
		} 
	)
	
