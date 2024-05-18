//package contextus.phobos
//
//import zio.*
//import zio.test.*
//import ru.tinkoff.phobos.syntax.*
//import ru.tinkoff.phobos.derivation.semiauto.*
//import ru.tinkoff.phobos.encoding.*
//import ru.tinkoff.phobos.decoding.*
//import PhobosZIO.*
//
//object XmlSpec extends ZIOSpecDefault:
//
//	case class TestXml(element: String)
//	object TestXml:
//		given XmlDecoder[TestXml] = deriveXmlDecoder("test")
//		given XmlEncoder[TestXml] = deriveXmlEncoder("test")
//
//
//	override def spec = suite("Xml parsing")(
//		test("get tags as string in parsed in element") {
//			val string = """<test><element>asfdsad\<b>hello</b>asdfsadf</element></test>"""
//			for {
//				testXml <- string.decodeXmlZIO[TestXml]
//			} yield assertTrue(
//				testXml == TestXml("<b>hello</b>"),
//			)
//		}
//	)
