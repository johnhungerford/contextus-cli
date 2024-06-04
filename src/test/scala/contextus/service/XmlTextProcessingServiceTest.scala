package contextus.service

import contextus.model.contextus.{ContentSection, SimpleDocContent}
import contextus.model.xml.{XmlContextusDoc, XmlContextusDocConversion}
import zio.*
import zio.test.*

object XmlTextProcessingServiceTest extends ZIOSpecDefault:

	override def spec = suite("XmlTextProcessingService")(
		test("should encode and decode simple tags") {
			val text = "asdfasd <b>asdfa <em>sdf</em> asdf</b> alskdjfalsdkjf <sup>superscript</sup> asdfasd <u>all of this is underlined, <sub>including this subscripted part</sub> and <del>including this part which is struck out</del></u>"
			for
				processingService <- ZIO.service[XmlTextProcessingService]
				encodedText = processingService.processPreIngest(text)
				decodedText = processingService.processPostIngest(encodedText)
			yield
				assertTrue(
					!encodedText.contains("<"),
					!encodedText.contains(">"),
					decodedText == text,
				)
		},
		test("should encode and decode note tags") {
			val text = """asdfasd <note number="5">asdfa asdf</note>"""
			val expectedText =
				"""asdfasd <sup class="footnote-marker">5</sup><i class="footnote">asdfa asdf</i>"""
			for
				processingService <- ZIO.service[XmlTextProcessingService]
				encodedText = processingService.processPreIngest(text)
				decodedText = processingService.processPostIngest(encodedText)
			yield
				assertTrue(
					!encodedText.contains("<"),
					!encodedText.contains(">"),
					decodedText == expectedText,
				)
		},
		test("should provide end to end processing") {
			val docString =
				"""<document>
				  |	  <title>Test Title</title>
				  |   <author>Test Author</author>
				  |   <category>some/category</category>
				  |   <schema paragraphBreak="line-break">
				  |    	 <level>paragraph</level>
				  |   </schema>
				  |   <version>
				  |   	 <title>Original version</title>
				  |   	 <source>http://johnhungerford.github.io</source>
				  |   </version>
				  |   <body>
				  |      Lorem <em>ipsum</em> dolor sit amet, consectetur <note number="1">adipiscing elit, sed</note> do eiusmod <b>tempor incididunt ut labore et dolore</b> magna aliqua. Ut enim ad minim <note number="2">veniam, quis nostrud <b>exercitation</b> ullamco laboris nisi ut aliquip ex ea <em>commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum</em> dolore eu fugiat nulla</note> pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
				  |   </body>
				  |</document>""".stripMargin

			import contextus.phobos.PhobosZIO.*

			val preprocessedString = XmlTextProcessingService.Live.processPreIngest(docString)

			for
				xmlDoc <- preprocessedString.decodeXml[XmlContextusDoc]
				contextusDoc <- XmlContextusDocConversion(XmlTextProcessingService.Live)
				  .convert(xmlDoc)
				paragraph <- contextusDoc.content match
					case SimpleDocContent(_, sections) =>
						sections.safeLast match
							case ContentSection.Text(str) =>
								Right(str)
							case _ => Left("didn't find text")
					case _ => Left("didn't find simple doc content")
			yield
				assertTrue(
					paragraph == """Lorem <em>ipsum</em> dolor sit amet, consectetur <sup class="footnote-marker">1</sup><i class="footnote">adipiscing elit, sed</i> do eiusmod <b>tempor incididunt ut labore et dolore</b> magna aliqua. Ut enim ad minim <sup class="footnote-marker">2</sup><i class="footnote">veniam, quis nostrud <b>exercitation</b> ullamco laboris nisi ut aliquip ex ea <em>commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum</em> dolore eu fugiat nulla</i> pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.""",
				)
		}
	).provide(XmlTextProcessingService.live)