package contextus.service

import zio.*

import scala.util.Try
import scala.xml.XML

/**
 * Service for processing text documents to allow functionality not currently 
 * supported by [[ru.tinkoff.phobos]]. Primarily used to escape XML used in text 
 * sections for formatting, and then unescaping them after converting to
 * [[contextus.model.xml.XmlContextusDoc]]
 */
trait XmlTextProcessingService:
	def processPreIngest(xmlText: String): String

	def processPostIngest(documentText: String): String

object XmlTextProcessingService:
	val live = ZLayer.succeed[XmlTextProcessingService](Live)

	object Live extends XmlTextProcessingService:
		val tags = Set("em", "b", "sup", "sub", "del", "u", "note")
		val tagsWithoutNote = tags - "note"

		private val openXmlPattern = s"""<((?:${tagsWithoutNote.mkString("|")})(?:\\s+.*?)?)>""".r
		private val closeXmlPattern = s"""</(${tags.mkString("|")}\\s*)>""".r

		private val openXmlNotePattern = s"""<note\\s*number\\s*=\\s*"(.+?)"\\s*>""".r

		private val xmlReplacements = List(
			openXmlPattern -> "[{[{$1}]}]",
			closeXmlPattern -> "{[{[$1]}]}",
			openXmlNotePattern -> """[{[{note number="$1"}]}]""",
			"""&amp;""".r -> """&""",
			"""&""".r -> """&amp;""",
		)

		override def processPreIngest(xmlText: String): String =
			val res = xmlReplacements.foldLeft(xmlText) {
				case (txt, (pattern, replacement)) =>
					pattern.replaceAllIn(txt, replacement)
			}
			res

		private val openContextusPattern = s"""\\[\\{\\[\\{((?:${tagsWithoutNote.mkString("|")})(?:\\s+.*?)?)\\}\\]\\}\\]""".r
		private val closeContextusPattern = s"""\\{\\[\\{\\[\\s*(${tagsWithoutNote.mkString("|")}\\s*)\\]\\}\\]\\}""".r

		private val openContextusNotePattern = s"""\\[\\{\\[\\{note\\s*number\\s*=\\s*"(.+?)"\\s*\\}\\]\\}\\]""".r
		private val closeContextusNotePattern = s"""\\{\\[\\{\\[note\\s*\\]\\}\\]\\}""".r


		private val contextusReplacements = List(
			openContextusPattern -> "<$1>",
			closeContextusPattern -> "</$1>",
			openContextusNotePattern -> """<sup class="footnote-marker">$1</sup><i class="footnote">""",
			closeContextusNotePattern -> """</i>""",
		)

		override def processPostIngest(documentText: String): String =
			contextusReplacements.foldLeft(documentText) {
				case (txt, (pattern, replacement)) =>
					pattern.replaceAllIn(txt, replacement)
//					Try(XML.loadString(s"<text-chunk-validation>replacedText</text-chunk-validation>"))
//						.toEither
//						.left
//						.map(msg => s"Invalid formatting XML. $msg")
			}

