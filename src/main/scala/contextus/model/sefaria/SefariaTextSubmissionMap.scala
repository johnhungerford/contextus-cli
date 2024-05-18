package contextus.model.sefaria

import contextus.model.xml.{XmlContextusDoc, ParagraphBreak, ParseIndent, Schema, Section}

import scala.collection.immutable.ListMap

final case class SefariaRef private (
	private val segments: List[String | List[Int]]
):
	def /(segment: String | Int): SefariaRef =
		copy(segments = segment match {
			case str: String => str :: segments
			case i: Int => segments match
				case (is: List[Int] @unchecked) :: next => (i :: is) :: next
				case other => List(i) :: other
		})

	def refString: String =
		segments.reverse.map {
			case str: String => str
			case is: List[Int] => is.reverse.mkString(".")
		}.mkString(", ")


object SefariaRef:
	def apply(segment: String, otherSegments: (String | Int)*): SefariaRef =
		otherSegments.foldLeft(SefariaRef(List(segment)))(_ / _)

case class SefariaTextSubmissionMap(
	versionTitle: String,
	versionSource: String,
	language: String,
	map: List[(SefariaRef, SefariaText)]
):
	lazy val submissions: List[(String, SefariaTextSubmission)] =
		map.map {
			case (ref, text) =>
				ref.refString -> SefariaTextSubmission(
					versionTitle,
					versionSource,
					text,
					language,
				)
		}

object SefariaTextSubmissionMap:
	private def textsFromSections(parentRef: SefariaRef, defaultSchema: Schema, sections: List[Section]): List[(SefariaRef, SefariaText)] =
		if sections.forall(_.title.isEmpty) then
			val text = SefariaText.fromContextusSections(sections, defaultSchema.paragraphBreak, defaultSchema.indent)
			List((parentRef, text))
		else sections.flatMap { section =>
			val newRef = section.title.map(title => parentRef / title).getOrElse(parentRef)
			val schema = section.schema.getOrElse(defaultSchema)
			textsFromSections(newRef, schema, sections)
		}


	def fromContextusDoc(contextusDoc: XmlContextusDoc): SefariaTextSubmissionMap =
		SefariaTextSubmissionMap(
			contextusDoc.version.title,
			contextusDoc.version.source,
			contextusDoc.version.language.getOrElse("en"),
			textsFromSections(
				SefariaRef(contextusDoc.title),
				contextusDoc.schema,
				List(contextusDoc.body.textAsSection),
			)
		)
