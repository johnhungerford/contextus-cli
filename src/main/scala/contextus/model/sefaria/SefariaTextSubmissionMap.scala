package contextus.model.sefaria

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
