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


/**
 * Utility data structure for correlating [[SefariaText]] text sections with 
 * [[SefariaRef]] references for document submissions. This is used for submitting 
 * complex documents, where each sub-document text must be submitted separately 
 * using that sub-document's reference.
 * 
 * Should be sufficient for constructing all text submission requests for a single 
 * version of a single complex document.
 * 
 * Note: not a DAO. Can be used to construct requests, but is not itself a request.
 *
 * @param versionTitle title of the text version being submitted
 * @param versionSource source of the text version being submitted
 * @param language version language
 * @param map list of references to inner documents and text to submit for them.
 *            List instead of map to preserve submission order.
 */
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
