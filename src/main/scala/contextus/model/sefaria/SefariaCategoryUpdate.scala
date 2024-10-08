package contextus.model.sefaria

import contextus.model.types.NonEmptyList
import contextus.json.*
import contextus.model.contextus.ContextusDocConversion

/**
 * DAO for adding a category to Sefaria
 * @param path path from root ancestor category to parent category
 * @param titles name of category including both english and hebrew
 */
final case class SefariaCategoryUpdate(
	path: List[String],
	titles: List[SefariaTitle],
)

object SefariaCategoryUpdate:
	def newCategory(path: NonEmptyList[String]): SefariaCategoryUpdate =
		SefariaCategoryUpdate(
			path = path,
			titles = List(
				SefariaTitle(path.safeLast, "en", Some(true)),
				SefariaTitle(ContextusDocConversion.asHebrew(path.safeLast), "he", Some(true)),
			)
		)
	
	given Encoder[SefariaCategoryUpdate] = deriveEncoder
