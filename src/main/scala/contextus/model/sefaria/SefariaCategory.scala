package contextus.model.sefaria

final case class SefariaCategory(
	name: String,
	categories: List[SefariaCategory]
):
	def allCategories: List[String] =
		name :: categories.flatMap(_.allCategories)
		
	def contains(categoryPath: List[String]): Boolean = categoryPath match
		case `name` :: next =>
			categories.exists(_.contains(next))
		case _ => false

	def get(innerName: String): Option[SefariaCategory] = categories.find(_.name == innerName)

	def find(name: String): Option[SefariaCategory] =
		get(name) match
			case Some(value) => Some(value)
			case None => categories.flatMap(_.find(name)).headOption


object SefariaCategory:

	def deduplicate(categories: List[SefariaCategory]): List[SefariaCategory] =
		categories.groupBy(_.name).map {
			case (name, categories) =>
				SefariaCategory(name, deduplicate(categories.flatMap(_.categories)))
		}.toList

	extension (categories: List[SefariaCategory])
		def deduplicated: List[SefariaCategory] = deduplicate(categories)
