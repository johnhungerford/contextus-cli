package contextus.model.sefaria

import scala.annotation.tailrec

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

	def validate(categories: List[SefariaCategory], categoryPath: List[String]): Option[(List[String], String)] =
		@tailrec
		def loop(currentPath: List[String], remainingPath: List[String], categories: List[SefariaCategory]): Option[(List[String], String)] =
			remainingPath match
				case Nil => None
				case cat :: nextRemainingPath =>
					categories.filter(_.name == cat) match
						case Nil => Some(currentPath -> cat)
						case nextCategories => loop(currentPath :+ cat, nextRemainingPath, nextCategories.flatMap(_.categories))

		loop(Nil, categoryPath, categories)

	def deduplicate(categories: List[SefariaCategory]): List[SefariaCategory] =
		categories.groupBy(_.name).map {
			case (name, categories) =>
				SefariaCategory(name, deduplicate(categories.flatMap(_.categories)))
		}.toList

	extension (categories: List[SefariaCategory])
		def deduplicated: List[SefariaCategory] = deduplicate(categories)

		def validated(categoryPath: List[String]): Option[(List[String], String)] =
			SefariaCategory.validate(categories, categoryPath)
