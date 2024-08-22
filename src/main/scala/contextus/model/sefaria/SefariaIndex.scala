package contextus.model.sefaria

import contextus.json.*

import scala.util.Random


/**
 * Models Sefaria's index tree. DAO for Sefaria index endpoint json response.
 * 
 * @param contents index tree in form of a list of [[SefariaIndexNode]] nodes
 */
final case class SefariaIndex(
	contents: List[SefariaIndexNode]
):
	def categories: List[SefariaCategory] =
		contents.flatMap(_.categoryOption.toList)

	def titles(categories: List[String] = Nil): List[String] =
		contents.flatMap(_.titles(categories, None))

object SefariaIndex:
	given Encoder[SefariaIndex] = Encoder.encodeList[SefariaIndexNode]
		.contramap[SefariaIndex](_.contents)
	given Decoder[SefariaIndex] = Decoder.decodeList[SefariaIndexNode]
		.map(SefariaIndex.apply)


sealed trait SefariaIndexNode:
	self =>
		def categoryOption: Option[SefariaCategory] = self match
			case _: SefariaIndexNode.DocumentNode => None
			case cat: SefariaIndexNode.CategoryNode =>
				cat.category.map(c => SefariaCategory(
					name = c,
					categories = cat.contents.toList.flatMap(_.flatMap(_.categoryOption.toList)),
				))

		def titles(categories: List[String] = Nil, parentCategory: Option[String] = None): List[String] = self match
			case doc: SefariaIndexNode.DocumentNode =>
				if categories.isEmpty || parentCategory.exists(categories.contains) then
					List(doc.title)
				else Nil
			case cat: SefariaIndexNode.CategoryNode =>
				val nextParentCategory = categories.find(cat.category.contains)
				cat.contents.toList.flatMap(_.flatMap(_.titles(categories, nextParentCategory)))

object SefariaIndexNode:
	final case class DocumentNode(
		categories: List[String],
//		order: Option[Int],
		primary_category: String,
		enShortDesc: Option[String],
		heShortDesc: Option[String],
		corpus: Option[String],
		heTitle: Option[String],
		title: String,
	) extends SefariaIndexNode

	object DocumentNode:
		given Encoder[DocumentNode] = deriveEncoder
		given Decoder[DocumentNode] = deriveDecoder

	final case class CategoryNode(
		contents: Option[List[SefariaIndexNode]],
//		order: Option[Int],
		enComplete: Option[Boolean],
		heComplete: Option[Boolean],
		enDesc: Option[String],
		heDesc: Option[String],
		enShortDesc: Option[String],
		heShortDesc: Option[String],
		heCategory: Option[String],
		category: Option[String],
	) extends SefariaIndexNode

	object CategoryNode:
		given Encoder[CategoryNode] = deriveEncoder
		given Decoder[CategoryNode] = deriveDecoder


	given Encoder[SefariaIndexNode] = encodeAny {
		case _: DocumentNode => Encoder[DocumentNode]
		case _: CategoryNode => Encoder[CategoryNode]
	}

	given Decoder[SefariaIndexNode] = decodeAny(
		Decoder[DocumentNode].wide,
		Decoder[CategoryNode].wide,
	)
