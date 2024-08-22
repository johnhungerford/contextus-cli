package contextus.model.contextus

import contextus.model.types.NonEmptyList
import contextus.model.sefaria.{SefariaAddressType, SefariaIndexEntry, SefariaRef, SefariaSchemaNode, SefariaText, SefariaTextSubmissionMap, SefariaTitle}
import cats.syntax.all.*

object ContextusDocConversion:
	/**
	 * Generates a [[SefariaIndexEntry]] from a [[ContextusDoc]] that can be used
	 * to add an entry to the Contextus index (but not text)
	 * 
	 * @param document [[ContextusDoc]] from which to generate a [[SefariaIndexEntry]]
	 * @return an index entry if conversion is successful, or an error message if unsuccessful.
	 *         Since [[ContextusDoc]] is more precisely typed than [[SefariaIndexEntry]],
	 *         this should never fail.
	 */
	def contextusDocToSefariaIndexEntry(
		document: ContextusDoc,
	): Either[String, SefariaIndexEntry] =
		for
			schemaOrSections <- document.content match
				case simple: SimpleDocContent =>
					Right(Right(simple.levels))
				case complex: NonEmptyList[ComplexDocContent] @unchecked =>
					complex.map(c => complexDocContentToSefariaSchemaNode(c))
						.sequence
						.map(nodes => Left(SefariaSchemaNode.Section(
							nodes = nodes,
							titles = List(SefariaTitle(document.title, "en", Some(true)), SefariaTitle(asHebrew(document.title), "he", Some(true))),
							title = document.title,
							heTitle = asHebrew(document.title),
							key = document.title,
						)))
		yield
			SefariaIndexEntry(
				title = document.title,
				heTitle = asHebrew(document.title),
				titleVariants = Nil,
				heTitleVariants = Nil,
				categories = document.category,
				heCategories = Nil,
				enDesc = document.description,
				enShortDesc = document.shortDescription,
				compDate = document.compositionYear.map(_ :: Nil),
				schemaOrSections = schemaOrSections,
			)

	/**
	 * Generates a [[SefariaTextSubmissionMap]] from a [[ContextusDoc]] that can be used
	 * to submit text for a document that has already been indexed.
	 * 
	 * @param document [[ContextusDoc]] document
	 * @return [[SefariaTextSubmissionMap]] containing all text submissions
	 */
	def contextusDocToSefariaTextSubmissionMap(
		document: ContextusDoc,
	): SefariaTextSubmissionMap =
		SefariaTextSubmissionMap(
			versionTitle = document.version.title,
			versionSource = document.version.source,
			language = document.version.language.getOrElse("en"),
			map = contextusDocContentToTextMap(document.title, document.content),
		)

	def asHebrew(engl: String): String =
		engl + " HEBREW"

	private def complexDocContentToSefariaSchemaNode(
		content: ComplexDocContent,
	): Either[String, SefariaSchemaNode] =
		content match
			case ComplexDocContent.Content(title, levels, sections) =>
				Right(SefariaSchemaNode.Text(
					title = title,
					titles = List(SefariaTitle(title, "en", Some(true)), SefariaTitle(asHebrew(title), "he", Some(true))),
					heTitle = asHebrew(title),
					depth = levels.length,
					addressTypes = levels.map(_ => SefariaAddressType.Integer),
					sectionNames = levels,
					heSectionNames = levels.map(asHebrew),
					key = title,
				))
			case ComplexDocContent.ContentList(title, content) =>
				for
					nodes <- content.map(complexDocContentToSefariaSchemaNode).sequence
				yield
					SefariaSchemaNode.Section(
						nodes = nodes,
						titles = List(SefariaTitle(title, "en", Some(true)), SefariaTitle(asHebrew(title), "he", Some(true))),
						title = title,
						heTitle = asHebrew(title),
						key = title,
					)

	private def contextusDocContentToTextMap(
		title: Title,
		content: NonEmptyList[ComplexDocContent] | SimpleDocContent,
	): List[(SefariaRef, SefariaText)] =
		val titleRef = SefariaRef(title)
		content match
			case simple: SimpleDocContent =>
				List(titleRef -> contentSectionsToSefariaText(simple.sections))
			case complex: NonEmptyList[ComplexDocContent] @unchecked =>
				complex.flatMap(complexDocContentToTextMapTuples(titleRef))

	private def complexDocContentToTextMapTuples(parentRef: SefariaRef)(complexDocContent: ComplexDocContent): List[(SefariaRef, SefariaText)] =
		complexDocContent match
			case ComplexDocContent.Content(title, levels, sections) =>
				List((parentRef / title) -> contentSectionsToSefariaText(sections))
			case ComplexDocContent.ContentList(title, content) =>
				content.flatMap(complexDocContentToTextMapTuples(parentRef / title))

	private def contentSectionsToSefariaText(sections: List[ContentSection]): SefariaText =
		SefariaText(sections.map(contentSectionToText))

	private def contentSectionToText(contentSection: ContentSection): String | SefariaText =
		contentSection match
			case ContentSection.Text(text) => text
			case ContentSection.Nested(subSections) =>
				contentSectionsToSefariaText(subSections)

