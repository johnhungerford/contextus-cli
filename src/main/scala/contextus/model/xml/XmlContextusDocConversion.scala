package contextus.model.xml

import contextus.model.contextus.{Author, Category, ComplexDocContent, ContentSection, ContextusDoc, Description, DocVersion, ShortDescription, SimpleDocContent, Title, Year}
import cats.syntax.all.*
import contextus.model.types.NonEmptyList

object XmlContextusDocConversion:
	def convert(xmlDoc: XmlContextusDoc): Either[String, ContextusDoc] = for {
		title <- Title.parse(xmlDoc.title)
		author <- Author.parse(xmlDoc.author)
		parsedCategories = xmlDoc.category.split(",").toList.map(v => Category.parse(v.trim))
		categoryList <- parsedCategories.sequence
		category <- NonEmptyList.parse(categoryList)
		compositionYear <- xmlDoc.publicationYear.map(Year.parse).sequence
		description <- xmlDoc.description.map(Description.parse).sequence
		shortDescription <- xmlDoc.shortDescription.map(ShortDescription.parse).sequence
		content <- convertBody(title, xmlDoc.body, xmlDoc.schema)
	} yield ContextusDoc(
		title = title,
		author = author,
		category = category,
		description = description,
		shortDescription = shortDescription,
		compositionYear = compositionYear,
		version = DocVersion(xmlDoc.version.title, xmlDoc.version.source, xmlDoc.version.language),
		content = content,
	)

	private def convertBody(
		title: Title,
		body: Body,
		schema: Schema,
	): Either[String, NonEmptyList[ComplexDocContent] | SimpleDocContent] =
		(body.sections, body.rawText.trim) match
			case (Nil, "") =>
				Left("Document body is empty: add text or sections (sections cannot be empty)")
			case (sections, "") =>
				convertTopLevelSections(title, sections, schema)
			case (_ :: _, _) =>
				Left("Document body has both text and sections: must have one or the other but not both")
			case (Nil, txt) =>
				schema.levels match
					case Nil => Left("Schema for a simple document has no levels: must include at least one level in your document schema for a simple document")
					case level :: Nil =>
						val section = Section(None, Some(level), Nil, txt)
						convertTopLevelSections(title, List(section), schema)
					case _ =>
						Left("Simple document with only one level of sections has schema with multiple levels: the number of levels in the schema must match the number of levels in the document")

	private def convertTopLevelSections(
		title: String,
		sections: List[Section],
		schema: Schema,
	): Either[String, NonEmptyList[ComplexDocContent] | SimpleDocContent] =
		if sections.isEmpty then
			Left("Document body is empty: add text or sections (sections cannot be empty)")
		else if sections.forall(v => v.title.isEmpty && v.schema.isEmpty) then
			convertSimpleSections(sections, schema)
		else if sections.forall(v => v.title.nonEmpty) then
			convertComplexSections(title, sections, schema).map {
				case content: ComplexDocContent.Content =>
					NonEmptyList(content)
				case nested: ComplexDocContent.ContentList =>
					nested.content
			}
		else Left("Document has some sections with titles and some sections without titles in the same level: must include titles for all sections within each level")

	private def convertSimpleSections(
		sections: List[Section],
		schema: Schema,
	): Either[String, SimpleDocContent] = for {
		levels <- NonEmptyList.parse(schema.levels)
			.left.map(_ => "Schema provided for simple document has no levels: document must have one or more levels")
		simpleDocSections <- sections.map(s => convertSimpleDocSection(s, schema)).sequence
		nonEmptySimpleDocSections <- NonEmptyList.parse(simpleDocSections)
			.left.map(_ => "Simple document has no sections: must provide at least one section at each level defined in schema")
	} yield SimpleDocContent(
		levels,
		nonEmptySimpleDocSections,
	)

	private def convertSimpleDocSection(
		section: Section,
		schema: Schema,
	): Either[String, ContentSection] =
		if section.title.nonEmpty then
			Left("Simple document section includes a title: simple documents cannot have titles within sections")
		else if section.schema.nonEmpty then
			Left("Simple document section includes a schema: simple documents cannot have schemas within sections")
		else if section.rawText.trim.nonEmpty && section.subSections.nonEmpty then
			Left("Section contains text and subsections: can only contain one or the other")
		else if section.rawText.trim.isEmpty && section.subSections.isEmpty then
			Left("Section is empty: each section must contain text or subsections")
		else if section.level.isEmpty then
			Left("Simple document section lacks a specified level: all sections that do not have a title must specify a level consistent with its schema.")
		else schema.popLevel match
			case None => Left("Section is empty: each section must contain text or subsections")
			case Some((level, _)) if !section.level.contains(level) =>
				Left(s"Section labeled with level ${section.level} corresponds to the level $level as defined in the schema.")
			case Some((_, nextSchema)) =>
				// Handle sub-sections (we've already established it does not have both subsections and text)
				if section.subSections.nonEmpty then for {
					convertedSubsections <- section.subSections.map(s => convertSimpleDocSection(s, nextSchema)).sequence
					nonEmptySections <- NonEmptyList.parse(convertedSubsections)
						.left.map(_ => "Unexpected error: multiple sections were converted and resulted in no sections. Contact your Contextus maintainer.")
				} yield ContentSection.Nested(nonEmptySections)

				// Handle text (we've already established it does not have both subsections and text)
				else section.parsedText(nextSchema.paragraphBreak, nextSchema.indent) match
					case None =>
						Left(s"Unexpected error: parsed text was empty after being validated as non-empty. Contact your maintainer.")
					case Some(textValue: String) =>
						if nextSchema.levels.nonEmpty then
							Left("Number of levels defined in schemas exceeds the section depth.")
						else Right(ContentSection.Text(textValue))

					case Some(textSections: List[String]) =>
						if nextSchema.levels.isEmpty then
							Left("Section depth exceeds the number of level defined in the schema")
						else if nextSchema.levels.length > 1 then
							Left("Number of levels defined in schemas exceeds the section depth.")
						else for {
							nonEmptySections <- NonEmptyList.parse(textSections.map(ContentSection.Text.apply))
								.left.map(_ => s"Unexpected error: document body is found to be empty after prior validation. Contact the maintainer.")
						} yield ContentSection.Nested(nonEmptySections)


	private def convertComplexSections(
		title: String,
		sections: List[Section],
		schema: Schema,
	): Either[String, ComplexDocContent] =
		if sections.isEmpty then
			Left("Section is empty: add text or sections (sections cannot be empty)")
		else if sections.forall(v => v.title.isEmpty) then
			val convertedSectionsEither = sections.map { section =>
				val innerSchema = section.schema.getOrElse(schema)
				convertSimpleDocSection(section, innerSchema)
			}
			for
				convertedSections <- convertedSectionsEither.sequence
				nonEmptyConvertedSections <- NonEmptyList.parse(convertedSections)
					.left.map(_ => "Unexpected error: complex document section was found to be empty after validating that it was non-empty. Contact your maintainer.")
				nonEmptyLevels <- NonEmptyList.parse(schema.levels)
					.left.map(_ => s"Schema for document ($title) has no levels defined.")
				typedTitle <- Title.parse(title).left.map(_ + s" ($title)")
			yield ComplexDocContent.Content(typedTitle, nonEmptyLevels, nonEmptyConvertedSections)
		else if sections.forall(v => v.title.nonEmpty) then
			val convertedSectionsEither = sections.map { section =>
				val innerSchema = section.schema.getOrElse(schema)
				convertComplexDocSection(section, innerSchema)
			}
			for
				convertedSections <- convertedSectionsEither.sequence
				nonEmptyConvertedSections <- NonEmptyList.parse(convertedSections)
					.left.map(_ => "Unexpected error: complex document section was found to be empty after validating that it was non-empty. Contact your maintainer.")
			yield ComplexDocContent.ContentList(title, nonEmptyConvertedSections)
		else Left("Document has some sections with titles and some sections without titles in the same level: must include titles for all sections within each level")

	private def convertComplexDocSection(
		section: Section,
		schema: Schema,
	): Either[String, ComplexDocContent] =
		section.title match
			case None => Left(s"Unexpected error: a section was found not have a title after being validated as having a title. Contact your maintainer.")
			case Some(title) =>
				val innerSchema = section.schema.getOrElse(schema)
				if section.textIsEmpty && section.subSections.isEmpty then
					Left(s"Inner document ($title) has neither text nor subsections: all sections must have text or subsections")
				else if !section.textIsEmpty && section.subSections.nonEmpty then
					Left(s"Inner document ($title) has both text and subsections: each section can have text or subsections, but not both")
				// If contains subsections
				else if section.subSections.nonEmpty then
					for
						convertedSections <- convertComplexSections(title, section.subSections, innerSchema)
					yield convertedSections

				// If contains text
				else innerSchema.popLevel match
					case None => Left(s"No levels defined for nested document ($title). All documents must have schema defined with at least one level.")
					case Some(_, nextSchema) if nextSchema.levels.nonEmpty =>
						Left(s"Number of levels defined in schema exceed the depth of sections for nested document $title.")
					case Some(_, _) => section.parsedText(innerSchema.paragraphBreak, innerSchema.indent) match
						case None =>
							Left(s"Unexpected error: nested document ($title) was found to have no parsed text after having been validated as having text. Contact your maintainer.")
						case Some(textValue: String) =>
							for
								levels <- NonEmptyList.parse(innerSchema.levels).left.map(_ => s"Schema for document $title has no levels defined")
								typedTitle <- Title.parse(title).left.map(_ + s" ($title)")
							yield ComplexDocContent.Content(
								typedTitle,
								levels,
								NonEmptyList(ContentSection.Text(textValue)),
							)

						case Some(textSections: List[String]) =>
							for {
								nonEmptySections <- NonEmptyList.parse(textSections.map(ContentSection.Text.apply))
									.left.map(_ => s"Unexpected error: document body is found to be empty after prior validation. Contact the maintainer.")
								nonEmptyLevels <- NonEmptyList.parse(innerSchema.levels)
									.left.map(_ => s"Schema for document $title has no levels defined.")
								typedTitle <- Title.parse(title).left.map(_ + s" ($title)")
							} yield ComplexDocContent.Content(
								typedTitle,
								nonEmptyLevels,
								nonEmptySections,
							)
