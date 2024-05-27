package contextus.model.xml

import contextus.model.contextus.{Author, Category, ComplexDocContent, ContentSection, ContextusDoc, Description, DocVersion, ShortDescription, SimpleDocContent, Title, Year}
import cats.syntax.all.*
import contextus.model.types.NonEmptyList
import contextus.service.XmlTextProcessingService

final case class ConvError(message: String, location: List[String]):
	def within(loc: List[String]): ConvError = copy(location = location ++ loc)
	def within(loc: String): ConvError = within(loc :: Nil)
	
	override def toString: String = location.reverse.mkString(" -> ") + ":\n" + message

object ConvError:
	extension (msg: String)
		def asErrLoc(location: String, otherLocations: List[String] = Nil) =
			ConvError(msg, location :: otherLocations)
		def asErr() = ConvError(msg, Nil)
		

final case class XmlContextusDocConversion(textProcessingService: XmlTextProcessingService):
	import XmlContextusDocConversion.CATEGORY_SEPARATOR
	import ConvError.{asErr, asErrLoc}

	def convert(xmlDoc: XmlContextusDoc): Either[ConvError, ContextusDoc] = for {
		titleStr <- xmlDoc.title.toRight("Missing required <title> tag".asErrLoc("<document>"))
		title <- Title.parse(titleStr).left.map(_.asErrLoc("<title>", List("<document>")))
		authorStr <- xmlDoc.author.toRight("Missing required <author> tag".asErrLoc("<document>"))
		author <- Author.parse(authorStr).left.map(_.asErrLoc("<author>", List("<document>")))
		categoryStr <- xmlDoc.category.toRight("Missing required <category> tag".asErrLoc("<document>"))
		parsedCategories = categoryStr
		  .split(CATEGORY_SEPARATOR)
		  .toList
		  .zipWithIndex
		  .map((v, i) => Category.parse(v.trim).left.map(_.asErrLoc(s"#${i + 1}", List("<category>", "<document>"))))
		categoryList <- parsedCategories.sequence
		category <- NonEmptyList
		  .parse(categoryList)
		  .left
		  .map(_ => "<category> cannot be empty".asErrLoc("<document>"))
		compositionYear <- xmlDoc
		  .publicationYear.map(Year.parse).sequence
		  .left.map(_.asErrLoc("<composition-year>", List("<document>")))
		description <- xmlDoc
		  .description.map(Description.parse).sequence
		  .left.map(_.asErrLoc("<description>", List("<document>")))
		shortDescription <- xmlDoc
		  .shortDescription.map(ShortDescription.parse).sequence
		  .left.map(_.asErrLoc("<short-description>", List("<document>")))
		body <- xmlDoc.body.toRight("Missing required <body> tag".asErrLoc("<document>"))
		schema <- xmlDoc.schema.toRight("Missing required <schema> tag".asErrLoc("<document>"))
		version <- xmlDoc.version.toRight("Missing required <version> tag".asErrLoc("<document>"))
		versionTitle <- version.title.toRight("Missing required <title> tag".asErrLoc("<version>", List("<document>")))
		versionSource <- version.source.toRight("Missing required <source> tag".asErrLoc("<version>", List("<document>")))
		content <- convertBody(title, body, schema).left.map(_.within(List("<body>", "<document>")))
		_ <- NonEmptyList
		  .parse(schema.levels).left.map(_ => "Schema provided for simple document has no levels: document must have one or more levels".asErrLoc("<schema>", List("<document>")))
	} yield ContextusDoc(
		title = title,
		author = author,
		category = category,
		description = description,
		shortDescription = shortDescription,
		compositionYear = compositionYear,
		version = DocVersion(versionTitle, versionSource, version.language),
		content = content,
	)

	private def convertBody(
		title: Title,
		body: Body,
		schema: Schema,
	): Either[ConvError, NonEmptyList[ComplexDocContent] | SimpleDocContent] =
		(body.sections, body.rawText.trim) match
			case (Nil, "") =>
				Left("Document body is empty: add text or sections (sections cannot be empty)".asErr())
			case (sections, "") =>
				convertTopLevelSections(title, sections, schema)
			case (_ :: _, _) =>
				Left("Document body has both text and sections: must have one or the other but not both".asErr())
			case (Nil, txt) =>
				schema.levels match
					case Nil => Left("Schema for a simple document has no levels: must include at least one level in your document schema for a simple document".asErr())
					case level :: Nil =>
						body.parsedText(textProcessingService, schema.paragraphBreak, schema.indent) match
							case None =>
								Left(s"Unexpected error: parsed text was empty after being validated as non-empty. Contact your maintainer.".asErr())
							case Some(textValue: String) =>
								Right(SimpleDocContent(NonEmptyList(level), NonEmptyList(ContentSection.Text(textValue))))
							case Some(textSections: List[String]) =>
								for {
									nonEmptySections <- NonEmptyList
										.parse(textSections.map(t =>{
											ContentSection.Text(t)
										}))
										.left
										.map(_ => s"Unexpected error: document body is found to be empty after prior validation. Contact the maintainer.".asErr())
								} yield SimpleDocContent(NonEmptyList(level), nonEmptySections)
					case _ =>
						Left("Simple document with only one level of sections has schema with multiple levels: the number of levels in the schema must match the number of levels in the document".asErr())

	private def convertTopLevelSections(
		title: String,
		sections: List[Section],
		schema: Schema,
	): Either[ConvError, NonEmptyList[ComplexDocContent] | SimpleDocContent] =
		if sections.isEmpty then
			Left("Document body is empty: add text or sections (sections cannot be empty)".asErr())
		else if sections.forall(v => v.title.isEmpty && v.schema.isEmpty) then
			convertSimpleSections(sections, schema)
		else if sections.forall(v => v.title.nonEmpty) then
			convertComplexSections(title, sections, schema).map {
				case content: ComplexDocContent.Content =>
					NonEmptyList(content)
				case nested: ComplexDocContent.ContentList =>
					nested.content
			}
		else Left("Document has some sections with titles and some sections without titles in the same level: must include titles for all sections within each level".asErr())

	private def convertSimpleSections(
		sections: List[Section],
		schema: Schema,
	): Either[ConvError, SimpleDocContent] = for {
		levels <- NonEmptyList.parse(schema.levels)
			.left.map(_ => "Schema provided for simple document has no levels: document must have one or more levels".asErr())
		simpleDocSections <- sections.zipWithIndex.map((s, i) => convertSimpleDocSection(s, schema).left.map(_.within(List(s"#${i + 1}", s"<section${s.level.map(v => s" level=\"$v\"").getOrElse("")}>")))).sequence
		nonEmptySimpleDocSections <- NonEmptyList.parse(simpleDocSections)
			.left.map(_ => "Simple document has no sections: must provide at least one section at each level defined in schema".asErr())
	} yield SimpleDocContent(
		levels,
		nonEmptySimpleDocSections,
	)

	private def convertSimpleDocSection(
		section: Section,
		schema: Schema,
	): Either[ConvError, ContentSection] =
		if section.title.nonEmpty then
			Left("Simple document section includes a title: simple documents cannot have titles within sections".asErr())
		else if section.schema.nonEmpty then
			Left("Simple document section includes a schema: simple documents cannot have schemas within sections".asErr())
		else if section.rawText.trim.nonEmpty && section.subSections.nonEmpty then
			Left("Section contains text and subsections: can only contain one or the other".asErr())
		else if section.rawText.trim.isEmpty && section.subSections.isEmpty then
			Left("Section is empty: each section must contain text or subsections".asErr())
		else if section.level.isEmpty then
			Left("Simple document section lacks a specified level: all sections that do not have a title must specify a level consistent with its schema.".asErr())
		else schema.popLevel match
			case None => Left("Section is empty: each section must contain text or subsections".asErr())
			case Some((level, _)) if !section.level.contains(level) =>
				Left(s"Section labeled as level \"${section.level.getOrElse("!!ERR!!")}\" corresponds to the level \"$level\" as defined in the schema.".asErr())
			case Some((_, nextSchema)) =>
				// Handle sub-sections (we've already established it does not have both subsections and text)
				if section.subSections.nonEmpty then for {
					convertedSubsections <- section.subSections.zipWithIndex.map((s, i) => convertSimpleDocSection(s, nextSchema).left.map(_.within(List(s"#${i + 1}", s"<section${s.level.map(v => s" level=\"$v\"").getOrElse("")}>")))).sequence
					nonEmptySections <- NonEmptyList.parse(convertedSubsections)
						.left.map(_ => "Unexpected error: multiple sections were converted and resulted in no sections. Contact your Contextus maintainer.".asErr())
				} yield ContentSection.Nested(nonEmptySections)

				// Handle text (we've already established it does not have both subsections and text)
				else section.parsedText(textProcessingService, nextSchema.paragraphBreak, nextSchema.indent) match
					case None =>
						Left(s"Unexpected error: parsed text was empty after being validated as non-empty. Contact your maintainer.".asErr())
					case Some(textValue: String) =>
						if nextSchema.levels.nonEmpty then
							Left("Number of levels defined in schemas exceeds the section depth.".asErr())
						else Right(ContentSection.Text(textValue))

					case Some(textSections: List[String]) =>
						if nextSchema.levels.isEmpty then
							Left("Section depth exceeds the number of level defined in the schema".asErr())
						else if nextSchema.levels.length > 1 then
							Left("Number of levels defined in schemas exceeds the section depth.".asErr())
						else for {
							nonEmptySections <- NonEmptyList.parse(textSections.map(t => ContentSection.Text(t)))
								.left.map(_ => s"Unexpected error: document body is found to be empty after prior validation. Contact the maintainer.".asErr())
						} yield ContentSection.Nested(nonEmptySections)


	private def convertComplexSections(
		title: String,
		sections: List[Section],
		schema: Schema,
	): Either[ConvError, ComplexDocContent] =
		if sections.isEmpty then
			Left("Section is empty: add text or sections (sections cannot be empty)".asErr())
		else if sections.forall(v => v.title.isEmpty) then
			val convertedSectionsEither = sections.zipWithIndex.map { (section, i) =>
				val innerSchema = section.schema.getOrElse(schema)
				convertSimpleDocSection(section, innerSchema)
				  .left
				  .map(_.within(List(s"#${i + 1}", s"<section${section.level.map(v => s" level=\"$v\"").getOrElse("")}>")))
			}
			for
				convertedSections <- convertedSectionsEither.sequence
				nonEmptyConvertedSections <- NonEmptyList.parse(convertedSections)
					.left.map(_ => "Unexpected error: complex document section was found to be empty after validating that it was non-empty. Contact your maintainer.".asErr())
				nonEmptyLevels <- NonEmptyList.parse(schema.levels)
					.left.map(_ => s"Schema for document ($title) has no levels defined.".asErr())
				typedTitle <- Title.parse(title).left.map(v => (v + s" ($title)").asErrLoc("<title>"))
			yield ComplexDocContent.Content(typedTitle, nonEmptyLevels, nonEmptyConvertedSections)
		else if sections.forall(v => v.title.nonEmpty) then
			val convertedSectionsEither = sections.zipWithIndex.map { (section, i) =>
				val innerSchema = section.schema.getOrElse(schema)
				convertComplexDocSection(section, innerSchema)
				  .left
				  .map(_.within(List(s"#${i + 1}", s"<section${section.level.map(v => s" level=\"$v\"").getOrElse("")}>")))
			}
			for
				convertedSections <- convertedSectionsEither.sequence
				nonEmptyConvertedSections <- NonEmptyList.parse(convertedSections)
					.left.map(_ => "Unexpected error: complex document section was found to be empty after validating that it was non-empty. Contact your maintainer.".asErr())
			yield ComplexDocContent.ContentList(title, nonEmptyConvertedSections)
		else Left("Document has some sections with titles and some sections without titles in the same level: must include titles for all sections within each level".asErr())

	private def convertComplexDocSection(
		section: Section,
		schema: Schema,
	): Either[ConvError, ComplexDocContent] =
		section.title match
			case None => Left(s"Unexpected error: a section was found not have a title after being validated as having a title. Contact your maintainer.".asErr())
			case Some(title) =>
				val innerSchema = section.schema.getOrElse(schema)
				if section.textIsEmpty && section.subSections.isEmpty then
					Left(s"Inner document ($title) has neither text nor subsections: all sections must have text or subsections".asErr())
				else if !section.textIsEmpty && section.subSections.nonEmpty then
					Left(s"Inner document ($title) has both text and subsections: each section can have text or subsections, but not both".asErr())
				// If contains subsections
				else if section.subSections.nonEmpty then
					for
						convertedSections <- convertComplexSections(title, section.subSections, innerSchema)
					yield convertedSections

				// If contains text
				else innerSchema.popLevel match
					case None => Left(s"No levels defined for nested document ($title). All documents must have schema defined with at least one level.".asErrLoc("<schema>"))
					case Some(_, nextSchema) if nextSchema.levels.nonEmpty =>
						Left(s"Number of levels defined in schema exceed the depth of sections for nested document $title.".asErrLoc("<schema>"))
					case Some(_, _) => section.parsedText(textProcessingService, innerSchema.paragraphBreak, innerSchema.indent) match
						case None =>
							Left(s"Unexpected error: nested document ($title) was found to have no parsed text after having been validated as having text. Contact your maintainer.".asErr())
						case Some(textValue: String) =>
							for
								levels <- NonEmptyList.parse(innerSchema.levels).left.map(_ => s"Schema for document $title has no levels defined".asErrLoc("<schema>"))
								typedTitle <- Title.parse(title).left.map(v => (v + s" ($title)").asErrLoc("<title>"))
							yield ComplexDocContent.Content(
								typedTitle,
								levels,
								NonEmptyList(ContentSection.Text(textValue)),
							)

						case Some(textSections: List[String]) =>
							for {
								nonEmptySections <- NonEmptyList.parse(textSections.map(ContentSection.Text.apply))
									.left.map(_ => s"Unexpected error: document body is found to be empty after prior validation. Contact the maintainer.".asErr())
								nonEmptyLevels <- NonEmptyList.parse(innerSchema.levels)
									.left.map(_ => s"Schema for document $title has no levels defined.".asErrLoc("<schema>"))
								typedTitle <- Title.parse(title).left.map(v => (v + s" ($title)").asErrLoc("<title>"))
							} yield ComplexDocContent.Content(
								typedTitle,
								nonEmptyLevels,
								nonEmptySections,
							)

object XmlContextusDocConversion:
	val CATEGORY_SEPARATOR = "/"
