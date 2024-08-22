package contextus.model.xml

import contextus.model.sefaria.SefariaIndexEntry
import contextus.model.sefaria.SefariaSchemaNode

object XmlContextusDocReverseConversion:

    /**
     * Generates an [[XmlContextusDoc]] from a [[SefariaIndexEntry]]. Note that this will not include any actual text, 
     * but only the document structure. This is useful for creating a document stub for filling out a new version.
     * 
     * @param indexEntry sefaria index entry for the document
     * @param version version of the document text you intend to add to empty document.
     * @return
     */
    def fromSefariaIndexEntry(
        indexEntry: SefariaIndexEntry,
        version: Option[Version],
    ): XmlContextusDoc =
        val schema = schemaFromSchemaOrSections(indexEntry.schemaOrSections)
        XmlContextusDoc(
            title = Some(indexEntry.title),
            alternateTitles =
                if indexEntry.titleVariants.length == 0 then None
                else Some(indexEntry.titleVariants),
            author = indexEntry.authors.flatMap(_.headOption.map(_.en)),
            publicationYear = indexEntry.compDate.flatMap(_.headOption),
            description = indexEntry.enDesc,
            shortDescription = indexEntry.enShortDesc,
            category = Some(indexEntry.categories.mkString("/")),
            schema = Some(schema),
            version = version,
            body = Some(bodyFromSchemaOrSections(indexEntry.schemaOrSections, schema.levels)),
        )

    private def schemaFromSchemaOrSections(schemaOrSections: Either[SefariaSchemaNode, List[String]]): Schema =
        schemaOrSections match
            case Left(schemaNode) => Schema(mostFrequentLevelsFromSchemaNode(schemaNode))
            case Right(levels) => Schema(levels)

    private def mostFrequentLevelsFromSchemaNode(schemaNode: SefariaSchemaNode): List[String] =
        val allLevels = allLevelsFromSchemaNode(schemaNode)
        val freqCount = allLevels.groupMap(identity)(_ => ()).mapValues(_.size)
        freqCount.toList.sortBy(-_._2).map(_._1).headOption.getOrElse(Nil)

    private def allLevelsFromSchemaNode(schemaNode: SefariaSchemaNode): List[List[String]] =
        schemaNode match
            case SefariaSchemaNode.Text(depth, addressTypes, sectionNames, titles, heSectionNames, title, heTitle, key) =>
                sectionNames :: Nil
            case SefariaSchemaNode.Section(nodes, titles, title, heTitle, key) =>
                nodes.flatMap(n => allLevelsFromSchemaNode(n))

    private def bodyFromSchemaOrSections(schemaOrSections: Either[SefariaSchemaNode, List[String]], defaultLevels: List[String]): Body =
        schemaOrSections match
            case Left(schemaNode) => bodyFromSchemaNode(schemaNode, defaultLevels)
            case Right(levels) => Body(sectionsFromLevels(levels), textFromLevels(levels))

    private def bodyFromSchemaNode(schemaNode: SefariaSchemaNode, defaultLevels: List[String]): Body =
        schemaNode match
            case textNode: SefariaSchemaNode.Text =>
                Body(sectionsFromLevels(textNode.sectionNames), textFromLevels(textNode.sectionNames))
            case sectionNode: SefariaSchemaNode.Section =>
                Body(sectionNode.nodes.map(v => sectionFromSchemaNode(v, defaultLevels)), "")

    private def sectionFromSchemaNode(schemaNode: SefariaSchemaNode, defaultLevels: List[String]): Section =
        schemaNode match
            case textNode: SefariaSchemaNode.Text =>
                val schema = if textNode.sectionNames == defaultLevels then None else Some(Schema(textNode.sectionNames))
                Section(Some(textNode.title), None, sectionsFromLevels(textNode.sectionNames), textFromLevels(textNode.sectionNames), schema)
            case sectionNode: SefariaSchemaNode.Section =>
                val newDefaultLevels = mostFrequentLevelsFromSchemaNode(schemaNode)
                val schema = if newDefaultLevels == defaultLevels then None else Some(Schema(newDefaultLevels))
                Section(Some(sectionNode.title), None, sectionNode.nodes.map(n => sectionFromSchemaNode(n, newDefaultLevels)), "", schema)
        

    private def sectionsFromLevels(levels: List[String]): List[Section] = levels match
        case Nil => Nil
        case _ :: Nil => Nil
        case level :: others =>
            List(
                Section(None, Some(level), sectionsFromLevels(others), "", None),
            )
    

    private def textFromLevels(levels: List[String]): String =
        if levels.length == 1 then
             s"This section must include text -- Subsection divisions for level ${levels.head} are parsed by multi line breaks"
        else ""
    

    
        
        
