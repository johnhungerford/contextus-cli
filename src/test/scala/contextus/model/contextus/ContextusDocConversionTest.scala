package contextus.model.contextus

import contextus.model.sefaria.{SefariaAddressType, SefariaIndexEntry, SefariaSchemaNode, SefariaTitle}
import zio.test.*

object ContextusDocConversionTest extends ZIOSpecDefault:
	
	val simpleDoc = ContextusDocTest.expectedSimpleDoc
	
	val simpleDocExpected = SefariaIndexEntry(
		title = simpleDoc.title,
		heTitle = simpleDoc.title + " HEBREW",
		titleVariants = Nil,
		heTitleVariants = Nil,
		categories = List("category1", "category2"),
		heCategories = Nil,
		schemaOrSections = Right(List("book", "chapter", "paragraph")),
		order = None,
		authors = None,
		enDesc = simpleDoc.description,
		heDesc = None,
		enShortDesc = simpleDoc.shortDescription,
		heShortDesc = None,
		hasErrorMargin = None,
		compDate = None,
		is_cited = None,
		compDateString = None,
	)
	
	val complexDoc = ContextusDocTest.expectedComplexDoc
	
	val complexDocExpected = SefariaIndexEntry(
		title = complexDoc.title,
		heTitle = complexDoc.title + " HEBREW",
		titleVariants = Nil,
		heTitleVariants = Nil,
		categories = List("category1", "category2"),
		heCategories = Nil,
		schemaOrSections = Left(List(
			SefariaSchemaNode.Text(
				title = "Preface",
				heTitle = "Preface HEBREW",
				titles = List(SefariaTitle("Preface", "en", Some(true))),
				key = "Preface",
				sectionNames = List("paragraph"),
				heSectionNames = List("paragraph HEBREW"),
				depth = 1,
				addressTypes = List(SefariaAddressType.Integer),
			),
			SefariaSchemaNode.Text(
				title = "Book 1",
				heTitle = "Book 1 HEBREW",
				titles = List(SefariaTitle("Book 1", "en", Some(true))),
				key = "Book 1",
				sectionNames = List("chapter", "paragraph"),
				heSectionNames = List("chapter HEBREW", "paragraph HEBREW"),
				depth = 2,
				addressTypes = List(SefariaAddressType.Integer, SefariaAddressType.Integer),
			),
			SefariaSchemaNode.Text(
				title = "Book 2",
				heTitle = "Book 2 HEBREW",
				titles = List(SefariaTitle("Book 2", "en", Some(true))),
				key = "Book 2",
				sectionNames = List("chapter", "paragraph"),
				heSectionNames = List("chapter HEBREW", "paragraph HEBREW"),
				depth = 2,
				addressTypes = List(SefariaAddressType.Integer, SefariaAddressType.Integer),
			),
		)),
		order = None,
		authors = None,
		enDesc = complexDoc.description,
		heDesc = None,
		enShortDesc = complexDoc.shortDescription,
		heShortDesc = None,
		hasErrorMargin = None,
		compDate = complexDoc.compositionYear.map(_ :: Nil),
		is_cited = None,
		compDateString = None,
	)
	
	override def spec = suite("ContextusDocConversionTest")(
		test("simple doc") {
			val sefariaIndexEntry = ContextusDocConversion
				.contextusDocToSefariaIndexEntry(simpleDoc)
			
			assertTrue(sefariaIndexEntry == Right(simpleDocExpected))
		},

		test("complex doc") {
			val sefariaIndexEntry = ContextusDocConversion
			  .contextusDocToSefariaIndexEntry(complexDoc)

			assertTrue(sefariaIndexEntry == Right(complexDocExpected))
		}
	)
