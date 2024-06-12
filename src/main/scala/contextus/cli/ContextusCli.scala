package contextus.cli

import contextus.service.{ContextusFileService, ContextusService, SefariaService}
import zio.*
import zio.stream.*
import zio.cli.*
import zio.cli.HelpDoc.Span.text
import zio.Console.printLine
import contextus.model.DomainError
import contextus.model.contextus.Title
import contextus.model.sefaria.{SefariaCategory, SefariaCategoryUpdate}
import zio.nio.file.Files
import zio.nio.file.Path
import contextus.model.xml.XmlContextusDocConversion.CATEGORY_SEPARATOR
import contextus.model.types.NonEmptyList
import contextus.model.xml.{Body, Schema, Section, Version, XmlContextusDoc}

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.util.Locale.Category
import scala.util.Try
import contextus.phobos.PhobosZIO
import contextus.model.sefaria.SefariaRef
import contextus.model.xml.XmlContextusDocReverseConversion
import scala.io.Source
import contextus.model.xml.Template

object ContextusCli:

	val documentArg = Args.text("document")
		.mapOrFail(txt => for {
			javaPath <- Try(java.nio.file.Path.of(txt)).toEither.left.map(err => HelpDoc.p(s"Unable to parse file path: ${txt} (${err.getMessage})"))
			exists <- Try(java.nio.file.Files.exists(javaPath)).toEither.left.map(err => HelpDoc.p(s"Unable to check file existence for $txt (${err.getMessage})"))
			_ <- if exists then Right(()) else Left(HelpDoc.p(s"No file or directory at path $txt"))
		} yield Path.fromJava(javaPath))

	val documentsArg: Args[ZStream[Any, DomainError.IOError, Path]] = documentArg
	  	.atLeast(1)
		.map( fileList => {
			val files = if fileList.isEmpty then List(Path("")) else fileList
			ZStream
				.fromIterable(files)
				.flatMap( path => ZStream.unwrap(for {
					isDir <- Files.isDirectory(path)
				} yield {
					if isDir then Files.walk(path, 1)
						.filter(_.filename.toString.split('.').lastOption.map(_.toLowerCase).contains("xml"))
						.mapError(ioExc => DomainError.IOError.FileIOError(path.toString, f"Failed to read directory ${path.toString}", Some(ioExc)))
					else ZStream(path)
				}))
		}) ?? "Path to an XML document to be processed, or a directory with XML files to be processed. Uses current working directory by default."

	val categoryArg: Args[NonEmptyList[String]] =
		Args
			.text("category").??(s"Path to a Contextus category. Levels should be separated by '${CATEGORY_SEPARATOR}''")
			.mapOrFail(txt => {
				NonEmptyList
					.parse(txt.split(CATEGORY_SEPARATOR).map(_.trim).toList)
					.left
					.map(_ => HelpDoc.p(s"Invalid category path $txt: provide at least one category level. (Separate levels with '${CATEGORY_SEPARATOR}')"))
			})

	import DomainError.{IOError, SefariaApiError, DecodingError, ValidationError}

	def handleErrorForDoc[R, T](
		path: Path,
		effect: ZIO[R, IOError | DecodingError | SefariaApiError | ValidationError | Unit, T]
	) = handleError(s"Unable to process ${path.filename.toString}", effect)
		*> printEmptyLine

	def handleError[R, T](
		message: String,
		effect: ZIO[R, IOError | DecodingError | SefariaApiError | ValidationError | Unit, T]
	) = effect.catchAll {
		case err: DomainError.IOError.HttpIOError =>
			Console.printLineError(message).orDie
				*> ZIO.foreachDiscard(err.underlying)(err => printUnderlyingError(err))
				*> Console.printLineError(s"Failure communicating with Contextus service: ${err.problem}").orDie
				*> Console.printLineError(s"${err.method}: ${err.url}").orDie
		case err: DomainError.IOError.FileIOError =>
			Console.printLineError(message).orDie
				*> ZIO.foreachDiscard(err.underlying)(err => printUnderlyingError(err))
				*> Console.printLineError(s"Failure reading or writing file: ${err.problem}").orDie
				*> Console.printLineError(s"Failed path: ${err.path}").orDie
		case err: DomainError.DecodingError =>
			Console.printLineError(message).orDie
				*> ZIO.foreachDiscard(err.underlying)(err => printUnderlyingError(err))
				*> Console.printLineError(s"Failure decoding data: ${err.problem}").orDie
//				*> Console.printLineError(err.typeId.fold(_.shortName, identity)).orDie
		case err: DomainError.SefariaApiError =>
			Console.printLineError(message).orDie
				*> ZIO.foreachDiscard(err.underlying)(err => printUnderlyingError(err))
				*> Console.printLineError(s"Unexpected failure response from Contextus${err.message.fold("")(v => s": $v")}").orDie
				*> Console.printLineError(s"${err.method}: ${err.url}${err.status.fold("")(v => s" --> $v")}").orDie
		case err: DomainError.ValidationError =>
			Console.printLineError(message).orDie
				*> Console.printLine("Invalid Contextus document:").orDie *> printValidationError(0, err)
		case () => ZIO.unit
	}

	val indexCommand = Command(
		name = "index",
		args = documentsArg,
	).withHelp("Add one or more documents to the Contextus table of contents without adding text (validates prior to submission)")
		.map(pathStream => pathStream.foreach( path => handleErrorForDoc(path, for {
			fileService <- ZIO.service[ContextusFileService]
			contextusDoc <- fileService.getDocument(path).flatMap {
				case Some(doc) => ZIO.succeed(doc)
				case None =>
					Console.printError(s"File ${path} does not exist").orDie
					  *> ZIO.fail(())
			}
			contextusService <- ZIO.service[ContextusService]
			_ <- contextusService.validateDocument(contextusDoc)
			_ <- contextusService.indexDocument(contextusDoc)
			_ <- Console.printLine(s"Successfully indexed ${contextusDoc.title} (${path})").orDie
		} yield ())))

	val addTextCommand = Command(
		name = "add-text",
		args = documentsArg,
	).withHelp("Add a text to one or more existing Contextus documents (validates prior to submission)")
		.map(pathStream => pathStream.foreach(path => handleErrorForDoc(path, for {
			fileService <- ZIO.service[ContextusFileService]
			contextusDoc <- fileService.getDocument(path).flatMap {
				case Some(doc) => ZIO.succeed(doc)
				case None =>
					Console.printError(s"File ${path} does not exist").orDie
					  *> ZIO.fail(())
			}
			contextusService <- ZIO.service[ContextusService]
			_ <- contextusService.validateDocument(contextusDoc)
			_ <- contextusService.submitDocumentVersion(contextusDoc)
			_ <- Console.printLine(s"Successfully added text to ${contextusDoc.title} (${path})").orDie
		} yield ())))

	val submitDocCommand = Command(
		name = "submit",
		args = documentsArg,
	).withHelp("Add one or more documents to Contextus: validates them, adds them to the table of contexts, and adds all texts")
		.map(pathStream => pathStream.foreach(path => handleErrorForDoc(path, for {
			fileService <- ZIO.service[ContextusFileService]
			contextusDoc <- fileService.getDocument(path).flatMap {
				case Some(doc) => ZIO.succeed(doc)
				case None =>
					Console.printError(s"File ${path} does not exist").orDie
						*> ZIO.fail(())
			}
			contextusService <- ZIO.service[ContextusService]
			_ <- contextusService.validateDocument(contextusDoc)
			_ <- contextusService.submitDocument(contextusDoc)
			_ <- Console.printLine(s"Successfully indexed and added text to ${contextusDoc.title} (${path})").orDie
		} yield ())))

	def printValidationError(indent: Int, error: DomainError.ValidationError): UIO[Unit] =
		val printReason = error.reason.fold(ZIO.unit)(reason => Console.printLine(s"${indentString * indent}$reason").orDie)
		val printUnderlying = ZIO.foreachDiscard(error.underlyingErrors)(
			err => printValidationError(indent + 1, err)
		)
		printReason *> printUnderlying


	val validateCommand = Command(
		name = "validate",
		args = documentsArg,
	).withHelp("Validate one or more documents, ensuring they are properly formatted for submission to Contextus")
		.map(pathStream => pathStream.foreach(path => handleErrorForDoc(path, for {
			fileService <- ZIO.service[ContextusFileService]
			contextusDoc <- fileService.getDocument(path).flatMap {
				case Some(doc) => ZIO.succeed(doc)
				case None =>
					Console.printError(s"File ${path} does not exist").orDie
					  *> ZIO.fail(())
			}
			contextusService <- ZIO.service[ContextusService]
			_ <- contextusService.validateDocument(contextusDoc)
			_ <-  Console.printLine(s"Valid Contextus document: ${contextusDoc.title} (${path.filename})").orDie
		} yield ())))

	private val indentString = "   "

	private def printCategory(indent: Int, category: SefariaCategory): UIO[Unit] =
		Console.printLine(s"${indentString * indent}${category.name}").orDie
			*> ZIO.foreachDiscard(category.categories)(subCategory => printCategory(indent + 1, subCategory))

	val listCategoriesCommand = Command(
		"list-categories",
	).withHelp("Displays all current Contextus categories")
		.map(_ => handleError("Unable to retrieve categories", for {
			sefariaService <- ZIO.service[SefariaService]
			categories <- sefariaService.getCategories
			_ <- ZIO.foreachDiscard(categories)(cat => printCategory(0, cat))
		} yield ()))

	val addCategoryCommand = Command(
		"add-category",
		categoryArg,
	).withHelp("Add a new category to Contextus")
	 	.map(category => {
			val update = SefariaCategoryUpdate.newCategory(category)
			handleError(
				s"Unable to add category ${category.mkString(CATEGORY_SEPARATOR)}",
				ZIO.serviceWithZIO[SefariaService](_.updateCategories(update))
			)
		})

	import contextus.phobos.PhobosZIO.*

	val templateOption =
		Options.enumeration[Template]("template")(
			"complex-books" -> Template.MultipleBooks,
			"complex-chapters" -> Template.NamedChapters,
			"poem" -> Template.Poem,
			"chapters" -> Template.SimpleChapters,
			"paragraphs" -> Template.SimpleParagraphs,
			"song" -> Template.Song,
		).withDefault(Template.SimpleParagraphs)

	val newDocumentCommand = Command(
		"new-document",
		templateOption,
		Args.text("title") ?? "Title of the new document",
	).withHelp("Create a new document")
		.map((template, title) => handleError("Failed to make a new document", {
			Title.parse(title) match
				case Left(err) => ZIO.fail(DomainError.DecodingError(Right("title"), err, None))
				case Right(_) =>
					val titleForFilename = title.replaceAll("""[^\w\s]""", " ").trim.replaceAll("""\s+""", "-").toLowerCase
					val filename = titleForFilename + ".xml"
					val filePath = Path(filename)

					for
						parsedTitle <- ZIO.fromEither(Title.parse(title))
							.mapError(str => DomainError.DecodingError(Right("Title"), str, None))
						documentString = template.doc(parsedTitle, None)
						fileExists <- Files.exists(filePath)
						_ <- ZIO.fail(IOError.FileIOError(filename, s"${filename} already exists!", None))
							.when(fileExists)
						_ <- Files.writeBytes(filePath, Chunk.fromArray(documentString.getBytes(StandardCharsets.UTF_8)))
							.mapError(ioe => DomainError.IOError.FileIOError(filename, "Failed to write document", Some(ioe)))
						_ <- Console.printLine(s"Created new document: ${filename}").orDie
					yield ()
		}))


	val versionOption = Options.text("version") ?? "Name of the new version"
	val sourceOption = Options.text("source").optional ?? "URL of the new version's source"
	val fromOption = Options.file("from-file", Exists.Yes).map(Left.apply).??("File to make a new version of") orElse Options.text("from-contextus").map(Right.apply).??("Title of document in contextus")

	val newVersionCommand = Command(
		"new-version",
		versionOption ++ sourceOption ++ fromOption,
	).withHelp("Create a new version of an existing document (creates a new XML document)")
		.map((versionTitle, versionSourceOpt, fromEither) => handleError("Failed to make a new document", {
			val versionSource = versionSourceOpt.getOrElse("SOURCE URL SHOULD GO HEAR")
			val version = Version(Some(versionTitle), Some(versionSource), None)
			val getExistingDocument = fromEither match
				case Left(file) =>
					val filePath = Path.fromJava(file)
					for 
						fileString <- Files
							.readAllBytes(filePath)
							.map(v => String(v.toArray, StandardCharsets.UTF_8))
							.mapError(ioe => DomainError.IOError.FileIOError(file.toString, "Failed to write document", Some(ioe)))
						xmlDoc <- fileString.decodeXmlZIO[XmlContextusDoc]
					yield
						xmlDoc.copy(version = Some(version))

				case Right(title) =>
					for
						sefariaService <- ZIO.service[SefariaService]
						entry <- sefariaService.getIndexEntry(SefariaRef(title))
						xmlDoc = XmlContextusDocReverseConversion.fromSefariaIndexEntry(entry, Some(version))
					yield
						xmlDoc

			val versionTitleForFilename = versionTitle.replaceAll("""[^\w\s]""", " ").trim.replaceAll("""\s+""", "-").toLowerCase

			for 
				xmlDoc <- getExistingDocument
				titleForFilename = xmlDoc.title.map(_.replaceAll("""[^\w\s]""", " ").trim.replaceAll("""\s+""", "-").toLowerCase + "-").getOrElse("")
				filename = titleForFilename + versionTitleForFilename + ".xml"
				filePath = Path(filename)
				fileExists <- Files.exists(filePath)
				_ <- ZIO.fail(IOError.FileIOError(filename, s"${filename} already exists!", None))
					.when(fileExists)
				documentString <- xmlDoc.toXmlPrettyZIO
					.mapError(ee => DomainError.IOError.FileIOError(filename, "Failed to encode document", Some(ee)))
				_ <- Files.writeBytes(filePath, Chunk.fromArray(documentString.getBytes(StandardCharsets.UTF_8)))
					.mapError(ioe => DomainError.IOError.FileIOError(filename, "Failed to write document", Some(ioe)))
				_ <- Console.printLine(s"Created new document: ${filename}").orDie
			yield ()
	}))

	val command = Command("contextus")
		.subcommands(
			submitDocCommand,
			validateCommand,
			indexCommand,
			addTextCommand,
			newDocumentCommand,
			newVersionCommand,
			listCategoriesCommand,
			addCategoryCommand,
			ConfigCommands.rootCommand,
			FilesystemCommands.lsCommand,
			TextCommands.fixTextCommand,
		)
	
	private val printEmptyLine = Console.printLine("").orDie

	def printUnderlyingError(thr: Throwable): UIO[Unit] =
//		ZIO.attempt(err.printStackTrace()).orDie
		ZIO.unit

	val app = CliApp.make(
		name = "Contextus",
		version = "0.1.0",
		summary = text("Utilities for adding and updating documents in Contextus"),
		command = command
	):
		effect => printEmptyLine *> effect

