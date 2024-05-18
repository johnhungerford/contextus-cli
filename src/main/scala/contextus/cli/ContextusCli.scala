package contextus.cli

import contextus.service.{ContextusFileService, ContextusService, SefariaService}
import zio.*
import zio.cli.*
import zio.cli.HelpDoc.Span.text
import zio.Console.printLine
import contextus.model.DomainError
import contextus.model.sefaria.SefariaCategory

import java.util.Locale.Category

object ContextusCli:
	val documentArg = Args.file("document", Exists.Yes) ?? "Path to an XML document to be processed"

	val indexCommand = Command(
		name = "index",
		args = documentArg,
	).withHelp("Add a document to the Contextus table of contents without adding text")
		.map(path => for {
			fileService <- ZIO.service[ContextusFileService]
			contextusDoc <- fileService.getDocument(path).flatMap {
				case Some(doc) => ZIO.succeed(doc)
				case None =>
					Console.printError(s"File ${path} does not exist").orDie
					  *> ZIO.fail(())
			}
			contextusService <- ZIO.service[ContextusService]
//			_ <- contextusService.validateDocument(contextusDoc)
			_ <- contextusService.indexDocument(contextusDoc)
		} yield ())

	val addVersionCommand = Command(
		name = "add-version",
		args = documentArg,
	).withHelp("Add a text version to an existing Contextus document")
		.map(path => for {
			fileService <- ZIO.service[ContextusFileService]
			contextusDoc <- fileService.getDocument(path).flatMap {
				case Some(doc) => ZIO.succeed(doc)
				case None =>
					Console.printError(s"File ${path} does not exist").orDie
					  *> ZIO.fail(())
			}
			contextusService <- ZIO.service[ContextusService]
//			_ <- contextusService.validateDocument(contextusDoc)
			_ <- contextusService.submitDocumentVersion(contextusDoc)
		} yield ())

	val submitDocCommand = Command(
		name = "submit",
		args = documentArg,
	).withHelp("Add a document to Contextus, including table of context and text")
		.map(path => for {
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
		} yield ())

	def printValidationError(indent: Int, error: DomainError.ValidationError): UIO[Unit] =
		val printReason = error.reason.fold(ZIO.unit)(reason => Console.printLine(s"${indentString * indent}$reason").orDie)
		val printUnderlying = ZIO.foreachDiscard(error.underlyingErrors)(
			err => printValidationError(indent + 1, err)
		)
		printReason *> printUnderlying


	val validateCommand = Command(
		name = "validate",
		args = documentArg,
	).withHelp("Validate a document, ensuring it is properly formatted for submission to Contextus")
		.map(path => for {
			fileService <- ZIO.service[ContextusFileService]
			contextusDoc <- fileService.getDocument(path).flatMap {
				case Some(doc) => ZIO.succeed(doc)
				case None =>
					Console.printError(s"File ${path} does not exist").orDie
					  *> ZIO.fail(())
			}
			contextusService <- ZIO.service[ContextusService]
			_ <- contextusService.validateDocument(contextusDoc)
			_ <-  Console.printLine(s"Valid Contextus document!").orDie
		} yield ())

	private val indentString = "   "

	private def printCategory(indent: Int, category: SefariaCategory): UIO[Unit] =
		Console.printLine(s"${indentString * indent}${category.name}").orDie
			*> ZIO.foreachDiscard(category.categories)(subCategory => printCategory(indent + 1, subCategory))


	val categoriesCommand = Command(
		"list-categories",
	).map(_ => for {
		sefariaService <- ZIO.service[SefariaService]
		categories <- sefariaService.getCategories
		_ <- ZIO.foreachDiscard(categories)(cat => printCategory(0, cat))
	} yield ())

	val command = Command("contextus")
	  .subcommands(
		  categoriesCommand,
		  validateCommand,
		  indexCommand,
		  addVersionCommand,
		  submitDocCommand,
		  FilesystemCommands.lsCommand,
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
			.catchAll {
				case err: DomainError.IOError.HttpIOError =>
					ZIO.foreachDiscard(err.underlying)(err => printUnderlyingError(err))
						*> Console.printLineError(s"Failure communicating with Contextus service: ${err.problem}").orDie
						*> Console.printLineError(s"${err.method}: ${err.url}").orDie
				case err: DomainError.IOError.FileIOError =>
					ZIO.foreachDiscard(err.underlying)(err => printUnderlyingError(err))
						*> Console.printLineError(s"Failure reading file: ${err.problem}").orDie
						*> Console.printLineError(err.path).orDie
				case err: DomainError.DecodingError =>
					ZIO.foreachDiscard(err.underlying)(err => printUnderlyingError(err))
						*> Console.printLineError(s"Failure decoding data: ${err.problem}").orDie
						*> Console.printLineError(err.typeId.fold(_.shortName, identity)).orDie
				case err: DomainError.SefariaApiError =>
					ZIO.foreachDiscard(err.underlying)(err => printUnderlyingError(err))
						*> Console.printLineError(s"Unexpected failure response from Contextus${err.message.fold("")(v => s": $v")}").orDie
						*> Console.printLineError(s"${err.method}: ${err.url}${err.status.fold("")(v => s" --> $v")}").orDie
				case err: DomainError.ValidationError =>
					Console.printLine("Invalid Contextus document:").orDie *> printValidationError(0, err)
				case () => ZIO.unit
			} *> printEmptyLine

