package contextus.service

import contextus.model.DomainError
import contextus.model.contextus.ContextusDoc
import zio.*
import zio.stream.*
import contextus.model.DomainError.*
import contextus.model.DomainError.IOError.HttpIOError
import contextus.model.contextus.ContextusDocConversion
import contextus.model.sefaria.{SefariaRef, SefariaText, SefariaTextSubmission}
import contextus.model.xml.XmlContextusDocConversion.CATEGORY_SEPARATOR

/**
 * Service for indexing and submitting [[ContextusDoc]]s. Basically a wrapper around 
 * [[SefariaService]] that does necessary conversions.
 */
trait ContextusService:
	import ContextusService.Error

	def indexDocument(document: ContextusDoc): IO[Error, Unit]

	def submitDocumentVersion(document: ContextusDoc): IO[Error, Unit]

	def submitDocument(document: ContextusDoc): IO[Error, Unit] =
		for {
			_ <- indexDocument(document)
			_ <- submitDocumentVersion(document)
		} yield ()

	def validateDocument(document: ContextusDoc): IO[Error | ValidationError, Unit]


object ContextusService:
	type Error = IOError.HttpIOError | DecodingError | SefariaApiError

	val live = ZLayer.fromFunction(Live.apply)

	case class Live(sefariaService: SefariaService) extends ContextusService:
		override def indexDocument(document: ContextusDoc): IO[Error, Unit] =
			for {
				indexEntry <- ZIO.fromEither(
					ContextusDocConversion.contextusDocToSefariaIndexEntry(document)
						.left.map(msg => DecodingError(Right("Contextus document"), msg, None))
				)
				_ <- sefariaService.addEntryToIndex(indexEntry)
			} yield ()

		override def submitDocumentVersion(document: ContextusDoc): IO[Error, Unit] =
			val textSubmissionMap =
				ContextusDocConversion.contextusDocToSefariaTextSubmissionMap(document)
			for {
				_ <- ZIO.foreachDiscard(textSubmissionMap.map) {
					case (ref, text) =>
						val textSubmission = SefariaTextSubmission(
							versionTitle = textSubmissionMap.versionTitle,
							versionSource = textSubmissionMap.versionSource,
							text = text,
							language = textSubmissionMap.language,
						)
						sefariaService.addText(ref, textSubmission)
							.foldZIO[Any, HttpIOError | DecodingError, Unit](
								{
									case err: SefariaApiError if err.message.contains("Could not find title in reference") =>
										ZIO.debug(s"Unable resolve ${ref.refString} to an indexed document. If you have already indexed ${document.title}, you may need wait as much as a day before being able to add all of its text.")
									case err: SefariaApiError =>
										ZIO.debug(s"Failed to upload ${ref.refString}: ${err.message}")
											*> ZIO.debug(s"${err.method}: ${err.url} (${err.status})")
									case err: (HttpIOError | DecodingError) =>
										ZIO.fail(err)
								},
								ZIO.succeed,
							)
				}
			} yield ()

		override def validateDocument(document: ContextusDoc): IO[Error | ValidationError, Unit] =
			sefariaService
				.getCategories
				.flatMap { categories =>
					categories.validated(document.category) match
						case None =>
							ZIO.unit
						case Some(Nil, cat) =>
							ZIO.fail(
								ValidationError("ContextusDoc", Some(document.category.mkString(CATEGORY_SEPARATOR)), Some(s"Unable to find category $cat in index"))
							)
						case Some(path, cat) =>
							ZIO.fail(
								ValidationError("ContextusDoc", Some(document.category.mkString(CATEGORY_SEPARATOR)), Some(s"Unable to find category $cat in ${path.mkString("/")}"))
							)
				}
