package contextus.service

import contextus.model.xml.{XmlContextusDoc, XmlContextusDocConversion}
import zio.*
import zio.stream.*
import contextus.model.DomainError.*
import contextus.model.contextus.ContextusDocConversion
import contextus.model.sefaria.{SefariaIndexSimpleEntry, SefariaRef, SefariaText, SefariaTextSubmission}
import contextus.validation.Validation

trait ContextusService:
	import ContextusService.Error

	def indexDocument(document: XmlContextusDoc): IO[Error, Unit]

	def submitDocumentVersion(document: XmlContextusDoc): IO[Error, Unit]

	def submitDocument(document: XmlContextusDoc): IO[Error, Unit] =
		for {
			_ <- indexDocument(document)
			_ <- submitDocumentVersion(document)
		} yield ()
		
	def validateDocument(document: XmlContextusDoc): IO[ValidationError, Unit]


object ContextusService:
	type Error = IOError.HttpIOError | DecodingError | SefariaApiError

	val live = ZLayer.fromFunction(Live.apply)

	case class Live(sefariaService: SefariaService) extends ContextusService:
		override def indexDocument(document: XmlContextusDoc): IO[Error, Unit] =
			for {
				contextusDoc <- ZIO.fromEither(
					XmlContextusDocConversion.convert(document)
						.left.map(msg => DecodingError(Right("XML document"), msg, None))
				)
				indexEntry <- ZIO.fromEither(
					ContextusDocConversion.contextusDocToSefariaIndexEntry(contextusDoc)
						.left.map(msg => DecodingError(Right("Contextus document"), msg, None))
				)
				_ <- sefariaService.addEntryToIndex(indexEntry)
			} yield ()

		override def submitDocumentVersion(document: XmlContextusDoc): IO[Error, Unit] =
			for {
				contextusDoc <- ZIO.fromEither(
					XmlContextusDocConversion.convert(document)
						.left.map(msg => DecodingError(Right("XML document"), msg, None))
				)
				textSubmissionMap = ContextusDocConversion.contextusDocToSefariaTextSubmissionMap(contextusDoc)
				_ <- ZIO.foreachDiscard(textSubmissionMap.map) {
					case (ref, text) =>
						val textSubmission = SefariaTextSubmission(
							versionTitle = textSubmissionMap.versionTitle,
							versionSource = textSubmissionMap.versionSource,
							text = text,
							language = textSubmissionMap.language,
						)
						sefariaService.addText(ref, textSubmission)
				}
			} yield ()

		override def validateDocument(document: XmlContextusDoc): IO[ValidationError, Unit] =
			Validation[XmlContextusDoc].validate(document) match
				case Some(value) => ZIO.fail(value)
				case None => ZIO.unit
