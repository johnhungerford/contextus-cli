package contextus.service

import contextus.model.contextus.ContextusDoc
import contextus.model.xml.{XmlContextusDoc, XmlContextusDocConversion}
import contextus.model.DomainError.{DecodingError, IOError}
import zio.*
import zio.stream.*
import zio.nio.file.*

import scala.jdk.CollectionConverters.*
import contextus.phobos.PhobosZIO.*

import java.nio.charset.StandardCharsets

trait ContextusFileService:
	def streamFromDirectory(directory: Path): ZStream[Any, ContextusFileService.Error, ContextusDoc]

	def getDocument(path: Path): IO[ContextusFileService.Error, Option[ContextusDoc]]

object ContextusFileService:
	type Error = DecodingError | IOError.FileIOError

	val live = ZLayer.fromZIO(ZIO.serviceWith[XmlTextProcessingService][ContextusFileService](Live.apply))

	private case class Live(textProcessingService: XmlTextProcessingService) extends ContextusFileService:
		def streamFromDirectory(directory: Path): ZStream[Any, Error, ContextusDoc] =
			Files.newDirectoryStream(directory)
				.mapError[Error] { err =>
					IOError.FileIOError(
						directory.toAbsolutePath.toString,
						"failed generating stream from directory iterator",
						Some(err),
					)
				}
				.mapZIO(getDocument) collect {
					case Some(doc) => doc
				}

		def getDocument(path: Path): IO[Error, Option[ContextusDoc]] =
			val unhandled = for {
				exists <- Files.exists(path)
				_ <- ZIO.fail(()).when(!exists)
				isDir <- Files.isDirectory(path)
				_ <- ZIO.fail(()).when(isDir)
				xml <- Files.readAllBytes(path).map(_.asString(StandardCharsets.UTF_8))
				processedXml = textProcessingService.processPreIngest(xml)
				xmlDoc <- processedXml.decodeXmlZIO[XmlContextusDoc]
				doc <- ZIO.fromEither(
					XmlContextusDocConversion(textProcessingService)
					  .convert(xmlDoc)
					  .left
					  .map(str => DecodingError(Right("XmlContextusDoc"), str, None))
				)
			} yield doc
			unhandled.foldZIO(
				{
					case () => ZIO.none
					case err: DecodingError => ZIO.fail(err)
					case err: Throwable =>
						ZIO.fail(IOError.FileIOError(
							path.toAbsolutePath.toString,
							err.getMessage,
							Some(err),
						))
				},
				v => ZIO.some(v)
			)