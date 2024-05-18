package contextus.service

import contextus.model.xml.XmlContextusDoc
import contextus.model.DomainError.{DecodingError, IOError}
import zio.*
import zio.stream.*

import java.nio.file.*
import scala.jdk.CollectionConverters.*
import contextus.phobos.PhobosZIO.*

trait ContextusFileService:
	def streamFromDirectory(directory: Path): ZStream[Any, ContextusFileService.Error, XmlContextusDoc]

	def getDocument(path: Path): IO[ContextusFileService.Error, Option[XmlContextusDoc]]

object ContextusFileService:
	type Error = DecodingError | IOError.FileIOError

	val live: ULayer[ContextusFileService] = ZLayer.succeed(Live)

	private object Live extends ContextusFileService:
		def streamFromDirectory(directory: Path): ZStream[Any, Error, XmlContextusDoc] =
			ZStream
				.fromIterator(Files.newDirectoryStream(directory).iterator().asScala)
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

		def getDocument(path: Path): IO[Error, Option[XmlContextusDoc]] =
			val unhandled = for {
				exists <- ZIO.attempt(Files.exists(path))
				_ <- ZIO.fail(()).when(!exists)
				isDir <- ZIO.attempt(Files.isDirectory(path))
				_ <- ZIO.fail(()).when(isDir)
				data <- ZIO.attempt(Files.readString(path))
				doc <- data.decodeXmlZIO[XmlContextusDoc]
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