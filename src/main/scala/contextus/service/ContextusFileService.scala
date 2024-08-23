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
import java.nio.file.attribute.{PosixFilePermission, PosixFilePermissions}


/**
 * Simple utility for file IO
 */
trait ContextusFileService:
	def streamFromDirectory(directory: Path): ZStream[Any, ContextusFileService.Error, ContextusDoc]

	def getDocument(path: Path): IO[ContextusFileService.Error, Option[ContextusDoc]]

	def useTemporaryFile[E, A](data: Chunk[Byte], executable: Boolean = false)(usePath: Path => IO[E, A]): IO[E | ContextusFileService.Error, A]

	def useTemporaryFiles[E, A](data: Chunk[(String, Chunk[Byte])], executable: Boolean = false)(usePaths: Chunk[Path] => IO[E, A]): IO[E | ContextusFileService.Error, A]

	def runFile(path: Path): IO[ContextusFileService.Error, Unit]

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
					  .map(err => DecodingError(Right("XmlContextusDoc"), err.toString, None))
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

		def useTemporaryFile[E, A](data: Chunk[Byte], executable: Boolean)(usePath: Path => IO[E, A]): IO[E | ContextusFileService.Error, A] =
			ZIO.scoped:
				for
					tmpDir <- Files.createTempDirectoryScoped(None, Nil).mapError(e => IOError.FileIOError("none", "failed to create temporary directory", Some(e)): ContextusFileService.Error)
					fullPath = tmpDir / "tmpfile"
					_ <- Files.writeBytes(fullPath, data).mapError(e => IOError.FileIOError(fullPath.toString, "failed to create temporary file", Some(e)): ContextusFileService.Error)
					_ <- Files.setPosixFilePermissions(fullPath, Set(PosixFilePermission.OWNER_EXECUTE))
						.when(executable)
						.mapError(e => IOError.FileIOError(fullPath.toString, "failed to make temporary file executable", Some(e)): ContextusFileService.Error)
					result <- usePath(fullPath)
				yield result

		def useTemporaryFiles[E, A](data: Chunk[(String, Chunk[Byte])], executable: Boolean)(usePaths: Chunk[Path] => IO[E, A]): IO[E | ContextusFileService.Error, A] =
			ZIO.scoped:
				for
					tmpDir <- Files.createTempDirectoryScoped(None, Nil).mapError(e => IOError.FileIOError("none", "failed to create temporary directory", Some(e)): ContextusFileService.Error)
					paths <-
						ZStream.fromChunk(data)
							.mapZIO: (name, data) =>
								val fullPath = tmpDir / name
								for
									_ <- Files.writeBytes(fullPath, data).mapError(e => IOError.FileIOError(fullPath.toString, "failed to create temporary file", Some(e)): ContextusFileService.Error)
									_ <- Files.setPosixFilePermissions(fullPath, PosixFilePermissions.fromString("rwxrwxrwx").asScala.toSet)
										.when(executable)
										.mapError(e => IOError.FileIOError(fullPath.toString, "failed to make temporary file executable", Some(e)): ContextusFileService.Error)
								yield fullPath
							.runCollect
					result <- usePaths(paths)
				yield result

		def runFile(path: Path): IO[ContextusFileService.Error, Unit] =
			val parent = path.parent
			val relPath = parent.map(_.relativize(path)).getOrElse(path)
			scala.sys.process.Process(s"cat ${relPath.toString}", path.parent.map(_.toFile)).!
			ZIO.attempt(scala.sys.process.Process(s"zsh ${relPath.toString}", path.parent.map(_.toFile)).!)
				.mapError(e => IOError.FileIOError(path.toString, "failed to run file", Some(e)): ContextusFileService.Error)
				.flatMap:
					case 0 => ZIO.unit
					case other =>
						ZIO.fail(IOError.FileIOError(path.toString, s"executing file failed with code $other", None))
