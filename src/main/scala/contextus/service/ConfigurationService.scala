package contextus.service

import zio.*
import zio.nio.file.*
import contextus.json.*
import contextus.model.DomainError

import java.io.FileNotFoundException
import java.nio.charset.StandardCharsets
import java.nio.file.NoSuchFileException

/**
 * Service to save and retrieve configuration information to/from a file. By default this will 
 * be in the user's home directory: $HOME/.contextus/config.json
 */
trait ConfigurationService:
	def setApiKey(key: String): ZIO[Any, ConfigurationService.Error, Unit]
	def getApiKey: ZIO[Any, ConfigurationService.Error, Option[String]]
	
	def setBaseUrl(url: String): ZIO[Any, ConfigurationService.Error, Unit]
	def getBaseUrl: ZIO[Any, ConfigurationService.Error, String]

object ConfigurationService:
	val live = ZLayer.succeed[ConfigurationService](Live)

	type Error = DomainError.IOError

	final case class Conf(
		apiKey: Option[String],
		baseUrl: String,
	)

	object Conf:
		val default = Conf(apiKey = None, baseUrl = "http://contextus.org")

		given Encoder[Conf] = deriveEncoder
		given Decoder[Conf] = deriveDecoder


	object Live extends ConfigurationService:
		private val homePath: ZIO[Any, DomainError.IOError.FileIOError, Path] =
			System.property("user.home")
				.foldZIO(
					err => ZIO.fail(DomainError.IOError.FileIOError(
						"user.home",
						"Unable to get home directory from system properties",
						Some(err),
					)),
					{
						case None => ZIO.fail(DomainError.IOError.FileIOError(
							"user.home",
							"Unable to get home directory from system properties",
							None,
						))
						case Some(value) => ZIO.succeed(Path(value))
					}
				)
		private val contextusPath = homePath.map(_ / ".contextus")
		private val configPath = contextusPath.map(_ / "config.json")
		private val getConf: ZIO[Any, DomainError.IOError.FileIOError, Conf] = (for {
			confPath <- configPath
			confStr <- Files
				.readAllBytes(confPath).map(_.asString(StandardCharsets.UTF_8))
			conf <- confStr.decodeJsonZIO[Conf]
		} yield conf)
			.catchAll {
				case _: NoSuchFileException =>
					  setConf(Conf.default).as(Conf.default)
				case ioExc: Throwable =>
					configPath.flatMap(confPath => ZIO.fail[DomainError.IOError.FileIOError](
						  DomainError.IOError.FileIOError(
							  confPath.toString, "Unable to read config file",
							  Some(ioExc),
						  )
					))
				case err: DomainError.IOError.FileIOError => ZIO.fail[DomainError.IOError.FileIOError](err)
				case _: DomainError.DecodingError => ZIO.succeed(Conf.default)
			}
		private def setConf(conf: Conf) =
			val confStr = conf.encodeJson
			for {
				ctxPath <- contextusPath
				_ <- Files.createDirectories(ctxPath)
						  .mapError[DomainError.IOError.FileIOError](err => DomainError.IOError.FileIOError(
							  ctxPath.toString, "Unable to contextus directory",
							  Some(err),
						  ))
				confPath <- configPath
				_ <- Files
					.writeBytes(confPath, Chunk.fromArray(confStr.getBytes(StandardCharsets.UTF_8)))
					.mapError[DomainError.IOError.FileIOError](ioExc => DomainError.IOError.FileIOError(confPath.toString, "Unable to write config file", Some(ioExc)))
			} yield ()
		private def updateConf(update: Conf => Conf) = for {
			oldConf <- getConf
			newConf = update(oldConf)
			_ <- setConf(newConf)
		} yield ()

		override def setApiKey(key: String): ZIO[Any, Error, Unit] =
			updateConf(_.copy(apiKey = Some(key)))
		override def getApiKey: ZIO[Any, Error, Option[String]] =
			getConf.map(_.apiKey)

		override def setBaseUrl(url: String): ZIO[Any, Error, Unit] =
			updateConf(_.copy(baseUrl = url))
		override def getBaseUrl: ZIO[Any, Error, String] =
			getConf.map(_.baseUrl)
