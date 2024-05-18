import contextus.cli.ContextusCli
import contextus.model.DomainError.DecodingError
import contextus.service.{ContextusFileService, ContextusService, HttpService, SefariaService}
import sttp.client3.httpclient.zio.HttpClientZioBackend
import zio.*
import zio.cli.*

import java.nio.file.Path

//object Somat extends ZIOCliDefault

object Main extends ZIOAppDefault:
	override def run = (for {
		args <- getArgs
		_ <- ContextusCli.app.run(args.toList)
	} yield ())
		.provideSome[ZIOAppArgs](
			ContextusFileService.live,
			ContextusService.live,
			SefariaService.live,
//			ZLayer.succeed(SefariaService.Conf("http://contextus.org", "90bd24b3b028d06f")),
			ZLayer.succeed(SefariaService.Conf("http://localhost:8000", "abcdefg")),
			HttpService.live,
			HttpClientZioBackend.layer(),
		)
		.catchSome {
			case _: ValidationError => ZIO.unit
		}
