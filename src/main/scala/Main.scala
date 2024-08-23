import contextus.cli.ContextusCli
import contextus.model.DomainError.DecodingError
import contextus.service.{CachingService, ConfigurationService, ContextusFileService, ContextusService, HttpService, SefariaService, UpdateService, XmlTextProcessingService}
import sttp.client3.httpclient.zio.HttpClientZioBackend
import zio.*
import zio.cli.*

import java.net.http.HttpClient
import java.nio.file.Path


object Main extends ZIOAppDefault:
	override def run = (for {
		args <- getArgs
		_ <- ContextusCli.app.run(args.toList)
	} yield ())
		.provideSome[ZIOAppArgs](
			ContextusFileService.live,
			ContextusService.live,
			SefariaService.live,
			XmlTextProcessingService.live,
			ConfigurationService.live,
			SefariaService.Conf.live,
//			ZLayer.succeed(SefariaService.Conf("http://localhost:8000", "abcdefg")),
			CachingService.live,
			HttpService.live,
			HttpClientZioBackend.layerUsingClient(HttpClient.newBuilder().followRedirects(HttpClient.Redirect.ALWAYS).build()),
			UpdateService.live,
		)
