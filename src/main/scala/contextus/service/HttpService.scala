package contextus.service

import contextus.model.DomainError.{DecodingError, HttpMethod}
import contextus.model.DomainError.IOError.HttpIOError
import zio.*
import io.circe.*
import io.circe.syntax.*

/**
 * Simple utility for http requests. Includes all functionality used by [[SefariaService]]
 */
trait HttpService:
	import HttpService.{Response, Error}

	def postForm(url: String, data: Map[String, String]): IO[Error, Response]

	def postJsonText(url: String, data: String): IO[Error, Response]

	def postJson[A: Encoder](url: String, data: A): IO[Error, Response] =
		val dataStr = data.asJson.noSpaces
		postJsonText(url, dataStr)

	def get(url: String): IO[Error, Response]
	
	def delete(url: String): IO[Error, Response]

import sttp.client3.*
import sttp.model.{Method, Uri, Part, MediaType}

object HttpService:
	type Error = HttpIOError | DecodingError

	final case class Response(status: Int, body: IO[Throwable | Unit, String])

	val live: ZLayer[SttpBackend[Task, Any], Nothing, HttpService] =
		ZLayer.fromFunction(Live.apply)

	final case class Live(
		backend: SttpBackend[Task, Any],
	) extends HttpService:
		override def postForm(url: String, data: Map[String, String]): IO[Error, Response] =
			for {
				uri <- ZIO.fromEither(Uri.parse(url))
					.mapError(msg => DecodingError(Left(Tag[Uri].tag), msg, None))

				request = basicRequest
					.post(uri)
					.body(data)
					.response(asString("UTF-8"))

				response <- backend
					.send(request)
					.mapError[HttpIOError](err => HttpIOError(uri.toString, HttpMethod.Post, err.getMessage, Some(err)))
				status = response.code.code
				body = ZIO.attempt(response.body.fold(identity, identity))
			} yield Response(status, body)

		override def postJsonText(url: String, data: String): IO[Error, Response] =
			for {
				uri <- ZIO
				  .fromEither(Uri.parse(url))
				  .mapError(msg => DecodingError(Left(Tag[Uri].tag), msg, None))

				request = basicRequest
					.post(uri)
					.body(data)
					.contentType(MediaType.ApplicationJson)
					.response(asString("UTF-8"))

				response <- backend
				  .send(request)
				  .mapError[HttpIOError](err => HttpIOError(uri.toString, HttpMethod.Post, err.getMessage, Some(err)))
				status = response.code.code
				body = ZIO.attempt(response.body.fold(identity, identity))
			} yield Response(status, body)

		override def get(url: String): IO[Error, Response] =
			for {
				uri <- ZIO
				  .fromEither(Uri.parse(url))
				  .mapError(msg => DecodingError(Left(Tag[Uri].tag), msg, None))

				request = basicRequest
					.get(uri)
					.response(asString("UTF-8"))

				response <- backend
				  .send(request)
				  .mapError[HttpIOError](err => HttpIOError(uri.toString, HttpMethod.Get, err.getMessage, Some(err)))
				status = response.code.code
				body = ZIO.attempt(response.body.fold(identity, identity))
			} yield Response(status, body)

		override def delete(url: String): IO[Error, Response] =
			for {
				uri <- ZIO
				  .fromEither(Uri.parse(url))
				  .mapError(msg => DecodingError(Left(Tag[Uri].tag), msg, None))

				request = basicRequest
					.delete(uri)
					.response(asString("UTF-8"))

				response <- backend
				  .send(request)
				  .mapError[HttpIOError](err => HttpIOError(uri.toString, HttpMethod.Delete, err.getMessage, Some(err)))
				status = response.code.code
				body = ZIO.attempt(response.body.fold(identity, identity))
			} yield Response(status, body)
