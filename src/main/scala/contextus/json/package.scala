package contextus

import cats.Show
import contextus.model.DomainError.DecodingError
import zio.*
import io.circe
import io.circe.Decoder.Result
import io.circe.{DecodingFailure, HCursor, Json, JsonObject, ParsingFailure}
import io.circe.syntax.*
import io.circe.parser.*

import scala.deriving.Mirror

package object json:

	val Json = circe.Json
	type Json = circe.Json
	val Encoder = circe.Encoder
	type Encoder[A] = circe.Encoder[A]
	val Decoder = circe.Decoder
	type Decoder[A] = circe.Decoder[A]

	inline def deriveEncoder[A](using inline A: Mirror.Of[A]): Encoder[A] =
		io.circe.generic.semiauto.deriveEncoder
	inline def deriveDecoder[A](using inline A: Mirror.Of[A]): Decoder[A] =
		io.circe.generic.semiauto.deriveDecoder

	extension [A: Encoder](value: A)
		def encodeJson: String =
			value.asJson.noSpaces

	extension (value: String)
		def decodeJson[A: Tag](using Decoder[A]): Either[DecodingError, A] =
			parse(value).flatMap(_.as[A])
				.left.map {
					case err @ ParsingFailure(message, underlying) =>
						DecodingError(Left(Tag[A].tag), message, Some(underlying))
					case failure: DecodingFailure =>
						DecodingError(Left(Tag[A].tag), Show[DecodingFailure].show(failure), Some(failure))
				}

		def decodeJsonZIO[A: Tag](using Decoder[A]): IO[DecodingError, A] =
			ZIO.fromEither(value.decodeJson[A])

	extension [A](decoder: Decoder[A])

		def wide[B >: A](using A <:< B): Decoder[B] =
			import cats.syntax.functor._
			decoder.widen

		def contramapJson(fn: circe.Json => Decoder.Result[circe.Json]): Decoder[A] =
			new Decoder[A]:
				override def apply(c: HCursor): Result[A] =
					fn(c.value).flatMap(json => decoder.decodeJson(json))

		def contramapJsonObject(fn: circe.JsonObject => Decoder.Result[circe.JsonObject]): Decoder[A] =
			contramapJson(json => for {
				obj <- json.as[circe.JsonObject]
				updatedObj <- fn(obj)
				updatedJson = circe.Json.fromJsonObject(updatedObj)
			} yield updatedJson)

	extension [A](encoder: Encoder[A])
		def mapJson(fn: circe.Json => circe.Json): Encoder[A] =
			new Encoder[A]:
				override def apply(a: A): Json =
					fn(encoder(a))

	extension [A](objEncoder: Encoder.AsObject[A])
		def mapJsonObject(fn: circe.JsonObject => circe.JsonObject): Encoder.AsObject[A] =
			new Encoder.AsObject[A]:
				override def encodeObject(a: A): JsonObject =
					fn(objEncoder.encodeObject(a))

	extension (jsonObj: JsonObject)
		def get(path: String*): Option[Json] =
			path.toList match
				case Nil => Some(Json.fromJsonObject(jsonObj))
				case segment :: next => for {
					nextJson <- jsonObj(segment)
					result <- nextJson.get(next*)
				} yield result

	extension (json: Json)
		def get(path: String*): Option[Json] =
			path.toList match
				case Nil => Some(json)
				case segment :: next => for {
					jsonObj <- json.asObject
					nextJson <- jsonObj(segment)
					result <- nextJson.get(next*)
				} yield result

	def decodeAny[A](decoder: Decoder[A], decoders: Decoder[A]*): Decoder[A] =
		(decoder +: decoders).reduce(_ or _)

	def encodeAny[A](fn: A => Encoder[?]): Encoder[A] =
		Encoder.instance(a => {
			val encoder = fn(a).asInstanceOf[Encoder[A]]
			encoder(a)
		})
