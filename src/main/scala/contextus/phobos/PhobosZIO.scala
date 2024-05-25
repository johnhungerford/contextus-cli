package contextus.phobos

import contextus.model.DomainError.DecodingError
import izumi.reflect.macrortti.LightTypeTagRef.SymName.SymTypeName
import ru.tinkoff.phobos.decoding.XmlDecoder
import zio.*
import ru.tinkoff.phobos.encoding.*

trait PhobosZIO:
	extension (xmlDoc: String)
		def decodeXml[A: XmlDecoder: Tag]: Either[DecodingError, A] =
			XmlDecoder[A]
				.decode(xmlDoc)
				.left
				.map( err => {
					DecodingError(
						Left(Tag[A].tag),
						s"${err.text} -- ${err.history.mkString("->")}",
						err.cause,
					)
				})
		def decodeXmlZIO[A: XmlDecoder: Tag]: IO[DecodingError, A] =
			ZIO.fromEither(xmlDoc.decodeXml)
	
	extension [A: XmlEncoder](xmlObject: A)
		def toXml: Either[EncodingError, String] = XmlEncoder[A].encode(xmlObject)
		def toXmlPretty: Either[EncodingError, String] = XmlEncoder[A].encodePretty(xmlObject)
		def toXmlZIO: IO[EncodingError, String] = ZIO.fromEither(xmlObject.toXml)
		def toXmlPrettyZIO: IO[EncodingError, String] = ZIO.fromEither(xmlObject.toXmlPretty)

object PhobosZIO extends PhobosZIO		
