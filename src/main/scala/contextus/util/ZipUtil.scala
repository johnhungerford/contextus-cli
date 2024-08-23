package contextus.util

import java.io.{ByteArrayInputStream, FileOutputStream, InputStream}
import java.nio.file.Path
import java.util.zip.{ZipFile, ZipInputStream}
import zio.*
import zio.stream.*

object ZipUtil:
  def unzip(zipData: Array[Byte]): ZStream[Any, Nothing, (String, Chunk[Byte])] = {
    val is = new ByteArrayInputStream(zipData)
    val zis = new ZipInputStream(is)

    val effect = ZIO.attempt(zis.getNextEntry).orDie.map(Option.apply).some

    ZStream.repeatZIOOption(effect).map { entry =>
      val data = zis.readAllBytes()
      (entry.getName, Chunk.fromArray(data))
    }
  }