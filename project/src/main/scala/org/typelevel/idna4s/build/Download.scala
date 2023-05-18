package org.typelevel.idna4s.build

import cats.syntax.all._
import java.net.URI
import java.nio.file.Path
import java.io.File
import java.io.InputStream
import java.io.FileOutputStream
import java.io.OutputStream
import sbt._
import scala.annotation.tailrec

object Download {

  private this final case class MultiException(a: Throwable, b: Option[MultiException])

  private def use[A, B](acquire: => Either[Throwable, A], use: A => Either[Throwable, B], release: A => Either[Throwable, Unit]): Either[Throwable, B] =
    acquire match {
      case Right(a) =>
        use(a) match {
          case Right(b) =>
            release(a).map(_ => b)
          case Left(e) =>
            release(a).flatMap(_ => Left(e))
        }
      case left => left
    }

  private def downloadInputStream(is: InputStream, location: File): Either[Throwable, Unit] = {

    @tailrec
    def go(is: InputStream, os: OutputStream, buf: Array[Byte]): Unit =
      is.read(buf) match {
        case -1 =>
          ()
        case len =>
          os.write(buf, 0, len)
      }

    if (location.exists) {
      Left(new IllegalArgumentException(s"Can not download InputStream contents to ${location}, because it already exists."))
    } else {
      val fos: FileOutputStream = new FileOutputStream(location)
      val result: Either[Throwable, Unit] =
        Either.catchNonFatal{
          go(is, fos, new Array[Byte](1024 * 1024))
        }
      Either.catchNonFatal(fos.close)
      result
    }

  def conditionalDownload(localPath: File, uri: URI): Either[Throwable, URI] =
    Either.catchNonFatal{
      if (localPath.exists) {
        localPath.toURI
      } else {
        val is: InputStream = uri.openStream
        }
      }
    }
}
