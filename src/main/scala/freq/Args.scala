package freq

import java.io._
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.Paths

import cats.data.{NonEmptyList, ValidatedNel}
import cats.effect.{Blocker, ContextShift, Sync}
import cats.implicits._
import com.monovore.decline._
import fs2._

final case class Args(in: Option[File], out: Option[File], chunkSize: Int, bufferSize: Option[Int]) {
  def input[F[_]](blocker: Blocker)(implicit F: Sync[F], CS: ContextShift[F]): Stream[F, ByteBuffer] =
    in.map { file =>
        Stream
          .bracket {
            blocker.delay(new FileInputStream(file).getChannel)
          } { channel =>
            blocker.delay(channel.close())
          }
          .flatMap { channel =>
            Stream.unfoldEval(0L -> math.min(channel.size(), Int.MaxValue.toLong)) {
              case (_, 0L) => F.pure(none[(ByteBuffer, (Long, Long))])
              case (p, sz) =>
                val size = math.min(sz, Int.MaxValue.toLong)
                blocker
                  .delay(channel.map(FileChannel.MapMode.READ_ONLY, p, size))
                  .widen[ByteBuffer]
                  .tupleRight((p + size) -> (channel.size() - p - size))
                  .map(_.some)
            }
          }
      }
      .getOrElse {
        io.stdin(chunkSize, blocker).chunks.map(_.toByteBuffer)
      }

  def output[F[_]](blocker: Blocker)(implicit F: Sync[F], CS: ContextShift[F]): Pipe[F, String, Unit] = { lines =>
    val bytes = lines.flatMap { str =>
      Stream.chunk(Chunk.array(str.getBytes(StandardCharsets.UTF_8)))
    }
    val writer = out
      .map { file =>
        val fos = F
          .catchNonFatal {
            new BufferedOutputStream(new FileOutputStream(file, false))
          }
          .widen[OutputStream]
        io.writeOutputStream(fos, blocker)
      }
      .getOrElse {
        io.stdout(blocker)
      }
    bytes.through(writer)
  }

}

object Args {
  def stringToFile(fileName: String): ValidatedNel[String, File] =
    Either
      .catchNonFatal {
        Paths.get(fileName).toFile
      }
      .toValidated
      .leftMap(e => s"Unable to open '$fileName': ${e.getMessage}")
      .toValidatedNel

  val in: Opts[Option[File]] = Opts
    .argument[String]("input.txt")
    .mapValidated {
      case "-" => none.validNel[String]
      case fnm =>
        stringToFile(fnm)
          .ensure(NonEmptyList.one(s"'$fnm' not exists"))(_.exists())
          .ensure(NonEmptyList.one(s"'$fnm' is not a file"))(_.isFile())
          .ensure(NonEmptyList.one(s"'$fnm' can not be read"))(_.canRead())
          .map(_.some)
    }

  val out: Opts[Option[File]] = Opts
    .argument[String]("output.txt")
    .mapValidated {
      case "-" => none.validNel[String]
      case fnm =>
        stringToFile(fnm)
          .ensure(NonEmptyList.one(s"'$fnm' can not be overwritten")) { f =>
            !f.exists() || f.canWrite
          }
          .map(_.some)
    }
    .withDefault(none)

  val bufferSize: Opts[Option[Int]] = Opts
    .option[Int]("buffer-size", "Length of read buffer.", short = "b")
    .orNone

  val chunkSize: Opts[Int] = Opts
    .option[Int]("chunk-size", "Length of one chunk when processing.", short = "c")
    .withDefault(16 * 1024)

  val parse: Opts[Args] = (in, out, chunkSize, bufferSize).mapN(Args(_, _, _, _))
}
