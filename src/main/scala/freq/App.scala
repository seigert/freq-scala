package freq

import java.nio.ByteBuffer

import cats.effect._
import cats.implicits._
import com.monovore.decline._
import com.monovore.decline.effect._
import fs2._
import it.unimi.dsi.fastutil.bytes.ByteArrayList

object App extends CommandIOApp("freq", "Counts '[a-zA-Z]+' words in input", version = "0.1.0") {
  private val a = 'a'.toByte
  private val z = 'z'.toByte
  private val A = 'A'.toByte
  private val Z = 'Z'.toByte

  def main: Opts[IO[ExitCode]] = Args.parse.map { args =>
    Blocker[IO]
      .use { blocker =>
        args
          .input[IO](blocker)
          .through(collect)
          .flatMap { dict =>
            Stream.fromIterator[IO](dict.drain)
          }
          .map {
            case (counter, word) => f"$counter%d $word%s%n"
          }
          .through(args.output(blocker))
          .compile
          .drain
      }
      .as(ExitCode.Success)
  }

  def collect[F[_]]: Pipe[F, ByteBuffer, FrequencyDict] = { buffers =>
    def loop(
        s: Stream[F, ByteBuffer],
        dict: FrequencyDict,
        lastHash: Int,
        lastWord: ByteArrayList
    ): Pull[F, FrequencyDict, Unit] =
      s.pull.uncons1.flatMap {
        case None =>
          if (lastWord.isEmpty) Pull.output1(dict)
          else Pull.output1(dict.register(lastHash, lastWord.toArray(new Array[Byte](lastWord.size()))))
        case Some(buffer -> nxt) =>
          var hash = lastHash
          val word = lastWord

          while (buffer.remaining() > 0) {
            if (buffer.remaining() % (1024 * 1024) == 0) {
            }

            var byte = buffer.get()
            if (a <= byte && byte <= z) {
              hash = Fnv1.next(hash, byte)
              word.add(byte)
            } else if (A <= byte && byte <= Z) {
              byte = (byte | 0x20).toByte
              hash = Fnv1.next(hash, byte)
              word.add(byte)
            } else if (!word.isEmpty) {
              dict.register(hash, word.toArray(new Array[Byte](word.size())))
              word.clear()
              hash = Fnv1.H
            }
          }

          loop(nxt, dict, hash, word)
      }

    loop(buffers, FrequencyDict(), Fnv1.H, new ByteArrayList(256)).stream
  }

}
