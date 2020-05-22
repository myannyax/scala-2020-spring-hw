import cats.effect.concurrent.MVar
import cats.effect.{ExitCode, IO, IOApp, Resource}
import scala.concurrent.duration._
import cats.syntax.all._

object Main extends IOApp {
  def runPrinter(mvar: MVar[IO, Long]): Resource[IO, Unit] = {
    def f: IO[Unit] = for {
      msg <- mvar.take
      _ <- IO(println(msg))
      _ <- f
    } yield ()

    Resource.make(f.start)(_.cancel).void
  }

  def runCounter(mvar: MVar[IO, Long]): Resource[IO, Unit] = {
    def f(counter: Long): IO[Unit] = for {
      _ <- IO.sleep(1.seconds)
      _ <- mvar.put(counter)
      _ <- f(counter + 1)
    } yield ()

    Resource.make(f(0).start)(_.cancel).void
  }

  override def run(args: List[String]): IO[ExitCode] = {
    def program: Resource[IO, Unit] = for {
      mvar <- Resource.make(MVar.empty[IO, Long])(_ => IO.unit)
      _ <- runCounter(mvar)
      _ <- runPrinter(mvar)
    } yield ()

    program.use(_ => IO.never)
  }
}
