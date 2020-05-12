import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.util.stream.Collectors

import cats.instances.list._
import cats.syntax.all._
import cats.{Applicative, Id, Monad}

import scala.collection.JavaConverters.asScalaBufferConverter
import scala.language.higherKinds

trait MkDir[F[_], Dir] {
  def mkDir(dir: Dir, name: String): F[Dir]
}

trait ListDir[F[_], Dir, File] {
  def listDir(dir: Dir): F[List[File]]
}

trait MkFile[F[_], Dir, File] {
  def mkFile(dir: Dir, name: String): F[File]
}

trait Filename[F[_], File] {
  def filename(file: File): F[String]
}

trait MoveFile[F[_], Dir, File] {
  def moveFile(file: File, dir: Dir): F[File]
}

trait Printer[F[_], File] {
  def printName(file: File): F[Unit]
}

class Program[F[_], Dir, File](implicit
                               F: Monad[F],
                               mkDir: MkDir[F, Dir],
                               mkFile: MkFile[F, Dir, File],
                               printer: Printer[F, File],
                               listDir: ListDir[F, Dir, File],
                               filename: Filename[F, File],
                               moveFile: MoveFile[F, Dir, File]) {
  def run(dir: Dir): F[Unit] = for {
    testDir <- mkDir.mkDir(dir, "test_dir")
    _ <- mkFile.mkFile(testDir, "foo")
    _ <- mkFile.mkFile(testDir, "bar")
    _ <- mkFile.mkFile(testDir, "baz")
    fileList <- listDir.listDir(testDir)
    _ <- fileList.traverse(file => for {
      _ <- printer.printName(file)
      name <- filename.filename(file)
      newDir <- mkDir.mkDir(testDir, name(0).toString)
      _ <- moveFile.moveFile(file, newDir)
    } yield ())
  } yield ()
}

class RealFileSystem[F[_] : Applicative]
  extends MkDir[F, Path]
    with ListDir[F, Path, Path]
    with MkFile[F, Path, Path]
    with Filename[F, Path]
    with MoveFile[F, Path, Path] {
  override def mkDir(dir: Path, name: String): F[Path] =
    Files.createDirectories(dir.resolve(name)).pure[F]

  override def mkFile(dir: Path, name: String): F[Path] =
    Files.createFile(dir.resolve(name)).pure[F]

  override def listDir(dir: Path): F[List[Path]] =
    Files.list(dir).filter(f => Files.isRegularFile(f)).collect(Collectors.toList[Path]).asScala.toList.pure[F]

  override def filename(file: Path): F[String] =
    file.getFileName.toString.pure[F]

  override def moveFile(file: Path, dir: Path): F[Path] =
    Files.move(file, dir.resolve(file.getFileName), StandardCopyOption.REPLACE_EXISTING).pure[F]
}

class ConsolePathPrinter[F[_] : Applicative] extends Printer[F, Path] {
  override def printName(file: Path): F[Unit] = println(file.getFileName).pure[F]
}


object TypeClasses {
  def main(args: Array[String]): Unit = {
    implicit val fs: RealFileSystem[Id] = new RealFileSystem[Id]
    implicit val printer: ConsolePathPrinter[Id] = new ConsolePathPrinter[Id]
    val program = new Program[Id, Path, Path]

    program.run(Paths.get("."))
  }
}