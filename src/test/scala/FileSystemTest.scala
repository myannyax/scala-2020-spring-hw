import java.nio.file.{Files, Path}
import java.util.stream.Collectors

import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.JavaConverters.asScalaBufferConverter

class FileSystemTest extends AnyFlatSpec with Matchers {

  trait Env {
    implicit val fs: RealFileSystem[Id] = new RealFileSystem[Id]
    implicit val printer: ConsolePathPrinter[Id] = new ConsolePathPrinter[Id]
    val program = new Program[Id, Path, Path]
    val directory: Path = Files.createTempDirectory("lab8")
    program.run(directory)
  }

  "Program" should "work?" in new Env {
    assert(Files.exists(directory.resolve("test_dir")))

    val expected: List[Path] = List(
      directory.resolve("test_dir"),
      directory.resolve("test_dir/b"),
      directory.resolve("test_dir/b/bar"),
      directory.resolve("test_dir/b/baz"),
      directory.resolve("test_dir/f"),
      directory.resolve("test_dir/f/foo"),
    )
    val actual: List[Path] = Files.walk(directory.resolve("test_dir")).sorted().collect(Collectors.toList[Path]).asScala.toList
    assert(actual == expected)
  }
}
