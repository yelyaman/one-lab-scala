package one.lab.tasks.week.two

import java.nio.file.{Files, Path, Paths}

import scala.io.StdIn._
import scala.jdk.CollectionConverters._

/**
  * Можете реализовать свою логику.
  * Главное чтобы работали команды и выводились ошибки при ошибочных действиях.
  * ll - показать все что есть в тек. папке
  * dir - показать только директории в тек. папке
  * ls - показать только файлы в тек. папке
  * cd some_folder - перейте из тек. папки в другую (учитывайте что путь можно сделать самым простым
  *                  то есть если я сейчас в папке /main и внутри main есть папка scala и я вызову
  *                  cd scala то мы должны просто перейти в папку scala. Реализация cd из текущей папки
  *                  по другому ПУТИ не требуется. Не забудьте только реализовать `cd ..`)
  *
  * Бонусные команды и идеи привествуются.
  *  Добавлены:
  *   pwd - текущая директория
  *   cat - создать txt файл
  *   mkdir - создать директорию
  *   rm - удалить директорию или файл
  */
object FileManager extends App {

  trait Command {
    def isSubstitutive: Boolean = false
  }

  case class PrintErrorCommand(error: String)             extends Command
  case class ListDirectoryCommand()                       extends Command
  case class ListFilesCommand()                           extends Command
  case class ListAllContentCommand()                      extends Command
  case class ShowWhereAmICommand()                        extends Command
  case class ContinueCommand()                            extends Command
  case class ExitCommand()                                extends Command
  case class CreateNewFileCommand(filename: String)       extends Command
  case class CreateNewDirectoryCommand(filename: String)  extends Command
  case class RemoveFileCommand(filename: String)          extends Command

  case class ChangeDirectoryCommand(destination: String)  extends Command {
    override val isSubstitutive: Boolean = true
  }

  case class ChangePathError(error: String)
  case class IncorrectFileExtensionError(error: String)
  case class DirectoryAlreadyExistsError(error: String)
  case class FileNotExistsError(error: String)
  case class BaseError(error: String)

  val filesFilter: Path => Boolean = path => path.toFile.isFile
  val directoryFilter: Path => Boolean = path => path.toFile.isDirectory

  def getDirectoryContent(path: String, func: Path => Boolean): List[String]                                       = {
    Files
      .list(Paths.get(path))
      .iterator()
      .asScala
      .filter(func)
      .map(x => x.toFile.getName)
      .toList
  }



  // Only .txt files
  def createFile(currentPath: String, filename: String): Either[IncorrectFileExtensionError, String] = {
    if (filename.split('.').last == "txt"){
      val newFile = Paths.get(currentPath + s"/$filename")
      Files.createFile(newFile)
      Right(s"$filename")
    } else if (getDirectoryContent(currentPath, filesFilter).contains(filename))
      Left(IncorrectFileExtensionError(s"$filename already exists"))
    else Left(IncorrectFileExtensionError("The extension is incorrect"))
  }

  def createDirectory(currentPath: String, filename: String): Either[DirectoryAlreadyExistsError, String] = {
    if (getDirectoryContent(currentPath, filesFilter).contains(filename)){
      Left(DirectoryAlreadyExistsError(s"'$filename' already exists"))
    } else {
      val newDir = Paths.get(currentPath + s"/$filename")
      Files.createDirectory(newDir)
      Right(s"$filename")
    }
  }

  def removeFile(current: String, filename: String): Either[FileNotExistsError, String] = {
    if (getDirectoryContent(current, directoryFilter).contains(filename) ||
      getDirectoryContent(current, filesFilter).contains(filename)){
      val file = Paths.get(current + s"/$filename")
      Files.delete(file)
      Right("")
    } else {
      Left(FileNotExistsError(s"'$filename' not exists"))
    }
  }

  def changePath(current: String, path: String): Either[ChangePathError, String] = {
    val root = System.getenv("SystemDrive")
    def changePathChecker(current: String, paths: List[String]): Either[ChangePathError, String] = paths match {
      case Nil => Right(s"$current")
      case dir :: _ if current == root && getDirectoryContent(current, directoryFilter).contains(path) =>
        changePathChecker(s"$current/$dir", paths.tail)
      case _ if current == root => Right(s"$current")
      case x :: _ if x == ".." => changePathChecker(current.split("/").init.mkString("/"), paths.tail)
      case x :: _ if getDirectoryContent(current, directoryFilter).contains(x) => changePathChecker(s"$current/$x", paths.tail)
      case _ => Left(ChangePathError("The system cannot find the specified path"))
    }
    changePathChecker(current, path.split("/").toList)
  }

  def parseCommand(input: List[String]): Command                                       = input.head match {
    case "" => ContinueCommand()
    case "exit" => ExitCommand()
    case "ll" => ListAllContentCommand()
    case "dir" => ListDirectoryCommand()
    case "ls" => ListFilesCommand()
    case "pwd" => ShowWhereAmICommand()
    case "cat" => CreateNewFileCommand(input.last)
    case "mkdir" => CreateNewDirectoryCommand(input.last)
    case "rm" => RemoveFileCommand(input.last)
    case "cd" => ChangeDirectoryCommand(input.last)
    case _ => PrintErrorCommand(s"'${input.head}' is not a command")
  }

  def handleCommand(command: Command, currentPath: String): Either[BaseError, String]        = command match {
    case ListAllContentCommand() => Right((getDirectoryContent(currentPath, directoryFilter)
      ++ getDirectoryContent(currentPath, filesFilter)).mkString("\n"))
    case ListDirectoryCommand() => Right(getDirectoryContent(currentPath, directoryFilter).mkString("\n"))
    case ListFilesCommand() => Right(getDirectoryContent(currentPath, filesFilter).mkString("\n"))
    case ShowWhereAmICommand() => Right(currentPath)
    case ContinueCommand() => Right(currentPath)
    case ExitCommand() => sys.exit()
    case CreateNewFileCommand(filename) => createFile(currentPath, filename)
      .fold(left => Left(BaseError(left.error)), right => Right(s"$currentPath/$right"))
    case CreateNewDirectoryCommand(filename) => createDirectory(currentPath, filename)
      .fold(left => Left(BaseError(left.error)), right => Right(s"$currentPath/$right"))
    case RemoveFileCommand(filename) => removeFile(currentPath, filename)
      .fold(left => Left(BaseError(left.error)), right => Right(s"$currentPath/$right"))
    case ChangeDirectoryCommand(destination) => changePath(currentPath, destination)
      .fold(left => Left(BaseError(left.error)), right => Right(right))
    case PrintErrorCommand(error) => Left(BaseError(error))
  }

    def main(basePath: String): Unit = {
      val input = readLine()
      input match {
      case command =>
        val parsed = parseCommand(command.split(" ").toList)
        val result = handleCommand(parsed, basePath)
        parsed match {
          case errorMsg: PrintErrorCommand =>
            println(errorMsg.error)
            print(s"$basePath> ")
            main(basePath)
          case _ =>
            result match {
              case Right(value) =>
                print(s"$value> ")
                main(value)
              case Left(errorMsg) =>
                println(s"${errorMsg.error}")
                print(s"$basePath> ")
                main(basePath)
            }
        }
      }
    }

  val curDirectory = System.getProperty("user.dir").split('\\').mkString("/")
  print(s"$curDirectory> ")
  main(curDirectory)
}
