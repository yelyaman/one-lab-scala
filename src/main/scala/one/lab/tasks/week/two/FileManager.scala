package one.lab.tasks.week.two

import java.nio.file.{Files, Paths}
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

  def getFiles(path: String): List[String]                                       = {
    Files
      .list(Paths.get(path))
      .iterator()
      .asScala
      .filter(x => x.toFile.isFile)
      .map(x => x.toFile.getName)
      .toList
  }

  def getDirectories(path: String): List[String]                                 = {
    val listFiles = Files
      .list(Paths.get(path))
      .iterator()
      .asScala
      .filter(x => x.toFile.isDirectory)
      .map(x => x.toFile.getName)
    listFiles.toList
  }

  def getAllContent(path: String): List[String] = {
    val listFiles = Files
      .list(Paths.get(path))
      .iterator()
      .asScala
      .map(x => x.toFile.getName)
    listFiles.toList
  }

  // Only .txt files
  def createFile(currentPath: String, filename: String): Either[IncorrectFileExtensionError, String] = {
    if (filename.split('.').last == "txt"){
      val newFile = Paths.get(currentPath + s"/$filename")
      Files.createFile(newFile)
      Right(s"$filename")
    } else if (getFiles(currentPath).contains(filename))
      Left(IncorrectFileExtensionError(s"$filename already exists"))
    else Left(IncorrectFileExtensionError("The extension is incorrect"))
  }

  def createDirectory(currentPath: String, filename: String): Either[DirectoryAlreadyExistsError, String] = {
    if (getDirectories(currentPath).contains(filename)){
      Left(DirectoryAlreadyExistsError(s"$filename already exists"))
    } else {
      val newDir = Paths.get(currentPath + s"/$filename")
      Files.createDirectory(newDir)
      Right(s"$filename")
    }
  }

  def removeFile(currentPath: String, filename: String): Either[FileNotExistsError, String] = {
    if (getAllContent(currentPath).contains(filename)){
      val file = Paths.get(currentPath + s"/$filename")
      Files.delete(file)
      Right("")
    } else {
      Left(FileNotExistsError(s"$filename not exists"))
    }
  }

  def changePath(current: String, path: String): Either[ChangePathError, String] = {
    def changePathChecker(current: String, paths: List[String]): Either[ChangePathError, String] = paths match {
      case Nil => Right(current)
      case _ if current == "C:" => Right(current)
      case x :: _ if x == ".." => changePathChecker(current.split("/").init.mkString("/"), paths.tail)
      case x :: _ if getDirectories(current).contains(x) => changePathChecker(current + "/" + x, paths.tail)
      case _ => Left(ChangePathError("The system cannot find the specified path"))
    }
    if (current == "C:") Right(current)
    else changePathChecker(current, path.split("/").toList)
  }

  def parseCommand(input: List[String]): Command                                       = input.head match {
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

  def handleCommand(command: Command, currentPath: String): String        = command match {
    case ListAllContentCommand() => getAllContent(currentPath).mkString("\n")
    case ListDirectoryCommand() => getDirectories(currentPath).mkString("\n")
    case ListFilesCommand() => getFiles(currentPath).mkString("\n")
    case ShowWhereAmICommand() => currentPath
    case CreateNewFileCommand(filename) => createFile(currentPath, filename) match {
      case Right(file) => s"$currentPath/$file"
      case Left(IncorrectFileExtensionError(error)) => error
    }
    case CreateNewDirectoryCommand(filename) => createDirectory(currentPath, filename) match {
      case Right(directory) => s"$currentPath/$directory"
      case Left(DirectoryAlreadyExistsError(error)) => error
    }
    case RemoveFileCommand(filename) => removeFile(currentPath, filename) match {
      case Right(directory) => s"$currentPath/$directory"
      case Left(FileNotExistsError(error)) => error
    }
    case ChangeDirectoryCommand(destination) => changePath(currentPath, destination) match {
      case Right(x) => x
      case Left(ChangePathError(error)) => error
    }
    case PrintErrorCommand(error) => error
  }

  def main(basePath: String): Unit                                               = {
    print(s"$basePath> ")
    def innerMain(path: String, input: String): Unit = input match {
      case "" => innerMain(path, readLine())
      case "exit" => println("Session finished")
      case command =>
        val parsed = parseCommand(command.split(" ").toList)
        val result = handleCommand(parsed, path)
        parsed match {
          case _: ChangeDirectoryCommand =>
            println(s"$result> ")
            if (result == "The system cannot find the specified path") {
              print(s"$path> ")
              innerMain(path, readLine())
            }
            else innerMain(result, readLine())
          case _: CreateNewDirectoryCommand =>
            print(s"$result ")
            if (result.contains("already exists")) {
              print(s"$path> ")
              innerMain(path, readLine())
            }
            else innerMain(result, readLine())
          case _: RemoveFileCommand =>
            print(s"$result> ")
            if (result.contains("not exists")) {
              print(s"$path> ")
              innerMain(path, readLine())
            }
            else innerMain(result, readLine())
          case _ =>
            println(result)
            print(s"$path> ")
            innerMain(path, readLine())
        }
    }
    innerMain(basePath, "")
  }

  val curDirectory = System.getProperty("user.dir").split('\\').mkString("/")
  main(curDirectory)
}
