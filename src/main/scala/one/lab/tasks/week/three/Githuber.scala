package one.lab.tasks.week.three

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.Future
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods.parse

import scala.util.Failure
import scala.util.Success

object Githuber extends App {
  implicit val system: ActorSystem                        = ActorSystem("lalka")
  implicit val materializer: ActorMaterializer            = ActorMaterializer.create(system)
  implicit val executionContext: ExecutionContextExecutor = ExecutionContext.global
  implicit val defaultFormats: DefaultFormats.type        = DefaultFormats

  // TODO: поля можете добавить какие хотите
  case class GithubUser(login: String, name: String, location: Option[String], company: Option[String])
  case class GithubRepository(name: String, description: Option[String],fork: Boolean, language: String)

  //  https://api.github.com/users/{$USER}
  def getGithubUser(username: String): Future[GithubUser] = Future {
    parse(requests.get(s"https://api.github.com/users/$username").text)
    .camelizeKeys.extract[GithubUser]
  }

  def getUserRepositories(username: String): Future[List[GithubRepository]] = Future {
    parse(requests.get(s"https://api.github.com/users/$username/repos").text)
      .camelizeKeys.extract[List[GithubRepository]]
  }

  def getUserInfo(username: String): Unit = {
    getGithubUser(username).onComplete {
      case Success(gitInfo) =>
        val name = gitInfo.name
        val login = gitInfo.login
        val location = gitInfo.location
        val company = gitInfo.company

        println(s"Name: $name \nLogin: $login")
        println(s"Location: ${location.getOrElse("no info")} " +
          s"\nOrganization: ${company.getOrElse("no info")}")
      case Failure(ex) => println(ex.getMessage)
    }
    Thread.sleep(3000) // Здесь не очень получилось(

    getUserRepositories(username).onComplete {
      case Success(repositories) => repositories.foreach(repo => {
        val name = repo.name
        val language = repo.language
        val fork = repo.fork
        val description = repo.description

        println(s"Repository: $name, language: $language, ${if (fork) "forked" else "not forked"}")
        println(s"\tDescription: ${description.getOrElse("no description")}")
      })
      case Failure(ex) => println(ex.getMessage)
    }

  }

  getUserInfo("yelyaman")
}
