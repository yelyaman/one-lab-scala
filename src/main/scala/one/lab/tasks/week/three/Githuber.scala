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

  case class GithubUser(login: String, name: String, location: Option[String], company: Option[String], repos_url: String)
  case class GithubRepository(name: String, description: Option[String], full_name: String, fork: Boolean, language: String)

  def getGithubUser(username: String): Future[GithubUser] =
    RestClientImpl.get(s"https://api.github.com/users/$username").map(x => parse(x).extract[GithubUser])

  def getUserRepositories(repoUrl: String): Future[List[GithubRepository]] =
    RestClientImpl.get(s"$repoUrl").map(x => parse(x).extract[List[GithubRepository]])

  def getUserInfo(username: String): Unit = {
    getGithubUser(username).flatMap(x => getUserRepositories(x.repos_url)).onComplete{
      case Success(repos) => repos.foreach(repo => {
        val userName = repo.full_name.split("/").head
        println(s"$userName has ${repos.length} repositories")
        println(s"Repository: ${repo.name}, language: ${repo.language}, ${if (repo.fork) "forked" else "not forked"}")
        println(s"\tDescription: ${repo.description.getOrElse("no description")}")
      })
      case Failure(ex) => println(ex.getMessage)
    }
  }

  getUserInfo("yelyaman")
}
