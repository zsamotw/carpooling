package controllers

import play.api.mvc._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import models._

@Singleton
class HomeController extends Controller {

  def index = Action { implicit request =>
    Ok(views.html.index(""))
  }

  def showUsers() = Action.async { implicit request =>
    Users.listAll map { users =>
      Ok(views.html.users(users))
    }
  }

  def addUser() = Action.async {implicit  request =>
    val user = userForm.form.bindFromRequest.get
    Users.add(user) map { res =>
      Ok(views.html.index(user.name))
    }
  }

/*  def addKindergarten() = Action{implicit request =>
    val kgForm = KindergartenSearchForm.form.bindFromRequest.get
    val kg = new Kindergarten(kgForm.kName, ListBuffer[User]())
    Kindergartens.add(kg)
    Ok(views.html.index("")(kgForm.kName))
  }*/

  def showUsersFromKindergarten = Action {implicit request =>
    val kg = KindergartenSearchForm.form.bindFromRequest.get
    val usersFrom = Users.findUsersFromKindergarten(kg.name)
    val res = Await.result(usersFrom, 2.seconds)
      Ok(views.html.kindergarten(res))
  }
}

