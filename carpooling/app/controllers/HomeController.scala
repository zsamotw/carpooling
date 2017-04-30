package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import models._

@Singleton
class HomeController @Inject() extends Controller {

  def index = Action { implicit request =>
    Ok(views.html.index())
  }

  def showUsers() = Action.async { implicit request =>
    Users.listAll map { users =>
      Ok(views.html.users(users))
    }
  }

  def addUser() = Action {implicit  request =>
    val user = userForm.form.bindFromRequest.get
    Users.add(user)
    Redirect(routes.HomeController.index)
  }

  def showUserFromKindergarten(kg: String) = Action {implicit request =>
    val usersFrom = Users.findUsersFromKindergarten(kg)
    Ok(views.html.kindergarten(usersFrom))
  }
}
