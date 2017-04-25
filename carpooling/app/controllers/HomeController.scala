package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import models._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

@Singleton
class HomeController @Inject() extends Controller {

  def index = Action { implicit request =>
    Ok(views.html.index())
  }

  def showUsers() = Action { implicit request =>
    Users.listAll map { users =>
      Ok(users.html.index(userForm.form, users))
    }
  }
}
