package controllers

import javax.inject._
import play.api.mvc._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import models._
import scala.collection.mutable.ListBuffer

@Singleton
class HomeController extends Controller {

  def index = Action { implicit request =>
    Ok(views.html.index("")(""))
  }

  def showUsers() = Action { implicit request =>
    val users = Users.listAll
    Ok(views.html.users(users))
  }

  def addUser() = Action {implicit  request =>
    val user = userForm.form.bindFromRequest.get
    Users.add(user)
    Ok(views.html.index(user.name)(""))
  }

  def addKindergarten() = Action{implicit request =>
    val kgForm = KindergartenSearchForm.form.bindFromRequest.get
    val kg = new Kindergarten(kgForm.kName, ListBuffer[User]())
    Kindergartens.add(kg)
    Ok(views.html.index("")(kgForm.kName))
  }

  def showUsersFromKindergarten = Action {implicit request =>
    val kg = KindergartenSearchForm.form.bindFromRequest.get
    val kindergartenTuple = Kindergartens.findUsersFromKindergarten(kg.kName)
    Ok(views.html.kindergarten(kindergartenTuple))
  }
}
