package controllers

import javax.inject.Inject
import models._
import play.api.mvc._
import play.api.i18n.{I18nSupport, MessagesApi}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._


class HomeController @Inject()(val messagesApi: MessagesApi)  extends Controller  with I18nSupport {

  def index = Action { implicit request =>
    request.session.get("connected").map { user =>
      Ok(views.html.index(user + " is connected"))
    }.getOrElse{
      Ok(views.html.index("Nobody is connected"))
    }
  }

  def login = Action { implicit request =>
    Ok(views.html.login(loginForm.form))
  }

  def validateLogin = Action { implicit request =>
    val login = loginForm.form.bindFromRequest.get
    if(Users.validateLogin(login)) Ok(views.html.index("You are logged with login: " + login.email)).withSession("connected" -> login.email)
    else Ok(views.html.incorrectLogin())
  }

  def userMenu() = Action { implicit request =>
    Ok(views.html.user(userForm.form))
    }

  def addUser() = Action {implicit  request =>
    val uForm = userForm.form.bindFromRequest.get
    val lenlon = Utils.searchGeoPoint(uForm)
    val user = User(uForm.email, uForm.password, uForm.name, uForm.surname, uForm.city, uForm.street, uForm.kindergarten, lenlon._1, lenlon._2)
    Users.add(user)
    Ok(views.html.index("User " + user.name + " was added"))
  }

  def kindergartenMenu() = Action { implicit request =>
    Ok(views.html.addkindergarten(KindergartenForm.form))
  }

  def addKindergarten() = Action{ implicit request =>
    val kgFromForm = KindergartenForm.form.bindFromRequest.get
    val kg = Kindergarten(kgFromForm.name, kgFromForm.street, kgFromForm.num, kgFromForm.city, "len", "lon")
    Kindergartens.add(kg)
    Ok(views.html.index("Kindergarten " + kg.name + " was added"))
  }

  def findKindergarten() = Action { implicit request =>
    Ok(views.html.findparentsfromkindergarten(KindergartenForm.form))
  }

  def showUsersFromKindergarten = Action {implicit request =>
    try {
    val kgFromForm = KindergartenForm.form.bindFromRequest.get
    val lenlon = Utils.searchGeoPoint(kgFromForm)
    val kg = Kindergarten(kgFromForm.name, kgFromForm.street, kgFromForm.num, kgFromForm.city, lenlon._1, lenlon._2)
    val usersFrom = Users.findUsersFromKindergarten(kgFromForm.name)
      Ok(views.html.parents(kg, usersFrom))
    } catch {
      case e: NoSuchElementException => Ok(views.html.index("There is no such kindergarten in db"))
    }
  }
}

