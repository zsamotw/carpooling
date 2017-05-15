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
    Ok(views.html.index("Nobody"))
  }

  def login = Action { implicit request =>
    Ok(views.html.login(loginForm.form))
  }

  def validateLogin = Action { implicit request =>
    val login = loginForm.form.bindFromRequest.get
    if(Users.validateLogin(login)) Ok(views.html.index("Logged"))
    else Ok(views.html.incorrectLogin())
  }

  def userMenu() = Action { implicit request =>
    Ok(views.html.user(userForm.form))
    }

  def addUser() = Action {implicit  request =>
    val uForm = userForm.form.bindFromRequest.get
    val lenlon = Users.searchGeoPoint(uForm)
    val user = User(uForm.email, uForm.password, uForm.name, uForm.surname, uForm.city, uForm.street, uForm.kindergarten, lenlon._1, lenlon._2)
    Users.add(user)
      Ok(views.html.index(user.name))
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
    Ok(views.html.kindergarten(usersFrom))
  }
}

