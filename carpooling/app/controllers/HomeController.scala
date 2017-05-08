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
    Ok(views.html.index("Not logged"))
  }

  def login = Action { implicit request =>
    Ok(views.html.login(loginForm.form))
  }

  def validateLogin = Action { implicit request =>
    val login = loginForm.form.bindFromRequest.get
    if(Users.validateLogin(login)) Ok(views.html.index("Logged"))
    else Ok(views.html.incorrectLogin())
  }

  def userMenu() = Action{ implicit request =>
    Ok(views.html.user(userForm.form))
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

