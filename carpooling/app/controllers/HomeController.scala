package controllers

import javax.inject.Inject
import models._
import play.api.mvc._
import play.api.i18n.{I18nSupport, MessagesApi}
import java.io.IOException

class HomeController @Inject()(val messagesApi: MessagesApi)  extends Controller  with I18nSupport {

  def index = Action { implicit request =>
    request.session.get("connected").map { email =>
      Ok(views.html.index("User with login:" + email + " is connected"))
    }.getOrElse {
      Ok(views.html.index("Nobody is connected"))
    }
  }

  def login = Action { implicit request =>
    Ok(views.html.login(loginForm.form))
  }

  def validateLoginAndPassword = Action { implicit request =>
    val login = loginForm.form.bindFromRequest.get
    if(Users.validateLogin(login)) Ok(views.html.index("You are logged with login: " + login.email)).withSession("connected" -> login.email)
    else Ok(views.html.incorrectLogin())
  }

  def logout = Action { implicit request =>
    Ok(views.html.index("Your session is finished. Logout")).withNewSession
  }

  def allKindergartens() = Action { implicit request =>
    val all = Kindergartens.listAll
    Ok(views.html.allkindergartens(all))
  }

  def userMenu() = Action { implicit request =>
    request.session.get("connected").map {email =>
      Ok(views.html.index("Hallo user with login: " + email + " .You can't create more account"))
    }.getOrElse {
      val kindergartens = Kindergartens.listAll
      Ok(views.html.adduser(userForm.form, kindergartens))
    }
  }

  def addUser() = Action { implicit  request =>
    try {
      val userFromForm = userForm.form.bindFromRequest.get
      val latLon = GeoUtils.searchGeoPoint(userFromForm)
      val user =
        User(
          userFromForm.email,
          userFromForm.password,
          userFromForm.name,
          userFromForm.surname,
          userFromForm.street,
          userFromForm.city,
          userFromForm.seats,
          KindergartenFormData(
            userFromForm.kgName,
            userFromForm.kgStreet,
            userFromForm.kgNum,
            userFromForm.kgCity),
          Set[String](),
          latLon._1,
          latLon._2)
      Users.isOnlyOne(user) match {
        case true => {
          val dataToDB = Users.add(user)
          MongoFactory.addUser(dataToDB)
          Ok(views.html.index("User " + user.name + " was added. You are login")).withSession("connected" -> user.email)
        }
        case false => Ok(views.html.index("User with this login exists"))
      }
    } catch {
      case e: IOException => Ok(views.html.index("Oooops, something wrong with address or internet connection"))
    }
  }

  def deleteUser() = Action { implicit request =>
    request.session.get("connected").map { email =>
      val user = Users.findUserByEmail(email)
      val dataToDB = Users.delete(user)
      MongoFactory.deleteUser(dataToDB)
      Ok(views.html.index("You just delete yourself user: " + email)).withNewSession
    } getOrElse {
      Ok(views.html.index("Problem with delete your accout"))
    }
   
  }
  def kindergartenMenu() = Action { implicit request =>
      Ok(views.html.addkindergarten(KindergartenForm.form))
  }

  def addKindergarten() = Action{ implicit request =>
    try {
      val kgFromForm = KindergartenForm.form.bindFromRequest.get
      val latLon = GeoUtils.searchGeoPoint(kgFromForm)
      val kg =
        Kindergarten(
          kgFromForm.name,
          kgFromForm.street,
          kgFromForm.num,
          kgFromForm.city,
          latLon._1,
          latLon._2,
          List[List[String]]())
      MongoFactory.add(kg)
      Ok(views.html.index("Kindergarten " + kg.name + " was added"))
    } catch {
      case e: IOException => Ok(views.html.index("Oooops, something wrong with kindergarten address or internet connection"))
    }
  }

  def findKindergarten() = Action { implicit request =>
    request.session.get("connected").map { email =>
      val kindergartens = Kindergartens.listAll
      Ok(views.html.findusersfromkindergarten(KindergartenForm.form, kindergartens))
    }.getOrElse {
      Ok(views.html.index("You have to login first"))
    }
  }

  def showUsersFromKindergarten = Action { implicit request =>
    try {
      val kgFromForm = KindergartenForm.form.bindFromRequest.get
      val kindergarten = Kindergartens.find(kgFromForm.name, kgFromForm.street, kgFromForm.num, kgFromForm.city)
      val usersFrom = Kindergartens.findUsersFromKindergarten(kindergarten)
      Ok(views.html.showusers(kindergarten, usersFrom))
    } catch {
      case e: NoSuchElementException => Ok(views.html.index("There is no such kindergarten in db"))
    }
  }

  def showUserPanel = Action { implicit request =>
    request.session.get("connected").map { email =>
      val user = Users.findUserByEmail(email)
      Ok(views.html.panel(user))
    }.getOrElse {
      Ok(views.html.index("You have to login first"))
    }
  }

  def sendRequest(emailFromGet: String) = Action { implicit request =>
    request.session.get("connected").map { loggedUserEmail =>
      val dataToDB = Users.addRequest(emailFromGet, loggedUserEmail)
      MongoFactory.updateUserRequests(dataToDB)
      Redirect(routes.HomeController.showUserPanel())
      }.getOrElse {
        Ok(views.html.index("You have to login first"))
      }
  }

  def replyForRequest(emailFromGet: String) = Action { implicit request =>
    request.session.get("connected").map { loggedUserEmail =>
      val dataToDBCarpools = Users.addToCarpools(emailFromGet, loggedUserEmail)
      val dataToDBRequests = Users.deleteRequest(emailFromGet, loggedUserEmail)
      MongoFactory.updateCarpools(dataToDBCarpools)
      MongoFactory.updateUserRequests(dataToDBRequests)
      Redirect(routes.HomeController.index())
    }.getOrElse {
      Ok(views.html.index("You have to login first"))
    }
  }

  def rejectRequest(emailFromGet: String) = Action { implicit request =>
    request.session.get("connected").map { loggedUserEmail =>
      val dataToDB = Users.deleteRequest(emailFromGet, loggedUserEmail)
      MongoFactory.updateUserRequests(dataToDB)
      Redirect(routes.HomeController.showUserPanel())
    } getOrElse {
      Ok(views.html.index("You have to login first"))
    }
  }

}

