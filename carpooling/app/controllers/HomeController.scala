package controllers

import java.io.IOException
import javax.inject.Inject
import models._
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc._
import scala.concurrent.Future


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
    request.session.get("connected").map { loggedUserEmail =>
      val user = Users.findUserByEmail(loggedUserEmail)
      val dataToDB = Users.delete(user)
      val userGroup = Users.usersFromGroup(loggedUserEmail)

      for(user <- userGroup) MongoFactory.updateUserIntDataInDB(user, "seats", 1, (x:Int, y: Int) => x + y)
      MongoFactory.deleteUser(dataToDB)
      Ok(views.html.index("You just delete yourself user: " + loggedUserEmail + " . We missing you like Facebook")).withNewSession
    } getOrElse {
      Ok(views.html.index("You have to login first"))
    }
  }

  def leaveGroup() = Action { implicit request =>
    request.session.get("connected").map { loggedUserEmail =>
      val userGroup = Users.usersFromGroup(loggedUserEmail)
      val dataToDB = Users.removeFromCarpools(loggedUserEmail)
      if(userGroup.length > 1) {
        MongoFactory.updateCarpools(dataToDB)
        for(user <- userGroup) MongoFactory.updateUserIntDataInDB(user, "seats", 1, (x:Int, y: Int) => x + y)
        Redirect(routes.HomeController.showUserPanel("You are alone....so what are you doing here?"))
      } else Redirect(routes.HomeController.showUserPanel("You are single. How you can leave youself...? Let's try to find carpooler."))
    } getOrElse {
      Ok(views.html.index("Problem with you login/email."))
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
      val loggedUserEmailOpt = request.session.get("connected")
      loggedUserEmailOpt match {
        case Some(loggedUserEmail) =>
          val loggedUser = Users.findUserByEmail(loggedUserEmail)
          val loggedUserGroup = usersFrom filter (group => group contains loggedUser)
          val restGroups = usersFrom filter(group => group != loggedUserGroup.flatten)
          Ok(views.html.showusers(kindergarten, loggedUserGroup, restGroups, "Users from search form"))
        case None => throw new NoSuchElementException
      }
    } catch {
      case e: NoSuchElementException => Ok(views.html.index("There is no such kindergarten in db or there are problems with finding users"))
    }
  }

  def showUsersFromMyKindergarten(msg: String) = Action { implicit request =>
    request.session.get("connected").map { loggedUserEmail =>
      val loggedUser = Users.findUserByEmail(loggedUserEmail)
      val kindergarten = Kindergartens.find(
        loggedUser.kindergarten.name,
        loggedUser.kindergarten.street,
        loggedUser.kindergarten.num,
        loggedUser.kindergarten.city)
      val usersFrom = Kindergartens.findUsersFromKindergarten(kindergarten)
      val loggedUserGroup = usersFrom filter (group => group contains loggedUser)
      val restGroups = usersFrom filter(group => group != loggedUserGroup.flatten)
      Ok(views.html.showusers(kindergarten, loggedUserGroup, restGroups, msg))
    } getOrElse {
      Ok(views.html.index("You have to login first"))
    }
  }

  def showUserPanel(msg: String) = Action { implicit request =>
    request.session.get("connected").map { email =>
      val user = Users.findUserByEmail(email)
      Ok(views.html.panel(user, msg))
    }.getOrElse {
      Ok(views.html.index("You have to login first"))
    }
  }

  def sendRequest(emailFromGet: String) = Action { implicit request =>
    request.session.get("connected").map { loggedUserEmail =>
      val loggedUserGroup = Users.usersFromGroup(loggedUserEmail)
      val requestedUserGroup = Users.usersFromGroup(emailFromGet)
      if(Users.areEnoughtSeats(loggedUserGroup, requestedUserGroup)) {
        val dataToDB = Users.addRequest(emailFromGet, loggedUserEmail)
        MongoFactory.updateUserRequests(dataToDB)
        Redirect(routes.HomeController.showUsersFromMyKindergarten("Request was sent. Let's make peace and love"))
      }
      else  Redirect(routes.HomeController.showUsersFromMyKindergarten("Not enought seats in users's cars. Find others users"))
      }.getOrElse {
        Ok(views.html.index("You have to login first"))
      }
  }

  def replyForRequest(emailFromGet: String) = Action { implicit request =>
    request.session.get("connected").map { loggedUserEmail =>
      val userToReplyGroup = Users.usersFromGroup(emailFromGet)
      val loggedUserGroup = Users.usersFromGroup(loggedUserEmail)
      if(Users.areEnoughtSeats(loggedUserGroup, userToReplyGroup)){
        val dataToDBCarpools = Users.addToCarpools(emailFromGet, loggedUserEmail)
        val dataToDBRequests = Users.deleteRequest(emailFromGet, loggedUserEmail)

        for(user <- userToReplyGroup) MongoFactory.updateUserIntDataInDB(user, "seats", loggedUserGroup.length, (x:Int, y: Int) => x - y)
        for(user <- loggedUserGroup) MongoFactory.updateUserIntDataInDB(user, "seats", userToReplyGroup.length, (x:Int, y: Int) => x - y)
        MongoFactory.updateCarpools(dataToDBCarpools)
        MongoFactory.updateUserRequests(dataToDBRequests)
        Redirect(routes.HomeController.showUserPanel("You have just replied for request. Bravo!"))
      } else Redirect(routes.HomeController.showUserPanel("Not enought seats in user's cars"))
    }.getOrElse {
      Ok(views.html.index("You have to login first"))
    }
  }

  def rejectRequest(emailFromGet: String) = Action { implicit request =>
    request.session.get("connected").map { loggedUserEmail =>
      val dataToDB = Users.deleteRequest(emailFromGet, loggedUserEmail)
      MongoFactory.updateUserRequests(dataToDB)
      Redirect(routes.HomeController.showUserPanel("Request rejected!!"))
    } getOrElse {
      Ok(views.html.index("You have to login first"))
    }
  }

}

