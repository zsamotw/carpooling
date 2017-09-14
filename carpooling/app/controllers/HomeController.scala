package controllers

import java.io.IOException
import javax.inject.Inject
import models._
import org.joda.time.DateTime
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc._
import scala.concurrent.Future


class HomeController @Inject()(val messagesApi: MessagesApi)  extends Controller  with I18nSupport {

  def index = Action { implicit request =>
    request.session.get("connected").map { email =>
      Ok(views.html.index("User with login: " + email + " is connected"))
    }.getOrElse {
      Ok(views.html.index("Nobody is connected"))
    }
  }

  def login = Action { implicit request =>
    Ok(views.html.login(loginForm.form))
  }

  def validateLoginAndPassword = Action { implicit request =>
    loginForm.form.bindFromRequest.fold(
      formWithError => {
        BadRequest(views.html.login(formWithError))
      },
      login => {
        if(Users.validateLogin(login)) Ok(views.html.index("You are logged with login: " + login.email)).withSession("connected" -> login.email)
        else Ok(views.html.incorrectLogin())
      }
    )
  }

  def logout = Action { implicit request =>
    Ok(views.html.index("Your session is finished. Logout")).withNewSession
  }

  def allKindergartens() = Action { implicit request =>
    val all = Kindergartens.listAll
    Ok(views.html.allkindergartens(all))
  }

  def userMenu() = Action { implicit request =>
    request.session.get("connected").map {loggedUserEmail =>
      Ok(views.html.index(s"Hallo user with login: $loggedUserEmail .You can't create more account"))
    }.getOrElse {
      val kindergartens = Kindergartens.listAll
      Ok(views.html.adduser(userForm.form, kindergartens))
    }
  }

  def addUser() = Action { implicit  request =>
    try {
      val userFromForm = userForm.form.bindFromRequest.get
      val latLon = GeoUtils.searchGeoPoint(userFromForm)
      val kindergarten = Kindergartens.find(userFromForm.kgName, userFromForm.kgStreet, userFromForm.kgNum, userFromForm.kgCity)
      val user =
        User(
          userFromForm.email,
          userFromForm.password,
          userFromForm.name,
          userFromForm.surname,
          userFromForm.street,
          userFromForm.city,
          userFromForm.seats,
          kindergarten,
          Set[String](),
          latLon._1,
          latLon._2)
      if (Users.isOnlyOne(user)) {
        val dataToDB = Users.add(user)
        MongoFactory.addUser(dataToDB)
        Ok(views.html.index(s"User ${user.name} was added. You are login")).withSession("connected" -> user.email)
      }
      else {
        Ok(views.html.index("User with this login exists"))
      }
    } catch {
      case e: IOException => Ok(views.html.index("Oooops, something wrong with address or internet connection"))
    }
  }

  def deleteUser() = Action { implicit request =>
    request.session.get("connected").map { loggedUserEmail =>
      val user = Users.findUserByEmail(loggedUserEmail)
      val dataToDB = Users.delete(user)

      MongoFactory.deleteUser(dataToDB)
      Ok(views.html.index(s"You just delete yourself user: $loggedUserEmail. We missing you like Facebook")).withNewSession
    } getOrElse {
      Ok(views.html.index("You have to login first"))
    }
  }

  def leaveGroup() = Action { implicit request =>
    request.session.get("connected").map { loggedUserEmail =>
      val dataToDB = Users.leaveGroup(loggedUserEmail)
      val message = MongoFactory.leaveGroup(dataToDB)
      Redirect(routes.HomeController.showUserPanel(message))
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
      val dataToDB = Kindergartens.add(kg)
      MongoFactory.add(dataToDB)
      Ok(views.html.index(s"Kindergarten ${kg.name} was added"))
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
      Ok(views.html.panel(user, msg, MessageForm.form))
    }.getOrElse {
      Ok(views.html.index("You have to login first"))
    }
  }

  def sendRequest(emailFromGet: String) = Action { implicit request =>
    request.session.get("connected").map { loggedUserEmail =>
      val loggedUserGroup = Users.usersFromGroup(loggedUserEmail)
      val requestedUserGroup = Users.usersFromGroup(emailFromGet)
      if(Users.areEnoughSeats(loggedUserGroup, requestedUserGroup)) {
        val dataToDB = Users.addRequest(emailFromGet, loggedUserEmail)
        MongoFactory.updateUserRequests(dataToDB)
        Redirect(routes.HomeController.showUsersFromMyKindergarten("Request was sent. Let's make peace and love"))
      }
      else  Redirect(routes.HomeController.showUsersFromMyKindergarten("Not enough seats in users's cars. Find others users"))
      }.getOrElse {
        Ok(views.html.index("You have to login first"))
      }
  }

  def replyForRequest(emailFromGet: String) = Action { implicit request =>
    request.session.get("connected").map { loggedUserEmail =>
      val userToReplyGroup = Users.usersFromGroup(emailFromGet)
      val loggedUserGroup = Users.usersFromGroup(loggedUserEmail)
      if(Users.areEnoughSeats(loggedUserGroup, userToReplyGroup)){
        val dataToDBCarpools = Users.addToCarpools(emailFromGet, loggedUserEmail)
        val dataToDBRequests = Users.deleteRequest(emailFromGet, loggedUserEmail)

        for(user <- userToReplyGroup) MongoFactory.updateUserIntDataInDB(user, "seats", loggedUserGroup.length, (x:Int, y: Int) => x - y)
        for(user <- loggedUserGroup) MongoFactory.updateUserIntDataInDB(user, "seats", userToReplyGroup.length, (x:Int, y: Int) => x - y)
        MongoFactory.updateCarpools(dataToDBCarpools)
        MongoFactory.updateUserRequests(dataToDBRequests)
        Redirect(routes.HomeController.showUserPanel("You have just replied for request. Bravo!"))
      } else Redirect(routes.HomeController.showUserPanel("Not enough seats in user's cars"))
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

  def addUserMessage = Action { implicit request =>
    request.session.get("connected").map { loggedUserEmail =>
      val user = Users.findUserByEmail(loggedUserEmail)
      val simpleUser = Users.convertToSimpleUser(user)
      MessageForm.form.bindFromRequest.fold(
        formWithErrors => {
          BadRequest(views.html.panel(user, "Fill form correctly", formWithErrors))
        },
        data => {
          val userMessage = UserMessage(
            new DateTime,
            Purpose(data.purpose),
            data.seats,
            new DateTime(data.year, data.month, data.day, data.hour, data.minutes),
            data.from,
            data.to,
            simpleUser)
          MongoFactory.add(userMessage)
          Ok(views.html.panel(user, "You message has been sent", MessageForm.form))
        }
      )

    }.getOrElse {
      Ok(views.html.index("You have to login first"))
    }
  }

  def showTimeline = Action { implicit request =>
    request.session.get("connected").map { loggedUserEmail =>
      val messStream = Messages.listAll
      val messages = messStream.take(100).toList.reverse
      Ok(views.html.timeline(messages))
    }.getOrElse {
      Ok(views.html.index("You have to login first"))
    }
  }

  def filterMessages(filterCode: String) = Action { implicit request =>
    request.session.get("connected").map { loggedUserEmail =>
      val messages = Messages.listAll.toList
      filterCode match {
        case "look-for-free-seat" =>
          val fraze = "Looking for free seat"
          val lookForFilter = Messages.purposeFilter(Purpose(fraze))
          val filteredMessages = Messages.filterTimeline(lookForFilter)(Messages.dateTimeAscending)(messages)
          Ok(views.html.timeline(filteredMessages))
        case "propose-free-seat" =>
          val fraze = "Propose free seat"
          val haveFreeFilter = Messages.purposeFilter(Purpose(fraze))
          val filteredMessages = Messages.filterTimeline(haveFreeFilter)(Messages.dateTimeDescending)(messages)
          Ok(views.html.timeline(filteredMessages))
        case "my-kindergarten" =>
          val loggedUserKindergarten = Users.findUserByEmail(loggedUserEmail).kindergarten
          val kgFilter = Messages.kindergartenFilter(loggedUserKindergarten)
          val filteredMessages = Messages.filterTimeline(kgFilter)(Messages.dateTimeAscending)(messages)
          Ok(views.html.timeline(filteredMessages))
        case "global-messages" =>
          val globalMessages = messages.collect(Messages.getGlobalMessages).sortWith(Messages.dateTimeAscending)
          Ok(views.html.timeline(globalMessages))
        case _ =>
          Ok(views.html.index("ooops something wrong with filter criteria"))
      }
    } getOrElse {
      Ok(views.html.index("You have to login first!"))
    }
  }

}

