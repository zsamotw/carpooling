package controllers

import java.io.IOException
import javax.inject.Inject

import models._
import org.joda.time.DateTime
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc._

class HomeController @Inject()(val messagesApi: MessagesApi)  extends Controller  with I18nSupport {

  lazy val loginMessage = "You can't do anything without login"

  def index() = Action { implicit request =>
    try {
      request.session.get("connected").map { loggedUserEmail =>
        val user = Users.findUserByEmail(loggedUserEmail)
        val sysMessage = s"${user.name} ${user.surname} is connected"
        Ok(views.html.index(sysMessage))
      }.getOrElse {
        Ok(views.html.index(loginMessage))
      }
    } catch {
      case e: NoSuchElementException =>
        val sysMessage = "Ooops! Problem with searching element. Check you connection with database"
        Ok(views.html.index(sysMessage))
    }
  }

  def indexWithMessage(sysMessage: String) = Action { implicit request =>
    request.session.get("connected").map { loggedUserEmail =>
      Ok(views.html.index(sysMessage))
    }.getOrElse{
      Ok(views.html.index(loginMessage))
    }
  }

  def login() = Action { implicit request =>
    try {
      request.session.get("connected").map { loggedUserEmail =>
        val user = Users.findUserByEmail(loggedUserEmail)
        val sysMessage = s"${user.name} you have just logged in. If you are not ${user.name} logout in the second!!!!"
        Ok(views.html.index(sysMessage))

      }.getOrElse {
        Ok(views.html.login(loginForm.form))
      }
    } catch {
      case e: NoSuchElementException =>
        val sysMessage = "Ooops! Problem with searching element. Check you connection with database"
        Ok(views.html.index(sysMessage))
    }
  }

  def validateLoginAndPassword() = Action { implicit request =>
    loginForm.form.bindFromRequest.fold(
      formWithError => {
        BadRequest(views.html.login(formWithError))
      },
      login => try {
        val user = Users.findUserByEmail(login.email)
        val sysMessage = s"Hello today. How are you ${user.name}?"
        if(Users.validateLogin(login)) Ok(views.html.index(sysMessage)).withSession("connected" -> login.email)
        else {
          val sysMessage = "Incorrect login or password"
          Ok(views.html.index(sysMessage))
        }
      } catch {
        case e: NoSuchElementException =>
          val sysMessage = "Incorrect user name. There isn't this user in our database"
          Ok(views.html.index(sysMessage))}
    )
  }

  def logout() = Action { implicit request =>
    val sysMessage = "Your session is finished. You are logout"
    Ok(views.html.index(sysMessage)).withNewSession
  }

  def allKindergartens() = Action { implicit request =>
    try {
      request.session.get("connected").map { loggedUserEmail =>
        val all = Kindergartens.listAll
        Ok(views.html.allkindergartens(all))
      }.getOrElse {
        Ok(views.html.index(loginMessage))
      }
    } catch {
      case e: NoSuchElementException =>
        val sysMessage = "Ooops! Problem with searching element. Check you connection with database"
        Ok(views.html.index(sysMessage))
      }
  }

  def userMenu() = Action { implicit request =>
    request.session.get("connected").map {loggedUserEmail =>
      val user = Users.findUserByEmail(loggedUserEmail)
      val sysMessage = s"Hallo user with login: ${user.name} ${user.surname}.You can't create more account"
      Ok(views.html.index(sysMessage))
    }.getOrElse {
      val kindergartens = Kindergartens.listAll
      Ok(views.html.adduser(userForm.form, kindergartens))
    }
  }

  def addUser() = Action { implicit  request =>
    try {
      userForm.form.bindFromRequest.fold(
        formWithError => {
          val kindergartens = Kindergartens.listAll
          BadRequest(views.html.adduser(formWithError, kindergartens))
        },
        userData => {
          val latLon = GeoUtils.searchGeoPoint(userData)
          val kindergarten = Kindergartens.emptyKindergarten
          val user =
            User(
              userData.email,
              userData.password,
              userData.name,
              userData.surname,
              userData.street,
              userData.city,
              userData.seats,
              kindergarten,
              Set[String](),
              latLon._1,
              latLon._2,
              false)
          if (Users.isOnlyOne(user)) {
            val dataToDB = Users.add(user)
            MongoFactory.addUser(dataToDB)
            val sysMessage = s"User: ${user.name} ${user.surname} has been added. You are login"
            Ok(views.html.index(sysMessage)).withSession("connected" -> user.email)
          }
          else {
            val sysMessage = "User with this login exists."
            Ok(views.html.index(sysMessage))
          }
        }
      )
    } catch {
      case e: IOException =>
        val sysMessage = "Oooops, something wrong with address or internet connection"
        Ok(views.html.index(sysMessage))
      case e: NoSuchElementException =>
        val sysMessage = "Ooops! Problem with finding element. Check you connection with database"
        Ok(views.html.index(sysMessage))
    }
  }

  def deleteUser() = Action { implicit request =>
    try {
      request.session.get("connected").map { loggedUserEmail =>
        val user = Users.findUserByEmail(loggedUserEmail)
        val dataToDB = Users.delete(user)

        MongoFactory.deleteUser(dataToDB)
        val sysMessage = s"${user.name}just delete yourself. We missing you like Facebook"
        Ok(views.html.index(sysMessage)).withNewSession
      } getOrElse {
        Ok(views.html.index(loginMessage))
      }
    } catch {
      case e: NoSuchElementException =>
        val sysMessage = "Ooops! Problem with finding element. Check you connection with database"
        Ok(views.html.index(sysMessage))
    }
  }

  def leaveGroup() = Action { implicit request =>
    try {
      request.session.get("connected").map { loggedUserEmail =>
        val dataToDB = Users.leaveGroup(loggedUserEmail)
        val message = MongoFactory.leaveGroup(dataToDB)
        Redirect(routes.HomeController.showUserPanel(message))
      } getOrElse {
        Ok(views.html.index(loginMessage))
      }
    } catch {
      case e: NoSuchElementException =>
        val sysMessage = "Ooops! Problem with finding element. Check you connection with database"
        Ok(views.html.index(sysMessage))
    }
  }

  def kindergartenMenu() = Action { implicit request =>
    request.session.get("connected").map { loggedUserEmail =>
      val user = Users.findUserByEmail(loggedUserEmail)
      user match {
        case u: User if(u.admin == false && Users.userEmailsGroup(u).length <= 1) =>
          Ok(views.html.addkindergarten(KindergartenForm.form))
        case _ =>
          val sysMessage = {
            if(user.admin == true) "You are admin.You can't add more kindergartens."
            else "You are linked with some people. Before create new kindergarten, first you have to leave your group in personal panel."
          }
          Ok(views.html.index(sysMessage))
      }
    }.getOrElse {
      Ok(views.html.index(loginMessage))
    }
  }

  def addKindergarten() = Action{ implicit request =>
    try {
      request.session.get("connected").map { loggedUserEmail =>
        KindergartenForm.form.bindFromRequest.fold(
          formWithError => {
            Ok(views.html.addkindergarten(formWithError))
          },
          kindergartenData => {
            val user = Users.findUserByEmail(loggedUserEmail)
            val latLon = GeoUtils.searchGeoPoint(kindergartenData)
            val usersList = List(List(loggedUserEmail)) //List[String](loggedUserEmail) :: List[List[String]]()
            val adminEmail = loggedUserEmail 
            val kgHashCode = (
              kindergartenData.name +
                kindergartenData.street +
                kindergartenData.num.toString +
                kindergartenData.city +
                adminEmail).hashCode.toString
            val kindergarten =
              Kindergarten(
                kindergartenData.name,
                kindergartenData.street,
                kindergartenData.num,
                kindergartenData.city,
                latLon._1,
                latLon._2,
                usersList,
                adminEmail,
                kgHashCode)
            val dataToDB = Kindergartens.addKindergarten(kindergarten, user)
            MongoFactory.addKindergarten(dataToDB)
            val sysMessage = s"Kindergarten ${kindergarten.name} on ${kindergarten.street} in ${kindergarten.city}was added by ${user.name} ${user.surname}"
            Ok(views.html.index(sysMessage))
          })
      }.getOrElse {
        Ok(views.html.index(loginMessage))
      }
    } catch {
      case e: IOException =>
        val sysMessage = "Oooops, something wrong with kindergarten address or internet connection"
        Ok(views.html.index(sysMessage))
      case e: NoSuchElementException =>
        val sysMessage = "Ooops! Problem with finding element. Check you connection with database"
        Ok(views.html.index(sysMessage))
    }
  }

  def addUserToKindergarten(kgHashCode: String) = Action { implicit request =>
    request.session.get("connected").map { loggedUserEmail =>
      val kindergarten = Kindergartens.find(kgHashCode)
      val user = Users.findUserByEmail(loggedUserEmail)
      (user, kindergarten) match {
        case (u: User, kg: Kindergarten) if(u.admin == false &&
            u.kindergarten.kgHashCode != kg.kgHashCode &&
            Users.userEmailsGroup(u).length <= 1) =>
          val dataToDB = Kindergartens.addUserToKindergarten(user, kindergarten)
          MongoFactory.addUserToKindergarten(dataToDB)
          val sysMessage = s"Success. You have changed your kindergarten. Your current kindergarten: ${kindergarten.name} on ${kindergarten.street} in ${kindergarten.city}"
          Redirect(routes.HomeController.indexWithMessage(sysMessage))
        case _ =>
          val sysMessage = {
            if(user.admin == true) "You are admin.You can't add more kindergartens."
            else if(user.kindergarten.kgHashCode == kindergarten.kgHashCode) "You can't add twice to the same kindergarten"
            else "You are linked with some people. First you have to leave your group in personal panel"
          }
          Redirect(routes.HomeController.indexWithMessage(sysMessage))
      }
    }.getOrElse {
      Ok(views.html.index(loginMessage))
    }
  }

  def findKindergarten() = Action { implicit request =>
    try {
      request.session.get("connected").map { loggedUserEmail =>
        val kindergartens = Kindergartens.listAll
        Ok(views.html.findusersfromkindergarten(KindergartenForm.form, kindergartens))
      }.getOrElse {
        Ok(views.html.index(loginMessage))
      }
    } catch {
      case e: NoSuchElementException =>
        val sysMessage = "Ooops! Problem with finding element. Check you connection with database"
        Ok(views.html.index(sysMessage))
    }
  }

  def showUsersFromKindergarten = Action { implicit request =>
    try {
      KindergartenForm.form.bindFromRequest.fold(
        formWithError => {
          val kindergartens = Kindergartens.listAll
          Ok(views.html.findusersfromkindergarten(formWithError, kindergartens))
        },
        kindergartenData => {
          val kindergarten = Kindergartens.find(kindergartenData.name, kindergartenData.street, kindergartenData.num, kindergartenData.city)
          val usersFrom = Kindergartens.findUsersFromKindergarten(kindergarten)
          val loggedUserEmailOpt = request.session.get("connected")
          loggedUserEmailOpt match {
            case Some(loggedUserEmail) =>
              val loggedUser = Users.findUserByEmail(loggedUserEmail)
              val loggedUserGroup = usersFrom filter (group => group contains loggedUser)
              val restGroups = usersFrom filter(group => group != loggedUserGroup.flatten)
              val sysMessage = s"All users from kinderagrten ${kindergarten.name} on ${kindergarten.street} are possible to find."
              Ok(views.html.showusers(kindergarten, loggedUserGroup, restGroups, sysMessage))
            case None => throw new NoSuchElementException
          }
        }
      )
    } catch {
      case e: NoSuchElementException =>
        val sysMessage = "There is no such kindergarten in db or there are problems with finding users"
        Ok(views.html.index(sysMessage))
    }
  }

  def showUsersFromMyKindergarten(sysMessage: String) = Action { implicit request =>
    try {
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
        Ok(views.html.showusers(kindergarten, loggedUserGroup, restGroups, sysMessage))
      } getOrElse {
        Ok(views.html.index(loginMessage))
      }
    } catch {
      case e: NoSuchElementException =>
        val sysMessage = "Ooops! Problem with finding element. Check you connection with database"
        Ok(views.html.index(sysMessage))
    }
  }

  def showUserPanel(sysMessage: String) = Action { implicit request =>
    try {
      request.session.get("connected").map { email =>
        val user = Users.findUserByEmail(email)
        Ok(views.html.panel(user, sysMessage, MessageForm.form))
      }.getOrElse {
        Ok(views.html.index(loginMessage))
      }
    } catch {
      case e: NoSuchElementException =>
        val sysMessage = "Ooops! Problem with finding element. Check you connection with database"
        Ok(views.html.index(sysMessage))
    }
  }

  def sendRequest(emailFromGet: String) = Action { implicit request =>
    try {
      request.session.get("connected").map { loggedUserEmail =>
        val loggedUserGroup = Users.usersFromGroup(loggedUserEmail)
        val requestedUserGroup = Users.usersFromGroup(emailFromGet)
        if(Users.areEnoughSeats(loggedUserGroup, requestedUserGroup)) {
          val dataToDB = Users.addRequest(emailFromGet, loggedUserEmail)
          MongoFactory.updateUserRequests(dataToDB)
          val sysMessage = "Request has been sent with success. Let's make peace and love"
          Redirect(routes.HomeController.showUsersFromMyKindergarten(sysMessage))
        } else {
          val sysMessage = "You or some users from the group don't have enough seats in cars. Find other group to join"
          Redirect(routes.HomeController.showUsersFromMyKindergarten(sysMessage))
        }
      }.getOrElse {
        Ok(views.html.index(loginMessage))
      }
    } catch {
      case e: NoSuchElementException =>
        val sysMessage = "Ooops! Problem with finding element. Check you connection with database"
        Ok(views.html.index(sysMessage))
    }
  }

  def replyForRequest(emailFromGet: String) = Action { implicit request =>
    try {
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
          val sysMessage = "You have just replied for request. Bravo!!! More peope on the group means less driving"
          Redirect(routes.HomeController.showUserPanel(sysMessage))
        } else {
          val sysMessage = "You or some users from the group don't have enough seats in cars. Find other group to join"
          Redirect(routes.HomeController.showUserPanel(sysMessage))
        }
      }.getOrElse {
        Ok(views.html.index(loginMessage))
      }
    } catch {
      case e: NoSuchElementException =>
        val sysMessage = "Ooops! Problem with finding element. Check you connection with database"
        Ok(views.html.index(sysMessage))
    }
  }

  def rejectRequest(emailFromGet: String) = Action { implicit request =>
    try {
      request.session.get("connected").map { loggedUserEmail =>
        val dataToDB = Users.deleteRequest(emailFromGet, loggedUserEmail)
        MongoFactory.updateUserRequests(dataToDB)
        val sysMessage = "Request rejected!!!"
        Redirect(routes.HomeController.showUserPanel(sysMessage))
      } getOrElse {
        Ok(views.html.index(loginMessage))
      }
    } catch {
      case e: NoSuchElementException =>
        val sysMessage = "Ooops! Problem with finding element. Check you connection with database"
        Ok(views.html.index(sysMessage))
    }
  }

  def addUserMessage = Action { implicit request =>
    try {
      request.session.get("connected").map { loggedUserEmail =>
        val user = Users.findUserByEmail(loggedUserEmail)
        val simpleUser = Users.convertToSimpleUser(user)
        MessageForm.form.bindFromRequest.fold(
          formWithErrors => {
            val sysMessage = "Fill form correctly!"
            BadRequest(views.html.panel(user, sysMessage, formWithErrors))
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
            val sysMessage = "You message has been sent!"
            Ok(views.html.panel(user, sysMessage, MessageForm.form))
          }
        )
      }.getOrElse {
        Ok(views.html.index(loginMessage))
      }
    } catch {
      case e: NoSuchElementException =>
        val sysMessage = "Ooops! Problem with finding element. Check you connection with database"
        Ok(views.html.index(sysMessage))
    }
  }

  def showTimeline = Action { implicit request =>
    try {
      request.session.get("connected").map { loggedUserEmail =>
        val messages = Messages.getAllWithTimeFilter
        val sysMessage = "Showing all messages"
        Ok(views.html.timeline(messages, sysMessage, MessageSearchForm.form))
      }.getOrElse {
        Ok(views.html.index(loginMessage))
      }
    } catch {
      case e: NoSuchElementException =>
        val sysMessage = "Ooops! Problem with finding element. Check you connection with database"
        Ok(views.html.index(sysMessage))
    }
  }

  def filterMessages() = Action { implicit request =>
    try {
      request.session.get("connected").map { loggedUserEmail =>
        val messages = Messages.getAllWithTimeFilter
        MessageSearchForm.form.bindFromRequest.fold(
          formWithErrors => {
            val sysMessage = "Fill form correctly!"
            BadRequest(views.html.timeline(messages, sysMessage, formWithErrors))
          },
          messagesSearchData => {
            val loggedUser = Users.findUserByEmail(loggedUserEmail)

            val kindFieldResult = {
              messagesSearchData.kind match {
                case "look-for-free-seats" =>
                  val filter = Messages.purposeFilter(Purpose("Looking for free seat"))
                  val sortingCriteria = Messages.dateAscending
                  val sysMessage = "Look for free setas."
                  (filter, sortingCriteria, sysMessage)
                case "propose-free-seats" =>
                  val filter = Messages.purposeFilter(Purpose("Propose free seat"))
                  val sortingCriteria = Messages.dateAscending
                  val sysMessage = "Propose free seats."
                  (filter, sortingCriteria, sysMessage)
                case "community-messages" =>
                  val filter = Messages.communityMessagesFilter
                  val sortingCriteria = Messages.creationDateTimeAscending
                  val sysMessage = "Community messages."
                  (filter, sortingCriteria, sysMessage)
                case "all" => (Messages.notFiltered, Messages.dateAscending, "All kinds of messages.")
                case _ => (Messages.notFiltered, Messages.dateAscending, "Oppps wrong filter criterium ")
              }
            }

            val areaFieldResult = {
              messagesSearchData.area match {
                case "your-kindergarten" =>
                  val filter = Messages.kindergartenFilter(loggedUser.kindergarten)
                  val sysMessage = "Messages from your kindergarten in category: "
                  (filter, sysMessage)
                case "your-city" =>
                  val filter = Messages.cityFilter(loggedUser.city)
                  val sysMessage = s"Messages from ${loggedUser.city} in category: "
                  (filter, sysMessage)
                case "all" => (Messages.notFiltered, "Messages from all kindergartens in category: ")
                case _ => (Messages.notFiltered, "Wrong area!!!")
              }
            }

            val(messagesFilter1, sysMessage1) = areaFieldResult
            val(messagesFilter2, sortingCriteria, sysMessage2) = kindFieldResult
            val finalFilter = Messages.everyFilters(messagesFilter2, messagesFilter1)
            val finalSysMessage = sysMessage1 + sysMessage2
            val finalMessages = Messages.filterTimeline(finalFilter)(sortingCriteria)(messages)
            Ok(views.html.timeline(finalMessages, finalSysMessage, MessageSearchForm.form))
          }
        )
      } getOrElse {
        Ok(views.html.index(loginMessage))
      }
    } catch {
      case e: NoSuchElementException =>
        val sysMessage = "Ooops! Problem with finding element. Check you connection with database"
        Ok(views.html.index(sysMessage))
    }
  }
}

