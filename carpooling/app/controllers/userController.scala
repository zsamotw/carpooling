package controllers

import java.io.IOException
import javax.inject.Inject
import models._
import org.joda.time.DateTime
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc._

class UserController @Inject()(val messagesApi: MessagesApi)  extends Controller  with I18nSupport {

  lazy val loginMessage = "You can't do anything without login."
  lazy val welcomeMessage = "Welcome and login!"

  def index() = Action { implicit request =>
    try {
      request.session.get("connected").map { loggedUserEmail =>
        val user = Users.findUserByEmail(loggedUserEmail)
        val sysMessage = s"${user.name} ${user.surname} is logout automaticly"
        Ok(views.html.index(sysMessage,LoginForm.form, UserForm.form)).withNewSession
      }.getOrElse {
        Ok(views.html.index(welcomeMessage,LoginForm.form, UserForm.form))
      }
    } catch {
      case e: NoSuchElementException =>
        val sysMessage = "Ooops! Problem with searching element. Check you connection with database"
        Ok(views.html.index(sysMessage,LoginForm.form, UserForm.form))
    }
  }

  // def indexWithMessage(sysMessage: String) = Action { implicit request =>
  //   request.session.get("connected").map { loggedUserEmail =>
  //     Ok(views.html.index(sysMessage,LoginForm.form, UserForm.form))
  //   }.getOrElse{
  //     Ok(views.html.index(loginMessage,LoginForm.form, UserForm.form))
  //   }
  // }

  def validateLoginAndPassword() = Action { implicit request =>
    LoginForm.form.bindFromRequest.fold(
      formWithError => {
        val sysMessage = "Fill form correctly"
        BadRequest(views.html.index(sysMessage, formWithError, UserForm.form))
      },
      login => try {
        val user = Users.findUserByEmail(login.email)
        val messages = Messages.getAllWithTimeFilter
        val sysMessage = s"Hello today. How are you ${user.name}?"
        if(Users.validateLogin(login)) Ok(views.html.mainboard(messages, MessageSearchForm.form,  MessageForm.form, sysMessage)).withSession("connected" -> login.email)
        else {
          val sysMessage = "Incorrect login or password"
          Ok(views.html.index(sysMessage,LoginForm.form, UserForm.form))
        }
      } catch {
        case e: NoSuchElementException =>
          val sysMessage = "Incorrect user name. There isn't this user in our database"
          Ok(views.html.index(sysMessage,LoginForm.form, UserForm.form))
      })
  }

  def logout() = Action { implicit request =>
    val sysMessage = "Your session is finished. You are logout"
    Ok(views.html.index(sysMessage,LoginForm.form, UserForm.form)).withNewSession
  }


  def mainBoard() = Action { implicit request =>
    try {
      request.session.get("connected").map { loggedUserEmail =>
        val messages = Messages.getAllWithTimeFilter
        val sysMessage = "You are in mainboard. Let's do great things!"
        Ok(views.html.mainboard(messages, MessageSearchForm.form, MessageForm.form, sysMessage))
      }.getOrElse {
        Ok(views.html.index(loginMessage,LoginForm.form, UserForm.form))
      }
    } catch {
      case e: NoSuchElementException =>
        val sysMessage = "Ooops! Problem with finding element. Check you connection with database"
        Ok(views.html.index(sysMessage,LoginForm.form, UserForm.form))
    }
  }

  def addUser() = Action { implicit  request =>
    try {
      UserForm.form.bindFromRequest.fold(
        formWithError => {
          val sysMessage = "Fill form correctly"
          BadRequest(views.html.index(sysMessage, LoginForm.form, formWithError))
        },
        userData => {
          val (lat, lon) = GeoUtils.searchGeoPoint(userData)
          val kindergarten = Kindergartens.initialKindergarten
          val user = Users.returnNewUser(
            userData.email,
            userData.password,
            userData.name,
            userData.surname,
            userData.street,
            userData.city,
            userData.seats,
            kindergarten,
            Set[String](),
            lat,
            lon,
            false)
          if (Users.isOnlyOne(user)) {
            val dataToDB = Users.add(user)
            MongoFactory.addUser(dataToDB)
            val sysMessage = s"User: ${user.name} ${user.surname} has been added. You are login"
            val messages = Messages.getAllWithTimeFilter
            Ok(views.html.mainboard(messages, MessageSearchForm.form, MessageForm.form,  sysMessage)).withSession("connected" -> user.email)
          }
          else {
            val sysMessage = "User with this login exists."
            Ok(views.html.index(sysMessage,LoginForm.form, UserForm.form))
          }
        }
      )
    } catch {
      case e: IOException =>
        println(e)
        val sysMessage = "Oooops, something wrong with address or internet connection"
        Ok(views.html.index(sysMessage,LoginForm.form, UserForm.form))
      case e: NoSuchElementException =>
        val sysMessage = "Ooops! Problem with finding element. Check you connection with database"
        Ok(views.html.index(sysMessage,LoginForm.form, UserForm.form))
    }
  }

  def deleteUser() = Action { implicit request =>
    try {
      request.session.get("connected").map { loggedUserEmail =>
        val user = Users.findUserByEmail(loggedUserEmail)
        val dataToDB = Users.delete(user)

        MongoFactory.deleteUser(dataToDB)
        val sysMessage = s"${user.name}just delete yourself. We missing you like Facebook"
        Ok(views.html.index(sysMessage,LoginForm.form, UserForm.form)).withNewSession
      } getOrElse {
        Ok(views.html.index(loginMessage,LoginForm.form, UserForm.form))
      }
    } catch {
      case e: NoSuchElementException =>
        val sysMessage = "Ooops! Problem with finding element. Check you connection with database"
        Ok(views.html.index(sysMessage,LoginForm.form, UserForm.form))
    }
  }

  def leaveGroup() = Action { implicit request =>
    try {
      request.session.get("connected").map { loggedUserEmail =>
        val user = Users.findUserByEmail(loggedUserEmail)
        val dataToDB = Users.leaveGroup(user)
        val message = MongoFactory.leaveGroup(dataToDB)
        Redirect(routes.UserController.showUserPanel(message))
      } getOrElse {
        Ok(views.html.index(loginMessage,LoginForm.form, UserForm.form))
      }
    } catch {
      case e: NoSuchElementException =>
        val sysMessage = "Ooops! Problem with finding element. Check you connection with database"
        Ok(views.html.index(sysMessage,LoginForm.form, UserForm.form))
    }
  }

  def showUserPanel(sysMessage: String) = Action { implicit request =>
    try {
      request.session.get("connected").map { loggedUserEmail =>
        val user = Users.findUserByEmail(loggedUserEmail)
        Ok(views.html.panel(user, sysMessage))
      }.getOrElse {
        Ok(views.html.index(loginMessage,LoginForm.form, UserForm.form))
      }
    } catch {
      case e: NoSuchElementException =>
        val sysMessage = "Ooops! Problem with finding element. Check you connection with database"
        Ok(views.html.index(sysMessage,LoginForm.form, UserForm.form))
    }
  }

  def sendRequest(emailFromGet: String) = Action { implicit request =>
    try {
      request.session.get("connected").map { loggedUserEmail =>
        val user = Users.findUserByEmail(loggedUserEmail)
        val userToSend = Users.findUserByEmail(emailFromGet)
        val loggedUserGroup = Users.usersFromGroup(user)
        val requestedUserGroup = Users.usersFromGroup(userToSend)
        if(Users.areEnoughSeats(loggedUserGroup, requestedUserGroup)) {
          val dataToDB = Users.addRequest(emailFromGet, loggedUserEmail)
          MongoFactory.updateUserRequests(dataToDB)
          val sysMessage = "Request has been sent with success. Let's make peace and love"
          Redirect(routes.KindergartenController.showUsersFromMyKindergarten(sysMessage))
        } else {
          val sysMessage = "You or some users from the group don't have enough seats in cars. Find other group to join"
          Redirect(routes.KindergartenController.showUsersFromMyKindergarten(sysMessage))
        }
      }.getOrElse {
        Ok(views.html.index(loginMessage,LoginForm.form, UserForm.form))
      }
    } catch {
      case e: NoSuchElementException =>
        val sysMessage = "Ooops! Problem with finding element. Check you connection with database"
        Ok(views.html.index(sysMessage,LoginForm.form, UserForm.form))
    }
  }

  def replyForRequest(emailFromGet: String) = Action { implicit request =>
    try {
      request.session.get("connected").map { loggedUserEmail =>
        val user = Users.findUserByEmail(loggedUserEmail)
        val userToReply = Users.findUserByEmail(emailFromGet)
        val userToReplyGroup = Users.usersFromGroup(userToReply)
        val loggedUserGroup = Users.usersFromGroup(user)
        if(Users.areEnoughSeats(loggedUserGroup, userToReplyGroup)){
          val dataToDBCarpools = Users.addToCarpools(emailFromGet, loggedUserEmail)
          val dataToDBRequests = Users.deleteRequest(emailFromGet, loggedUserEmail)

          for(user <- userToReplyGroup) MongoFactory.updateUserIntDataInDB(user, "seats", loggedUserGroup.length, (x:Int, y: Int) => x - y)
          for(user <- loggedUserGroup) MongoFactory.updateUserIntDataInDB(user, "seats", userToReplyGroup.length, (x:Int, y: Int) => x - y)
          MongoFactory.updateCarpools(dataToDBCarpools)
          MongoFactory.updateUserRequests(dataToDBRequests)
          val sysMessage = "You have just replied for request. Bravo!!! More peope on the group means less driving"
          Redirect(routes.UserController.showUserPanel(sysMessage))
        } else {
          val sysMessage = "You or some users from the group don't have enough seats in cars. Find other group to join"
          Redirect(routes.UserController.showUserPanel(sysMessage))
        }
      }.getOrElse {
        Ok(views.html.index(loginMessage,LoginForm.form, UserForm.form))
      }
    } catch {
      case e: NoSuchElementException =>
        val sysMessage = "Ooops! Problem with finding element. Check you connection with database"
        Ok(views.html.index(sysMessage,LoginForm.form, UserForm.form))
    }
  }

  def rejectRequest(emailFromGet: String) = Action { implicit request =>
    try {
      request.session.get("connected").map { loggedUserEmail =>
        val dataToDB = Users.deleteRequest(emailFromGet, loggedUserEmail)
        MongoFactory.updateUserRequests(dataToDB)
        val sysMessage = "Request rejected!!!"
        Redirect(routes.UserController.showUserPanel(sysMessage))
      } getOrElse {
        Ok(views.html.index(loginMessage,LoginForm.form, UserForm.form))
      }
    } catch {
      case e: NoSuchElementException =>
        val sysMessage = "Ooops! Problem with finding element. Check you connection with database"
        Ok(views.html.index(sysMessage,LoginForm.form, UserForm.form))
    }
  }
}

