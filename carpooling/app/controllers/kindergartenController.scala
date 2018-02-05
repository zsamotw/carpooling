package controllers

import java.io.IOException
import javax.inject.Inject
import models._
import org.joda.time.DateTime
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc._

class KindergartenController @Inject()(val messagesApi: MessagesApi)  extends Controller  with I18nSupport {

  lazy val loginMessage = "You can't do anything without login"

  def allKindergartens() = Action { implicit request =>
    try {
      request.session.get("connected").map { loggedUserEmail =>
        val all = Kindergartens.listAll
        Ok(views.html.allkindergartens(all))
      }.getOrElse {
        Ok(views.html.index(loginMessage,LoginForm.form, UserForm.form))
      }
    } catch {
      case e: NoSuchElementException =>
        val sysMessage = "Ooops! Problem with searching element. Check you connection with database"
        Ok(views.html.index(sysMessage,LoginForm.form, UserForm.form))
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

          val messages = Messages.getAllWithTimeFilter
          Ok(views.html.mainboard(messages, MessageSearchForm.form, MessageForm.form, sysMessage))
      }
    }.getOrElse {
      Ok(views.html.index(loginMessage,LoginForm.form, UserForm.form))
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
            val messages = Messages.getAllWithTimeFilter
            Ok(views.html.mainboard(messages, MessageSearchForm.form, MessageForm.form, sysMessage))
          })
      }.getOrElse {
        Ok(views.html.index(loginMessage,LoginForm.form, UserForm.form))
      }
    } catch {
      case e: IOException =>
        val sysMessage = "Oooops, something wrong with kindergarten address or internet connection"
        Ok(views.html.index(sysMessage,LoginForm.form, UserForm.form))
      case e: NoSuchElementException =>
        val sysMessage = "Ooops! Problem with finding element. Check you connection with database"
        Ok(views.html.index(sysMessage,LoginForm.form, UserForm.form))
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
          Redirect(routes.UserController.indexWithMessage(sysMessage))
        case _ =>
          val sysMessage = {
            if(user.admin == true) "You are admin.You can't add more kindergartens."
            else if(user.kindergarten.kgHashCode == kindergarten.kgHashCode) "You can't add twice to the same kindergarten"
            else "You are linked with some people. First you have to leave your group in personal panel"
          }
          Redirect(routes.UserController.indexWithMessage(sysMessage))
      }
    }.getOrElse {
      Ok(views.html.index(loginMessage,LoginForm.form, UserForm.form))
    }
  }

  def findKindergarten() = Action { implicit request =>
    try {
      request.session.get("connected").map { loggedUserEmail =>
        val kindergartens = Kindergartens.listAll
        Ok(views.html.findusersfromkindergarten(KindergartenForm.form, kindergartens))
      }.getOrElse {
        Ok(views.html.index(loginMessage,LoginForm.form, UserForm.form))
      }
    } catch {
      case e: NoSuchElementException =>
        val sysMessage = "Ooops! Problem with finding element. Check you connection with database"
        Ok(views.html.index(sysMessage,LoginForm.form, UserForm.form))
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
        Ok(views.html.index(sysMessage,LoginForm.form, UserForm.form))
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
        Ok(views.html.index(loginMessage,LoginForm.form, UserForm.form))
      }
    } catch {
      case e: NoSuchElementException =>
        val sysMessage = "Ooops! Problem with finding element. Check you connection with database"
        Ok(views.html.index(sysMessage,LoginForm.form, UserForm.form))
    }
  }
}
