package controllers

import java.io.IOException
import javax.inject.Inject
import models._
import org.joda.time.DateTime
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc._

class MessageController @Inject()(val messagesApi: MessagesApi)  extends Controller  with I18nSupport {

  lazy val loginMessage = "You can't do anything without login"

  def addUserMessage = Action { implicit request =>
    try {
      request.session.get("connected").map { loggedUserEmail =>
        val user = Users.findUserByEmail(loggedUserEmail)
        val simpleUser = Users.convertToSimpleUser(user)
        MessageForm.form.bindFromRequest.fold(
          formWithErrors => {
            val sysMessage = "Fill form correctly!"
            val messages = Messages.getAllWithTimeFilter
            BadRequest(views.html.mainboard(messages, MessageSearchForm.form, formWithErrors, sysMessage))
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
            val messages = Messages.getAllWithTimeFilter
            val sysMessage = s"You message has been sent! $userMessage"
            Ok(views.html.mainboard(messages, MessageSearchForm.form, MessageForm.form, sysMessage))
          }
        )
      }.getOrElse {
        Ok(views.html.index(loginMessage,LoginForm.form, UserForm.form))
      }
    } catch {
      case e: NoSuchElementException =>
        val sysMessage = "Ooops! Problem with finding element. Check you connection with database"
        Ok(views.html.index(sysMessage,LoginForm.form, UserForm.form))
    }
  }

  def filterMessages() = Action { implicit request =>
    try {
      request.session.get("connected").map { loggedUserEmail =>
        val messages = Messages.getAllWithTimeFilter
        MessageSearchForm.form.bindFromRequest.fold(
          formWithErrors => {
            val sysMessage = "Fill form correctly!"
            BadRequest(views.html.mainboard(messages, formWithErrors, MessageForm.form, sysMessage))
          },
          messagesSearchData => {
            val user = Users.findUserByEmail(loggedUserEmail)

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
                  val filter = Messages.kindergartenFilter(user.kindergarten)
                  val sysMessage = "Messages from your kindergarten in category: "
                  (filter, sysMessage)
                case "your-city" =>
                  val filter = Messages.cityFilter(user.city)
                  val sysMessage = s"Messages from ${user.city} in category: "
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
            Ok(views.html.mainboard(finalMessages, MessageSearchForm.form, MessageForm.form, finalSysMessage))
          }
        )
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
