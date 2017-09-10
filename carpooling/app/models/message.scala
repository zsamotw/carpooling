package models

import com.mongodb.casbah.Imports._
import play.api.data.Form
import play.api.data.Forms._
import org.joda.time.DateTime

trait Purpose {
  val statement: String
}

case class ProposeFreeSeat(override val statement: String) extends Purpose

case class LookingForFreeSeat(override val statement: String) extends Purpose

object Purpose {
  val propose = "Propose free seat"
  val lookFor = "Looking for free seat"

  def apply(statement: String) = {
    statement match {
      case `propose` => ProposeFreeSeat(statement)
      case `lookFor` => LookingForFreeSeat(statement)
    }
  }
}

case class MessageFormData(purpose: String, seats: Int, date: Int, from: String, to: String)

trait Message {
  val dateTime: DateTime

  def >(other: Message): Boolean = this.dateTime.isAfter(other.dateTime)

  def <(other: Message): Boolean = other.dateTime.isAfter(this.dateTime)
}

case class GlobalMessage(dateTime: DateTime = new DateTime, content: String) extends Message {
  override def toString = s"${dateTime.toDate}: $content"
}

case class UserMessage(
                        dateTime: DateTime = new DateTime,
                        purpose: Purpose,
                        seats: Int,
                        date: Int,
                        from: String,
                        to: String,
                        user: SimpleUser) extends Message {

  override def toString =
    s"""Purpose: ${purpose.statement}
       | Seats: $seats
       | Date: $date
       | From: $from
       | To: $to
       | Who: ${user.name} ${user.surname} from kindergarten ${user.kgName} on ${user.kgStreet} in ${user.kgCity}
       | Contact: ${user.email}
       | Created: ${dateTime.toDate}
     """
}

object MessageForm {
  val form = Form(
    mapping(
      "purpose" -> text,
      "seats" -> number,
      "date" -> number,
      "from" -> text,
      "to" -> text
    )(MessageFormData.apply)(MessageFormData.unapply)
  )
}

object UserMessages {
  def listAll: List[UserMessage] = {
    val messages = MongoFactory.userMessages.find
    convertCursorToMessagesList(messages)
  }

  type MessgFilter = UserMessage => Boolean

  def timelineFilter(p: MessgFilter, messages: List[UserMessage]): List[UserMessage] = messages.filter(p)

  val purposeFilter: Purpose => MessgFilter = purpose => message => message.purpose == purpose

  val kindergartenFilter: KindergartenFormData => MessgFilter = kindergarten => message =>
    message.user.kgCity == kindergarten.city &&
      message.user.kgName == kindergarten.name &&
      message.user.kgStreet == kindergarten.street &&
      message.user.kgNum == kindergarten.num

  def convertCursorToMessagesList(mongoMessages: MongoCursor): List[UserMessage] = {
    val res =
      for {messMongo <- mongoMessages
        dateTime = messMongo.getAs[DateTime]("datetime").get
        purpose = messMongo.getAs[String]("purpose").get
        seats = messMongo.getAs[Int]("seats").get
        date = messMongo.getAs[Int]("date").get
        from = messMongo.getAs[String]("from").get
        to = messMongo.getAs[String]("to").get
        userEmail = messMongo.getAs[String]("useremail").get
        userName = messMongo.getAs[String]("username").get
        userSurname = messMongo.getAs[String]("usersurname").get
        userStreet = messMongo.getAs[String]("userstreet").get
        userCity = messMongo.getAs[String]("usercity").get
        kgName = messMongo.getAs[String]("kindergartenname").get
        kgStreet = messMongo.getAs[String]("kindergartenstreet").get
        kgNum = messMongo.getAs[Int]("kindergartennum").get
        kgCity = messMongo.getAs[String]("kindergartencity").get
      } yield UserMessage(
        dateTime,
        Purpose(purpose),
        seats,
        date,
        from,
        to,
        SimpleUser(userEmail, userName, userSurname, userStreet, userCity, kgName, kgStreet, kgNum, kgCity))
    res.toList
  }
}

object GlobalMessages {
  def listAll: List[GlobalMessage] = {
    val messages = MongoFactory.globalMessages.find
    convertCursorToMessagesList(messages)
  }

  def convertCursorToMessagesList(mongoMessages: MongoCursor): List[GlobalMessage] = {
    val res =
      for { messMongo <- mongoMessages
            dateTime = messMongo.getAs[DateTime]("datetime").get
            content = messMongo.getAs[String]("content").get
      } yield GlobalMessage(dateTime, content)
    res.toList
  }
}