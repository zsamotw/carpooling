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

  def apply(statement: String): Purpose = {
    statement match {
      case `propose` => ProposeFreeSeat(statement)
      case `lookFor` => LookingForFreeSeat(statement)
    }
  }
}

/**
  * Classes for forms -> 1) form for creating message
  *                      2) form for searching messages
  */

case class MessageFormData(
  purpose: String,
  seats: Int,
  year: Int,
  month: Int,
  day: Int,
  hour: Int,
  minutes: Int,
  from: String,
  to: String)

object MessageForm {
  val form = Form(
    mapping(
      "purpose" -> text,
      "seats" -> number,
      "year" -> number,
      "month" -> number,
      "day" -> number,
      "hour" -> number,
      "minutes" -> number,
      "from" -> text,
      "to" -> text
    )(MessageFormData.apply)(MessageFormData.unapply)
  )
}

case class MessageSearchFormData(kind: String, area: String)

object MessageSearchForm {
  val form = Form(
    mapping(
      "kind" -> text,
      "area" -> text
    )(MessageSearchFormData.apply)(MessageSearchFormData.unapply)
  )
}

/**
  * Trait Message and two subclasses: GlobalMessage and UserMessage
  */

trait Message {

  val creationDateTime: DateTime

  def >(other: Message): Boolean = this.creationDateTime.isAfter(other.creationDateTime)

  def <(other: Message): Boolean = other.creationDateTime.isAfter(this.creationDateTime)
}

case class GlobalMessage(creationDateTime: DateTime = new DateTime, kindergarten: Kindergarten, content: String) extends Message {
  override def toString: String =
    s"""$content
       | Created: ${creationDateTime.toDate}""".stripMargin
}

case class UserMessage(
  creationDateTime: DateTime = new DateTime,
  purpose: Purpose,
  seats: Int,
  date: DateTime,
  from: String,
  to: String,
  user: SimpleUser) extends Message {
  override def toString =
    s"""Purpose: ${purpose.statement}
       | Seats: $seats
       | Date: $date
       | From: $from
       | To: $to
       | Who: ${user.name} ${user.surname} from kindergarten ${user.kindergarten.name} on ${user.kindergarten.street} in ${user.kindergarten.city}
       | Contact: ${user.email}
       | Created: ${creationDateTime.toDate}
     """.stripMargin
}

object Messages {
  def listAll: Stream[Message] = {
    val messages = MongoFactory.messages.find
    convertCursorToMessagesStream(messages)
  }

  /*
   * Filters for messages
   */

  val getUserMessages: PartialFunction[Message, Message] = { case mess if mess.isInstanceOf[UserMessage] => mess }

  val getGlobalMessages: PartialFunction[Message, Message] = { case mess if mess.isInstanceOf[GlobalMessage] => mess }

  type MessagesFilter = Message => Boolean

  type MessageOrder = (Message, Message) => Boolean

  val purposeFilter: Purpose => MessagesFilter = purpose => {
    case message: UserMessage => message.purpose == purpose
    case _ => false
  }

  val kindergartenFilter: Kindergarten => MessagesFilter = kindergarten => {
    case message: UserMessage => {
      message.user.kindergarten.city == kindergarten.city &&
      message.user.kindergarten.name == kindergarten.name &&
      message.user.kindergarten.street == kindergarten.street &&
      message.user.kindergarten.num == kindergarten.num
    }
    case message: GlobalMessage => {
      message.kindergarten.city == kindergarten.city &&
      message.kindergarten.name == kindergarten.name &&
      message.kindergarten.street == kindergarten.street &&
      message.kindergarten.num == kindergarten.num
    }
  }

  val globalMessagesFilter: MessagesFilter = mess => {
    mess match {
      case m: GlobalMessage => true
      case _ => false
    }
  }

  val notFiltered: MessagesFilter = mess => true

  val creationDateTimeAscending: MessageOrder = _ > _

  val creationDateTimeDescending: MessageOrder = _ < _

  val dateAscending: MessageOrder = {
    case (mess1: UserMessage, mess2: UserMessage) => mess1.date.isAfter(mess2.date)
    case _ => false
  }

  val dateDescending: MessageOrder = {
    case (mess1: UserMessage, mess2: UserMessage) => mess2.date.isAfter(mess1.date)
    case _ => false
  }

  def everyFilters(filters: MessagesFilter*): MessagesFilter = message => filters forall(filter => filter(message))

  def filterTimeline(pred: MessagesFilter)(sortMethod: MessageOrder)( messages: List[Message]): List[Message] = messages filter pred sortWith sortMethod

  def convertCursorToMessagesStream(mongoMessages: MongoCursor): Stream[Message] = {
    val res =
      for {messMongo <- mongoMessages
        purposeOpt = messMongo.getAs[String]("purpose")
        mess =
          purposeOpt match {
            case Some(pupose) =>
              val dateTime = messMongo.getAs[DateTime]("datetime").get
              val purpose = messMongo.getAs[String]("purpose").get
              val seats = messMongo.getAs[Int]("seats").get
              val date = messMongo.getAs[DateTime]("date").get
              val from = messMongo.getAs[String]("from").get
              val to = messMongo.getAs[String]("to").get
              val userEmail = messMongo.getAs[String]("useremail").get
              val userName = messMongo.getAs[String]("username").get
              val userSurname = messMongo.getAs[String]("usersurname").get
              val userStreet = messMongo.getAs[String]("userstreet").get
              val userCity = messMongo.getAs[String]("usercity").get
              val userSeats = messMongo.getAs[Int]("userseats").get
              val userLen = messMongo.getAs[String]("userlen").get
              val userLon = messMongo.getAs[String]("userlon").get
              val kgName = messMongo.getAs[String]("kindergartenname").get
              val kgStreet = messMongo.getAs[String]("kindergartenstreet").get
              val kgNum = messMongo.getAs[Int]("kindergartennum").get
              val kgCity = messMongo.getAs[String]("kindergartencity").get
              val kgLen = messMongo.getAs[String]("kindergartenlen").get
              val kgLon = messMongo.getAs[String]("kindergartenlon").get
              val kgUserEmails = messMongo.getAs[List[List[String]]]("kindergartenusersemails").get
              UserMessage(
                dateTime,
                Purpose(purpose),
                seats,
                date,
                from,
                to,
                SimpleUser(
                  userEmail,
                  userName,
                  userSurname,
                  userStreet,
                  userCity,
                  userSeats,
                  userLen,
                  userLon,
                  Kindergarten(kgName, kgStreet, kgNum, kgCity, kgLen,kgLon, kgUserEmails)))
            case None =>
              val dateTime = messMongo.getAs[DateTime]("datetime").get
              val kgName = messMongo.getAs[String]("kindergartenname").get
              val kgStreet = messMongo.getAs[String]("kindergartenstreet").get
              val kgNum = messMongo.getAs[Int]("kindergartennum").get
              val kgCity = messMongo.getAs[String]("kindergartencity").get
              val kgLen = messMongo.getAs[String]("kindergartenlen").get
              val kgLon = messMongo.getAs[String]("kindergartenlon").get
              val kgUserEmails = messMongo.getAs[List[List[String]]]("kindergartenusersemails").get
              val content = messMongo.getAs[String]("content").get
              GlobalMessage(dateTime, Kindergarten(kgName, kgStreet, kgNum, kgCity, kgLen, kgLon, kgUserEmails), content)
          }
      } yield mess
    res.toStream
  }
}
