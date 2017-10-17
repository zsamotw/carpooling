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

case class CommunityMessage(creationDateTime: DateTime = new DateTime, kindergarten: Kindergarten, content: String) extends Message {
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
  def listAlltoStream: Stream[Message] = {
    val messages = MongoFactory.messages.find
    convertCursorToMessagesStream(messages)
  }

  def getAllWithTimeFilter: List[Message] = {
    val timeNow = new DateTime
    val messStream = Messages.listAlltoStream
    messStream.filter(Messages.timeFilter(timeNow)).toList.reverse
  }

  /*
   * Filters for messages
   */

  val getUserMessages: PartialFunction[Message, Message] = { case message if message.isInstanceOf[UserMessage] => message }

  val getCommunityMessages: PartialFunction[Message, Message] = { case message if message.isInstanceOf[CommunityMessage] => message }

  type MessagesFilter = Message => Boolean

  type MessageOrder = (Message, Message) => Boolean

  val timeFilter: DateTime => MessagesFilter = dateTime => {
    case message: UserMessage => message.date isAfter dateTime
    case message: CommunityMessage => true
  }


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
    case message: CommunityMessage => {
      message.kindergarten.city == kindergarten.city &&
      message.kindergarten.name == kindergarten.name &&
      message.kindergarten.street == kindergarten.street &&
      message.kindergarten.num == kindergarten.num
    }
  }

    val cityFilter: String => MessagesFilter = city => {
      case message: UserMessage => message.user.kindergarten.city == city
      case message: CommunityMessage => message.kindergarten.city == city
  }

  val communityMessagesFilter: MessagesFilter = mess => {
    mess match {
      case message: CommunityMessage => true
      case _ => false
    }
  }

  val notFiltered: MessagesFilter = message => true

  val creationDateTimeAscending: MessageOrder = _ > _

  val creationDateTimeDescending: MessageOrder = _ < _

  val dateAscending: MessageOrder = {
    case (mess1: UserMessage, mess2: UserMessage) => mess1.date.isAfter(mess2.date)
    case _ => false
  }

  val dateDescending: MessageOrder = {
    case (message1: UserMessage, message2: UserMessage) => message2.date.isAfter(message1.date)
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
            case Some(p) =>
              val dateTime = messMongo.getAs[DateTime]("datetime").get
              val purpose = messMongo.getAs[String]("purpose").get
              val seats = messMongo.getAs[Int]("seats").get
              val date = messMongo.getAs[DateTime]("date").get
              val from = messMongo.getAs[String]("from").get
              val to = messMongo.getAs[String]("to").get
              val userEmail = messMongo.getAs[String]("useremail").get
              val user = Users.findUserByEmail(userEmail)
              val simpleUser = Users.convertToSimpleUser(user)
              UserMessage(
                dateTime,
                Purpose(purpose),
                seats,
                date,
                from,
                to,
                simpleUser)
            case None =>
              val dateTime = messMongo.getAs[DateTime]("datetime").get
              val kgName = messMongo.getAs[String]("kindergartenname").get
              val kgStreet = messMongo.getAs[String]("kindergartenstreet").get
              val kgNum = messMongo.getAs[Int]("kindergartennum").get
              val kgCity = messMongo.getAs[String]("kindergartencity").get
              val content = messMongo.getAs[String]("content").get
              val kindergarten = Kindergartens.find(kgName, kgStreet, kgNum, kgCity)
              CommunityMessage(dateTime, kindergarten, content)
          }
      } yield mess
    res.toStream
  }
}
