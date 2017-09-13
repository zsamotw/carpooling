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

case class MessageFormData(purpose: String, seats: Int, date: Int, from: String, to: String)

trait Message {
  val dateTime: DateTime

  def >(other: Message): Boolean = this.dateTime.isAfter(other.dateTime)

  def <(other: Message): Boolean = other.dateTime.isAfter(this.dateTime)
}

case class GlobalMessage(dateTime: DateTime = new DateTime, content: String) extends Message {
  override def toString: String =
    s"""$content
       | Created: ${dateTime.toDate}""".stripMargin
}

case class UserMessage(dateTime: DateTime = new DateTime,
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

object Messages {
  def listAll: Stream[Message] = {
    val messages = MongoFactory.messages.find
    convertCursorToMessagesList(messages)
  }

  val getUserMessages: PartialFunction[Message, Message] = { case mess if mess.isInstanceOf[UserMessage] => mess }

  val getGlobalMessages: PartialFunction[Message, Message] = { case mess if mess.isInstanceOf[GlobalMessage] => mess }

  type MessagesFilter = Message => Boolean

  type MessageOrder = (Message, Message) => Boolean

  def filterTimeline(pred: MessagesFilter)(sortMethod: MessageOrder)( messages: List[Message]): List[Message] = messages filter pred sortWith sortMethod

  val purposeFilter: Purpose => MessagesFilter = purpose => {
    case message: UserMessage => message.purpose == purpose
    case _ => false
  }

  val kindergartenFilter: Kindergarten => MessagesFilter = kindergarten => {
    case message: UserMessage =>
      message.user.kgCity == kindergarten.city &&
        message.user.kgName == kindergarten.name &&
        message.user.kgStreet == kindergarten.street &&
        message.user.kgNum == kindergarten.num
    case _ => false
  }

  val dateTimeAscending: MessageOrder = _ > _

  val dateTimeDescending: MessageOrder = _ < _

  def convertCursorToMessagesList(mongoMessages: MongoCursor): Stream[Message] = {
    val res =
      for {messMongo <- mongoMessages
        purposeOpt = messMongo.getAs[String]("purpose")
        mess =
          purposeOpt match {
            case Some(pupose) =>
              val dateTime = messMongo.getAs[DateTime]("datetime").get
              val purpose = messMongo.getAs[String]("purpose").get
              val seats = messMongo.getAs[Int]("seats").get
              val date = messMongo.getAs[Int]("date").get
              val from = messMongo.getAs[String]("from").get
              val to = messMongo.getAs[String]("to").get
              val userEmail = messMongo.getAs[String]("useremail").get
              val userName = messMongo.getAs[String]("username").get
              val userSurname = messMongo.getAs[String]("usersurname").get
              val userStreet = messMongo.getAs[String]("userstreet").get
              val userCity = messMongo.getAs[String]("usercity").get
              val kgName = messMongo.getAs[String]("kindergartenname").get
              val kgStreet = messMongo.getAs[String]("kindergartenstreet").get
              val kgNum = messMongo.getAs[Int]("kindergartennum").get
              val kgCity = messMongo.getAs[String]("kindergartencity").get
              UserMessage(
                dateTime,
                Purpose(purpose),
                seats,
                date,
                from,
                to,
                SimpleUser(userEmail, userName, userSurname, userStreet, userCity, kgName, kgStreet, kgNum, kgCity))
            case None =>
              val dateTime = messMongo.getAs[DateTime]("datetime").get
              val content = messMongo.getAs[String]("content").get
              GlobalMessage(dateTime, content)
          }
      } yield mess
    res.toStream
  }
}