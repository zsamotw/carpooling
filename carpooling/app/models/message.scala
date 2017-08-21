package models

import com.mongodb.casbah.Imports._
import play.api.data.Form
import play.api.data.Forms._

trait Purpose {
  val statement: String
}

case class ProposeFreeSeat(override val statement: String) extends Purpose

case class LookingForFreeSeat(override val statement: String) extends Purpose

object Purpose {
  val propose = "Propose free seat"
  val lookfor = "Looking for free seat"

  def apply(statement: String) = {
    statement match {
      case propose => ProposeFreeSeat(statement)
      case lookfor => LookingForFreeSeat(statement)
    }
  }
}

case class MessageFormData(val purpose: String, val seats: Int, val data: Int, val from: String, val to: String)

case class Message(
  val purpose: Purpose,
  val seats: Int,
  val data: Int,
  val from: String,
  val to: String,
  val user: SimpleUser)

object MessageForm {
  val form = Form(
    mapping(
      "purpose" -> text,
      "seats" -> number,
      "data" -> number,
      "from" -> text,
      "to" -> text
    )(MessageFormData.apply)(MessageFormData.unapply)
  )
}

object Messages {
  def listAll = {
    val messages = MongoFactory.messages.find
    convertCursorToMessagesList(messages).sortWith(_.data > _.data)
  }

  type MessgFilter = Message => Boolean

  def timelineFilter(p: MessgFilter, messages: List[Message]) = messages.filter(p)

  val purposeFilter: Purpose => MessgFilter = purpose => message => message.purpose == purpose

//  def kindergartenFilter(kindergarten: KindergartenFormData)(message: Message)

  def convertCursorToMessagesList(mongoMessages: com.mongodb.casbah.MongoCursor) = {
    val res =
      for { messMongo <- mongoMessages
        purpose = messMongo.getAs[String]("purpose").get
        seats = messMongo.getAs[Int]("seats").get
        data = messMongo.getAs[Int]("data").get
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
      } yield Message(
        Purpose(purpose),
        seats,
        data,
        from,
        to,
        SimpleUser(userEmail, userName, userSurname, userStreet, userCity, kgName, kgStreet, kgNum, kgCity))
    res.toList
  }

}
 
