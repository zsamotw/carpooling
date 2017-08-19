package models

import com.mongodb.casbah.Imports._
import play.api.data.Form
import play.api.data.Forms._

case class MessageFromForm(val purpose: String, val seats: Int, val data: Int, val from: String, val to: String)

case class Message(
  val purpose: String,
  val seats: Int,
  val data: Int,
  val from: String,
  val to: String,
  val userName: String,
  val userSurname: String,
  val userStreet: String,
  val userCity: String,
  val userEmail: String,
  val kgName: String,
  val kgStreet: String,
  val kgNum: Int,
  val kgCity: String)

object MessageForm {
  val form = Form(
    mapping(
      "purpose" -> text,
      "seats" -> number,
      "data" -> number,
      "from" -> text,
      "to" -> text
    )(MessageFromForm.apply)(MessageFromForm.unapply)
  )
}

object Messages {
  def listAll = {
    val messages = MongoFactory.messages.find
    convertCursorToMessagesList(messages)
  }

  type MessgFilter = Message => Boolean

  def timelineFilter(p: MessgFilter, messages: List[Message]) = messages.filter(p)

  val purposeFilter: String => MessgFilter = purpose => message => message.purpose == purpose

//  def kindergartenFilter(kindergarten: KindergartenFormData)(message: Message)

  def convertCursorToMessagesList(mongoMessages: com.mongodb.casbah.MongoCursor) = {
    val res =
      for { messMongo <- mongoMessages
        purpose = messMongo.getAs[String]("purpose").get
        seats = messMongo.getAs[Int]("seats").get
        data = messMongo.getAs[Int]("data").get
        from = messMongo.getAs[String]("from").get
        to = messMongo.getAs[String]("to").get
        userName = messMongo.getAs[String]("username").get
        userSurname = messMongo.getAs[String]("usersurname").get
        userStreet = messMongo.getAs[String]("userstreet").get
        userCity = messMongo.getAs[String]("usercity").get
        userEmail = messMongo.getAs[String]("useremail").get
        kgName = messMongo.getAs[String]("kindergartenname").get
        kgStreet = messMongo.getAs[String]("kindergartenstreet").get
        kgNum = messMongo.getAs[Int]("kindergartennum").get
        kgCity = messMongo.getAs[String]("kindergartencity").get
      } yield Message(purpose, seats, data, from, to, userName, userSurname, userStreet, userCity, userEmail, kgName, kgStreet, kgNum, kgCity)
    res.toList
  }

}
 
