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
  val userAddress: String,
  val userEmail: String,
  val kindergartenName: String,
  val kindergartenAddress: String)

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

  //  def find(p: MessgFilter, messages: List[Message]) = messages.filter(p)
  //  val kindergartenFilter = 

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
        userAddress = messMongo.getAs[String]("useraddress").get
        userEmail = messMongo.getAs[String]("useremail").get
        kgName = messMongo.getAs[String]("kindergartenname").get
        kgAddress = messMongo.getAs[String]("kindergartenaddress").get
      } yield Message(purpose, seats, data, from, to, userName, userSurname, userAddress, userEmail, kgName, kgAddress)
    res.toList
  }

}
 
