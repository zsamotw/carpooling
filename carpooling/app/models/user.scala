package models

import com.mongodb.casbah.Imports._
import play.api.Play._
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json._
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.Source

case class User(email: String, password: String, name: String, surname: String, city: String, street: String, kindergarten: String, len: String, lon: String)

case class UserFormData(email: String, password: String, name: String, surname: String, city: String, street: String, kindergarten: String)

case class Login(email: String, password: String)

object userForm {
  val form = Form(
    mapping(
      "email" -> text,
      "password" -> text,
      "name" -> text,
      "surname" -> text,
      "city" -> text,
      "street" -> text,
      "kindergarten" -> text)(UserFormData.apply)(UserFormData.unapply))
}

object loginForm {
  val form = Form(
    mapping(
      "email" -> text,
      "password" -> text)(Login.apply)(Login.unapply))
}

object Users {

  def validateLogin(login: Login): Boolean = {
    val userOpt = MongoFactory.users.findOne(MongoDBObject("email" -> login.email))
    userOpt match {
      case None => false
      case Some(u) => if (u.getAs[String]("password").get == login.password) true else false
    }
  }

  def add(user: User) = {
    MongoFactory.users += MongoFactory.buildMongoDbUser(user)
  }

  def listAll = {
    val allUsers = MongoFactory.users.find
    convertCursorToList(allUsers)
  }

  def findUsersFromKindergarten(kg: String) = {
    val usersFrom = MongoFactory.users.find("kindergarten" $eq kg)
    convertCursorToList(usersFrom)
  }

  def convertCursorToList(MongoUsers: com.mongodb.casbah.MongoCursor) = {
    val res =
      for {userMongo <- MongoUsers
      val email = userMongo.getAs[String]("email").get
      val password = userMongo.getAs[String]("password").get
      val name = userMongo.getAs[String]("name").get
      val surname = userMongo.getAs[String]("surname").get
      val street = userMongo.getAs[String]("street").get
      val city = userMongo.getAs[String]("city").get
      val kindergarten = userMongo.getAs[String]("kindergarten").get
      val lat = userMongo.getAs[String]("len").get
      val lon = userMongo.getAs[String]("lon").get
    } yield new User(email, password, name, surname, street, city, kindergarten, lat, lon)
    res.toList
  }
}
