package models

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.Source

import play.api.Play._
import play.api.data.Form
import play.api.data.Forms._
import play.api.db.slick.DatabaseConfigProvider
import play.api.libs.json._
import slick.driver.JdbcProfile
import slick.driver.SQLiteDriver.api._

/// only for branch nobd
import scala.collection.mutable.ListBuffer

case class User(email: String, password: String, name: String, surname: String, city: String, street: String, kindergarten: String, len: String, lon: String)

case class UForm(email: String, password: String, name: String, surname: String, city: String, street: String, kindergarten: String)

case class Login(email: String, password: String)

object userForm {
  val form = Form (
    mapping (
      "email" -> text,
      "password" -> text,
      "name" -> text,
      "surname" -> text,
      "city" -> text,
      "street" -> text,
      "kindergarten" -> text
    ) (UForm.apply) (UForm.unapply)
  )
}

object loginForm {
  val form = Form (
    mapping (
      "email" -> text,
      "password" -> text
    ) (Login.apply)(Login.unapply)
  )
}

/*class UserTableDef(tag: Tag) extends Table[User](tag, "UsersDb") {
  def email = column[String] ("email")
  def password = column[String] ("password")
  def name = column[String] ("name")
  def surname = column[String]("surname")
  def city = column[String]("city")
  def street = column[String]("street")
  def kindergarten = column[String]("kindergarten")

  override def * = (email, password, name, surname, city, street, kindergarten) <> (User.tupled, User.unapply)
}*/

object Users {

/*  val dbConfig = DatabaseConfigProvider.get[JdbcProfile](current)
  import dbConfig.driver.api._*/

  var users: ListBuffer[User] = ListBuffer()

  def validateLogin(login: Login): Boolean = {
    val user = users find (_.email == login.email)
    user match {
      case None => false
      case Some(u:User) => if (u.password == login.password) true else false
     }
  }

  def add(user: User) = {
    users += user
  }

  def searchGeoPoint(user: UForm) = {
    val query = "http://nominatim.openstreetmap.org/search/" + user.street + "," + user.city + ",Poland?format=json&polygon=1&addressdetails=1&limit=1"
    val res = Source.fromURL(query).mkString
    val jsonRes = Json.parse(res)
    val lat = (jsonRes \\ "lat").head.toString
    val lon = (jsonRes \\ "lon").head.toString
    (lat,lon)
  }

  def listAll = {
    for(user <- users) {
      println(user.toString)
    }
  }

  def findUsersFromKindergarten(kg: String) = {
    users filter(_.kindergarten == kg)
    }
}
