package models

import play.api.data.Form
import play.api.data.Forms._
import play.api.db.slick.DatabaseConfigProvider
import play.api.Play._
import slick.driver.SQLiteDriver.api._
import slick.driver.JdbcProfile
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.io.Source

case class User(email: String, password: String, name: String, surname: String, city: String, street: String, kindergarten: String)

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
    ) (User.apply) (User.unapply)
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

class UserTableDef(tag: Tag) extends Table[User](tag, "UsersDb") {
  def email = column[String] ("email")
  def password = column[String] ("password")
  def name = column[String] ("name")
  def surname = column[String]("surname")
  def city = column[String]("city")
  def street = column[String]("street")
  def kindergarten = column[String]("kindergarten")

  override def * = (email, password, name, surname, city, street, kindergarten) <> (User.tupled, User.unapply)
}

object Users {

  val dbConfig = DatabaseConfigProvider.get[JdbcProfile](current)
  import dbConfig.driver.api._

  var users = TableQuery[UserTableDef]

  def validateLogin(login: Login) = {
    val userFuture = dbConfig.db.run(users.filter(_.email === login.email).result)
    val user = Await.result(userFuture, 2.seconds)(0)
    if(user.password == login.password) true else false
  }

  def add(user: User) = {
    users += user
    dbConfig.db.run(users.result)
  }

  def searchGeoPoint(user: User) = {
    val query = "http://nominatim.openstreetmap.org/search/" + user.street + "," + user.city + ", Poland?format=json&polygon=1"
    val jsonResult = Source.fromURL(query)
  }

  def listAll = {
    dbConfig.db.run(users.result)
  }

  def findUsersFromKindergarten(kg: String) = {
    dbConfig.db.run(users.filter(_.kindergarten === kg).result)
    }
}
