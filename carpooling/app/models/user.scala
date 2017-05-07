package models

import scala.io.Source
import play.api.data.Form
import play.api.data.Forms._
import play.api.db.slick.DatabaseConfigProvider
import play.api.Play.current
import slick.driver.SQLiteDriver.api._
import slick.driver.JdbcProfile

case class User(name: String, surname: String, city: String, street: String, email: String, kindergarten: String)

object userForm {
  val form = Form (
    mapping (
      "name" -> text,
      "surname" -> text,
      "city" -> text,
      "street" -> text,
      "email" -> text,
      "kindergarten" -> text
    ) (User.apply) (User.unapply)
  )
}

class UserTableDef(tag: Tag) extends Table[User](tag, "UsersDb") {
  def name = column[String] ("name")
  def surname = column[String]("surname")
  def city = column[String]("city")
  def street = column[String]("street")
  def email = column[String]("email")
  def kindergarten = column[String]("kindergarten")

  override def * = (name, surname, city, street, email, kindergarten) <> (User.tupled, User.unapply)
}

object Users {

  val dbConfig = DatabaseConfigProvider.get[JdbcProfile](current)
  import dbConfig.driver.api._

  var users = TableQuery[UserTableDef]


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
