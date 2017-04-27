package models

import play.api.Play
import play.api.data.Form
import play.api.data.Forms._
//import play.api.db.slick.DatabaseConfigProvider
import slick.driver.JdbcProfile
import slick.driver.H2Driver.api._
import scala.concurrent.ExecutionContext.Implicits.global


case class User(name: String, surname: String, city: String, street: String, email: String, kindergarten: String)

case class UserFormData(name: String, surname: String, city: String, street: String, email: String, kindergarten: String)

object userForm {
  val form = Form (
    mapping (
      "name" -> text,
      "surname" -> text,
      "city" -> text,
      "street" -> text,
      "email" -> text,
      "kindergarten" -> text
    ) (UserFormData.apply) (UserFormData.unapply)
  )
}

class UserTableDef(tag: Tag) extends Table[User](tag, "user") {
  def id = column[Long] ("id", O.PrimaryKey, O.AutoInc)
  def name = column[String] ("name")
  def surname = column[String]("surname")
  def city = column[String]("city")
  def street = column[String]("street")
  def email = column[String]("email")
  def kindergarten = column[String]("kindergarten")
}

object Users {

  val h2meml = {
    url = "jdbc:h2:mem:test1"
    driver = org.h2.Driver
    connectionPopl = disabled
    keepAliveConnection = true
  }

  val db = Database.forConfig("h2meml")

  val users = TableQuery[UserTableDef]

  def add(user: User) = {
    db.run(users += user).map(res => "User added").recover {
      case ex: Exception => ex.getCause.getMessage
    }
  }

  def listAll = {
    db.run(users.result)
  }
}