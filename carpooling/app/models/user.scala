package models

import play.api.Play
import play.api.data.Form
import play.api.data.Forms._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import slick.driver.JdbcProfile
import slick.driver.H2Driver.api._

import scala.concurrent.Await._
import scala.concurrent.ExecutionContext.Implicits.global


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


class UserTableDef(tag: Tag) extends Table[User](tag, "user") {
  def name = column[String] ("name")
  def surname = column[String]("surname")
  def city = column[String]("city")
  def street = column[String]("street")
  def email = column[String]("email")
  def kindergarten = column[String]("kindergarten")

  override def * = (name, surname, city, street, email, kindergarten) <> (User.tupled, User.unapply)
}

object Users {

  val db = Database.forConfig("h2meml")

  lazy val users = TableQuery[UserTableDef]

  def add(user: User): Future[String] = {
    db.run(users += user).map(res => "User added").recover {
      case ex: Exception => ex.getCause.getMessage
    }
  }

  def listAll: Future[Seq[User]] = {
    db.run(users.result)
  }

  def findUsersFromKindergarten(kg: String) = {

    Await.result(db.run(users.result), 2.seconds)
  }
}