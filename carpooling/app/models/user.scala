package models

import play.api.data.Form
import play.api.data.Forms._
import play.api.db.slick.DatabaseConfigProvider
import play.api.Play
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import slick.driver.H2Driver.api._
import slick.driver.JdbcProfile
import scala.collection.mutable.ListBuffer
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


/*class UserTableDef(tag: Tag) extends Table[User](tag, "myDb") {
  def name = column[String] ("name")
  def surname = column[String]("surname")
  def city = column[String]("city")
  def street = column[String]("street")
  def email = column[String]("email")
  def kindergarten = column[String]("kindergarten")

  override def * = (name, surname, city, street, email, kindergarten) <> (User.tupled, User.unapply)
}*/

object Users {

  //val dbConfig = DatabaseConfigProvider.get[JdbcProfile](Play.current)

  var users = List[User]()


  def add(user: User) = {
    users = user :: users
    val kg = Kindergartens.kindergartens find (_.kName == user.kindergarten)
    kg.get.users += user
  }

  def listAll = {
    users
  }

  def findUsersFromKindergarten(kg: String) = {
    val usersFrom = users.filter(_.kindergarten == kg)
    usersFrom match {
      case Nil => (users.filter(_.kindergarten contains(kg)), "There are no kindergarten. Some Other with similar name:")
      case _ => (usersFrom, "Users from:")
    }
  }
}