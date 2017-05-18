//Don't work yet


package models

import play.api.Play
import play.api.data.Form
import play.api.data.Forms._
import scala.concurrent.Future
import slick.driver.JdbcProfile
import slick.driver.H2Driver.api._
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global

case class Kindergarten(name: String, street: String, num: Int, city: String, len: String, lon: String)

case class KindergartenFormData(name: String, street: String, num: Int, city: String)

/*class KindergartenTableDef(tag: Tag) extends Table[Kindergarten](tag, "myDb") {

  def name = column[String] ("name")
  override def * = (name) <> (Kindergarten.tupled, Kindergarten.unapply)
}*/

object KindergartenForm {

  val form = Form (
    mapping (
      "name" -> text,
      "street" -> text,
      "num" -> number,
      "city" -> text
    ) (KindergartenFormData.apply) (KindergartenFormData.unapply)
  )
}

object Kindergartens {

  var kindergartens = List[Kindergarten]()

  def add(kindergarten: Kindergarten) = {
    kindergartens = kindergarten :: kindergartens
  }

  def listAll = kindergartens

  // def findUsersFromKindergarten(kName: String) = {
  //   val kindergarten = kindergartens filter(_.kName == kName)
  //   kindergarten match {
  //     case Nil => (kindergartens filter(_.kName contains(kName)), "No kindergarten. Some similar:")
  //     case _ => (kindergarten, "Users from:")
  //   }
  //}
}


