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

case class Kindergarten(kName: String, users: ListBuffer[User])

case class KindergartenFormData(kName: String)

object KindergartenSearchForm {

  val form = Form (
    mapping (
      "kName" -> text
    ) (KindergartenFormData.apply) (KindergartenFormData.unapply)
  )
}

object Kindergartens {

  var kindergartens = List[Kindergarten]()

  def add(kindergarten: Kindergarten) = {
    kindergartens = kindergarten :: kindergartens
  }

  def listAll = kindergartens

  def findUsersFromKindergarten(kName: String) = {
    val kindergarten = kindergartens filter(_.kName == kName)
    kindergarten match {
      case Nil => (kindergartens filter(_.kName contains(kName)), "No kindergarten. Some similar:")
      case _ => (kindergarten, "Users from:")
    }
  }
}

