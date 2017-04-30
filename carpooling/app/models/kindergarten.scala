package models

import play.api.Play
import play.api.data.Form
import play.api.data.Forms._
import scala.concurrent.Future
import scala.slick.driver
import slick.driver.JdbcProfile
import slick.driver.H2Driver.api._
import scala.concurrent.ExecutionContext.Implicits.global

case class Kindergarten(kName: String, kStreet: String, kCity: String)

/*object KindergartenForm {

  val form = play.api.data.Form (
    mapping (
      "kName" -> text,
      "kStreet" -> text,
      "kCity" -> text
    )
  ) (Kindergarten.apply)(Kindergarten.unapply)
}*/

