package models

import play.api.Play
import play.api.data.Form
import play.api.data.Forms._
import scala.concurrent.Future
import scala.collection.mutable.ListBuffer

case class Kindergarten(name: String, street: String, num: Int, city: String, len: String, lon: String)

case class KindergartenFormData(name: String, street: String, num: Int, city: String)

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
    MongoFactory.kindergartens += MongoFactory.buildMongoDbKindergarten(kindergarten)
  }

  def find(kgName: String, kgStreet: String, kgCity: String) = {
    
  }

  def listAll = kindergartens

}


