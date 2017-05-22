package models

import com.mongodb.casbah.Imports._
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

  def add(kindergarten: Kindergarten) = {
    MongoFactory.kindergartens += MongoFactory.buildMongoDbKindergarten(kindergarten)
  }

  def find(kgName: String, kgStreet: String, kgCity: String) = {
    val kgMongo = MongoFactory.kindergartens.findOne(MongoDBObject("name" -> kgName, "street" -> kgStreet, "city" -> kgCity)).get
    convertMongoDBObjectToKindergarten(kgMongo)
    
  }

  def listAll = {
    val kindergartens = MongoFactory.kindergartens.find
    convertCursorToList(kindergartens)
  }

  def convertCursorToList(MongoKindergatens: com.mongodb.casbah.MongoCursor) = {
    val res =
      for {kgMongo <- MongoKindergatens
        val name = kgMongo.getAs[String]("name").get
        val street = kgMongo.getAs[String]("street").get
        val num = kgMongo.getAs[Int]("num").get
        val city = kgMongo.getAs[String]("city").get
        val len = kgMongo.getAs[String]("len").get
        val lon = kgMongo.getAs[String]("lon").get
      } yield new Kindergarten(name, street, num, city, len, lon)
    res.toList
  }

  def convertMongoDBObjectToKindergarten(kgMongo: MongoDBObject) = {
    val name = kgMongo.getAs[String]("name").get
    val street = kgMongo.getAs[String]("street").get
    val num = kgMongo.getAs[Int]("num").get
    val city = kgMongo.getAs[String]("city").get
    val len = kgMongo.getAs[String]("len").get
    val lon = kgMongo.getAs[String]("lon").get
    new Kindergarten(name, street, num, city, len, lon)
  }
}


