package models

import com.mongodb.casbah.Imports._
import play.api.data.Form
import play.api.data.Forms._

case class Kindergarten(name: String, street: String, num: Int, city: String, len: String, lon: String, usersEmails: List[String])

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

  def listAll = {
    val kindergartens = MongoFactory.kindergartens.find
    convertCursorToList(kindergartens)
  }

  def find(kgName: String, kgStreet: String, kgCity: String) = {
    val query = MongoDBObject("name" -> kgName, "street" -> kgStreet, "city" -> kgCity)
    val kgMongo = MongoFactory.kindergartens.findOne(query)
    kgMongo match {
      case Some(kg) => convertDBObjectToKindergarten(kg)
      case None => throw new NoSuchElementException
    }
  }

  def findUsersFromKindergarten(kindergarten: Kindergarten) = {
    val usersEmails = kindergarten.usersEmails
    val emailsList = for (email <- usersEmails) yield email
    val usersMongo = for (email <- emailsList) yield MongoFactory.users.findOne(MongoDBObject("email" -> email.trim)).get //TODO: add match
    val users = for(user <- usersMongo) yield Users.convertDBObjectToUser(user)
    users
  }

  def convertCursorToList(MongoKindergatens: com.mongodb.casbah.MongoCursor) = {
    val res =
      for { kgMongo <- MongoKindergatens
        name = kgMongo.getAs[String]("name").get
        street = kgMongo.getAs[String]("street").get
        num = kgMongo.getAs[Int]("num").get
        city = kgMongo.getAs[String]("city").get
        len = kgMongo.getAs[String]("len").get
        lon = kgMongo.getAs[String]("lon").get
        usersemails = kgMongo.getAs[String]("usersemails").get.split(",").map(_.trim).toList.drop(1)
      } yield new Kindergarten(name, street, num, city, len, lon, usersemails)
    res.toList
  }

  def convertDBObjectToKindergarten(kgMongo: MongoDBObject) = {
    val name = kgMongo.getAs[String]("name").get
    val street = kgMongo.getAs[String]("street").get
    val num = kgMongo.getAs[Int]("num").get
    val city = kgMongo.getAs[String]("city").get
    val len = kgMongo.getAs[String]("len").get
    val lon = kgMongo.getAs[String]("lon").get
    val userEmails = kgMongo.getAs[String]("usersemails").get.split(",").toList.drop(1)
    new Kindergarten(name, street, num, city, len, lon, userEmails)
  }
}


