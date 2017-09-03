package models

import com.mongodb.casbah.Imports._
import play.api.data.Form
import play.api.data.Forms._

case class Kindergarten(
  name: String,
  street: String,
  num: Int,
  city: String,
  len: String,
  lon: String,
  usersEmails: List[List[String]])

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
  def listAll: List[Kindergarten] = {
    val kindergartens = MongoFactory.kindergartens.find
    convertCursorToKindergartensList(kindergartens)
  }

  def find(kgName: String, kgStreet: String, kgNum: Int, kgCity: String): Kindergarten = {
    val query = MongoDBObject(
      "name" -> kgName,
      "street" -> kgStreet,
      "num" -> kgNum,
      "city" -> kgCity)
    val kgMongo = MongoFactory.kindergartens.findOne(query)
    kgMongo match {
      case Some(kg) => convertDBObjectToKindergarten(kg)
      case None => throw new NoSuchElementException
    }
  }

  def findUsersFromKindergarten(kindergarten: Kindergarten): List[List[User]] = {
    val usersEmails = kindergarten.usersEmails
    val users = {
      for(groupEmails <- usersEmails) yield {
        val group =
          for(email <- groupEmails) yield Users.findUserByEmail(email)
        group
      }
    }
    users
  }

  def convertCursorToKindergartensList(mongoKindergatens: MongoCursor): List[Kindergarten] = {
    val res =
      for { kgMongo <- mongoKindergatens
        name = kgMongo.getAs[String]("name").get
        street = kgMongo.getAs[String]("street").get
        num = kgMongo.getAs[Int]("num").get
        city = kgMongo.getAs[String]("city").get
        len = kgMongo.getAs[String]("len").get
        lon = kgMongo.getAs[String]("lon").get
        usersemails = kgMongo.getAs[List[List[String]]]("usersemails").get
      } yield Kindergarten(name, street, num, city, len, lon, usersemails)
    res.toList
  }

  def convertDBObjectToKindergarten(kgMongo: MongoDBObject): Kindergarten = {
    val name = kgMongo.getAs[String]("name").get
    val street = kgMongo.getAs[String]("street").get
    val num = kgMongo.getAs[Int]("num").get
    val city = kgMongo.getAs[String]("city").get
    val len = kgMongo.getAs[String]("len").get
    val lon = kgMongo.getAs[String]("lon").get
    val usersEmails = {
      val listMongo = kgMongo.get("usersemails").get
      val obj = MongoDBObject("list" -> listMongo)
      val res = obj.as[BasicDBList]("list")
      val list ={
        for(el <- res) yield {
          val elemList = {
            val obj = MongoDBObject("list" -> el)
            val res2 = obj.as[BasicDBList]("list")
            for(e <- res2) yield {
              e.toString
            }
          }
          elemList.toList
        }
      }
      list.toList
    }
    Kindergarten(name, street, num, city, len, lon, usersEmails)
  }
}