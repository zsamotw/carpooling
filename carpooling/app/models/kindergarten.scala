package models

import com.mongodb.casbah.Imports._
import org.joda.time.DateTime
import play.api.data.Form
import play.api.data.Forms._

case class Kindergarten(
  name: String,
  street: String,
  num: Int,
  city: String,
  lat: Double,
  lon: Double,
  usersEmails: List[List[String]],
  admin: String,
  kgHashCode: String) {

  def numberOfUsers = usersEmails.flatten.length

  def numberOfGroups = usersEmails.length
}

/*
 * Case class and object for creating kindergartn from form
 */

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

/*
 * Methods for kindergartens
 */

object Kindergartens {

  val initialKindergarten = Kindergarten(
    "-","-", 0, "-", 0.0.toDouble, 0.0.toDouble, List[List[String]](), "-", "-")

  def returnNewKindergarten(
    name: String,
    street: String,
    num: Int,
    city: String,
    lat: Double,
    lon: Double,
    users: List[List[String]],
    adminEmail: String,
    hashCode: String): Kindergarten = Kindergarten(name, street, num, city, lat, lon, users, adminEmail, hashCode)

  def returnHashCode(name: String, street: String, num: Int, city: String, adminEmail: String): String =
    (name + street + num.toString + city + adminEmail).hashCode.toString

  def listAll: List[Kindergarten] = {
    val kindergartens = MongoFactory.kindergartens.find
    convertCursorToKindergartensList(kindergartens)
  }

  def addKindergarten(kindergarten: Kindergarten, user: User): (Kindergarten, User, DBObject, DBObject, (DBObject, DBObject), CommunityMessage) = {
    /*
     * Delete user from users emails list in current kindergarten DB
     */

    val dataToDeleteUserFromEmailList = deleteUserFromEmailsListInKindergarten(user)

    /*
     * Set kindergarten data to change in user DB and next kindergarten DB.
     */
    
    val query = MongoDBObject(
      "email" -> user.email)
    val update = MongoDBObject("$set" -> MongoDBObject(
      "admin" -> true,
      "kgname" -> kindergarten.name,
      "kgstreet" -> kindergarten.street,
      "kgnum" -> kindergarten.num,
      "kgcity" -> kindergarten.city))

    val content = s"New kindergarten has been added: ${kindergarten.name} on ${kindergarten.street} in ${kindergarten.city} by ${user.name} ${user.surname}"
    val message = CommunityMessage(new DateTime, kindergarten, content)
    (kindergarten, user, query, update, dataToDeleteUserFromEmailList, message)
  }

  def deleteUserFromEmailsListInKindergarten(user: User): (DBObject, DBObject) = {
    val kindergarten = user.kindergarten

    val usersEmailsGroup = for {
      group <- kindergarten.usersEmails
      if group contains user.email
      email <- group} yield email

    val usersEmailsGroupAfter = for {
      email <- usersEmailsGroup
      if email != user.email
    } yield email

    val usersEmailsAfter =
      if (usersEmailsGroupAfter isEmpty) for(group <- kindergarten.usersEmails; if group != usersEmailsGroup) yield group
      else usersEmailsGroupAfter :: (for(group <- kindergarten.usersEmails; if group != usersEmailsGroup) yield group)

    val query = MongoDBObject(
      "name" -> kindergarten.name,
      "street" -> kindergarten.street,
      "num" -> kindergarten.num,
      "city" -> kindergarten.city)
    val update = MongoDBObject("$set" -> MongoDBObject("usersemails" -> usersEmailsAfter))

    (query, update)
  }

  def addUserToKindergarten(user: User, kindergarten: Kindergarten): (DBObject, DBObject, DBObject, DBObject, (DBObject, DBObject), CommunityMessage) = {
    /*
     * Delete user from users emails list in current kindergarten DB
     */
    val dataToDeleteUserFromEmailList = deleteUserFromEmailsListInKindergarten(user)

    /*
     * Set kindergarten data to change in user DB and next kindergarten DB.
     */
    val usersEmailsAfter = List(user.email) :: kindergarten.usersEmails
    val queryKg = MongoDBObject(
      "name" -> kindergarten.name,
      "street" -> kindergarten.street,
      "num" -> kindergarten.num,
      "city" -> kindergarten.city)
    val updateKg = MongoDBObject("$set" -> MongoDBObject("usersemails" -> usersEmailsAfter))

    val queryU = MongoDBObject("email" -> user.email)
    val updateU = MongoDBObject("$set" -> MongoDBObject(
      "kgname" -> kindergarten.name,
      "kgstreet" -> kindergarten.street,
      "kgnum" -> kindergarten.num,
      "kgcity" -> kindergarten.city))

    val content = s"${user.name} ${user.surname} change kindergarten to ${kindergarten.name} on ${kindergarten.street} in ${kindergarten.city}"
    val message = CommunityMessage(new DateTime, kindergarten, content)

    (queryKg, updateKg, queryU, updateU, dataToDeleteUserFromEmailList, message)
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
      case None => Kindergartens.initialKindergarten//throw new NoSuchElementException
    }
  }

  def find(kgHashCode: String): Kindergarten = {
    val query = MongoDBObject(
      "hashcode" -> kgHashCode)
    val kgMongo = MongoFactory.kindergartens.findOne(query)
    kgMongo match {
      case Some(kg) => convertDBObjectToKindergarten(kg)
      case None => Kindergartens.initialKindergarten//throw new NoSuchElementException
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
    for{kgMongo <- mongoKindergatens.toList} yield convertDBObjectToKindergarten(kgMongo)
  }

  def convertDBObjectToKindergarten(kgMongo: MongoDBObject): Kindergarten = {
    val name = kgMongo.getAs[String]("name").get
    val street = kgMongo.getAs[String]("street").get
    val num = kgMongo.getAs[Int]("num").get
    val city = kgMongo.getAs[String]("city").get
    val lat = kgMongo.getAs[Double]("lat").get
    val lon = kgMongo.getAs[Double]("lon").get
    val admin = kgMongo.getAs[String]("admin").get
    val hashCode = kgMongo.getAs[String]("hashcode").get
    val usersEmails = {
      val listMongo = kgMongo.get("usersemails").get
      val obj = MongoDBObject("list" -> listMongo)
      val res = obj.as[BasicDBList]("list").toList
      val list ={
        for(el <- res) yield {
          val elemList = {
            val obj = MongoDBObject("list" -> el)
            val res2 = obj.as[BasicDBList]("list").toList
            for(e <- res2) yield {
              e.toString
            }
          }
          elemList
        }
      }
      list
    }
    Kindergarten(name, street, num, city, lat, lon, usersEmails, admin, hashCode)
  }
}
