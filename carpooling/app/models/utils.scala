package models

import scala.io.Source
import play.api.libs.json._
import com.mongodb.casbah.MongoCollection
import com.mongodb.casbah.MongoConnection
import com.mongodb.casbah.Imports._

object GeoUtils {

  def searchGeoPoint(user: UserFormData) = {
    val street = user.street split(" ") mkString("%20")
    val city = user.city split(" ") mkString("%20")
    val query = "http://nominatim.openstreetmap.org/search/" + street + "," + city + ",Poland?format=json&polygon=1&addressdetails=1&limit=1"
    parseLatLonFromQuery(query)
  }

  def searchGeoPoint(kg: KindergartenFormData) = {
     val street = kg.street split(" ") mkString("%20")
    val city = kg.city split(" ") mkString("%20")
    val query = "http://nominatim.openstreetmap.org/search/" + street + "," + city + ",Poland?format=json&polygon=1&addressdetails=1&limit=1"
    parseLatLonFromQuery(query)
  }

  def parseLatLonFromQuery(query: String) = {
    val res = Source.fromURL(query).mkString
    val jsonRes = Json.parse(res)
    val lat = (jsonRes \\ "lat").head.toString
    val lon = (jsonRes \\ "lon").head.toString
    (lat, lon)
  }
}

object MongoFactory {

  private val Database = "carpooling"
  private val Users = "users"
  private val Kindergartens = "kindergartens"

  val connection = MongoClient()
  val db = connection(Database)
  val users = db(Users)
  val kindergartens = db(Kindergartens)

  def buildMongoDbUser(user: User): MongoDBObject = {
    val builder = MongoDBObject.newBuilder
    builder += "email" -> user.email
    builder += "password" -> user.password
    builder += "name" -> user.name
    builder += "surname" -> user.surname
    builder += "street" -> user.street
    builder += "city" -> user.city
    builder += "seats" -> user.seats
    builder += "kgname" -> user.kindergarten.name
    builder += "kgstreet" -> user.kindergarten.street
    builder += "kgnum" -> user.kindergarten.num
    builder += "kgcity" -> user.kindergarten.city
    builder += "requests" -> user.requests
    builder += "len" -> user.len
    builder += "lon" -> user.lon
    builder.result
  }

  def buildMongoDbKindergarten(kg: Kindergarten) = {
    val builder = MongoDBObject.newBuilder
    builder += "name" -> kg.name
    builder += "street" -> kg.street
    builder += "num" -> kg.num
    builder += "city" -> kg.city
    builder += "len" -> kg.len
    builder += "lon" -> kg.lon
    builder += "usersemails" -> List[List[String]]()
    builder.result
  }

  def addUser(data: (User, DBObject, DBObject)) = {
    val(user, query, update) = data
    kindergartens.findAndModify(query, update)
    users += buildMongoDbUser(user)
  }

  def deleteUser(data: (User, DBObject, DBObject)) = {
    val(user, query, update) = data
    kindergartens.findAndModify(query, update)
    users.remove("email" $eq user.email)
  }

  def findUserinDB(user: User) = {
    val userMongo = users.findOne(MongoDBObject("email" -> user.email))
    userMongo match {
      case None => throw new NoSuchElementException
      case Some(u) => u
    }
  }

  def updateUserRequests(data: (User, String, (Set[String], String) => Set[String])) = {
    val(requestedUser, loggedUserEmail, f) = data
    val userRequestsAfter = f(requestedUser.requests, loggedUserEmail)
    val query = MongoDBObject("email" -> requestedUser.email)
    val upadate = MongoDBObject("$set" -> MongoDBObject("requests" -> userRequestsAfter.toList))
    users.findAndModify(query, upadate)
  }

  def updateCarpools(data: (DBObject, DBObject)) = {
    val(query, update) = data
    kindergartens.findAndModify(query, update)
  }

    def updateUserStringDatainDB(user: User, field: String, data: String) = {
    val query = MongoDBObject("email" -> user.email)
    val upadate = MongoDBObject("$set" -> MongoDBObject(field -> data))
    MongoFactory.users.findAndModify(query, upadate)
  }

  def updateUserIntDataInDB(user: User, field: String, data: Int, f: (Int, Int) => Int ) = {
    val userMongo = MongoFactory.findUserinDB(user)
    val dataBefore = userMongo.getAs[Int]("seats")
    dataBefore match {
      case Some(dataBefore) =>
        val dataAfter = f(dataBefore, data)
        val query = MongoDBObject("email" -> user.email)
        val upadate = MongoDBObject("$set" -> MongoDBObject("field" -> dataAfter))
        MongoFactory.users.findAndModify(query, upadate)
      case None => throw new NoSuchElementException
    }
  }

}


