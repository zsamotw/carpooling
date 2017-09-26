package models

import scala.io.Source
import play.api.libs.json._
import com.mongodb.casbah.MongoCollection
import com.mongodb.casbah.MongoConnection
import com.mongodb.casbah.Imports._
import com.mongodb.casbah.commons.conversions.scala._

/*
 * Object for searching geopoint of user and kindergarten
 */

object GeoUtils {
  def searchGeoPoint(user: UserFormData): (String, String) = {
    val street = user.street split(" ") mkString("%20")
    val city = user.city split(" ") mkString("%20")
    val query = "http://nominatim.openstreetmap.org/search/" + street + "," + city + ",Poland?format=json&polygon=1&addressdetails=1&limit=1"
    parseLatLonFromQuery(query)
  }

  def searchGeoPoint(kg: KindergartenFormData): (String, String) = {
     val street = kg.street split(" ") mkString("%20")
    val city = kg.city split(" ") mkString("%20")
    val query = "http://nominatim.openstreetmap.org/search/" + street + "," + city + ",Poland?format=json&polygon=1&addressdetails=1&limit=1"
    parseLatLonFromQuery(query)
  }

  def parseLatLonFromQuery(query: String): (String, String) = {
    val res = Source.fromURL(query).mkString
    val jsonRes = Json.parse(res)
    val lat = (jsonRes \\ "lat").head.toString
    val lon = (jsonRes \\ "lon").head.toString
    (lat, lon)
  }
}

/*
 * Object for working with Mongo Database
 */

object MongoFactory {
  RegisterJodaTimeConversionHelpers()

  private val Database = "carpooling"
  private val Users = "users"
  private val Kindergartens = "kindergartens"
  private val Messages = "messages"

  val connection = MongoClient()
  val db = connection(Database)
  val users = db(Users)
  val kindergartens = db(Kindergartens)
  val messages = db(Messages)

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

  def buildMongoDbKindergarten(kg: Kindergarten): MongoDBObject = {
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

  def buildMongoDbUserMessage(message: UserMessage): MongoDBObject = {
    val builder = MongoDBObject.newBuilder
    builder += "datetime" -> message.creationDateTime
    builder += "purpose" -> message.purpose.statement
    builder += "seats" -> message.seats
    builder += "date" -> message.date
    builder += "from" -> message.from
    builder += "to" -> message.to
    builder += "useremail" -> message.user.email
    builder += "username" -> message.user.name
    builder += "usersurname" -> message.user.surname
    builder += "userstreet" -> message.user.street
    builder += "usercity" -> message.user.city
    builder += "userseats" -> message.user.seats
    builder += "userlen" -> message.user.len
    builder += "userlon" -> message.user.lon
    builder += "kindergartenname" -> message.user.kindergarten.name
    builder += "kindergartenstreet" -> message.user.kindergarten.street
    builder += "kindergartennum" -> message.user.kindergarten.num
    builder += "kindergartencity" -> message.user.kindergarten.city
    builder += "kindergartenlen" -> message.user.kindergarten.len
    builder += "kindergartenlon" -> message.user.kindergarten.lon
    builder += "kindergartenusersemails" -> message.user.kindergarten.usersEmails
    builder.result
  }

  def buildMongoDbGlobalMessage(message: GlobalMessage): MongoDBObject = {
    val builder = MongoDBObject.newBuilder
    builder += "datetime" -> message.creationDateTime
    builder += "kindergartenname" -> message.kindergarten.name
    builder += "kindergartenstreet" -> message.kindergarten.street
    builder += "kindergartennum" -> message.kindergarten.num
    builder += "kindergartencity" -> message.kindergarten.city
    builder += "kindergartenlen" -> message.kindergarten.len
    builder += "kindergartenlon" -> message.kindergarten.lon
    builder += "kindergartenusersemails" -> message.kindergarten.usersEmails
    builder += "content" -> message.content
    builder.result
  }

  def addUser(data: (User, DBObject, DBObject, GlobalMessage)) {
    val(user, query, update, message) = data
    kindergartens.findAndModify(query, update)
    users += buildMongoDbUser(user)
    add(message)
  }

  def deleteUser(data: (User, List[User], DBObject, DBObject)) {
    val(user, userGroup, query, update) = data
    for(user <- userGroup filter(_ != user)) MongoFactory.updateUserIntDataInDB(user, "seats", 1, (x:Int, y: Int) => x + y)
    kindergartens.findAndModify(query, update)
    users.remove("email" $eq user.email)
  }

  def leaveGroup(data: (User, List[User],(DBObject, DBObject, GlobalMessage))): String = {
    val(user, userGroup, dataToDB) = data
    if(userGroup.length > 1) {
      val numberOfOthersUsers = userGroup.length - 1
      updateCarpools(dataToDB)
      updateUserIntDataInDB(user, "seats", numberOfOthersUsers, (x: Int, y: Int) => x + y)
      for (user <- userGroup filter(_ != user)) updateUserIntDataInDB(user, "seats", 1, (x: Int, y: Int) => x + y)
      val(_,_,message) = dataToDB
      message.content
    } else "You are single. You can't leave yourself. Let's try to find carpoolers"
  }

  def findUserinDB(user: User): DBObject = {
    val userMongo = users.findOne(MongoDBObject("email" -> user.email))
    userMongo match {
      case None => throw new NoSuchElementException
      case Some(u) => u
    }
  }

  def updateUserRequests(data: (User, String, (Set[String], String) => Set[String])) {
    val(requestedUser, loggedUserEmail, f) = data
    val userRequestsAfter = f(requestedUser.requests, loggedUserEmail)
    val query = MongoDBObject("email" -> requestedUser.email)
    val update = MongoDBObject("$set" -> MongoDBObject("requests" -> userRequestsAfter.toList))
    users.findAndModify(query, update)
  }

  def updateCarpools(data: (DBObject, DBObject, GlobalMessage)) {
    val(query, update, message) = data
    kindergartens.findAndModify(query, update)
    add(message)
  }

  def updateUserStringDatainDB(user: User, field: String, data: String) {
    val query = MongoDBObject("email" -> user.email)
    val upadate = MongoDBObject("$set" -> MongoDBObject(field -> data))
    MongoFactory.users.findAndModify(query, upadate)
  }

  def updateUserIntDataInDB(user: User, field: String, data: Int, f: (Int, Int) => Int ) {
    val userMongo = MongoFactory.findUserinDB(user)
    val dataBefore = userMongo.getAs[Int](field)
    dataBefore match {
      case Some(dBefore) =>
        val dataAfter = f(dBefore, data)
        val query = MongoDBObject("email" -> user.email)
        val upadate = MongoDBObject("$set" -> MongoDBObject(field -> dataAfter))
        MongoFactory.users.findAndModify(query, upadate)
      case None => throw new NoSuchElementException
    }
  }

  def add(data: (Kindergarten, GlobalMessage)) {
    val(kindergarten, message) = data
    kindergartens += buildMongoDbKindergarten(kindergarten)
    add(message)
  }

  def add(message: UserMessage) {
    messages += buildMongoDbUserMessage(message)
  }

  def add(message: GlobalMessage): Unit = {
    messages += buildMongoDbGlobalMessage(message)
  }
}
