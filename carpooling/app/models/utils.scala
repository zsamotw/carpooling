package models

import scala.io.Source
import play.api.libs.json._
import com.mongodb.casbah.MongoCollection
import com.mongodb.casbah.MongoConnection
import com.mongodb.casbah.Imports._

object GeoUtils {

  def searchGeoPoint(user: UserFormData) = {
    val query = "http://nominatim.openstreetmap.org/search/" + user.street + "," + user.city + ",Poland?format=json&polygon=1&addressdetails=1&limit=1"
    parseLatLonFromQuery(query)
  }

  def searchGeoPoint(kg: KindergartenFormData) = {
    val query = "http://nominatim.openstreetmap.org/search/" + kg.street + "," + kg.city + ",Poland?format=json&polygon=1&addressdetails=1&limit=1"
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

  private val SERVER = "localhost"
  private val PORT = 27017
  private val DATABASE = "carpooling"
  private val USERS = "users"
  private val KINDERGARTENS = "kindergartens"

  val connection = MongoClient()
  val db = connection(DATABASE)
  val users = db(USERS)
  val kindergartens = db(KINDERGARTENS)

  def buildMongoDbUser(user: User): MongoDBObject = {
    val builder = MongoDBObject.newBuilder
    builder += "email" -> user.email
    builder += "password" -> user.password
    builder += "name" -> user.name
    builder += "surname" -> user.surname
    builder += "street" -> user.street
    builder += "city" -> user.city
    builder += "kindergarten" -> user.kindergarten
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
    builder.result
  }
}


