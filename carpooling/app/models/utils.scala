package models

import scala.io.Source
import play.api.libs.json._

object Utils {

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
