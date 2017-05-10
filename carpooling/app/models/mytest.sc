import play.api.libs.json._
import scala.io.Source


def searchGeoPoint(street: String) = {
  val query = "http://nominatim.openstreetmap.org/search/" + street + ",Krakow, Poland?format=json&polygon=1"
  val jsonResult = Source.fromURL(query)
}

val x = searchGeoPoint("miechowity")
val street = "Miechowity"
val query = "http://nominatim.openstreetmap.org/search/" + street + ",Krakow, Poland?format=json&polygon=1"

val res = Source.fromURL("http://nominatim.openstreetmap.org/search/Miechowity,Krakow,Poland?format=json&polygon=1&addressdetails=1&limit=1").mkString

val jsonRes = Json.parse(res)
val latRead = (JsPath \\ "lat").read[Double]
val lonRead = (JsPath \\ "lon").read[Double]

val latResult = jsonRes.validate[Double](latRead)
val lonResult = jsonRes.validate[Double](lonRead)

val lat = latResult match {
  case s: JsSuccess[Double] => s.get
  case e: JsError => 0.0
}

val lon = lonResult match {
  case s: JsSuccess[Double] => s.get
  case e: JsError => 0.0
}
(lat,lon)

//val jv = res.Json.parse(res)



//http://nominatim.openstreetmap.org/search/Miechowity,Krakow,Poland?format=json&polygon=1&addressdetails=1&limit=1