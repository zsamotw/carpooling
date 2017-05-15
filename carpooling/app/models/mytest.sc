import play.api.libs.json._
import scala.io.Source



val street = "Miechowity"
val query = "http://nominatim.openstreetmap.org/search/" + street + ",Krakow,Poland?format=json&polygon=1&addressdetails=1&limit=1"
val tom = "http://tomaszwiech.com"


val res = Source.fromURL(query).mkString

val jsonRes = Json.parse(res)
val lat = (jsonRes \\ "lat").head.toString()
val lon = (jsonRes \\ "lon").head.toString

/*val lat = latResult match {
  case s: JsSuccess[Double] => s.get
  case e: JsError => 0.0
}

val lon = lonResult match {
  case s: JsSuccess[Double] => s.get
  case e: JsError => 0.0
}*/
(lat,lon)

//val jv = res.Json.parse(res)



//http://nominatim.openstreetmap.org/search/Miechowity,Krakow,Poland?format=json&polygon=1&addressdetails=1&limit=1