import play.api.libs.json._
import scala.io.Source


def searchGeoPoint(street: String) = {
  val query = "http://nominatim.openstreetmap.org/search/" + street + ",Krakow, Poland?format=json&polygon=1"
  val jsonResult = Source.fromURL(query)
}

val x = searchGeoPoint("miechowity")
val street = "Miechowity"
val query = "http://nominatim.openstreetmap.org/search/" + "mickiewicza" + ",Krakow, Poland?format=json&polygon=1"

val res = Source.fromURL("http://nominatim.openstreetmap.org/search/mickiewicza,Krakow,Poland?format=json&polygon=1&addressdetails=1&limit=1").mkString
val d = Source.fromURL("http://wyborcza.pl")

val jsonRes = Json.parse(res)
val lat = (jsonRes \\ "lat").toString()
val lon = (jsonRes \\ "lon").map(_.as[String])

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