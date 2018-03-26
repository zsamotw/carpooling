import play.api.libs.json._
import scala.io.Source
import org.joda.time.DateTime


val a = Array(1,2,3,4)
a(0)

val yy = List(List(1,2,3), List(4,5,6), List(8,9,0))

yy.length

for {
  group <- yy
  if group contains(1)
  n <- group
  if n != 1
} yield n

yy partition(x => x contains(1))

"hallo".toList

trait general
case class g1() extends general
case class g2() extends general

val xs = List(g1, g1, g1)
val ys = List(g2, g2, g2)
xs ::: ys


val dt1 = new DateTime(2016, 9,12,12,5)
val dt = new DateTime()
dt.hourOfDay()
dt.getDayOfMonth

trait S {
  val date = new DateTime()
}

case class B(s: String) extends S

val x = B("akakaka")

val e = x.date
val ee = x.date.toDate
val eee = x.date.toDate

val y = B("")
val f = y.date
val ff = y.date.toDate

f isAfter e
ff.after(ee)

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
