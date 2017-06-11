package models

import com.mongodb.casbah.Imports._
import play.api.data.Form
import play.api.data.Forms._

case class User(email: String, password: String, name: String, surname: String, city: String, street: String, kindergarten: String, len: String, lon: String)

case class UserFormData(email: String, password: String, name: String, surname: String, city: String, street: String, kindergarten: String)

case class Login(email: String, password: String)

object userForm {
  val form = Form(
    mapping(
      "email" -> text,
      "password" -> text,
      "name" -> text,
      "surname" -> text,
      "city" -> text,
      "street" -> text,
      "kindergarten" -> text)(UserFormData.apply)(UserFormData.unapply))
}

object loginForm {
  val form = Form(
    mapping(
      "email" -> text,
      "password" -> text)(Login.apply)(Login.unapply))
}

object Users {

  def validateLogin(login: Login): Boolean = {
    val userMongoOpt = MongoFactory.users.findOne(MongoDBObject("email" -> login.email))
    userMongoOpt match {
      case None => false
      case Some(user) => if (user.getAs[String]("password").get == login.password) true else false
    }
  }

  def add(user: User) = {
    MongoFactory.users += MongoFactory.buildMongoDbUser(user)
    val kindergarten = MongoFactory.kindergartens.findOne("name" $eq user.kindergarten)
    kindergarten match {
      case Some(kg) =>
        val usersEmails = kg.as[String]("usersemails")
        val usersEmailsAfter = usersEmails + ", " + user.email
        val query = MongoDBObject("name" -> kg.as[String]("name"))
        val update = MongoDBObject("$set" -> MongoDBObject("usersemails" -> usersEmailsAfter))
        MongoFactory.kindergartens.findAndModify(query, update)
      case None => throw new NoSuchElementException
    }
  }

  def listAll = {
    val allUsers = MongoFactory.users.find
    convertCursorToList(allUsers)
  }

  def findUsersFromKindergarten(kg: String) = {
    val usersFrom = MongoFactory.users.find("kindergarten" $eq kg)
    convertCursorToList(usersFrom)

  }

  def findLoggedUser(email: String) = {
    val loggedUserOpt = MongoFactory.users.findOne("email" $eq email)
    loggedUserOpt match {
      case Some(u) =>
        val name = u.as[String]("name")
        val surname = u.as[String]("surname")
        val street = u.as[String]("street")
        val city = u.as[String]("city")
        val kindergarten = u.as[String]("kindergarten")
        val email = u.as[String]("email")
        (name, surname, street, city, kindergarten, email)
      case None => throw new NoSuchElementException
    }
  }

  def convertCursorToList(MongoUsers: com.mongodb.casbah.MongoCursor) = {
    val res =
      for {userMongo <- MongoUsers
        email = userMongo.getAs[String]("email").get
        password = userMongo.getAs[String]("password").get
        name = userMongo.getAs[String]("name").get
        surname = userMongo.getAs[String]("surname").get
        street = userMongo.getAs[String]("street").get
        city = userMongo.getAs[String]("city").get
        kindergarten = userMongo.getAs[String]("kindergarten").get
        len = userMongo.getAs[String]("len").get
        lon = userMongo.getAs[String]("lon").get
    } yield new User(email, password, name, surname, street, city, kindergarten, len, lon)
    res.toList
  }

  def convertDBObjectToUser(userMongo: MongoDBObject) = {
    val email = userMongo.getAs[String]("email").get
    val password =  userMongo.getAs[String]("password").get
    val name = userMongo.getAs[String]("name").get
    val surname =  userMongo.getAs[String]("surname").get
    val street =  userMongo.getAs[String]("street").get
    val city =  userMongo.getAs[String]("city").get
    val kindergarten =  userMongo.getAs[String]("kindergarten").get
    val len =  userMongo.getAs[String]("len").get
    val lon =  userMongo.getAs[String]("lon").get
    new User(email, password, name, surname, city, street, kindergarten, len, lon)


  }

}
