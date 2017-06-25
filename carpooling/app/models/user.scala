package models

import com.mongodb.casbah.Imports._
import play.api.data.Form
import play.api.data.Forms._

case class User(
  email: String,
  password: String,
  name: String,
  surname: String,
  street: String,
  city: String,
  seats: Int,
  kindergarten: KindergartenFormData,
  requests: Set[String],
  len: String,
  lon: String)

case class UserFormData(
  email: String,
  password: String,
  name: String,
  surname: String,
  street: String,
  city: String,
  seats: Int,
  kgName: String,
  kgStreet: String,
  kgNum: Int,
  kgCity: String)

case class Login(email: String, password: String)

object userForm {

  val form = Form(
    mapping(
      "email" -> text,
      "password" -> text,
      "name" -> text,
      "surname" -> text,
      "street" -> text,
      "city" -> text,
      "seats" -> number,
      "kgName" -> text,
      "kgStreet" -> text,
      "kgNum" -> number,
      "kgCity" -> text)(UserFormData.apply)(UserFormData.unapply))
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

  def isOnlyOne(user: User) = {
    val userMongo = MongoFactory.users.findOne(MongoDBObject("email" -> user.email))
    userMongo match {
      case None => true
      case Some(user) => false
    }
  }

  def add(user: User) = {
    MongoFactory.users += MongoFactory.buildMongoDbUser(user)
    val kindergarten = Kindergartens.find(user.kindergarten.name, user.kindergarten.street, user.kindergarten.num, user.kindergarten.city)
    val usersEmailsAfter = (kindergarten.usersEmails ::: List(List(user.email)))
    val query = MongoDBObject("name" -> kindergarten.name, "street" -> kindergarten.street, "num" -> kindergarten.num, "city" -> kindergarten.city)
    val update = MongoDBObject("$set" -> MongoDBObject("usersemails" -> usersEmailsAfter))
    MongoFactory.kindergartens.findAndModify(query, update)
  }

  def delete(user: User) = {
    val kindergarten = Kindergartens.find(user.kindergarten.name, user.kindergarten.street, user.kindergarten.num, user.kindergarten.city)
    val usersEmailsGroup = kindergarten.usersEmails filter(group => group contains user.email)
    val usersEmailsGroupAfter = usersEmailsGroup map(group => group filter(email => email != user.email))
    val usersEmailsAfter = (kindergarten.usersEmails filter (group => group != usersEmailsGroup.flatten)) ::: usersEmailsGroupAfter
    val query = MongoDBObject("name" -> kindergarten.name, "street" -> kindergarten.street, "num" -> kindergarten.num, "city" -> kindergarten.city)
    val update = MongoDBObject("$set" -> MongoDBObject("usersemails" -> usersEmailsAfter))
    MongoFactory.kindergartens.findAndModify(query, update)
    MongoFactory.users.remove("email" $eq user.email)
  }

  def listAll = {
    val allUsers = MongoFactory.users.find
    convertCursorToList(allUsers)
  }

  def findUserByEmail(email: String) = {
    val UserOpt = MongoFactory.users.findOne("email" $eq email)
    UserOpt match {
      case Some(u) =>
        convertDBObjectToUser(u)
      case None => throw new NoSuchElementException
    }
  }

  def addRequest(requestedUserEmail: String, loggedUserEmail: String) = {
    val requestedUser = findUserByEmail(requestedUserEmail)
    updateUserRequestsInDB(requestedUser, loggedUserEmail, (xs,y) => xs + y)
  }

  def addToCarpools(userToReplyEmail: String, loggedUserEmail: String) = {
    val loggedUser = findUserByEmail(loggedUserEmail)
    val userToReply = findUserByEmail(userToReplyEmail)
    val kindergarten = Kindergartens.find(
      loggedUser.kindergarten.name,
      loggedUser.kindergarten.street,
      loggedUser.kindergarten.num,
      loggedUser.kindergarten.city)

    val loggedUserGroup = kindergarten.usersEmails filter(group => group contains loggedUser.email)
    val userToReplyGroup = kindergarten.usersEmails filter(group => group contains userToReply.email)
    val commonGroup = if (loggedUserGroup != userToReplyGroup) (loggedUserGroup ::: userToReplyGroup) else loggedUserGroup
    val usersEmailsWithout = kindergarten.usersEmails filter(group =>
      !(group contains loggedUser.email) && !(group contains userToReply.email))
    val usersEmailsAfter = usersEmailsWithout ::: commonGroup

    val query = MongoDBObject("name" -> kindergarten.name, "street" -> kindergarten.street, "num" -> kindergarten.num, "city" -> kindergarten.city)
    val update = MongoDBObject("$set" -> MongoDBObject("usersemails" -> usersEmailsAfter))
    MongoFactory.kindergartens.findAndModify(query, update)
    updateUserRequestsInDB(loggedUser, userToReplyEmail, (xs, y) => xs - y)
  }

  def updateUserRequestsInDB(user: User, data: String, f: ((Set[String], String) => Set[String])) = {
    val userRequestAfter = f(user.requests, data)
    val query = MongoDBObject("email" -> user.email)
    val upadate = MongoDBObject("$set" -> MongoDBObject("requests" -> userRequestAfter.toList))
    MongoFactory.users.findAndModify(query, upadate)
  }

  def updateNumberOfSeatsInDB(user: MongoDBObject,f: (Int) => Int ) = {
    val userSeats = user.as[Int]("seats")
    val userSeatsAfter = f(userSeats)
    val userEmail = user.as[String]("email")
    val query = MongoDBObject("email" -> userEmail)
    val upadate = MongoDBObject("$set" -> MongoDBObject("seats" -> userSeatsAfter))
    MongoFactory.users.findAndModify(query, upadate)
  }

  def convertCursorToList(MongoUsers: com.mongodb.casbah.MongoCursor) = {
    val res =
      for { userMongo <- MongoUsers
        email = userMongo.getAs[String]("email").get
        password = userMongo.getAs[String]("password").get
        name = userMongo.getAs[String]("name").get
        surname = userMongo.getAs[String]("surname").get
        street = userMongo.getAs[String]("street").get
        city = userMongo.getAs[String]("city").get
        seats = userMongo.getAs[Int]("seats").get
        kgName = userMongo.getAs[String]("kgname").get
        kgStreet = userMongo.getAs[String]("kgstreet").get
        kgNum = userMongo.getAs[Int]("kgnum").get
        kgCity = userMongo.getAs[String]("kgcity").get
        requests = userMongo.getAs[List[String]]("requests").get.toSet
        len = userMongo.getAs[String]("len").get
        lon = userMongo.getAs[String]("lon").get
      } yield User(
        email,
        password,
        name,
        surname,
        street,
        city,
        seats,
        KindergartenFormData(
          kgName,
          kgStreet,
          kgNum.toInt,
          kgCity),
        requests,
        len,
        lon)
    res.toList
  }

  def convertDBObjectToUser(userMongo: MongoDBObject) = {
    val email = userMongo.getAs[String]("email").get
    val password =  userMongo.getAs[String]("password").get
    val name = userMongo.getAs[String]("name").get
    val surname =  userMongo.getAs[String]("surname").get
    val street =  userMongo.getAs[String]("street").get
    val city =  userMongo.getAs[String]("city").get
    val seats = userMongo.getAs[Int]("seats").get
    val kgName =  userMongo.getAs[String]("kgname").get
    val kgStreet =  userMongo.getAs[String]("kgstreet").get
    val kgNum =  userMongo.getAs[Int]("kgnum").get
    val kgCity =  userMongo.getAs[String]("kgcity").get
    val requests = userMongo.getAs[List[String]]("requests").get.toSet
    val len =  userMongo.getAs[String]("len").get
    val lon =  userMongo.getAs[String]("lon").get
    User(
      email,
      password,
      name,
      surname,
      street,
      city,
      seats,
      KindergartenFormData(
        kgName,
        kgStreet,
        kgNum.toInt,
        kgCity),
      requests,
      len,
      lon)
  }
}
