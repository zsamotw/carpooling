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

case class SimpleUser(
  email: String,
  name: String,
  surname: String,
  street: String,
  city: String,
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

  def isOnlyOne(user: User): Boolean = {
    val userMongo = MongoFactory.users.findOne(MongoDBObject("email" -> user.email))
    userMongo match {
      case None => true
      case Some(user) => false
    }
  }

  def listAll: List[User] = {
    val allUsers = MongoFactory.users.find
    convertCursorToUsersList(allUsers)
  }

  def add(user: User): (User, DBObject, DBObject) = {
    val kindergarten = Kindergartens.find(
      user.kindergarten.name,
      user.kindergarten.street,
      user.kindergarten.num,
      user.kindergarten.city)
    val usersEmailsAfter = (kindergarten.usersEmails ::: List(List(user.email)))
    val query = MongoDBObject(
      "name" -> kindergarten.name,
      "street" -> kindergarten.street,
      "num" -> kindergarten.num,
      "city" -> kindergarten.city)
    val update = MongoDBObject("$set" -> MongoDBObject("usersemails" -> usersEmailsAfter))
    (user, query, update)
  }

  def delete(user: User): (User, DBObject, DBObject) = {
    val kindergarten = Kindergartens.find(
      user.kindergarten.name,
      user.kindergarten.street,
      user.kindergarten.num,
      user.kindergarten.city)
    val usersEmailsGroup = kindergarten.usersEmails filter(group => group contains user.email)
    val usersEmailsGroupAfter = usersEmailsGroup map(group => group filter(email => email != user.email))
    val usersEmailsAfter = {
      if ((usersEmailsGroupAfter.flatten) isEmpty) kindergarten.usersEmails filter (group => group != usersEmailsGroup.flatten)
      else (kindergarten.usersEmails filter (group => group != usersEmailsGroup.flatten)) ::: usersEmailsGroupAfter
    }
    val query = MongoDBObject(
      "name" -> kindergarten.name,
      "street" -> kindergarten.street,
      "num" -> kindergarten.num,
      "city" -> kindergarten.city)
    val update = MongoDBObject("$set" -> MongoDBObject("usersemails" -> usersEmailsAfter))
    (user, query, update)
  }

  def findUserByEmail(email: String): User = {
    val userOpt = MongoFactory.users.findOne("email" $eq email)
    userOpt match {
      case Some(user) =>
        convertDBObjectToUser(user)
      case None => throw new NoSuchElementException
    }
  }

  def addRequest(requestedUserEmail: String, loggedUserEmail: String): (User, String, (Set[String], String) => Set[String]) = {
    val requestedUser = findUserByEmail(requestedUserEmail)
    val f = (xs: Set[String], y: String) => xs + y 
    (requestedUser, loggedUserEmail, f)
  }

  def deleteRequest(userToReplyEmail: String, loggedUserEmail: String): (User, String, (Set[String], String) => Set[String]) = {
    val loggedUser = findUserByEmail(loggedUserEmail)
    val f = (xs: Set[String], y: String) => xs - y
    (loggedUser, userToReplyEmail, f)
  }

  def areEnoughtSeats(fstGroup: List[User], scdGroup: List[User]): Boolean = {
    fstGroup.forall(user => user.seats >= scdGroup.length) && scdGroup.forall(user => user.seats >= fstGroup.length)
  }

  def usersFromGroup(loggedUserEmail: String): List[User] = {
    val loggedUser = findUserByEmail(loggedUserEmail)
    val kindergarten = Kindergartens.find(
      loggedUser.kindergarten.name,
      loggedUser.kindergarten.street,
      loggedUser.kindergarten.num,
      loggedUser.kindergarten.city)
    val loggedUserGroup = kindergarten.usersEmails filter(group => group contains loggedUser.email)
    val group = {
      for(email <- loggedUserGroup.flatten) yield findUserByEmail(email)
    }
    group
  }

  def addToCarpools(userToReplyEmail: String, loggedUserEmail: String): (DBObject, DBObject) = {
    val loggedUser = findUserByEmail(loggedUserEmail)
    val kindergarten = Kindergartens.find(
      loggedUser.kindergarten.name,
      loggedUser.kindergarten.street,
      loggedUser.kindergarten.num,
      loggedUser.kindergarten.city)

    val loggedUserGroupEmailsList = kindergarten.usersEmails filter(group => group contains loggedUserEmail)
    val userToReplyGroupEmailsList = kindergarten.usersEmails filter(group => group contains userToReplyEmail)
    val commonGroupEmailsList = if (loggedUserGroupEmailsList != userToReplyGroupEmailsList)
      loggedUserGroupEmailsList.flatten ::: userToReplyGroupEmailsList.flatten
    else loggedUserGroupEmailsList
    val restUsersEmails = kindergarten.usersEmails filter(group =>
      !(group contains loggedUserEmail) && !(group contains userToReplyEmail))
    val usersEmailsAfter = restUsersEmails ::: List(commonGroupEmailsList)

    val query = MongoDBObject(
      "name" -> kindergarten.name,
      "street" -> kindergarten.street,
      "num" -> kindergarten.num,
      "city" -> kindergarten.city)
    val update = MongoDBObject("$set" -> MongoDBObject("usersemails" -> usersEmailsAfter))
    (query, update)
  }

  def removeFromCarpools(loggedUserEmail: String): (DBObject, DBObject) = {
    val loggedUser = findUserByEmail(loggedUserEmail)
    val kindergarten = Kindergartens.find(
      loggedUser.kindergarten.name,
      loggedUser.kindergarten.street,
      loggedUser.kindergarten.num,
      loggedUser.kindergarten.city)

    val loggedUserGroupEmailsList = kindergarten.usersEmails filter(group => group contains loggedUserEmail)
    val groupAfter = loggedUserGroupEmailsList.flatten filter (email => email != loggedUserEmail)
    val restUsersEmails = kindergarten.usersEmails filter(group => !(group contains loggedUserEmail))
    val usersEmailsAfter = List(List(loggedUserEmail)) ::: List(groupAfter) ::: restUsersEmails

    val query = MongoDBObject(
      "name" -> kindergarten.name,
      "street" -> kindergarten.street,
      "num" -> kindergarten.num,
      "city" -> kindergarten.city)
    val update = MongoDBObject("$set" -> MongoDBObject("usersemails" -> usersEmailsAfter))
    (query, update)
  }

  def convertCursorToUsersList(MongoUsers: com.mongodb.casbah.MongoCursor): List[User] = {
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

  def convertDBObjectToUser(userMongo: MongoDBObject): User = {
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

  def convertToSimpleUser(user: User) = SimpleUser(
    user.email,
    user.name,
    user.surname,
    user.street,
    user.city,
    user.kindergarten.name,
    user.kindergarten.street,
    user.kindergarten.num,
    user.kindergarten.city)
}
