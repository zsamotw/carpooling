package models

import com.mongodb.casbah.Imports._
import org.joda.time.DateTime
import play.api.data.Form
import play.api.data.Forms._
import scala.util.Try

case class User(
  email: String,
  password: String,
  name: String,
  surname: String,
  street: String,
  city: String,
  seats: Int,
  kindergarten: Kindergarten, 
  requests: Set[String],
  len: String,
  lon: String,
  admin: Boolean)

case class SimpleUser(
  email: String,
  name: String,
  surname: String,
  street: String,
  city: String,
  seats: Int,
  len: String,
  lon: String,
  kindergarten: Kindergarten,
  admin: Boolean)

/**
  * Classes and objects for creating user and login from form
  */


case class UserFormData(
  email: String,
  password: String,
  name: String,
  surname: String,
  street: String,
  city: String,
  seats: Int)


object UserForm {
  val form = Form(
    mapping(
      "email" -> text,
      "password" -> text,
      "name" -> text,
      "surname" -> text,
      "street" -> text,
      "city" -> text,
      "seats" -> number)(UserFormData.apply)(UserFormData.unapply))
}

case class Login(email: String, password: String)

object LoginForm {
  val form = Form(
    mapping(
      "email" -> text,
      "password" -> text)(Login.apply)(Login.unapply))
}

/*
 * Methods for Users
 */


object Users {

  def returnNewUser(
    email: String,
    password: String,
    name: String,
    surname: String,
    street: String,
    city: String,
    seats: Int,
    kindergarten: Kindergarten,
    requests: Set[String],
    len: String,
    lon: String,
    admin: Boolean) = User(email, password, name, surname, street, city, seats, kindergarten, requests, len, lon, admin)

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
      case Some(u) => false
    }
  }

  def findUserByEmail(email: String): User = {
    val userOpt = MongoFactory.users.findOne("email" $eq email)
    userOpt match {
      case Some(user) =>
        convertDBObjectToUser(user)
      case None => throw new NoSuchElementException
    }
  }

  def listAll: List[User] = {
    val allUsers = MongoFactory.users.find
    convertCursorToUsersList(allUsers)
  }

  def add(user: User): (User, CommunityMessage) = {
    val content = s"Welcome new User: ${user.name} ${user.surname}"
    val message = CommunityMessage(new DateTime, user.kindergarten, content)
    (user, message)
  }

  def delete(user: User): (User, List[User], DBObject, DBObject) = {
    val userGroup = usersFromGroup(user)
    val kindergarten = user.kindergarten

    val usersEmailsGroup = for {
      group <- kindergarten.usersEmails
      if group contains user.email
      email <- group} yield email

    val usersEmailsGroupAfter = for {
      email <- usersEmailsGroup
      if email != user.email
    } yield email

    val usersEmailsAfter =
      if (usersEmailsGroupAfter isEmpty) {
        for(group <- kindergarten.usersEmails; if group != usersEmailsGroup) yield group
      }
      else {
        usersEmailsGroupAfter :: (for(group <- kindergarten.usersEmails; if group != usersEmailsGroup) yield group)
      }

    val query = MongoDBObject(
      "name" -> kindergarten.name,
      "street" -> kindergarten.street,
      "num" -> kindergarten.num,
      "city" -> kindergarten.city)

    val update = MongoDBObject("$set" -> MongoDBObject("usersemails" -> usersEmailsAfter))

    (user, userGroup, query, update)
  }

  def leaveGroup(user: User): (User, List[User], (DBObject, DBObject, CommunityMessage)) = {
    val userGroup = Users.usersFromGroup(user)
    val dataToDB = Users.removeFromCarpools(user)
    (user, userGroup, dataToDB)
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

  def areEnoughSeats(fstGroup: List[User], scdGroup: List[User]): Boolean =
    fstGroup.forall(user => user.seats >= scdGroup.length) && scdGroup.forall(user => user.seats >= fstGroup.length)

  def usersFromGroup(user: User): List[User] = {
    val kindergarten = user.kindergarten
    val loggedUserGroup = kindergarten.usersEmails filter(group => group contains user.email)

    val group = {
      for(email <- loggedUserGroup.flatten) yield findUserByEmail(email)
    }
    group
  }

  def userEmailsGroup(user: User): List[String] = {
    val emailsGroup = user.kindergarten.usersEmails filter(group => group contains user.email)
    val groupWithout = {
      for(email <- emailsGroup.flatten) yield email
    }
    groupWithout
  }

  def addToCarpools(userToReplyEmail: String, loggedUserEmail: String): (DBObject, DBObject, CommunityMessage) = {
    val loggedUser = Users.findUserByEmail(loggedUserEmail)
    val kindergarten = loggedUser.kindergarten

    val loggedUserGroupEmailsList = for {
      group <- kindergarten.usersEmails
      if group contains loggedUserEmail
      email <- group} yield email

    val userToReplyGroupEmailsList = for {
      group <- kindergarten.usersEmails
      if group contains userToReplyEmail
      email <- group} yield email

    val commonGroupEmailsList =
      if (loggedUserGroupEmailsList != userToReplyGroupEmailsList) loggedUserGroupEmailsList ::: userToReplyGroupEmailsList
      else loggedUserGroupEmailsList

    val restUsersEmails = for {
      group <- kindergarten.usersEmails
      if!(group contains loggedUserEmail) && !(group contains userToReplyEmail)
    } yield group

    val usersEmailsAfter =  commonGroupEmailsList :: restUsersEmails

    val fstG = loggedUserGroupEmailsList map(email => findUserByEmail(email))
    val scdG = userToReplyGroupEmailsList map(email => findUserByEmail(email))
    val content = s"Two group from ${kindergarten.name} has joined: " + userGroupToString(fstG) + " | " + userGroupToString(scdG)
    val message = CommunityMessage(new DateTime, kindergarten, content)

    val query = MongoDBObject(
      "name" -> kindergarten.name,
      "street" -> kindergarten.street,
      "num" -> kindergarten.num,
      "city" -> kindergarten.city)
    val update = MongoDBObject("$set" -> MongoDBObject("usersemails" -> usersEmailsAfter))

    (query, update, message)
  }

  def removeFromCarpools(user: User): (DBObject, DBObject, CommunityMessage) = {
    val kindergarten = user.kindergarten

    val usersEmailsWithoutLoggedUser = for {
      group <- kindergarten.usersEmails
      if group contains user
      email <- group
      if email != user.email
    } yield email

    val restUsersEmails = kindergarten.usersEmails filter(group => !(group contains user.email))
    val usersEmailsAfter = List(user.email) :: usersEmailsWithoutLoggedUser :: restUsersEmails

    val content = s"User: ${user.name} ${user.surname} from ${user.kindergarten.name} has just leaved his group"
    val message = CommunityMessage(new DateTime, user.kindergarten, content)
    val query = MongoDBObject(
      "name" -> kindergarten.name,
      "street" -> kindergarten.street,
      "num" -> kindergarten.num,
      "city" -> kindergarten.city)
    val update = MongoDBObject("$set" -> MongoDBObject("usersemails" -> usersEmailsAfter))

    (query, update, message)
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
        kindergarten = Kindergartens.find(kgName, kgStreet, kgNum,kgCity)
        admin = userMongo.getAs[Boolean]("admin").get
      } yield User(
        email,
        password,
        name,
        surname,
        street,
        city,
        seats,
        kindergarten,
        requests,
        len,
        lon,
        admin)
    res.toList
  }


  def convertToSimpleUser(user: User) = SimpleUser(
    user.email,
    user.name,
    user.surname,
    user.street,
    user.city,
    user.seats,
    user.len,
    user.lon,
    user.kindergarten,
    user.admin)


  def userGroupToString(group: List[User]): String = {
    group map(user => s"${user.name} ${user.surname}") mkString(", ")
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
    val kindergarten = Kindergartens.find(kgName, kgStreet, kgNum,kgCity)
    val admin =  userMongo.getAs[Boolean]("admin").get
    User(
      email,
      password,
      name,
      surname,
      street,
      city,
      seats,
      kindergarten,
      requests,
      len,
      lon,
      admin)
  }
}
