package models

import com.mongodb.casbah.Imports._
import play.api.data.Form
import play.api.data.Forms._

case class User(
  email: String,
  password: String,
  name: String,
  surname: String,
  city: String,
  street: String,kindergarten: KindergartenFormData,
  requests: List[String],
  carpools: List[String],
  len: String,
  lon: String)

case class UserFormData(
  email: String,
  password: String,
  name: String,
  surname: String,
  city: String,
  street: String,
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
      "city" -> text,
      "street" -> text,
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
    val kindergarten = MongoFactory.kindergartens.findOne(MongoDBObject(
      "name" -> user.kindergarten.name,
      "street" -> user.kindergarten.street,
      "num" -> user.kindergarten.num,
      "city" -> user.kindergarten.city))

    kindergarten match {
      case Some(kg) =>
        val usersEmails = kg.as[String]("usersemails")
        val usersEmailsAfter = usersEmails + "," + user.email
        val query = MongoDBObject("name" -> kg.as[String]("name"))
        val update = MongoDBObject("$set" -> MongoDBObject("usersemails" -> usersEmailsAfter))
        MongoFactory.kindergartens.findAndModify(query, update)
      case None => throw new NoSuchElementException
    }
  }

  def delete(user: User) = {
    val kindergarten = MongoFactory.kindergartens.findOne(MongoDBObject(
      "name" -> user.kindergarten.name,
      "street" -> user.kindergarten.street,
      "num" -> user.kindergarten.num,
      "city" -> user.kindergarten.city))

    kindergarten match {
      case Some(kg) => 
        val usersEmails = kg.as[String]("usersemails")
        val usersEmailsAfter = usersEmails.split(",").toList.filter(e => e != user.email).mkString(",")
        val query = MongoDBObject("name" -> kg.as[String]("name"))
        val update = MongoDBObject("$set" -> MongoDBObject("usersemails" -> usersEmailsAfter))
        MongoFactory.kindergartens.findAndModify(query, update)
      case None => throw new NoSuchElementException
    }
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

  def addRequest(email: String, loggedUserEmail: String) = {
    val mongoUserOpt = MongoFactory.users.findOne("email" $eq email)
    mongoUserOpt match {
      case Some(u) =>
        upadteUserInDB(u, "requests", loggedUserEmail)
        // val userRequests = u.as[String]("requests")
        // val userRequestsAfter = userRequests + "," + loggedUserEmail
        // val query = MongoDBObject("email" -> email)
        // val update = MongoDBObject("$set" -> MongoDBObject("requests" -> userRequestsAfter))
        // MongoFactory.users.findAndModify(query, update)
      case None => throw new NoSuchElementException
    }
  }

 

  def addToCarpools(requestedUseremail: String, loggedUserEmail: String) = {

    // def addEmail(user: MongoDBObject, email: String) = {
    //   val userCarpools = u.as[String]("carpools")
    //   val userEmail = u.as[String]("email")
    //   val userCarpoolsAfter = userCarpools + "," + email
    //   val query = MongoDBObject("email" -> userEmail)
    //   val update = MongoDBObject("$set" -> MongoDBObject("carpools" -> userCarpoolsAfter))
    //   MongoFactory.users.findAndModify(query, update)
    // }

    val loggedUserOpt = MongoFactory.users.findOne("loggedUserEmail" $eq email)
    val requestUserOpt = MongoFactory.users.findOne("email" $eq email)
    loggedUserOpt match {
      case Some(u) =>
        updateUserInDB(u, "carpools", requestedUseremail)
      case None => throw new NoSuchElementException
    }
    requestUserOpt match {
      case Some(u) =>
        updateUserInDB(u, "carpools", loggedUserEmail)
      case None => throw new NoSuchElementException
    }
  }

  def convertCarpoolsToUsersList(user: User) = {
    //zrob liste maili
    //przez for yield MogoDBobjecy + covert to user
    //
  }

  def upadateUserInDB(user: MongoDBObject, field: String, data: String) = {
    val userFiled = user.as[String](field)
    val userFieldAfter = userField + "," + data
    val userEmail = user.as[String]("email")
    val query = MongoDBObject(field -> userEmail)
    val upadate = MongoDBObject("$set" -> MongoDBObject(field -> userFieldAfter))
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
        kgName = userMongo.getAs[String]("kgname").get
        kgStreet = userMongo.getAs[String]("kgstreet").get
        kgNum = userMongo.getAs[Int]("kgnum").get
        kgCity = userMongo.getAs[String]("kgcity").get
        requests = userMongo.getAs[String]("requests").get.split(",").map(_.trim).toList.drop(1)
        carpools = userMongo.getAs[String]("carpools").get.split(",").map(_.trim).toList.drop(1)
        len = userMongo.getAs[String]("len").get
        lon = userMongo.getAs[String]("lon").get
      } yield User(
        email,
        password,
        name,
        surname,
        street,
        city,
        KindergartenFormData(
          kgName,
          kgStreet,
          kgNum.toInt,
          kgCity),
        requests,
        carpools,
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
    val kgName =  userMongo.getAs[String]("kgname").get
    val kgStreet =  userMongo.getAs[String]("kgstreet").get
    val kgNum =  userMongo.getAs[Int]("kgnum").get
    val kgCity =  userMongo.getAs[String]("kgcity").get
    val requests = userMongo.getAs[String]("requests").get.split(",").map(_.trim).toList.drop(1)
    val carpools = userMongo.getAs[String]("carpools").get.split(",").map(_.trim).toList.drop(1)
    val len =  userMongo.getAs[String]("len").get
    val lon =  userMongo.getAs[String]("lon").get
    User(
      email,
      password,
      name,
      surname,
      city,
      street,
      KindergartenFormData(
        kgName,
        kgStreet,
        kgNum.toInt,
        kgCity),
      requests,
      carpools,
      len,
      lon)
  }
}
