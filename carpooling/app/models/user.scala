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
  requests: Set[String],
  carpools: Set[String],
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
        val usersEmails = kg.as[List[String]]("usersemails")
        val usersEmailsAfter = (usersEmails ::: List(user.email))
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
        val usersEmails = kg.as[List[String]]("usersemails")
        val usersEmailsAfter = usersEmails.filter(e => e != user.email)
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
        updateUserSetInDB(u, "requests", loggedUserEmail, (xs,y) => xs + y)
      case None =>
        throw new NoSuchElementException
    }
  }


  def addToCarpools(requestedUseremail: String, loggedUserEmail: String) = {

    val loggedUserOpt = MongoFactory.users.findOne("email" $eq loggedUserEmail)
    val requestUserOpt = MongoFactory.users.findOne("email" $eq requestedUseremail)
    loggedUserOpt match {
      case Some(u) =>
        updateUserSetInDB(u, "carpools", requestedUseremail, (xs,y) => xs + y)
        updateUserSetInDB(u, "requests", requestedUseremail, (xs,y) => xs - y)
      case None => throw new NoSuchElementException
    }
    requestUserOpt match {
      case Some(u) =>
        updateUserSetInDB(u, "carpools", loggedUserEmail, (xs,y) => xs + y)
 

      case None => throw new NoSuchElementException
    }
  }

  def convertCarpoolsToUsersList(user: User) = {
    //create emails list
    //convert to users list
    //
  }

  def updateUserFieldInDB(user: MongoDBObject, fieldName: String, data: String) = {
    val userField = user.as[String](fieldName)
    val userFieldAfter = userField + "," + data
    val userEmail = user.as[String]("email")
    val query = MongoDBObject("email" -> userEmail)
    val upadate = MongoDBObject("$set" -> MongoDBObject(fieldName -> userFieldAfter))
    MongoFactory.users.findAndModify(query, upadate)
  }

  def updateUserSetInDB(user: MongoDBObject, fieldName: String, data: String, f: ((Set[String], String) => Set[String])) = {
    val userSet = (user.as[List[String]](fieldName)).toSet
    val userSetAfter = f(userSet, data)
    val userEmail = user.as[String]("email")
    val query = MongoDBObject("email" -> userEmail)
    val upadate = MongoDBObject("$set" -> MongoDBObject(fieldName -> userSetAfter.toList))
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
        requests = userMongo.getAs[List[String]]("requests").get.toSet
        carpools = userMongo.getAs[List[String]]("carpools").get.toSet
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
    val requests = userMongo.getAs[List[String]]("requests").get.toSet
    val carpools = userMongo.getAs[List[String]]("carpools").get.toSet
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
