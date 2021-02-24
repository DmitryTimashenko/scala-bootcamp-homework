package com.evolutiongaming.bootcamp.typeclass.v2

object TypeclassTask extends App {

  trait HashCode[T] {
    def hash(t: T): Int
  }

  object HashCode {
    def apply[T: HashCode]: HashCode[T] = implicitly[HashCode[T]]
  }

  implicit class HashCodeSyntax[A: HashCode](x: A) {
    def hash: Int = HashCode[A].hash(x)
  }

  implicit val hashcodeString: HashCode[String] = string => string.hashCode

}

object Task1 {
  final case class Money(amount: BigDecimal)

  implicit val moneyOrdering: Ordering[Money] = Ordering[BigDecimal].on(_.amount)
}

object Task2 {

  final case class User(id: String, name: String)
  trait Show[T] { // fancy toString
    def show(entity: T): String
  }

  object Show {
    def apply[S: Show]: Show[S] = implicitly[Show[S]]
  }

  implicit class ShowSyntax[S: Show](x: S) {
    def show: String = Show[S].show(x)
  }

  implicit val showUser: Show[User] = user => s"id: ${user.id}, name: ${user.name}"

  User("1", "Oleg").show
}

object Task3 {
  type Error = String

  trait Parse[T] { // invent any format you want or it can be csv string
    def parse(entity: String): Either[Error, T]
  }

  final case class User(id: String, name: String)

  object Parse {
    def apply[P: Parse]: Parse[P] = implicitly[Parse[P]]
  }

  implicit class ParseSyntax[P: Parse](x: String) {
    def parse[A: Parse]: Either[Error, A] = Parse[A].parse(x)
  }

  implicit val parseUser: Parse[User] = csvString => {
    csvString.split(",").toList match {
      case id :: name :: Nil => Right(User(id, name))
      case _                 => Left(s"Cannot parse string '$csvString' as User")
    }
  }

  "lalala".parse
}

object Task4 {
  final case class Client(uid: String, fullname: String, email: String)

  trait Equals[T] {
    def equals(left: T, right: T): Boolean
  }

  object Equals {
    def apply[T: Equals]: Equals[T] = implicitly[Equals[T]]
  }

  implicit class EqualsSyntax[T: Equals](a: T) {
    def ===(b: T): Boolean = Equals[T].equals(a, b)
  }

  implicit val clientEquals: Equals[Client] =
    (a: Client, b: Client) => a.email == b.email && a.fullname == b.fullname

  val a: Client = Client("c1472aab-762a-4949-93a6-de087d3806b1", "Jennifer Lopez", "J-LO@gmail.com")
  val b: Client = Client("f6d0012d-5b15-478b-8ce6-4cdb892abe76", "Jennifer Lopez", "J-LO@gmail.com")

  a === b
}

object AdvancedHomework {
  // TODO: create a typeclass for flatMap method
}

