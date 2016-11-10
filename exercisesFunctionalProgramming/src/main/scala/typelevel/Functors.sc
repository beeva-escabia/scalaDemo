import cats._

object Functors{

  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    def map[A,B](fa: Option[A])(f: A => B) = fa map f
  }

  implicit val listFunctor: Functor[List] = new Functor[List] {
    def map[A, B](fa: List[A])(f: A => B) = fa map f
  }

  val len: String => Int = _.length
  Functor[List].map(List("qwer", "adsfg"))(len)

  Functor[Option].map(Some("adsf"))(len)

}