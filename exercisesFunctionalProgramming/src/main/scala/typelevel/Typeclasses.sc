
object Typeclasses{

  //Type classes
  trait Show[A]{
    def show(f: A): String
  }

  implicit val stringShow = new Show[String]{
    def show(s: String) = s
  }

  def log[A](a: A)(implicit s: Show[A]) = println(s.show(a))
  def log2[A: Show](a: A) = println(implicitly[Show[A]].show(a))


  implicit def optionShow[A](implicit sa: Show[A]) = new Show[Option[A]] {
    def show(oa: Option[A]): String = oa match {
      case None => "None"
      case Some(a) => "Some("+ sa.show(a) + ")"
    }
  }

  log(Option(Option("hello")))

}