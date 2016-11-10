object Typeclasses{

  trait Show[A]{
    def show(f: A): String
  }

  implicit val stringShow = new Show[String]{
    def show(s: String) = s
  }

  def log[A](a: A)(implicit s: Show[A]) = println(s.show(a));



  log("a string")

}