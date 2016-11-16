package money

trait Currency {
  def getCode: String
}

object Currency {

  object USD extends Currency {
    override def getCode: String = "USD"
  }

  object EUR extends Currency {
    override def getCode: String = "EUR"
  }

  object GBP extends Currency {
    override def getCode: String = "GBP"
  }

  def apply(s: String): Currency = s.toUpperCase match {
    case "USD" => USD
    case "EUR" => EUR
    case "GBP" => GBP
  }

}


