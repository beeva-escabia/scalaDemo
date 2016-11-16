package money

case class Converter(conversion: Conversion) extends {

  def convert(from: Currency, to: Currency): BigDecimal = {
    if(from == to) 1
    else conversion.getOrElse((from, to), 1 / conversion((to, from)))
  }

}