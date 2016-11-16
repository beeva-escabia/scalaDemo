import money.Currency.{EUR, GBP, USD}
import money.{Conversion, Converter, Currency, Money}

val conversion: Conversion = Map(
  (GBP, EUR) -> 1.39,
  (EUR, USD) -> 1.08,
  (GBP, USD) -> 1.5
)

implicit val converter = Converter(conversion)

implicit class BigDecimalOps(val value: BigDecimal)  {
  def apply(currency: Currency)(implicit converter: Converter): Money = Money(value, currency)
}

implicit class IntOps(val value: Int) {
  def apply(currency: Currency)(implicit converter: Converter): Money = (value: BigDecimal).apply(currency)
}

implicit class DoubleOps(val value: Double) {
  def apply(currency: Currency)(implicit converter: Converter): Money = (value: BigDecimal).apply(currency)
}



// Prueba


val result = 42(USD) - 35(EUR)
val resultToPound = result to GBP
