import money.Currency.{EUR, GBP, USD}
import money.{Conversion, Converter, Money}

val conversion: Conversion = Map(
  (GBP, EUR) -> 1.39,
  (EUR, USD) -> 1.08,
  (GBP, USD) -> 1.5
)

val converter = Converter(conversion)

val result = Money(42, USD)(converter) + Money(35, EUR)(converter)

val resultToPound = result.to(GBP)

println(resultToPound)