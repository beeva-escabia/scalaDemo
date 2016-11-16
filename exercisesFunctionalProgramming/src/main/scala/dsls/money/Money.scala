package money

case class Money(amount: BigDecimal, currency: Currency)(implicit converter: Converter){

  def apply(amount: BigDecimal, currency: Currency)(implicit converter: Converter): Money = new Money(amount,currency)
  
  def +(thatMoney: Money): Money = performOperation(thatMoney, _ + _)

  def -(thatMoney: Money): Money = performOperation(thatMoney, _ - _)

  def performOperation(thatMoney: Money, operation: (BigDecimal, BigDecimal) => BigDecimal): Money = {
    thatMoney match {
      case Money(v, c) if c == currency => Money(operation(amount, v), currency)
      case Money(v, c) => performOperation(thatMoney.to(currency), operation)
    }
  }

  def to(thatCurrency: Currency): Money = {
    val rate = converter.convert(currency, thatCurrency)
    Money(amount * rate, thatCurrency)
  }
}