package scala.io

trait Output {
  def println(m: Any)
  def printf(format: String, args: Any*)
  def flush()
}
trait ConsoleOutput extends Output {
  override def println(m: Any) { Console.println(m) }
  override def printf(format: String, args: Any*) { Console.printf(format, args: _*) }
  override def flush() { Console.flush() }
}
