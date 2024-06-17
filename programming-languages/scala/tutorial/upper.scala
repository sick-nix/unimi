class Upper {
  def upper(strings: String*): Seq[String] = {
    strings.map((s: String) => s.toUpperCase())
  }
}

@main def l() = {
  val up = new Upper
  Console.println(up.upper("A", "First", "Scala", "Program"))
}
