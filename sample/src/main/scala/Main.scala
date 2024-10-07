
package sample

class Bar(a: String) {
  println("Hello, World!", a)

  val b = 3

  b

  def foo(c: Int) = {
    a + b + c
  }
}

object Bar {
  new Bar("foo").foo(3)
}
