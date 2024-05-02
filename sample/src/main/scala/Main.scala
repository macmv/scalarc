

class Foo(a: String) {
  println("Hello, World!", a)

  val b = 3

  b

  def foo(c: Int) = {
    a + b + c

    for (i <- 0 to 10) {
      a + b + c + i
      println(i)
    }
  }

  Foo
}
