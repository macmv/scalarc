
package sample

class Bar(a: String) {
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

  new Foo(2 + 3) {
    def foo = 3
  }
}

val foo = 3

foo
