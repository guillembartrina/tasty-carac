package tastycarac.core.tasty


class IdGenerator(prefix: String = ""):
  var counter: Int = -1
  def nextId: String =
    counter += 1
    s"$prefix#$counter"

class Counter():
  var counter: Int = -1
  def next: Int =
    counter += 1
    counter
