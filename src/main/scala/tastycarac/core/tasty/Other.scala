package tastycarac.core.tasty


class IdGenerator(prefix: String = ""):
  var counter: Int = -1
  def nextId: String =
    counter += 1
    s"$prefix#$counter"
  def reset: Unit =
    counter = -1

class Counter():
  var counter: Int = -1
  def next: Int =
    counter += 1
    counter
  def reset: Unit =
    counter = -1
