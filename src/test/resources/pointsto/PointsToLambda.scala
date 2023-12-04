package pointstolambda

class A()

val lamb = (x: A) => x

object Main:

  def nonlamb(x: A) = x

  def main(): Unit =
    val x = A()
    val y = lamb(x)
    val y2 = ((x: A) => x)(x)
    val z = nonlamb(x)
