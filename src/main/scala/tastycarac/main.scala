package tastycarac

import java.nio.file.{Paths}

import datalog.dsl.*
import datalog.execution.*
import datalog.storage.*

import tastycarac.core.*

@main def main() =
  val program = Program(SemiNaiveExecutionEngine(DefaultStorageManager()))

  val x = 
    new Tasty(
      List(Paths.get("target/scala-3.3.0/classes/tastycarac")),
      Set(TestRSX)
    )(using program)

  println(x.get{TestFS.TestF}.solve())


  
  
