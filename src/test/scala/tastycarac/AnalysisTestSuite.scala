package tastycarac

import java.io.File
import java.nio.file.{Path, Paths, Files, FileSystems}
import java.net.URI

import tastyquery.jdk.ClasspathLoaders
import tastyquery.Classpaths.*
import tastyquery.Contexts.*

import dotty.tools.dotc

import datalog.dsl.Program
import datalog.execution.SemiNaiveExecutionEngine
import datalog.storage.DefaultStorageManager

import tastycarac.core.*


abstract class AnalysisTestSuite(file: String, sets: Set[FactSet | RuleSet]) extends munit.FunSuite:
  private var tmpDir: Path = null

  var tasty: Tasty = null

  override def beforeAll(): Unit =
    tmpDir = Files.createTempDirectory("tmp")
    val inputPath = Path.of(getClass.getResource("/" + file).getPath)

    val scalaClasspath = sys.env.get("TASTYCARAC_DEFAULTCLASSPATH").get.split(";").map(Paths.get(_)).toList

    dotc.Main.process(Array(
      "-d", tmpDir.toString,
      "-classpath", scalaClasspath.mkString(File.pathSeparator),
      inputPath.toString
    ))

    val program = Program(SemiNaiveExecutionEngine(DefaultStorageManager()))
    tasty = program.loadTasty(List(tmpDir), sets)

  override def afterAll(): Unit =
    def del(p: Path): Unit =
      if Files.isDirectory(p) then Files.list(p).forEach(del)
      Files.delete(p)
    del(tmpDir)
