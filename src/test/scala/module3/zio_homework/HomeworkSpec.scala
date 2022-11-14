package module3.zio_homework

import zio.test.Assertion.equalTo
import zio.test.environment.{TestClock, TestConsole, TestRandom}
import zio.test._
import zio.{Ref, ZIO, console}
import zio.duration.durationInt
import zio.test.TestAspect.{before, beforeAll, sequential}

import scala.language.postfixOps

object HomeworkSpec extends DefaultRunnableSpec {
  override def spec = suite("homework")(
    guessProgramSpec,
    doWhileSpec,
    fiberSpec
  ) @@ sequential @@ beforeAll(TestConsole.clearOutput *> TestConsole.clearInput <* TestRandom.clearInts)

  private val guessProgramSpec = suite("guessProgram")(
    testM("happy path") {
      for {
        _ <- TestRandom.feedInts(1)
        _ <- TestConsole.feedLines("1")
        _ <- guessProgram
        result <- TestConsole.output
      } yield {
        assert(result.last)(equalTo("Вы угадали!\n"))
      }
    },
    testM("failure") {
      val target = 1
      for {
        _ <- TestRandom.feedInts(target)
        _ <- TestConsole.feedLines("2")
        _ <- guessProgram
        result <- TestConsole.output
      } yield {
        assert(result.last)(equalTo(s"Неправильный ответ! Загаданное число: $target\n"))
      }
    },
    testM("not a number") {
      for {
        _ <- TestRandom.feedInts(1)
        _ <- TestConsole.feedLines("NOT A NUMBER")
        _ <- guessProgram
        result <- TestConsole.output
      } yield {
        assert(result.last)(equalTo(s"Ошибка! Введено не число!\n"))
      }
    }
  )

  private val doWhileSpec = {
    val p = { x: Int => x <= 0 }

    def f(r: Ref[Int]) = for {
      x <- r.get
      _ <- console.putStrLn(s"REPEAT: #$x")
      _ <- r.set(x - 1)
      next <- r.get
    } yield next

    def testApp(times: Int, expected: Int) = for {
      _ <- TestConsole.clearOutput
      r <- Ref.make(times)
      _ <- doWhile(p)(f(r))
      result <- TestConsole.output
    } yield {
      assert(result.size)(equalTo(expected))
    }

    suite("doWhile")(
      testM("repeat x times") {
        checkM(Gen.int(1, 10)) { times =>
          testApp(times, times)
        }
      },
      testM("run at least once") {
        testApp(0, 1)
      }
    )
  }

  private val fiberSpec = {
    suite("fibers")(
      testM("Task 4.1") {
        val target = 1
        for {
          _ <- TestRandom.clearInts
          _ <- TestRandom.feedInts(target)
          app <- task4_1.fork
          _ <- TestClock.adjust(1 seconds)
          x <- app.join
        } yield {
          assert(target)(equalTo(x))
        }
      },
      testM("Task 4.2") {
        val target = 1 to 10
        for {
          _ <- TestRandom.feedInts(target.toList: _*)
          xs <- (ZIO.reduceAll(ZIO.succeed(0), task4_2) { (acc, x) => acc + x }).fork
          _ <- TestClock.adjust(10 seconds)
          result <- xs.join
        } yield {
          assert(target.sum)(equalTo(result))
        }
      },
      testM("Task 4.3") {
        val target = 1 to 10
        val targetTime = 10
        for {
          _ <- TestRandom.clearInts
          _ <- TestRandom.feedInts(target.toList: _*)
          xs <- task4_3.fork
          _ <- TestClock.adjust(targetTime seconds)
          result <- xs.join
          output <- TestConsole.output
        } yield {
          assert(target.sum)(equalTo(result)) &&
            assert(s"${target.sum}\n")(equalTo(output.last)) &&
            assert(s"Running time $targetTime\n")(equalTo(output.dropRight(1).last))
        }
      },
      testM("Task 4.4") {
        val target = 1 to 10
        val targetTime = 1
        for {
          _ <- TestRandom.clearInts
          _ <- TestRandom.feedInts(target.toList: _*)
          xs <- task4_4.fork
          _ <- TestClock.adjust(targetTime seconds)
          result <- xs.join
          output <- TestConsole.output
        } yield {
          assert(target.sum)(equalTo(result)) &&
            assert(s"${target.sum}\n")(equalTo(output.last)) &&
          assert(s"Running time $targetTime\n")(equalTo(output.dropRight(1).last))

        }
      }
    )
  }

}
