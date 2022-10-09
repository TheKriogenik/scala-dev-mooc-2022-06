package module3

import module3.zioConcurrency.{printEffectRunningTime, result, sendToDB}
import module3.zio_homework.EffectTimeLogger.EffectTimeLogger
import module3.zio_homework.config.AppConfig
import zio.{Has, Layer, RIO, Task, UIO, ULayer, URIO, URLayer, ZIO, ZLayer}
import zio.clock.{Clock, sleep}
import zio.console._
import zio.duration.durationInt
import zio.macros.accessible
import zio.random._

import java.io.IOException
import java.util.concurrent.TimeUnit
import scala.io.StdIn
import scala.language.postfixOps

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в когнсоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */


  lazy val guessProgram: RIO[Any with Random with Console, Unit] = for {
    target: Int <- ZIO.accessM[Random](_.get.nextIntBetween(1, 3))
    console <- ZIO.access[Console](_.get)
    _ <- console.putStrLn("Угадайте число от 1 до 3: ")
    res <- console.getStrLn.flatMap(x => ZIO.effect(x.toInt))
      .foldM(
        _ => console.putStrLn("Ошибка! Введено не число!") *> ZIO.effectTotal(Option.empty),
        x => ZIO.effectTotal(Option(x)))
    _ <- res.map { x =>
      if (x == target) {
        console.putStrLn("Вы угадали!")
      } else {
        console.putStrLn(s"Неправильный ответ! Загаданное число: $target")
      }
    }.getOrElse(ZIO.unit)
  } yield ()

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   *
   */

  def doWhile[R, E, A](p: A => Boolean)(f: => ZIO[R, E, A]): ZIO[R, E, A] = f >>= {
    x =>
      if (p(x))
        ZIO.succeed(x)
      else
        doWhile(p)(f)
  }

  def testDoWhile: RIO[Any with Console, Unit] = {
    val f = for {
      console <- ZIO.access[Console](_.get)
      _ <- console.putStr("Введите число: ")
      e <- console.getStrLn.map(x => x.toInt).orDie
    } yield (e)

    for {
      e <- doWhile { x: Int => x > 0 }(f)
      _ <- ZIO.access[Console](_.get.putStrLn("КОНЕЦ: " + e))
    } yield ()
  }

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */


  def loadConfigOrDefault: URIO[Console, AppConfig] = for {
    cfg <- config.load.foldM(
      e => ZIO.succeed(AppConfig("80", "localhost")) <* ZIO.accessM[Console](_.get.putStrLn("Ошибка чтения файла конфигурации! Использованы стандартные настройки." + e)),
      x => ZIO.succeed(identity(x))
    )
    _ <- ZIO.accessM[Console](_.get.putStrLn(cfg.toString))
  } yield cfg


  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff: RIO[Any with Random with Clock, Int] = for {
    clock <- ZIO.environment[Clock].map(x => x.get)
    _ <- clock.sleep(1 seconds)
    res <- ZIO.accessM[Random](_.get.nextIntBetween(0, 10))
  } yield res

  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects: Iterable[ZIO[Any with Random with Clock, Throwable, Int]] = ZIO.replicate(10)(eff)


  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекци "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  lazy val app: ZIO[Any with Clock with Console with Random, Throwable, Int] = for {
    result <- printEffectRunningTime(ZIO.reduceAll(ZIO.succeed(0), effects) { (acc, x) => acc + x })
    _ <- ZIO.accessM[Console](_.get.putStrLn(result.toString))
  } yield result


  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  lazy val appSpeedUp: RIO[Any with Console with Random with Clock, Int] = for {
    fiber <- printEffectRunningTime(ZIO.reduceAllPar(ZIO.succeed(0), effects) { (acc, x) => acc + x }).fork
    result <- fiber.join
    _ <- ZIO.accessM[Console](_.get.putStrLn(result.toString))
  } yield result


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * молжно было использовать аналогично zio.console.putStrLn например
   */

  object EffectTimeLogger {

    type EffectTimeLogger = Has[EffectTimeLogger.Service]

    trait Service {
      def log[R, E, A](zio: => ZIO[R, E, A]): ZIO[R with Console with Clock, E, A]
    }

    def live: URLayer[Clock with Console, EffectTimeLogger] = ZLayer.fromEffect {
      def service(console: Console.Service, clock: Clock.Service): Service = new Service {
        override def log[R, E, A](zio: => ZIO[R, E, A]): ZIO[R, E, A] = for {
          startTime <- clock.currentTime(TimeUnit.MILLISECONDS)
          res <- zio
          endTime <- clock.currentTime(TimeUnit.MILLISECONDS)
          _ <- console.putStrLn(s"Running time: ${endTime - startTime}ms")
        } yield res
      }

      for {
        console <- ZIO.access[Console](_.get)
        clock <- ZIO.access[Clock](_.get)
      } yield service(console, clock)
    }

    def log[R, E, A](zio: => ZIO[R, E, A]): ZIO[R with Clock with Console with EffectTimeLogger, E, A] = for {
      logger <- ZIO.access[EffectTimeLogger](_.get)
      result <- logger.log(zio)
    } yield result

  }


  /**
   * 6.
   * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
   *
   *
   */

  lazy val appWithTimeLogg: ZIO[Any with Console with Random with Clock with EffectTimeLogger, Throwable, Int] = for {
    //    logger <- ZIO.access[EffectTimeLogger](_.get)
    //    result <- logger.log(app)
    result <- EffectTimeLogger.log(app)
  } yield result

  /**
   *
   * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
   */

  lazy val runApp: ZIO[Clock with Random with Console, Throwable, Int] = appWithTimeLogg.provideSomeLayer[Clock with Random with Console](EffectTimeLogger.live)

}
