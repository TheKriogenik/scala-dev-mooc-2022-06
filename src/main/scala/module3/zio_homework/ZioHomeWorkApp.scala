package module3.zio_homework

import module3.zio_homework.EffectTimeLogger.EffectTimeLogger
import zio.clock.Clock
import zio.console.Console
import zio.random.Random
import zio.{ExitCode, Has, URIO}

object ZioHomeWorkApp extends zio.App {
  override def run(args: List[String]): URIO[Clock with Random with Console, ExitCode] =
//    (appWithTimeLogg.provideSomeLayer[Clock with Random with Console](EffectTimeLogger.live)).exitCode
  loadConfigOrDefault.exitCode
}
