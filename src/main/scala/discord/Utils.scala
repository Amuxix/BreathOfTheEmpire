package discord

import cats.data.EitherT
import cats.effect.IO
import net.dv8tion.jda.api.requests.RestAction

import scala.concurrent.Promise

type Maybe[T] = EitherT[IO, Throwable, T]

extension [A](action: RestAction[A])
  def toIO: IO[A] = IO.fromFuture(IO {
    val p = Promise[A]()
    action.queue(p.success, p.failure)
    p.future
  })
