package discord

import cats.data.EitherT
import cats.effect.IO
import cats.effect.kernel.Resource
import cats.syntax.either.*
import discord.Discord.*
import net.dv8tion.jda.api.{JDA, JDABuilder}
import net.dv8tion.jda.api.entities.channel.middleman.MessageChannel
import net.dv8tion.jda.api.requests.RestAction
import org.typelevel.log4cats.Logger

import scala.concurrent.Promise

type Maybe[T] = EitherT[IO, Throwable, T]

extension [A](action: RestAction[A])
  def toIO: IO[A] = IO.fromFuture(IO {
    val p = Promise[A]()
    action.queue(p.success, p.failure)
    p.future
  })

class Discord(val jda: JDA):
  def channelByID(id: DiscordID): Maybe[Channel] =
    getter[Long](id.toLong, "channel", jda.getChannelById(classOf[MessageChannel], _), new Channel(_))

object Discord:
  final private[discord] class PartiallyAppliedGetter[ID](private val dummy: Boolean = true) extends AnyVal:
    def apply[A, B](id: ID, what: String, get: ID => A, transform: A => B): Maybe[B] =
      EitherT.fromOption(Option(get(id)), new Exception(s"Failed to get $what with id $id")).map(transform)

  def getter[ID]: PartiallyAppliedGetter[ID] = new PartiallyAppliedGetter[ID]

  final private[discord] class PartiallyAppliedActionGetter[ID](private val dummy: Boolean = true) extends AnyVal:
    def apply[A, B](id: ID, what: String, get: ID => RestAction[A], transform: A => B): Maybe[B] =
      EitherT(get(id).toIO.attempt.map(_.leftMap(_ => new Exception(s"Failed to get $what using: $id")))).map(transform)

  def actionGetter[ID]: PartiallyAppliedActionGetter[ID] = new PartiallyAppliedActionGetter[ID]

  def apply(
    token: String,
  )(using Logger[IO]): Resource[IO, Discord] =
    val acquire = IO {
      JDABuilder
        .createDefault(token)
        .build()
        .awaitReady()
    }

    Resource
      .make(acquire)(jda => IO(jda.shutdown()))
      .evalTap(_ => Logger[IO].debug("JDA acquired"))
      .onFinalize(Logger[IO].debug("JDA released"))
      .map(new Discord(_))
