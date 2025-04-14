package discord

import cats.data.{EitherT, OptionT}
import cats.effect.IO
import cats.effect.kernel.Resource
import cats.instances.list.*
import cats.syntax.either.*
import cats.syntax.traverse.*
import discord.DiscordClient.*
import discord.Maybe
import net.dv8tion.jda.api.{JDA, JDABuilder}
import net.dv8tion.jda.api.entities.Guild
import net.dv8tion.jda.api.entities.channel.middleman.MessageChannel
import net.dv8tion.jda.api.requests.RestAction
import org.typelevel.log4cats.Logger

import scala.jdk.CollectionConverters.*

class DiscordClient(val jda: JDA):
  def channelByID(id: DiscordID)(using Logger[IO]): OptionT[IO, Channel] =
    OptionT {
      getter[Long](
        id.toLong,
        "channel",
        (id: Long) => jda.getChannelById(classOf[MessageChannel], id),
        (channel: MessageChannel) => new Channel(channel, channelGuildByID(channel.getIdLong)),
      )
        .foldF(
          error => Logger[IO].error(error.getMessage).as(None),
          channel => IO.pure(Some(channel)),
        )
    }

  def channelsByIDs(ids: List[DiscordID])(using Logger[IO]): IO[List[Channel]] =
    ids.flatTraverse(id => channelByID(id).value.map(_.toList))

  def guilds: Set[Guild] = jda.getGuilds.asScala.toSet

  private def channelGuildByID(id: Long): Guild = guilds.find(_.getChannels.asScala.exists(_.getIdLong == id)).get

object DiscordClient:
  final private[discord] class PartiallyAppliedGetter[ID](private val dummy: Boolean = true) extends AnyVal:
    def apply[A, B](id: ID, what: String, get: ID => A, transform: A => B): Maybe[B] =
      EitherT.fromOption[IO](Option(get(id)), new Exception(s"Failed to get $what with id $id")).map(transform)

  def getter[ID]: PartiallyAppliedGetter[ID] = new PartiallyAppliedGetter[ID]

  final private[discord] class PartiallyAppliedActionGetter[ID](private val dummy: Boolean = true) extends AnyVal:
    def apply[A, B](id: ID, what: String, get: ID => RestAction[A], transform: A => B): Maybe[B] =
      EitherT(get(id).toIO.attempt.map(_.leftMap(_ => new Exception(s"Failed to get $what using: $id")))).map(transform)

  def actionGetter[ID]: PartiallyAppliedActionGetter[ID] = new PartiallyAppliedActionGetter[ID]

  def apply(
    token: String,
  )(using Logger[IO]): Resource[IO, DiscordClient] =
    val acquire = IO {
      JDABuilder
        .createDefault(token)
        .build()
        .awaitReady()
    }

    def shutdown(jda: JDA) = IO {
      jda.shutdown()
      jda.awaitShutdown()
    }.void

    Resource
      .make(acquire)(shutdown)
      .evalTap(_ => Logger[IO].debug("Discord client acquired"))
      .onFinalize(Logger[IO].debug("Discord client released"))
      .map(new DiscordClient(_))
