package discord

import cats.data.{EitherT, OptionT}
import cats.effect.IO
import cats.effect.kernel.Resource
import cats.instances.list.*
import cats.syntax.either.*
import cats.syntax.traverse.*
import discord.DiscordClient.*
import net.dv8tion.jda.api.{JDA, JDABuilder}
import net.dv8tion.jda.api.entities.Guild
import net.dv8tion.jda.api.entities.channel.Channel as JDAChannel
import net.dv8tion.jda.api.entities.channel.concrete.{ForumChannel as JDAForumChannel, TextChannel as JDATextChannel}
import net.dv8tion.jda.api.requests.RestAction
import org.typelevel.log4cats.Logger

import scala.jdk.CollectionConverters.*

class DiscordClient(val jda: JDA):
  def guilds: Set[Guild] = jda.getGuilds.asScala.toSet

  private def channelGuild(channelID: Long): Guild =
    guilds.find(_.getChannels.asScala.exists(_.getIdLong == channelID)).get

  def textChannel(id: DiscordID)(using Logger[IO]): OptionT[IO, TextChannel] =
    getter[Long](
      id.toLong,
      "TextChannel",
      jda.getTextChannelById,
      (channel: JDATextChannel) => new TextChannel(channel, channelGuild(channel.getIdLong)),
    )

  def channels(ids: List[DiscordID])(using Logger[IO]): IO[List[Channel]] =
    ids.flatTraverse { id =>
      getter[Long](
        id.toLong,
        "Channel",
        jda.getChannelCache.getElementById,
        (channel: JDAChannel) => new Channel(channel, channelGuild(channel.getIdLong)).toSpecific,
      ).value
        .map(_.toList)
    }

  def forumChannel(id: DiscordID)(using Logger[IO]): OptionT[IO, ForumChannel] =
    getter[Long](
      id.toLong,
      "ForumChannel",
      jda.getForumChannelById,
      (channel: JDAForumChannel) => new ForumChannel(channel, channelGuild(channel.getIdLong)),
    )

object DiscordClient:
  final private[discord] class PartiallyAppliedGetter[ID](private val dummy: Boolean = true) extends AnyVal:
    def apply[A, B](id: ID, what: String, get: ID => A, transform: A => B)(using Logger[IO]): OptionT[IO, B] =
      OptionT {
        EitherT
          .fromOption[IO](Option(get(id)), new Exception(s"Failed to get $what with id $id"))
          .foldF(
            error => Logger[IO].error(error.getMessage).as(None),
            channel => IO.pure(Some(transform(channel))),
          )
      }

  def getter[ID]: PartiallyAppliedGetter[ID] = new PartiallyAppliedGetter[ID]

  final private[discord] class PartiallyAppliedActionGetter[ID](private val dummy: Boolean = true) extends AnyVal:
    def apply[A, B](id: ID, what: String, get: ID => RestAction[A], transform: A => B)(using
      Logger[IO],
    ): OptionT[IO, B] =
      OptionT {
        EitherT(get(id).toIO.attempt.map(_.leftMap(_ => new Exception(s"Failed to get $what using: $id"))))
          .foldF(
            error => Logger[IO].error(error.getMessage).as(None),
            channel => IO.pure(Some(transform(channel))),
          )
      }

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
