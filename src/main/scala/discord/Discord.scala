package discord

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.instances.list.*
import cats.syntax.foldable.*
import empire.{Article, PublishCategory}
import fs2.Pipe
import fs2.Stream
import org.typelevel.log4cats.Logger

class Discord(
  windsChannels: List[Channel],
  mandatesChannels: List[Channel],
  motionsChannels: List[Channel],
  ritualsChannels: List[Channel],
  othersChannels: List[Channel],
)(using Logger[IO]):
  private def publishArticleAsText(channel: Channel, article: Article): IO[Unit] =
    channel.sendMessage(s"[$article](<${article.uri}>)").void

  private val log: Pipe[IO, (Channel, Article), (Channel, Article)] =
    _.evalTap { (channel, article) =>
      Logger[IO].info(s"Publishing $article to ${channel.guild.getName}/${channel.name}").void
    }

  private val assignPublishChannels: Pipe[IO, Article, (Channel, Article)] =
    _.flatMap { article =>
      (article.publishCategory match
        case PublishCategory.Wind    => Stream.emits(windsChannels)
        case PublishCategory.Mandate => Stream.emits(mandatesChannels)
        case PublishCategory.Motion  => Stream.emits(motionsChannels)
        case PublishCategory.Ritual  => Stream.emits(ritualsChannels)
        case PublishCategory.Other   => Stream.emits(othersChannels)
      )
        .map(_ -> article)
    }

  private val publishAsText: Pipe[IO, (Channel, Article), Unit] =
    _.evalMap(publishArticleAsText.tupled)

  val publishArticle: Pipe[IO, Article, Unit] =
    _.through(assignPublishChannels)
      .through(log)
      .through(publishAsText)

object Discord:
  def warnMissingGuilds(guilds: Map[Long, String], channels: List[Channel], what: String)(using Logger[IO]): IO[Unit] =
    guilds.keySet.diff(channels.map(_.guild.getIdLong).toSet).map(guilds.apply).toList.traverse_ { guildName =>
      Logger[IO].warn(s"$guildName has no channel to publish $what!")
    }

  def warnMissingGuildChannels(
    client: DiscordClient,
    windsChannels: List[Channel],
    mandatesChannels: List[Channel],
    motionsChannels: List[Channel],
    ritualsChannels: List[Channel],
    othersChannels: List[Channel],
  )(using Logger[IO]): IO[Unit] =
    val guilds = client.guilds.map(guild => guild.getIdLong -> guild.getName).toMap
    List(
      warnMissingGuilds(guilds, windsChannels, "Winds"),
      warnMissingGuilds(guilds, mandatesChannels, "Mandates"),
      warnMissingGuilds(guilds, motionsChannels, "Motions"),
      warnMissingGuilds(guilds, ritualsChannels, "Rituals"),
      warnMissingGuilds(guilds, othersChannels, "Other Articles"),
    ).sequence_

  def apply(config: Configuration)(using Logger[IO]): Resource[IO, Discord] =
    DiscordClient(config.token).evalMap { client =>
      for
        windsChannels    <- client.channelsByIDs(config.windsChannels)
        mandatesChannels <- client.channelsByIDs(config.mandatesChannels)
        motionsChannels  <- client.channelsByIDs(config.motionsChannels)
        ritualsChannels  <- client.channelsByIDs(config.ritualsChannels)
        othersChannels   <- client.channelsByIDs(config.othersChannels)
        _                <- warnMissingGuildChannels(
                              client,
                              windsChannels,
                              mandatesChannels,
                              motionsChannels,
                              ritualsChannels,
                              othersChannels,
                            )
      yield new Discord(
        windsChannels,
        mandatesChannels,
        motionsChannels,
        ritualsChannels,
        othersChannels,
      )
    }
