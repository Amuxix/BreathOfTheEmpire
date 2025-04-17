package discord

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.instances.list.*
import cats.syntax.foldable.*
import fs2.Pipe
import fs2.Stream
import org.typelevel.log4cats.Logger

class Discord(
  windsChannels: List[Channel],
  mandatesChannels: List[Channel],
  motionsChannels: List[Channel],
  ritualsChannels: List[Channel],
  othersChannels: List[Channel],
  tags: Map[DiscordID, Map[PublishCategory, DiscordID]],
)(using Logger[IO]):
  private def publishArticleAsText(channel: TextChannel, article: Article): IO[Unit] =
    channel.sendMessage(s"[$article](<${article.uri}>)").void

  private def publishArticleAsPost(channel: ForumChannel, article: Article): IO[Unit] =
    val tagID = tags.get(channel.discordID).flatMap(_.get(article.publishCategory)).toList
    for
      extraInfo <- article.extraInfo
      _         <- channel.createForumPost(article.toString, extraInfo.getOrElse("") + article.uri.toString, tagID*)
    yield ()

  private val log: Pipe[IO, (Channel, Article), (Channel, Article)] =
    _.evalTap { (channel, article) =>
      Logger[IO].info(s"Publishing $article to ${channel.guild.getName}/${channel.name}")
    }

  private val assignPublishChannels: Pipe[IO, Article, (Channel, Article)] =
    _.flatMap { article =>
      (article.publishCategory match
        case PublishCategory.WindOfFortune => Stream.emits(windsChannels)
        case PublishCategory.WindOfWar     => Stream.emits(windsChannels)
        case PublishCategory.Mandate       => Stream.emits(mandatesChannels)
        case PublishCategory.Motion        => Stream.emits(motionsChannels)
        case PublishCategory.Ritual        => Stream.emits(ritualsChannels)
        case PublishCategory.Other         => Stream.emits(othersChannels)
      )
        .map(_ -> article)
    }

  val publish: Pipe[IO, (Channel, Article), Unit] =
    _.evalMap {
      case (channel: TextChannel, article)  => publishArticleAsText(channel, article)
      case (channel: ForumChannel, article) => publishArticleAsPost(channel, article)
      case _                                => IO.unit
    }

  val publishArticle: Pipe[IO, Article, Unit] =
    _.through(assignPublishChannels)
      .through(log)
      .through(publish)
      .as(())

object Discord:
  def warnMissingGuilds(
    guilds: Map[Long, String],
    channels: List[Channel],
    what: String,
  )(using Logger[IO]): IO[Unit] =
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
        windsChannels    <- client.channels(config.windsChannels)
        mandatesChannels <- client.channels(config.mandatesChannels)
        motionsChannels  <- client.channels(config.motionsChannels)
        ritualsChannels  <- client.channels(config.ritualsChannels)
        othersChannels   <- client.channels(config.othersChannels)
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
        config.tagMap,
      )
    }
