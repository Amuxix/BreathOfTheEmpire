package discord

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.instances.list.*
import cats.syntax.foldable.*
import fs2.Pipe
import fs2.Stream
import net.dv8tion.jda.api.EmbedBuilder
import net.dv8tion.jda.api.entities.MessageEmbed
import org.typelevel.log4cats.Logger

import java.awt.Color
import scala.concurrent.duration.*

class Discord(
  windsOfFortuneChannels: List[Channel],
  windsOfWarChannels: List[Channel],
  diplomacyChannels: List[Channel],
  appraisalsChannels: List[Channel],
  mandatesChannels: List[Channel],
  motionsChannels: List[Channel],
  ritualsChannels: List[Channel],
  itemsChannels: List[Channel],
  tags: Map[DiscordID, Map[PublishCategory, DiscordID]],
  maxDescriptionLength: Int,
)(using Logger[IO]):
  private def removeLinksFromText(text: String): String =
    text.replaceAll("\\[(.+?)]\\(<.+?>\\)", "$1")

  private def truncateExtraInfo(extraInfo: String, maxSize: Int): String =
    if extraInfo.length <= maxSize then extraInfo
    else
      val paragraphs = extraInfo.split("\n").toList
      paragraphs
        .foldLeft((List.empty[String], maxSize - 5, false)) {
          case ((paragraphs, _, true), _)                                                       =>
            (paragraphs, 0, true)
          case ((paragraphs, remaining, false), paragraph) if remaining >= paragraph.length + 1 =>
            (paragraphs :+ paragraph, remaining - paragraph.length - 1, false)
          case ((paragraphs, remaining, false), paragraph)                                      =>
            val (words, _, _) = paragraph.split(" ").toList.foldLeft((List.empty[String], remaining, false)) {
              case ((words, _, true), _)                                            =>
                (words, 0, true)
              case ((words, remaining, false), word) if remaining < word.length + 1 =>
                (words, 0, true)
              case ((words, remaining, false), word)                                =>
                (words :+ word, remaining - word.length - 1, false)
            }
            (paragraphs :+ words.mkString(" "), 0, true)
        }(0)
        .mkString("", "\n", "\n\n### ...")

  private def messageEmbed(article: Article, removeLinks: Boolean): IO[MessageEmbed] =
    article.extraInfo
      .map(extraInfo => if removeLinks then removeLinksFromText(extraInfo) else extraInfo)
      .map { extraInfo =>
        new EmbedBuilder()
          .setTitle(article.title)
          .setDescription(truncateExtraInfo(extraInfo, maxDescriptionLength).replaceAll("\n{4,}", "\n\n\n"))
          .setUrl(article.uri.toString)
          .setFooter(
            (s"${article.season} ${article.year}" +: article.mainCategory +: article.extraCategories).mkString("  "),
          )
          .setColor(Color(150, 255, 120))
          .build()
      }

  private def publishArticleAsText(channel: TextChannel, article: Article): IO[Unit] =
    messageEmbed(article, removeLinks = true).flatMap(channel.sendEmbed).void

  private def publishArticleAsPost(channel: ForumChannel, article: Article): IO[Unit] =
    val tagID = tags.get(channel.discordID).flatMap(_.get(article.publishCategory)).toList
    for
      embed <- messageEmbed(article, removeLinks = false)
      _     <- channel.createForumPost(article.title, embed, tagID*)
    yield ()

  private val log: Pipe[IO, (Channel, Article), (Channel, Article)] =
    _.evalTap { (channel, article) =>
      Logger[IO].info(s"Publishing ${article.show} to ${channel.guild.getName}/${channel.name}")
    }

  private val assignPublishChannels: Pipe[IO, Article, (Channel, Article)] =
    _.flatMap { article =>
      (article.publishCategory match
        case PublishCategory.WindOfFortune => Stream.emits(windsOfFortuneChannels)
        case PublishCategory.WindOfWar     => Stream.emits(windsOfWarChannels)
        case PublishCategory.Diplomacy     => Stream.emits(diplomacyChannels)
        case PublishCategory.Appraisal     => Stream.emits(appraisalsChannels)
        case PublishCategory.Mandate       => Stream.emits(mandatesChannels)
        case PublishCategory.Motion        => Stream.emits(motionsChannels)
        case PublishCategory.Ritual        => Stream.emits(ritualsChannels)
        case PublishCategory.Item          => Stream.emits(itemsChannels)
      )
        .map(_ -> article)
    }

  private val publish: Pipe[IO, (Channel, Article), Unit] =
    _.meteredStartImmediately((1 / 10).seconds).evalMap {
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
  )(
    channels: List[Channel],
    what: String,
  )(using Logger[IO]): IO[Unit] =
    guilds.keySet.diff(channels.map(_.guild.getIdLong).toSet).map(guilds.apply).toList.traverse_ { guildName =>
      Logger[IO].warn(s"$guildName has no channel to publish $what!")
    }

  def warnMissingGuildChannels(
    client: DiscordClient,
    channels: (List[Channel], String)*,
  )(using Logger[IO]): IO[Unit] =
    val guilds = client.guilds.map(guild => guild.getIdLong -> guild.getName).toMap
    channels.toList.traverse_(warnMissingGuilds(guilds))

  def apply(config: Configuration)(using Logger[IO]): Resource[IO, Discord] =
    DiscordClient(config.token).evalMap { client =>
      for
        windsOfFortuneChannels <- client.channels(config.windsOfFortuneChannels)
        windsOfWarChannels     <- client.channels(config.windsOfWarChannels)
        diplomacyChannels      <- client.channels(config.diplomacyChannels)
        appraisalsChannels     <- client.channels(config.appraisalsChannels)
        mandatesChannels       <- client.channels(config.mandatesChannels)
        motionsChannels        <- client.channels(config.motionsChannels)
        ritualsChannels        <- client.channels(config.ritualsChannels)
        itemsChannels          <- client.channels(config.itemsChannels)
        _                      <- warnMissingGuildChannels(
                                    client,
                                    (windsOfFortuneChannels, "Winds of Fortune"),
                                    (windsOfWarChannels, "Winds of War"),
                                    (diplomacyChannels, "Diplomacy"),
                                    (appraisalsChannels, "Appraisals"),
                                    (mandatesChannels, "Mandates"),
                                    (motionsChannels, "Motions"),
                                    (ritualsChannels, "Rituals"),
                                    (itemsChannels, "Items"),
                                  )
      yield new Discord(
        windsOfFortuneChannels,
        windsOfWarChannels,
        diplomacyChannels,
        appraisalsChannels,
        mandatesChannels,
        motionsChannels,
        ritualsChannels,
        itemsChannels,
        config.tagMap,
        config.maxDescriptionLength,
      )
    }
