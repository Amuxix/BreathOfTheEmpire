package discord

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.instances.list.*
import cats.syntax.foldable.*
import empire.{Opportunity, OpportunityType, Season}
import fs2.Pipe
import fs2.Stream
import net.dv8tion.jda.api.EmbedBuilder
import net.dv8tion.jda.api.entities.MessageEmbed
import org.typelevel.log4cats.Logger

import java.awt.Color

class Discord(
  windsOfFortuneChannels: List[TextChannel],
  windsOfWarChannels: List[TextChannel],
  diplomacyChannels: List[TextChannel],
  appraisalsChannels: List[TextChannel],
  mandatesChannels: List[TextChannel],
  motionsChannels: List[TextChannel],
  magicChannels: List[TextChannel],
  itemsChannels: List[TextChannel],
  titlesChannels: List[TextChannel],
  commissionsChannels: List[TextChannel],
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

  extension (season: Season)
    def toColor: Color = season match
      case Season.Winter => Color(186, 225, 255)
      case Season.Autumn => Color(255, 223, 186)
      case Season.Spring => Color(255, 205, 214)
      case Season.Summer => Color(255, 255, 186)

  private def articleToMessageEmbed(article: Article): IO[MessageEmbed] =
    article.extraInfo
      .map(extraInfo => removeLinksFromText(extraInfo))
      .map { extraInfo =>
        new EmbedBuilder()
          .setTitle(article.title)
          .setDescription(truncateExtraInfo(extraInfo, maxDescriptionLength).replaceAll("\n{4,}", "\n\n\n"))
          .setUrl(article.uri.toString)
          .setFooter(article.categories.mkString("  "))
          .setColor(article.season.toColor)
          .build()
      }

  private def opportunityToMessageEmbed(opportunity: Opportunity): IO[MessageEmbed] =
    IO {
      new EmbedBuilder()
        .setTitle(opportunity.title)
        .setDescription(opportunity.body)
        .setUrl(opportunity.source.toString)
        .setFooter(s"${opportunity.season} ${opportunity.year}  ${opportunity.`type`}")
        .setColor(opportunity.season.toColor)
        .build()
    }

  private def assignArticleToPublishChannels(article: Article): List[TextChannel] =
    article.publishCategory match
      case PublishCategory.WindOfFortune => windsOfFortuneChannels
      case PublishCategory.WindOfWar     => windsOfWarChannels
      case PublishCategory.Diplomacy     => diplomacyChannels
      case PublishCategory.Appraisal     => appraisalsChannels
      case PublishCategory.Mandate       => mandatesChannels
      case PublishCategory.Motion        => motionsChannels
      case PublishCategory.Magic         => magicChannels
      case PublishCategory.Item          => itemsChannels

  private def assignOpportunitiesToPublishChannels(opportunity: Opportunity): List[TextChannel] =
    opportunity.`type` match
      case OpportunityType.Title      => titlesChannels
      case OpportunityType.Commission => commissionsChannels

  private def logArticlePublishing(article: Article, channel: TextChannel) =
    Logger[IO].info(s"Publishing ${article.show} to ${channel.guild.getName}/${channel.name}")

  private def logOpportunityPublishing(opportunity: Opportunity, channel: TextChannel) =
    Logger[IO].info(s"Publishing ${opportunity.title} to ${channel.guild.getName}/${channel.name}")

  private def linkToPublishChannels[A](
    assignChannels: A => List[TextChannel],
    toEmbed: A => IO[MessageEmbed],
  ): Pipe[IO, A, (A, TextChannel, MessageEmbed)] =
    _.flatMap { a =>
      val channels = assignChannels(a)
      if channels.nonEmpty then
        Stream.eval(toEmbed(a)).flatMap(embed => Stream.emits(channels).map(channel => (a, channel, embed)))
      else Stream.empty
    }

  def publish[A](
    assignChannels: A => List[TextChannel],
    toEmbed: A => IO[MessageEmbed],
    log: (A, TextChannel) => IO[Unit],
  ): Pipe[IO, A, Unit] =
    _.through(linkToPublishChannels(assignChannels, toEmbed))
      .evalTap((a, channel, _) => log(a, channel))
      .evalMap((_, channel, embed) => channel.sendEmbed(embed))
      .as(())

  val publishAll: Pipe[IO, Article, Unit] =
    _.broadcastThrough(
      publish(assignArticleToPublishChannels, articleToMessageEmbed, logArticlePublishing),
      _.flatMap(article => Stream.emits(article.opportunities))
        .through(publish(assignOpportunitiesToPublishChannels, opportunityToMessageEmbed, logOpportunityPublishing)),
    )

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
        windsOfFortuneChannels <- client.textChannels(config.windsOfFortuneChannels)
        windsOfWarChannels     <- client.textChannels(config.windsOfWarChannels)
        diplomacyChannels      <- client.textChannels(config.diplomacyChannels)
        appraisalsChannels     <- client.textChannels(config.appraisalsChannels)
        mandatesChannels       <- client.textChannels(config.mandatesChannels)
        motionsChannels        <- client.textChannels(config.motionsChannels)
        magicChannels          <- client.textChannels(config.magicChannels)
        itemsChannels          <- client.textChannels(config.itemsChannels)
        titlesChannels         <- client.textChannels(config.titlesChannels)
        commissionsChannels    <- client.textChannels(config.commissionsChannels)
        _                      <- warnMissingGuildChannels(
                                    client,
                                    (windsOfFortuneChannels, "Winds of Fortune"),
                                    (windsOfWarChannels, "Winds of War"),
                                    (diplomacyChannels, "Diplomacy"),
                                    (appraisalsChannels, "Appraisals"),
                                    (mandatesChannels, "Mandates"),
                                    (motionsChannels, "Motions"),
                                    (magicChannels, "Magic"),
                                    (itemsChannels, "Items"),
                                    (titlesChannels, "Titles"),
                                    (commissionsChannels, "Commissions"),
                                  )
      yield new Discord(
        windsOfFortuneChannels,
        windsOfWarChannels,
        diplomacyChannels,
        appraisalsChannels,
        mandatesChannels,
        motionsChannels,
        magicChannels,
        itemsChannels,
        titlesChannels,
        commissionsChannels,
        config.maxDescriptionLength,
      )
    }
