package empire

import cats.effect.{IO, IOApp, Resource}
import cats.syntax.option.*
import discord.{Article, Discord, PublishCategory}
import fs2.{Pipe, Stream}
import fs2.io.file.{Files, Path}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import wiki.{Category, MainCategory, Page, Wiki}

import java.nio.file.NoSuchFileException
import java.time.Instant
import scala.concurrent.duration.*

//https://discord.com/oauth2/authorize?client_id=1361380675168768223&scope=bot&permissions=377957125120
object BreathOfTheEmpire extends IOApp.Simple:
  private def readLastInstant(path: Path)(using Logger[IO]): IO[Option[Instant]] =
    Files[IO]
      .readUtf8Lines(path)
      .compile
      .foldMonoid
      .map {
        case instant if instant.isEmpty => None
        case instant                    => Instant.parse(instant).some
      }
      .recover { case _: NoSuchFileException => None }
      .flatTap(
        _.fold(Logger[IO].debug("last instant not found"))(instant => Logger[IO].debug(s"last instant is $instant")),
      )

  private def writeLastInstant(path: Path, instant: Instant): IO[Unit] =
    Stream.emit(instant.toString).through(Files[IO].writeUtf8Lines(path)).compile.drain

  def publishCategory(category: MainCategory): PublishCategory = category match
    case Category.MilitaryCouncil => PublishCategory.WindOfWar
    case Category.WindsOfMagic    => PublishCategory.WindOfFortune
    case Category.TradeWinds      => PublishCategory.WindOfFortune
    case Category.Appraisal       => PublishCategory.Appraisal
    case Category.ForeignNations  => PublishCategory.Diplomacy
    case Category.WindsOfFortune  => PublishCategory.WindOfFortune
    case Category.Tonics          => PublishCategory.Item
    case Category.Rituals         => PublishCategory.Ritual
    case Category.MagicItems      => PublishCategory.Item
    case Category.SenateMotion    => PublishCategory.Motion
    case Category.Mandate         => PublishCategory.Mandate

  val toArticle: Pipe[IO, Page, Article] =
    _.map { case Page(title, year, season, mainCategory, extraCategories, uri, extraInfo) =>
      Article(
        title,
        year,
        season.toString,
        publishCategory(mainCategory),
        mainCategory.name,
        extraCategories.map(_.name),
        uri,
        extraInfo,
      )
    }

  def startingInstant(
    lastInstant: Path,
    interval: FiniteDuration,
  )(using Logger[IO]): IO[Instant] =
    readLastInstant(lastInstant).map(_.getOrElse(Instant.now.minusSeconds(interval.toSeconds)))

  private def publishNewArticlesCreatedAfter(
    wiki: Wiki,
    discord: Discord,
    instant: Instant,
  )(using Logger[IO]): IO[Instant] =
    Logger[IO].debug(s"Looking for new articles since $instant...") *>
      wiki
        .pagesCreatedAfter(instant)
        .evalMap { (instant, pageStream) =>
          pageStream
            .through(toArticle)
            .through(discord.publishArticle)
            .compile
            .toList
            .map(list => instant -> list.size)
        }
        .compile
        .toList
        .flatMap { list =>
          Logger[IO].debug(s"Published ${list.map(_(1)).sum} articles.").as(list.map(_(0)).max)
        }

  def stream(
    wiki: Wiki,
    discord: Discord,
    lastInstant: Path,
    interval: FiniteDuration,
  )(using Logger[IO]): Stream[IO, Unit] =
    Stream
      .eval(startingInstant(lastInstant, interval))
      .evalMap(publishNewArticlesCreatedAfter(wiki, discord, _))
      .flatMap(Stream.iterateEval(_)(publishNewArticlesCreatedAfter(wiki, discord, _)))
      .meteredStartImmediately(interval)
      .evalMap(_ => writeLastInstant(lastInstant, Instant.now))

  override def run: IO[Unit] =
    (for
      given Logger[IO] <- Stream.eval(Slf4jLogger.create[IO])
      config           <- Stream.resource(Resource.eval(Configuration.fromConfig()))
      wiki             <- Stream.resource(Wiki(config.wiki))
      discord          <- Stream.resource(Discord(config.discord))
      _                <- stream(wiki, discord, config.lastInstantPath, config.interval)
    yield ()).compile.drain
