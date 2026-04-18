package empire

import cats.effect.{IO, IOApp, Resource}
import cats.syntax.option.*
import discord.{Article, Discord, PublishCategory}
import fs2.{Pipe, Stream}
import fs2.io.file.{Files, Path}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import retry.*
import wiki.{Category, Main, Page, Wiki}

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

  def publishCategory(category: Main): PublishCategory = category match
    case Category.WindsOfMagic    => PublishCategory.Magic
    case Category.Plenipotentiary => PublishCategory.Magic
    case Category.Rituals         => PublishCategory.Magic
    case Category.MilitaryCouncil => PublishCategory.WindOfWar
    case Category.Appraisal       => PublishCategory.Appraisal
    case Category.ForeignNations  => PublishCategory.Diplomacy
    case Category.Tonics          => PublishCategory.Item
    case Category.MagicItems      => PublishCategory.Item
    case Category.SenateMotion    => PublishCategory.Motion
    case Category.Mandate         => PublishCategory.Mandate
    case Category.TradeWinds      => PublishCategory.WindOfFortune
    case Category.WindsOfFortune  => PublishCategory.WindOfFortune

  val toArticle: Pipe[IO, Page, Article] =
    _.map { case Page(title, year, season, mainCategory, extraCategories, opportunities, uri, extraInfo) =>
      Article(
        title,
        year,
        season,
        publishCategory(mainCategory),
        mainCategory.name,
        extraCategories.map(_.name),
        opportunities,
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
            .through(discord.publishAll)
            .compile
            .toList
            .map(list => instant -> list.size)
        }
        .compile
        .toList
        .flatMap { list =>
          Logger[IO].debug(s"Published ${list.map(_(1)).sum} articles/opportunities.").as(list.map(_(0)).max)
        }

  def stream(
    wiki: Wiki,
    discord: Discord,
    lastInstant: Path,
    interval: FiniteDuration,
  )(using Logger[IO]): Stream[IO, Unit] =
    def publishWithRetry(instant: Instant): IO[Instant] =
      retryingOnErrors(publishNewArticlesCreatedAfter(wiki, discord, instant))(
        policy = RetryPolicies.capDelay(6.hours, RetryPolicies.exponentialBackoff[IO](interval)),
        errorHandler = ResultHandler.retryOnAllErrors { (err, details) =>
          val next = details.nextStepIfUnsuccessful match
            case RetryDetails.NextStep.GiveUp                                  =>
              "Giving up"
            case RetryDetails.NextStep.DelayAndRetry(delay) if delay >= 1.hour =>
              s"Retrying in ${delay.toMinutes / 60.0}h"
            case RetryDetails.NextStep.DelayAndRetry(delay)                    =>
              s"Retrying in ${delay.toMinutes}m"
          Logger[IO].warn(err)(s"Request threw an exception on attempt #${details.retriesSoFar + 1}, $next...")
        },
      )

    Stream
      .eval(startingInstant(lastInstant, interval))
      .evalMap(publishWithRetry)
      .flatMap(Stream.iterateEval(_)(publishWithRetry))
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
