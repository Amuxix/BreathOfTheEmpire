package empire

import cats.effect.{IO, IOApp, Resource}
import cats.implicits.catsSyntaxOptionId
import discord.{Discord, DiscordClient}
import fs2.{Pipe, Stream}
import fs2.io.file.{Files, Path}
import org.http4s.Uri
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import wiki.{CategoryModel, Page, Wiki}

import java.nio.file.NoSuchFileException
import java.time.Instant
import scala.concurrent.duration.*

object BreathOfTheEmpire extends IOApp.Simple:
  private def readLastInstant(path: Path): IO[Option[Instant]] =
    Files[IO]
      .readUtf8Lines(path)
      .compile
      .foldMonoid
      .map {
        case instant if instant.isEmpty => None
        case instant                    => Instant.parse(instant).some
      }
      .recover { case _: NoSuchFileException => None }

  def writeLastInstant(path: Path, instant: Instant): IO[Unit] =
    Stream.emit(instant.toString).through(Files[IO].writeUtf8Lines(path)).compile.drain

  val toArticle: Pipe[IO, (Page, Option[Uri]), Article] =
    _.collect {
      case (page @ Page(Some(title), Some(categories)), Some(uri)) if categories.nonEmpty =>
        Article(
          title,
          PublishCategory.fromMainCategory(page.lowestCategory),
          page.topCategory.name,
          page.extraCategories.map(_.name),
          uri,
        )
    }

  def meteredInstantStream(
    lastInstant: Path,
    interval: FiniteDuration,
  ): Stream[IO, Instant] =
    Stream
      .eval(readLastInstant(lastInstant))
      .flatMap {
        case Some(instant) =>
          Stream.emit(instant) ++ Stream.iterate(Instant.now)(_.plusSeconds(interval.toSeconds))
        case None          =>
          Stream.iterate(Instant.now.minusSeconds(interval.toSeconds))(_.plusSeconds(interval.toSeconds))
      }
      .meteredStartImmediately(interval)

  private def publishNewArticlesCreatedAfter(
    wiki: Wiki,
    discord: Discord,
    instant: Instant,
  )(using Logger[IO]): IO[Unit] =
    Logger[IO].debug("Looking for new articles...") *>
      wiki
        .pagesCreatedAfter(instant)
        .through(toArticle)
        .through(discord.publishArticle)
        .compile
        .drain

  def stream(
    wiki: Wiki,
    discord: Discord,
    lastInstant: Path,
    interval: FiniteDuration,
  )(using Logger[IO]): Stream[IO, Unit] =
    meteredInstantStream(lastInstant, interval)
      .evalTap(publishNewArticlesCreatedAfter(wiki, discord, _))
      .evalMap(writeLastInstant(lastInstant, _))

  def resources(using Logger[IO]) = for
    config  <- Resource.eval(Configuration.fromConfig())
    wiki    <- Wiki(config.wiki)
    discord <- DiscordClient(config.discord.token)
  yield (wiki, discord, config.lastInstantPath, config.interval)

  override def run: IO[Unit] =
    (for
      given Logger[IO] <- Stream.eval(Slf4jLogger.create[IO])
      config           <- Stream.resource(Resource.eval(Configuration.fromConfig()))
      wiki             <- Stream.resource(Wiki(config.wiki))
      discord          <- Stream.resource(Discord(config.discord))
      _                <- stream(wiki, discord, config.lastInstantPath, config.interval)
    yield ()).compile.drain
