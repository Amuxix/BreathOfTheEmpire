package empire

import cats.effect.{IO, IOApp, Resource}
import cats.implicits.catsSyntaxOptionId
import discord.Discord
import fs2.{Pipe, Stream}
import fs2.io.file.{Files, Path}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import wiki.{Page, Wiki}

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

  def writeLastInstant(path: Path, instant: Instant): IO[Unit] =
    Stream.emit(instant.toString).through(Files[IO].writeUtf8Lines(path)).compile.drain

  val toArticle: Pipe[IO, Page, Article] =
    _.map { case Page(title, mainCategory, extraCategories, uri) =>
      Article(
        title,
        PublishCategory.fromMainCategory(mainCategory),
        mainCategory.name,
        extraCategories.map(_.name),
        uri,
      )
    }

  def meteredInstantStream(
    lastInstant: Path,
    interval: FiniteDuration,
  )(using Logger[IO]): Stream[IO, Instant] =
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
    Logger[IO].debug(s"Looking for new articles since $instant...") *>
      wiki
        .pagesCreatedAfter(instant)
        .through(toArticle)
        .through(discord.publishArticle)
        .compile
        .toList
        .flatMap(list => Logger[IO].debug(s"Published ${list.size} articles."))

  def stream(
    wiki: Wiki,
    discord: Discord,
    lastInstant: Path,
    interval: FiniteDuration,
  )(using Logger[IO]): Stream[IO, Unit] =
    meteredInstantStream(lastInstant, interval)
      .evalTap(publishNewArticlesCreatedAfter(wiki, discord, _))
      .evalMap(_ => writeLastInstant(lastInstant, Instant.now))

  override def run: IO[Unit] =
    (for
      given Logger[IO] <- Stream.eval(Slf4jLogger.create[IO])
      config           <- Stream.resource(Resource.eval(Configuration.fromConfig()))
      wiki             <- Stream.resource(Wiki(config.wiki))
      discord          <- Stream.resource(Discord(config.discord))
      _                <- stream(wiki, discord, config.lastInstantPath, config.interval)
    yield ()).compile.drain
