package wiki

import cats.effect.IO
import cats.effect.kernel.Resource
import fs2.Stream
import org.http4s.Uri
import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import org.typelevel.log4cats.Logger

import java.time.Instant
import java.time.temporal.ChronoUnit
import scala.concurrent.duration.*

class WikiClient(client: Client[IO], wiki: Uri):
  val API                                                                  = wiki / "mediawiki-public" / "api.php"
  def createdEvents(from: Instant)(using Logger[IO]): Stream[IO, Logevent] =
    val uri = API.withQueryParams(
      Map(
        "action"        -> "query",
        "format"        -> "json",
        "list"          -> "logevents",
        "formatversion" -> "2",
        "leprop"        -> "title|ids",
        "leaction"      -> "create/create",
        "lenamespace"   -> "0",
        "ledir"         -> "newer",
        "lelimit"       -> "500",
        "lestart"       -> from.truncatedTo(ChronoUnit.MICROS).toString,
      ),
    )
    Stream
      .eval(client.expect[LogeventQueryResponse](uri))
      .flatMap { query =>
        Stream.unfoldLoopEval(query) {
          case LogeventQueryResponse(None, query)                                                       =>
            IO.pure(query -> None)
          case LogeventQueryResponse(Some(continue), query) if continue.continue.contains("lecontinue") =>
            client
              .expect[LogeventQueryResponse](uri.withQueryParam("lecontinue", continue.lecontinue.get))
              .map(next => query -> Some(next))
        }
      }
      .spaced(60.seconds / 200)
      .flatMap(query => Stream.emits(query.logevents))

  def pagesCategories(pageIds: List[Int])(using Logger[IO]): Stream[IO, WikiPage] =
    val uri = API.withQueryParams(
      Map(
        "action"        -> "query",
        "format"        -> "json",
        "prop"          -> "categories",
        "formatversion" -> "2",
        "cllimit"       -> "500",
        "redirects"     -> "1",
        "pageids"       -> pageIds.mkString("|"),
      ),
    )
    Stream
      .eval(client.expect[PageQueryResponse](uri))
      .flatMap { query =>
        Stream.unfoldLoopEval(query) {
          case PageQueryResponse(None, query, _)               =>
            Logger[IO].trace("No continue") *> IO.pure(query -> None)
          case PageQueryResponse(_, query, Some(true))         =>
            Logger[IO].trace("Batch complete") *> IO.pure(query -> None)
          case PageQueryResponse(_, None, _)                   =>
            Logger[IO].trace("No query response") *> IO.pure(None -> None)
          case PageQueryResponse(Some(continue), query, batch) =>
            client
              .expect[PageQueryResponse] {
                uri
                  .withQueryParam("clcontinue", continue.clcontinue.get)
              }
              .map(next => query -> Some(next)) <* IO.println(s"Getting more $continue $batch")
        }
      }
      .spaced(60.seconds / 200)
      .flatMap(query => Stream.emits(query.toList.flatMap(_.pages)))

  def articleUri(title: String): Uri = wiki / "empire-wiki" / title.replaceAll(" ", "_")

  def article(uri: Uri): IO[String] =
    client.get(uri)(_.bodyText.compile.foldMonoid)

object WikiClient:
  def apply(API: Uri)(using Logger[IO]): Resource[IO, WikiClient] =
    EmberClientBuilder
      .default[IO]
      .build
      .evalTap(_ => Logger[IO].debug("WikiClient acquired"))
      .onFinalize(Logger[IO].debug("WikiClient released"))
      .map(new WikiClient(_, API))
