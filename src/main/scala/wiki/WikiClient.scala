package wiki

import cats.effect.IO
import cats.effect.kernel.Resource
import fs2.Stream
import io.circe.Decoder
import org.http4s.Uri
import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.client.Client
import org.http4s.client.middleware.Logger as LoggerMiddle
import org.http4s.ember.client.EmberClientBuilder
import org.typelevel.log4cats.Logger

import java.time.Instant
import java.time.temporal.ChronoUnit
import scala.concurrent.duration.*

class WikiClient(client: Client[IO], val wiki: Uri):
  val API = wiki / "mediawiki-public" / "api.php"

  def actionParams(action: String) = Map(
    "action"        -> action,
    "format"        -> "json",
    "formatversion" -> "2",
    "redirects"     -> "1",
  )

  def query[Q: Decoder](continueParam: String, params: (String, String)*)(using Logger[IO]): Stream[IO, Q] =
    val uri = API.withQueryParams(actionParams("query") ++ params)
    Stream
      .eval(client.expect[SingleQueryResponse[Q]](uri))
      .flatMap { queryResponse =>
        Stream.unfoldLoopEval(queryResponse) {
          case SingleQueryResponse(Some(continue), data) =>
            client
              .expect[SingleQueryResponse[Q]](uri.withQueryParam(continueParam, continue))
              .map(next => data -> Some(next))
          case SingleQueryResponse(None, data)           =>
            IO.pure(data -> None)
        }
      }
      .spaced(60.seconds / 200)
      .flatMap(data => Stream.emits(data))

  def createdEvents(from: Instant)(using Logger[IO]): Stream[IO, Logevent] =
    query[Logevent](
      "lecontinue",
      "list"        -> "logevents",
      "leprop"      -> "title|ids",
      "leaction"    -> "create/create",
      "lenamespace" -> "0",
      "ledir"       -> "newer",
      "lelimit"     -> "500",
      "lestart"     -> from.truncatedTo(ChronoUnit.MICROS).toString,
    )

  def pagesCategories(pageIds: List[Int])(using Logger[IO]): Stream[IO, WikiPage] =
    query[WikiPage](
      "clcontinue",
      "prop"    -> "categories",
      "cllimit" -> "500",
      "pageids" -> pageIds.mkString("|"),
    )

  def pageUri(title: String): Uri = wiki / "empire-wiki" / title.replaceAll(" ", "_")

  def page(uri: Uri): IO[String] =
    client.get(uri)(_.bodyText.compile.foldMonoid)

  def firstSectionOfPage(pageId: Int): IO[PageSection] =
    val uri = API.withQueryParams(
      actionParams("parse") ++ Map(
        "pageid"     -> pageId.toString,
        "prop"       -> "wikitext|sections",
        "section"    -> "1",
        "disabletoc" -> "1",
      ),
    )
    client.expect[PageSection](uri)

object WikiClient:
  def apply(API: Uri)(using Logger[IO]): Resource[IO, WikiClient] =
    EmberClientBuilder
      .default[IO]
      .build
      .evalTap(_ => Logger[IO].debug("WikiClient acquired"))
      .onFinalize(Logger[IO].debug("WikiClient released"))
      // .map(FollowRedirect(maxRedirects = 3))
      .map(
        LoggerMiddle(
          logHeaders = false,
          logBody = true,
          logAction = Some((msg: String) => Logger[IO].trace(msg)),
        ),
      )
      .map(new WikiClient(_, API))
