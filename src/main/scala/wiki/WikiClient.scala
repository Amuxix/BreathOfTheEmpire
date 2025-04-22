package wiki

import cats.effect.IO
import cats.effect.kernel.Resource
import fs2.Stream
import io.circe.Decoder
import org.http4s.{ProductComment, ProductId, Uri}
import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.client.Client
import org.http4s.client.middleware.{GZip, Logger as LoggerMiddle}
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.headers.`User-Agent`
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
    "curtimestamp"  -> "true",
  )

  def query[Q: Decoder](continueParam: String, params: (String, String)*)(using
    Logger[IO],
  ): Stream[IO, (Instant, List[Q])] =
    val uri = API.withQueryParams(actionParams("query") ++ params)
    Stream
      .eval(client.expect[SingleQueryResponse[Q]](uri))
      .flatMap { queryResponse =>
        Stream.unfoldLoopEval(queryResponse) {
          case SingleQueryResponse(Some(continue), timestamp, data) =>
            client
              .expect[SingleQueryResponse[Q]](uri.withQueryParam(continueParam, continue))
              .map(next => (timestamp, data) -> Some(next))
          case SingleQueryResponse(None, timestamp, data)           =>
            IO.pure((timestamp, data) -> None)
        }
      }
      .spaced(60.seconds / 200)

  def createdEvents(from: Instant)(using Logger[IO]): Stream[IO, (Instant, List[Logevent])] =
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
    ).flatMap((_, pages) => Stream.emits(pages))

  def pageUri(title: String): Uri = wiki / "empire-wiki" / title.replaceAll(" ", "_")

  def page(uri: Uri): IO[String] =
    client.get(uri)(_.bodyText.compile.foldMonoid)

  def pageSection(pageId: Int, section: Int): IO[String] =
    val uri = API.withQueryParams(
      actionParams("parse") ++ Map(
        "prop"                      -> "text",
        "disabletoc"                -> "1",
        "disablelimitreport"        -> "1",
        "disableeditsection"        -> "1",
        "disablestylededuplication" -> "1",
        "disabletoc"                -> "1",
        "section"                   -> section.toString,
        "pageid"                    -> pageId.toString,
      ),
    )
    client.expect[PageSection](uri).flatMap(section => XMLRender.render(section.text, wiki))

object WikiClient:
  def apply(API: Uri)(using Logger[IO]): Resource[IO, WikiClient] =
    EmberClientBuilder
      .default[IO]
      .withHttp2
      .withUserAgent(`User-Agent`(ProductId("breath-of-the-empire"), ProductComment("breathoftheempire@aifosi.top")))
      .build
      .evalTap(_ => Logger[IO].debug("WikiClient acquired"))
      .onFinalize(Logger[IO].debug("WikiClient released"))
      .map(GZip()(_))
      .map(
        LoggerMiddle(
          logHeaders = false,
          logBody = true,
          logAction = Some((msg: String) => Logger[IO].trace(msg)),
        ),
      )
      .map(new WikiClient(_, API))
