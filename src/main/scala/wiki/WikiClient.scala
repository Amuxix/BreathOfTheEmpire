package wiki

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.effect.std.Semaphore
import cats.syntax.flatMap.*
import fs2.Stream
import io.circe.Decoder
import org.http4s.{ProductComment, ProductId, Status, Uri}
import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.client.Client
import org.http4s.client.middleware.{GZip, Logger as LoggerMiddle, Retry, RetryPolicy}
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

  def query[Q: Decoder](continueParam: String, params: (String, String)*): Stream[IO, (Instant, List[Q])] =
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

  def createdEvents(from: Instant): Stream[IO, (Instant, List[Logevent])] =
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

  def pagesCategories(pageIds: List[Int]): Stream[IO, WikiPage] =
    query[WikiPage](
      "clcontinue",
      "prop"    -> "categories",
      "cllimit" -> "500",
      "pageids" -> pageIds.mkString("|"),
    ).flatMap((_, pages) => Stream.emits(pages))

  def pageUri(title: String): Uri = wiki / "empire-wiki" / title.replaceAll(" ", "_")

  def page(uri: Uri): IO[String] =
    client.get(uri)(_.bodyText.compile.foldMonoid)

  def parsedPage(pageId: Int): IO[ParsedPage] =
    val uri = API.withQueryParams(
      actionParams("parse") ++ Map(
        "prop"                      -> "text",
        "disabletoc"                -> "1",
        "disablelimitreport"        -> "1",
        "disableeditsection"        -> "1",
        "disablestylededuplication" -> "1",
        // "section"                   -> section.toString,
        "pageid"                    -> pageId.toString,
      ),
    )
    client.expect[ParsedPage](uri)

  def renderedFirstSection(page: ParsedPage): IO[(String, List[Category & TextCategory])] =
    IO.blocking {
      val (text, categories) = XMLRender.render(page.text, wiki, pageUri, "table")
      val trimmed            = text
        .replaceFirst("\n*#+ [^\n]+\n*", "") // remove first title
        .takeWhile(_ != '#')                 // keep only till text title
        .split("\n")
        .flatMap {
          case string if string.matches("^- .+?$") => None
          case string                              => Some(string)
        }
        .mkString("\n")                      // remove bullet points
      (trimmed, categories)
    }

object WikiClient:
  private val retryPolicy: RetryPolicy[IO] = RetryPolicy[IO](
    backoff = RetryPolicy.exponentialBackoff(maxWait = 1.minute, maxRetry = 5),
    retriable =
      (req, result) => RetryPolicy.defaultRetriable(req, result) || result.exists(_.status == Status.TooManyRequests),
  )

  private def throttled(client: Client[IO], minGap: FiniteDuration): Resource[IO, Client[IO]] =
    for
      sem       <- Resource.eval(Semaphore[IO](1))
      lastReqAt <- Resource.eval(IO.ref(Duration.Zero: FiniteDuration))
    yield Client[IO] { req =>
      Resource.eval(
        sem.permit.surround {
          IO.monotonic.flatMap { now =>
            lastReqAt.get.flatMap { last =>
              val wait = minGap - (now - last)
              IO.whenA(wait > Duration.Zero)(IO.sleep(wait))
            }
          } *> IO.monotonic.flatMap(lastReqAt.set)
        },
      ) >> client.run(req)
    }

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
      .flatMap(throttled(_, 300.millis))
      .map(Retry(retryPolicy))
      .map(new WikiClient(_, API))
