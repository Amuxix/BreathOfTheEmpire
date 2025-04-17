package wiki

import cats.effect.IO
import cats.syntax.option.*
import fs2.{Pipe, Stream}
import org.typelevel.log4cats.Logger

import java.time.Instant
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

class Wiki(client: WikiClient, categoryBatch: Int)(using Logger[IO]):
  private val overviewRegex =
    "\\d{3}YE (Winter|Autumn|Spring|Summer) (Solstice|Equinox) (winds of (war|fortune)|Military Council orders)".r

  private val mergeAllPageCategories: Pipe[IO, WikiPage, WikiPage] = pages =>
    Stream.evalSeq {
      pages.compile.toList.map { pages =>
        pages
          .groupBy(page => page.title -> page.pageid)
          .map { case ((title, pageid), pages) =>
            val categories =
              pages.foldLeft(List.empty[CategoryModel])((acc, page) => acc ++ page.categories.toList.flatten)
            WikiPage(title, pageid, categories.headOption.map(_ => categories))
          }
          .toSeq
      }
    }

  private val categories: Pipe[IO, Int, WikiPage] =
    _.chunkN(categoryBatch).flatMap { pageIdChunk =>
      client
        .pagesCategories(pageIdChunk.toList)
        .through(mergeAllPageCategories)
    }

  private def pageIDsCreatedAfter(startInstant: Instant): Stream[IO, Int] =
    client
      .createdEvents(startInstant)
      .collect {
        case logevent if !overviewRegex.matches(logevent.title) => logevent.pageid
      }

  def cleanUpSectionText(text: String): String =
    List(
      (text: String) => text.replaceAll("==.+?==\\n?", ""),
      (text: String) => text.replaceAll("\\{\\{PDFLink.*?\\}\\}\\n?", ""),
      (text: String) => text.replaceAll("<div.+?>.+?</div>\\n?", ""),
      (text: String) =>
        "\\[\\[([^|\\]]+?)\\]\\]".r
          .replaceAllIn(text, (m: Match) => s"[${m.group(1)}](<${client.pageUri(m.group(1))}>)"),
      (text: String) =>
        "\\[\\[(.*?)\\|(.+?)\\]\\]".r
          .replaceAllIn(text, (m: Match) => s"[${m.group(2)}](<${client.pageUri(m.group(1))}>)"),
      (text: String) => text.replaceAll("''(.+?)''", "*$1*"),
      (text: String) => text.replaceAll("\\n", "\n"),
    ).foldLeft(text)((text, f) => f(text))

  private val toPage: Pipe[IO, WikiPage, Page] =
    _.collect {
      case page @ WikiPage(Some(title), Some(pageid), _)
          if !overviewRegex.matches(title) && page.mainCategories.nonEmpty =>
        val mainCategory = page.mainCategories.minBy(_.ordinal)

        Page(
          title,
          mainCategory,
          page.parsedCategories.collect { case c: ExtraCategory => c }.toList.sortBy(_.ordinal),
          client.pageUri(title),
          extraInfo(pageid, mainCategory),
        )
    }

  private def extraInfo(pageID: Int, mainCategory: Category & MainCategory): Option[IO[ExtraInfo]] =
    mainCategory match
      case Category.WindsOfFortune =>
        client
          .firstSectionOfPage(pageID)
          .map(section => ExtraInfo(section.section, cleanUpSectionText(section.text)))
          .some
      case _                       => None

  def pagesCreatedAfter(startInstant: Instant): Stream[IO, Page] =
    pageIDsCreatedAfter(startInstant)
      .through(categories)
      .through(toPage)

object Wiki:
  def apply(config: Configuration)(using Logger[IO]) =
    WikiClient(config.empireUri).map(new Wiki(_, config.categoryBatchSize))
