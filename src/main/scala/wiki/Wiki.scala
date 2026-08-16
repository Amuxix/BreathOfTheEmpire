package wiki

import cats.effect.IO
import fs2.{Pipe, Stream}
import org.typelevel.log4cats.Logger
import wiki.Category.extractCategories
import wiki.Wiki.yearSeasonRegex

import java.time.Instant
import scala.util.matching.Regex

class Wiki(client: WikiClient, categoryBatch: Int):
  private val overviewRegex = s"$yearSeasonRegex (Solstice|Equinox) (winds of (war|fortune)|Military Council orders)".r

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

  private val filterOutOverviews: Pipe[IO, Logevent, Int] =
    _.collect {
      case logevent if !overviewRegex.matches(logevent.title) => logevent.pageid
    }

  extension (text: String)
    private def noFirstTitle     = text.replaceFirst("\n*#+ [^\n]+\n*", "")
    private def noDateSection    = text.replaceFirst("#+ Date[^#]*", "")
    private def onlyFirstSection = text.takeWhile(_ != '#') // keep only till text title
    private def noLists          = text
      .split("\n")
      .flatMap {
        case string if string.matches("^- .+?$") => None
        case string                              => Some(string)
      }
      .mkString("\n")
    private def condenseNewLines = text.replaceAll("\n+", "\n")

  extension (page: ParsedPage)
    private def renderedAndCategorised(
      mainCategory: Category & Main,
    ): IO[(ParsedPage, String, List[(Category & Text, Int)])] =
      IO {
        val text       = XMLRender.render(page.text, client.wiki, client.pageUri, "table")
        val categories = text.extractCategories
        val trimmed    = mainCategory match
          case Category.SenateMotion => text.noFirstTitle.noDateSection.condenseNewLines
          case _                     => text.noFirstTitle.onlyFirstSection.noLists.condenseNewLines

        (page, trimmed, categories)
      }

  private val toPage: Pipe[IO, WikiPage, Page] =
    _.collect {
      case wikiPage @ WikiPage(Some(title), Some(pageID), _)
          if !overviewRegex.matches(title) && wikiPage.mainCategories.nonEmpty && wikiPage.yearAndSeason.nonEmpty =>
        val (year, season) = wikiPage.yearAndSeason.get
        (wikiPage, title, pageID, year, season)
    }.evalMap { (wikiPage, title, pageID, year, season) =>
      val pageUri = client.pageUri(title)

      client
        .parsedPage(pageID)
        .flatMap(_.renderedAndCategorised(wikiPage.mainCategory))
        .map { (parsedPage, renderedText, categories) =>
          Page(
            title,
            year,
            season,
            wikiPage.mainCategory,
            Category.sort(wikiPage.extraCategories ++ categories),
            OpportunityExtractor(parsedPage, client.wiki, client.pageUri, pageUri, year, season),
            pageUri,
            renderedText,
          )
        }
    }

  def pagesCreatedAfter(startInstant: Instant): Stream[IO, (Instant, Stream[IO, Page])] =
    client
      .createdEvents(startInstant)
      .map { (instant, pages) =>
        val stream = Stream
          .emits(pages)
          .through(filterOutOverviews)
          .through(categories)
          .through(toPage)

        instant -> stream
      }

object Wiki:
  val yearSeasonRegex: Regex = "(\\d{3})YE (Winter|Autumn|Spring|Summer)".r

  def apply(config: Configuration)(using Logger[IO]) =
    WikiClient(config.empireUri).map(new Wiki(_, config.categoryBatchSize))
