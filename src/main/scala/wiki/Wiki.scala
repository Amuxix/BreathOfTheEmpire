package wiki

import cats.effect.IO
import fs2.{Pipe, Stream}
import org.typelevel.log4cats.Logger
import wiki.Wiki.yearSeasonRegex

import java.time.Instant
import scala.util.matching.Regex

class Wiki(client: WikiClient, categoryBatch: Int)(using Logger[IO]):
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

  private val toPage: Pipe[IO, WikiPage, Page] =
    _.collect {
      case page @ WikiPage(Some(title), Some(pageID), _)
          if !overviewRegex.matches(title) && page.mainCategories.nonEmpty && page.yearAndSeason.nonEmpty =>
        val (year, season) = page.yearAndSeason.get

        client.pageSection(pageID, 1).memoize.map { extraInfo =>
          Page(
            title,
            year,
            season,
            page.mainCategories.minBy(_.ordinal),
            page.parsedCategories.collect { case c: ExtraCategory => c }.toList.sortBy(_.ordinal),
            client.pageUri(title),
            extraInfo,
          )
        }
    }.evalMap(identity)

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
