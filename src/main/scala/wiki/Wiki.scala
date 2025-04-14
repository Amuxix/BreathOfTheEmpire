package wiki

import cats.effect.IO
import fs2.{Pipe, Stream}
import org.typelevel.log4cats.Logger

import java.time.Instant

class Wiki(client: WikiClient, categoryBatch: Int)(using Logger[IO]):
  private val overviewRegex =
    "\\d{3}YE (Winter|Autumn|Spring|Summer) (Solstice|Equinox) (winds of (war|fortune)|Military Council orders)".r

  private val mergeAllArticleCategories: Pipe[IO, WikiPage, WikiPage] = pages =>
    Stream.evalSeq {
      pages.compile.toList.map { pages =>
        pages
          .groupBy(_.title)
          .map { (title, pages) =>
            val categories =
              pages.foldLeft(List.empty[CategoryModel])((acc, page) => acc ++ page.categories.toList.flatten)
            WikiPage(title, categories.headOption.map(_ => categories))
          }
          .toSeq
      }
    }

  private val categories: Pipe[IO, Int, WikiPage] =
    _.chunkN(categoryBatch).flatMap { pageIdChunk =>
      client
        .pagesCategories(pageIdChunk.toList)
        .through(mergeAllArticleCategories)
    }

  private def pageIDsCreatedAfter(startInstant: Instant): Stream[IO, Int] =
    client
      .createdEvents(startInstant)
      .collect {
        case logevent if !overviewRegex.matches(logevent.title) => logevent.pageid
      }

  private val filterPages: Pipe[IO, WikiPage, Page] =
    _.collect {
      case page @ WikiPage(Some(title), _) if !overviewRegex.matches(title) && page.parsedCategories.nonEmpty =>
        val mainCategories = page.parsedCategories.collect { case c: MainCategory => c }
        Page(
          title,
          mainCategories.minBy(_.ordinal),
          page.parsedCategories.collect { case c: ExtraCategory => c }.toList.sortBy(_.ordinal),
          client.articleUri(title),
        )
    }

  def pagesCreatedAfter(startInstant: Instant): Stream[IO, Page] =
    pageIDsCreatedAfter(startInstant)
      .through(categories)
      .through(filterPages)

object Wiki:
  def apply(config: Configuration)(using Logger[IO]) =
    WikiClient(config.empireUri).map(new Wiki(_, config.categoryBatchSize))
