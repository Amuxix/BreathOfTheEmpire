package wiki

import cats.effect.IO
import fs2.{Pipe, Stream}
import org.http4s.Uri
import org.typelevel.log4cats.Logger

import java.time.Instant

class Wiki(client: WikiClient, categoryBatch: Int)(using Logger[IO]):
  private val overviewRegex =
    "\\d{3}YE (Winter|Autumn|Spring|Summer) (Solstice|Equinox) (winds of (war|fortune)|Military Council orders)".r

  private val mergeAllArticleCategories: Pipe[IO, Page, Page] = pages =>
    Stream.evalSeq {
      pages.compile.toList.map { pages =>
        pages
          .groupBy(_.title)
          .map { (title, pages) =>
            val categories =
              pages.foldLeft(List.empty[CategoryModel])((acc, page) => acc ++ page.categories.toList.flatten)
            Page(title, categories.headOption.map(_ => categories))
          }
          .toSeq
      }
    }

  private val categories: Pipe[IO, Int, Page] =
    _.chunkN(categoryBatch).flatMap { pageIdChunk =>
      Stream
        .fromIterator[IO](Category.values.iterator, categoryBatch)
        .chunks
        .flatMap { categoryChunk =>
          client.pagesCategories(pageIdChunk.toList, categoryChunk.toList)
        }
        .through(mergeAllArticleCategories)
    }

  private def pageIDsCreatedAfter(startInstant: Instant): Stream[IO, Int] =
    client
      .createdEvents(startInstant)
      .collect {
        case page if !overviewRegex.matches(page.title) => page.pageid
      }

  private val addUri: Pipe[IO, Page, (Page, Option[Uri])] =
    _.map(page => page -> page.title.map(client.articleUri))

  def pagesCreatedAfter(startInstant: Instant): Stream[IO, (Page, Option[Uri])] =
    pageIDsCreatedAfter(startInstant)
      .through(categories)
      .through(addUri)

object Wiki:
  def apply(config: Configuration)(using Logger[IO]) =
    WikiClient(config.empireUri).map(new Wiki(_, config.categoryBatchSize))
