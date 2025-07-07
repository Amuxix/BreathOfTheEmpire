package discord

import cats.effect.IO
import org.http4s.Uri

case class Article(
  title: String,
  year: Int,
  season: String,
  publishCategory: PublishCategory,
  mainCategory: String,
  extraCategories: List[String],
  uri: Uri,
  extraInfo: IO[String],
):
  lazy val extraCategoriesString = if extraCategories.isEmpty then "" else extraCategories.mkString(", ", ", ", "")
  lazy val show: String          = s"[$mainCategory$extraCategoriesString] $title"
