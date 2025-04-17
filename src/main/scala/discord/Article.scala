package discord

import cats.effect.IO
import org.http4s.Uri

case class Article(
  title: String,
  publishCategory: PublishCategory,
  mainCategory: String,
  extraCategories: List[String],
  uri: Uri,
  extraInfo: IO[Option[String]],
):
  lazy val extraCategoriesString     = if extraCategories.isEmpty then "" else extraCategories.mkString(", ", ", ", "")
  override lazy val toString: String = s"[$mainCategory$extraCategoriesString] $title"
