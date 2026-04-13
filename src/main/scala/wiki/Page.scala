package wiki

import cats.effect.IO
import empire.{Opportunity, Season}
import org.http4s.Uri

case class Page(
  title: String,
  year: Int,
  season: Season,
  mainCategory: Category & MainCategory,
  extraCategories: List[Category & ExtraCategory],
  opportunities: List[Opportunity],
  uri: Uri,
  extraInfo: IO[String],
):
  lazy val extraCategoriesString     = if extraCategories.isEmpty then "" else extraCategories.mkString(", ", ", ", "")
  override lazy val toString: String = s"[$mainCategory$extraCategoriesString] $title"
