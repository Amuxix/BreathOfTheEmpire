package wiki

import empire.{Opportunity, Season}
import org.http4s.Uri

case class Page(
  title: String,
  year: Int,
  season: Season,
  mainCategory: Category & Main,
  extraCategories: List[Category & (Extra | Text)],
  opportunities: List[Opportunity],
  uri: Uri,
  extraInfo: String,
):
  lazy val extraCategoriesString     = if extraCategories.isEmpty then "" else extraCategories.mkString(", ", ", ", "")
  override lazy val toString: String = s"[$mainCategory$extraCategoriesString] $title"
