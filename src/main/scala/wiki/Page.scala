package wiki

import empire.{Opportunity, Season}
import org.http4s.Uri

case class Page(
  title: String,
  year: Int,
  season: Season,
  mainCategory: Category & MainCategory,
  extraCategories: List[Category & ExtraCategory],
  textCategories: List[Category & TextCategory],
  opportunities: List[Opportunity],
  uri: Uri,
  extraInfo: String,
):
  lazy val extraCategoriesString     = if extraCategories.isEmpty then "" else extraCategories.mkString(", ", ", ", "")
  override lazy val toString: String = s"[$mainCategory$extraCategoriesString] $title"
