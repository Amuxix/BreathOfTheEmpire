package wiki

import org.http4s.Uri

case class Page(
  title: String,
  mainCategory: Category & MainCategory,
  extraCategories: List[Category & ExtraCategory],
  uri: Uri,
):
  lazy val extraCategoriesString     = if extraCategories.isEmpty then "" else extraCategories.mkString(", ", ", ", "")
  override lazy val toString: String = s"[$mainCategory$extraCategoriesString] $title"
