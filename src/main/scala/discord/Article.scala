package discord

import empire.Season
import org.http4s.Uri

case class Article(
  title: String,
  year: Int,
  season: Season,
  publishCategory: PublishCategory,
  mainCategory: String,
  extraCategories: List[String],
  uri: Uri,
  body: String,
):
  lazy val categories: List[String] =
    s"$season $year" +: mainCategory +: extraCategories
  lazy val show: String             = s"[${categories.mkString(", ")}] $title"
