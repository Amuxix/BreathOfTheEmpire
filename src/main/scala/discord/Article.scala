package discord

import empire.{Opportunity, Season}
import org.http4s.Uri

case class Article(
  title: String,
  year: Int,
  season: Season,
  publishCategory: PublishCategory,
  mainCategory: String,
  extraCategories: List[String],
  opportunities: List[Opportunity],
  uri: Uri,
  extraInfo: String,
):
  lazy val categories: List[String] =
    s"$season $year" +: mainCategory +: (opportunities.headOption.map(_ => "Opportunity").toList ++ extraCategories)
  lazy val show: String             = s"[${categories.mkString(", ")}] $title"
