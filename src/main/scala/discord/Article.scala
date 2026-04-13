package discord

import cats.effect.IO
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
  extraInfo: IO[String],
):
  lazy val categories: List[String] =
    s"$season $year" +: mainCategory +: (extraCategories ++ opportunities.headOption.map(_ => "Opportunity"))
  lazy val show: String             = s"[${categories.mkString(", ")}] $title"
