package empire

import org.http4s.Uri

enum OpportunityType:
  case Title, Commission

case class Opportunity(
  `type`: OpportunityType,
  title: String,
  body: String,
  tags: List[String],
  source: Uri,
  year: Int,
  season: Season,
)
