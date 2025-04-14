package wiki

import io.circe.Decoder

case class Continue(
  clcontinue: Option[String],
  grccontinue: Option[String],
  lecontinue: Option[String],
  continue: String,
) derives Decoder

case class Logevent(
  // logid: Int,
  // ns: Int,
  title: String,
  pageid: Int,
  // logpage: Int,
  // timestamp: String,
  // comment: String,
  // parsedcomment: String,
  // tags: List[String],
) derives Decoder

case class LogeventQuery(
  logevents: List[Logevent],
) derives Decoder

case class LogeventQueryResponse(
  continue: Option[Continue],
  query: LogeventQuery,
) derives Decoder

case class CategoryModel(
  // ns: Int,
  title: String,
) derives Decoder

case class WikiPage(
  // ns: Int,
  title: Option[String],
  // missing: Option[Boolean],
  // pageid: Option[Int],
  categories: Option[List[CategoryModel]],
) derives Decoder:
  lazy val parsedCategories: Set[Category] =
    categories.toSet.flatten
      .flatMap(category => Category.fromString(category.title.replace("Category:", "")))

case class PageQuery(
  pages: List[WikiPage],
) derives Decoder

case class PageQueryResponse(
  continue: Option[Continue],
  query: Option[PageQuery],
  batchcomplete: Option[Boolean],
) derives Decoder
