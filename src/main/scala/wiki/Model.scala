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

case class Page(
  // ns: Int,
  title: Option[String],
  // missing: Option[Boolean],
  // pageid: Option[Int],
  categories: Option[List[CategoryModel]],
) derives Decoder:
  lazy val parsedCategories: List[Category] =
    categories.toList.flatten
      .flatMap(category => Category.fromString(category.title.replace("Category:", "")).toOption)

  lazy val lowestCategory  = parsedCategories.collect { case c: MainCategory => c }.minBy(_.order)
  lazy val topCategory     = parsedCategories.collect { case c: MainCategory => c }.maxBy(_.order)
  lazy val extraCategories = parsedCategories.collect { case c: ExtraCategory => c }

case class PageQuery(
  pages: List[Page],
) derives Decoder

case class PageQueryResponse(
  continue: Option[Continue],
  query: Option[PageQuery],
  batchcomplete: Option[Boolean],
) derives Decoder
