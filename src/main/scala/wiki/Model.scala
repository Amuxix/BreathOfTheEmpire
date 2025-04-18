package wiki

import cats.instances.option.*
import cats.syntax.traverse.*
import io.circe.{Decoder, HCursor}
import io.circe.Decoder.Result

import scala.xml.{Elem, XML}

case class SingleQueryResponse[Q: Decoder](
  continue: Option[String],
  data: List[Q],
)

object SingleQueryResponse:
  private def extractContinue(c: HCursor): Result[Option[String]] =
    val outer = c.downField("continue")
    outer.keys
      .flatMap(_.filterNot(_ == "continue").headOption)
      .traverse(outer.downField(_).as[String])

  private def extractQuery[Q: Decoder](c: HCursor): Result[List[Q]] =
    val outer = c.downField("query")
    outer.keys
      .flatMap(_.filterNot(_ == "redirects").headOption)
      .toList
      .flatTraverse(outer.downField(_).as[List[Q]])

  given [Q: Decoder] => Decoder[SingleQueryResponse[Q]] = (c: HCursor) =>
    for
      continue <- extractContinue(c)
      data     <- extractQuery(c)
    yield SingleQueryResponse(continue, data)

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

case class CategoryModel(
  // ns: Int,
  title: String,
) derives Decoder

case class WikiPage(
  // ns: Int,
  title: Option[String],
  // missing: Option[Boolean],
  pageid: Option[Int],
  categories: Option[List[CategoryModel]],
) derives Decoder:
  lazy val parsedCategories: Set[Category] =
    categories.toSet.flatten
      .flatMap(category => Category.fromString(category.title.replace("Category:", "")))

  lazy val mainCategories = parsedCategories.collect { case c: MainCategory => c }

case class PageSection(
  text: Elem,
)

object PageSection:
  given Decoder[PageSection] =
    _.downField("parse").downField("text").as[String].map(text => PageSection(XML.loadString(text)))
