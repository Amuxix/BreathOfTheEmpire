package wiki

import empire.{Opportunity, OpportunityType, Season}
import org.http4s.Uri
import wiki.XMLRender.{hasClass, styleContains}

import scala.xml.{Elem, Node}

object Opportunities:
  extension (elem: Node)
    private def allOf(label: String): List[Node]        = elem.child.toList.collect {
      case elem: Elem if elem.label == label => elem
    }
    private def firstChild(label: String): Option[Node] = elem.allOf(label).headOption

  extension (elem: Option[Node]) private def allOf(label: String): List[Node] = elem.toList.flatMap(_.allOf(label))

  private val renderedTitleExtractor = "(?:\\n|.)*?\\*\\*(.+?)\\*\\*((?:\\n|.)+)".r

  private def createOpportunity(
    table: String,
    tags: List[String],
    `type`: OpportunityType,
    page: Uri,
    year: Int,
    season: Season,
  ) =
    table match
      case renderedTitleExtractor(title, body) => Opportunity(`type`, title, body, tags, page, year, season)

  private def inner(wiki: Uri, pageUri: String => Uri, page: Uri, year: Int, season: Season)(
    node: Node,
    opportunities: List[Opportunity],
    anchor: Option[String],
  ): (List[Opportunity], Option[String]) =
    lazy val newAnchor                    = node.attribute("id").flatMap(_.map(_.toString).headOption).orElse(anchor)
    val (newOpportunities, updatedAnchor) = node match
      case elem: Elem if elem.label == "table" && elem.hasClass("mw-collapsible") =>
        val rows          = elem.firstChild("tbody").allOf("tr")
        val opportunities = rows match
          case titleRow :: secondRow :: _ =>
            lazy val (table, textCategories) = XMLRender.render(elem, wiki, pageUri)
            lazy val tags                    = textCategories.map(_.name).distinct
            lazy val hasType                 = secondRow.firstChild("td").exists(XMLRender.render(_, wiki, pageUri)._1.contains("Type"))
            titleRow.firstChild("td").toList.collect {
              case titleCell: Elem if titleCell.styleContains("background-color: LightBlue".r) && hasType =>
                createOpportunity(
                  table,
                  tags,
                  OpportunityType.Commission,
                  newAnchor.fold(page)(page.withFragment),
                  year,
                  season,
                )

              case titleCell: Elem if titleCell.styleContains("background-color: LightGreen".r) && hasType =>
                createOpportunity(
                  table,
                  tags,
                  OpportunityType.Title,
                  newAnchor.fold(page)(page.withFragment),
                  year,
                  season,
                )
            }

          case _ => List.empty
        (opportunities, newAnchor)

      case _ =>
        node.child.foldLeft((List.empty[Opportunity], newAnchor)) { case ((opportunities, anchor), node) =>
          inner(wiki, pageUri, page, year, season)(node, opportunities, anchor)
        }
    (opportunities ++ newOpportunities, updatedAnchor)

  def extractOpportunities(
    pageSection: ParsedPage,
    wiki: Uri,
    pageUri: String => Uri,
    page: Uri,
    year: Int,
    season: Season,
  ): List[Opportunity] =
    inner(wiki, pageUri, page, year, season)(pageSection.text, List.empty, None)(0)
