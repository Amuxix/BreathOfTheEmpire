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

  private def createOpportunity(table: String, `type`: OpportunityType, page: Uri, year: Int, season: Season) =
    table match
      case renderedTitleExtractor(title, body) => Opportunity(`type`, title, body, page, year, season)

  private def inner(wiki: Uri, page: Uri, year: Int, season: Season)(
    opportunities: List[Opportunity],
    node: Node,
  ): List[Opportunity] =
    opportunities ++ (node match
      case elem: Elem
          if elem.label == "table" && elem.hasClass(
            "mw-collapsible",
            "table",
            "table-striped",
            "table-bordered",
            "table-condensed",
            "mw-made-collapsible",
          ) =>
        val rows = elem.firstChild("tbody").allOf("tr")
        rows match
          case titleRow :: secondRow :: _ =>
            lazy val table   = XMLRender.render(elem, wiki)
            lazy val hasType = secondRow.firstChild("td").exists(XMLRender.render(_, wiki).contains("Type"))
            titleRow.firstChild("td").toList.collect {
              case titleCell: Elem if titleCell.styleContains("background-color: LightBlue".r) && hasType =>
                createOpportunity(table, OpportunityType.Commission, page, year, season)

              case titleCell: Elem if titleCell.styleContains("background-color: LightGreen".r) && hasType =>
                createOpportunity(table, OpportunityType.Title, page, year, season)
            }

          case _ => List.empty
      case _ => node.child.foldLeft(List.empty)(inner(wiki, page, year, season)))

  def extractOpportunities(
    pageSection: ParsedPage,
    wiki: Uri,
    page: Uri,
    year: Int,
    season: Season,
  ): List[Opportunity] =
    inner(wiki, page, year, season)(List.empty, pageSection.text)
