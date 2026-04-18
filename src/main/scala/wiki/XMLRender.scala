package wiki

import org.http4s.Uri

import scala.util.matching.Regex
import scala.xml.{Elem, Node, Text as XmlText}

object XMLRender:
  extension (elem: Elem)
    def hasClass(possibilites: String*): Boolean =
      elem.attribute("class").toList.flatten.map(_.toString).flatMap(_.split(" ")).intersect(possibilites).nonEmpty
    def styleContains(regex: Regex): Boolean     =
      regex.findFirstIn(elem.attribute("style").toList.flatten.mkString).nonEmpty

  sealed private trait Decision
  private case object Skip                              extends Decision
  private case object Render                            extends Decision
  private case class RenderWithPrefix(prefix: String)   extends Decision
  private case class RenderWithSuffix(suffix: String)   extends Decision
  private case class RenderWithWrapper(wrapper: String) extends Decision

  private def inner(wiki: Uri, ignoredLabels: String*)(rendered: String, node: Node): String =
    lazy val childrenRendered = node.child.foldLeft("")(inner(wiki, ignoredLabels*))

    node match
      case XmlText(text)                   => rendered + text
      case elem: Elem if elem.label == "a" =>
        rendered + elem
          .attribute("href")
          .map(_.foldLeft("")(inner(wiki, ignoredLabels*)))
          .fold(childrenRendered)(href => s"[$childrenRendered](<${wiki.addPath(href.dropWhile(_ == '/'))}>)")
      case elem: Elem if elem.label == "p" =>
        rendered + childrenRendered.replaceAll("\n", " ") + "\n" + "\n"

      case elem: Elem =>
        val decision: Decision = elem.label match
          case label if ignoredLabels.contains(label) => Skip
          case "h2"                                   => RenderWithPrefix("# ")
          case "h3"                                   => RenderWithPrefix("## ")
          case "big" | "h4" => RenderWithPrefix("### ")
          case "small" => RenderWithPrefix("-# ")
          case "li"    => RenderWithPrefix("- ")
          case "b"     => RenderWithWrapper("**")
          case "i"     => RenderWithWrapper("*")
          case "s"     => RenderWithWrapper("__")
          case "tr"    => RenderWithSuffix("\n")
          case "td"    => RenderWithSuffix("\t")
          case "span" | "ul" | "sup " | "sup" | "table" | "tbody" => Render
          case "div" if elem.hasClass("mw-parser-output")                                => Render
          case "div" if elem.styleContains("float: ?right".r)                            => Skip
          case "div" if elem.styleContains("float: ?left".r)                             => Skip
          case "div" if elem.hasClass("captioned-image", "ic", "embedvideo", "ic-inner") => Skip
          case "div" if elem.hasClass("quote", "box", "mw-collapsible")                  => Skip
          case "div" if elem.styleContains("grid-template-columns: \\d+fr \\d+fr".r)     => Render
          case "div" if elem.styleContains("max-width:\\d+px".r)                         => Render
          case "br"                                                                      => Skip
          case other                                                                     =>
            println(s"unmatched $other tag found")
            println(s"class= ${elem.attribute("class").toList.flatten.mkString("|")}")
            println(s"style= ${elem.attribute("style").toList.flatten.mkString("|")}")
            Render

        lazy val renderChildren = (prefix: String, suffix: String) => rendered + prefix + childrenRendered + suffix

        decision match
          case Skip                       => rendered
          case Render                     => renderChildren("", "")
          case RenderWithPrefix(prefix)   => renderChildren(prefix, "")
          case RenderWithSuffix(suffix)   => renderChildren("", suffix)
          case RenderWithWrapper(wrapper) => renderChildren(wrapper, wrapper)

      case _ => ""

  def render(
    node: Node,
    wiki: Uri,
    pageUri: String => Uri,
    ignoredLabels: String*,
  ): (String, List[Category & Text]) =
    LinkEnricher.enrich(inner(wiki, ignoredLabels*)("", node), pageUri)
