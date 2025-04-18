package wiki

import cats.effect.IO
import org.http4s.Uri

import scala.util.matching.Regex
import scala.xml.{Elem, Node, Text}

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
  private case class RenderWithWrapper(wrapper: String) extends Decision

  private def inner(wiki: Uri)(rendered: String, node: Node): String =
    lazy val childrenRendered = node.child.foldLeft("")(inner(wiki))

    node match
      case Text(text)                      => rendered + text
      case elem: Elem if elem.label == "a" =>
        rendered + elem
          .attribute("href")
          .map(_.foldLeft("")(inner(wiki)))
          .fold(childrenRendered)(href => s"[$childrenRendered](<${wiki / href}>)")
      case elem: Elem if elem.label == "p" =>
        rendered + childrenRendered.replaceAll("\n", " ") + "\n" + "\n"

      case elem: Elem =>
        val decision: Decision = elem.label match
          case "h2" => RenderWithPrefix("# ")
          case "h3" => RenderWithPrefix("## ")
          case "big" | "h4" => RenderWithPrefix("### ")
          case "small" => RenderWithPrefix("-# ")
          case "li"    => RenderWithPrefix("- ")
          case "b"     => RenderWithWrapper("**")
          case "i"     => RenderWithWrapper("*")
          case "s"     => RenderWithWrapper("__")
          case "span" | "ul" | "sup " | "sup" => Render
          case "div" if elem.hasClass("mw-parser-output")                                         => Render
          case "div" if elem.styleContains("float: ?right".r)                                     => Skip
          case "div" if elem.styleContains("float: ?left".r)                                      => Skip
          case "div" if elem.hasClass("captioned-image", "ic", "embedvideo", "ic-inner", "quote", "box") => Skip
          case "table" | "br" => Skip
          case other =>
            println {
              (
                other,
                elem.attributes,
                elem.attribute("class").toList.flatten.mkString("|"),
                elem.attribute("style").toList.flatten.mkString("|"),
              )
            }
            Render

        lazy val renderChildren = (prefix: String, suffix: String) => rendered + prefix + childrenRendered + suffix

        decision match
          case Skip                       => rendered
          case Render                     => renderChildren("", "")
          case RenderWithPrefix(prefix)   => renderChildren(prefix, "")
          case RenderWithWrapper(wrapper) => renderChildren(wrapper, wrapper)

      case _ => ""

  def render(node: Node, wiki: Uri): IO[String] = IO.blocking(inner(wiki)("", node).replaceFirst("\n*#+ [a-zA-Z ]+\n*", ""))
