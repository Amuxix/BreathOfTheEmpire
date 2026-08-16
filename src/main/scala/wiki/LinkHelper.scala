package wiki

import org.http4s.Uri

import scala.util.matching.Regex

object LinkHelper:
  private val linkPattern: Regex = """\[([^\]]*)\]\(<[^>]*>\)""".r

  extension (text: String)
    def addLinks(
      pageUri: String => Uri,
    ): String =
      val initialLinks = linkPattern.findAllMatchIn(text).map(m => (m.start, m.end)).toList
      Rules.allRules
        .foldLeft((text, initialLinks)) { case ((current, links), rule) =>
          rule.pattern
            .findFirstMatchIn(current)
            .find(m => !links.exists((s, e) => m.start >= s && m.end <= e))
            .fold((current, links)) { m =>
              val uri          = pageUri(rule.page).renderString
              val replacement  = s"[${rule.replacement.getOrElse(m.matched)}](<$uri>)"
              val delta        = replacement.length - (m.end - m.start)
              val shiftedLinks = links.map {
                case (s, e) if s >= m.end => (s + delta, e + delta)
                case link                 => link
              }
              (
                current.patch(m.start, replacement, m.end - m.start),
                (m.start, m.start + replacement.length) :: shiftedLinks,
              )
            }
        }
        .head

    def withoutLinks: String = linkPattern.replaceAllIn(text, "$1")
