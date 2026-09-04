package wiki

import scala.util.matching.Regex

object EnrichmentHelper:
  private val urlPattern: Regex = """\([^)]*?\)""".r
  private lazy val replacements = Rules.allRules.collect { case Rule(pattern, _, Some(replacement)) =>
    (pattern, replacement)
  }
  extension (text: String)
    def enrich: String          =
      replacements.foldLeft(text) { case (text, (pattern, replacement)) =>
        // This could be done outside the fold and passed as part of accumulator and updated to improve efficiency
        val links = urlPattern.findAllMatchIn(text).map(m => (m.start, m.end)).toList
        pattern.replaceSomeIn(
          text,
          `match` => {
            val matchIsInURL = links.exists((start, end) => `match`.start >= start && `match`.end <= end)
            Option.unless(matchIsInURL)(replacement)
          },
        )
      }
