package wiki

import org.http4s.Uri
import pureconfig.ConfigReader
import pureconfig.error.CannotConvert

final case class WikiConfiguration(
  empireUri: Uri,
  categoryBatchSize: Int,
) derives ConfigReader

object WikiConfiguration:
  given ConfigReader[Uri] = ConfigReader[String].emap { uri =>
    Uri.fromString(uri).left.map { parsingFailure =>
      CannotConvert(uri, "Uri", parsingFailure.message)
    }
  }
