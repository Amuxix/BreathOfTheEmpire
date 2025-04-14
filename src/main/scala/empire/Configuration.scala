package empire

import cats.effect.IO
import com.typesafe.config.{Config, ConfigFactory}
import discord.Configuration as DiscordConfig
import fs2.io.file.Path
import org.http4s.Uri
import pureconfig.{ConfigReader, ConfigSource}
import pureconfig.error.CannotConvert
import pureconfig.module.catseffect.syntax.*
import wiki.Configuration as WikiConfig

import scala.concurrent.duration.FiniteDuration

case class Configuration(
  discord: DiscordConfig,
  // postgres: PostgresConfiguration,
  wiki: WikiConfig,
  lastInstantPath: Path,
  interval: FiniteDuration,
) derives ConfigReader

object Configuration:
  given ConfigReader[Uri]  = ConfigReader[String].emap { uri =>
    Uri.fromString(uri).left.map { parsingFailure =>
      CannotConvert(uri, "Uri", parsingFailure.message)
    }
  }
  given ConfigReader[Path] = ConfigReader[String].map(Path(_))

  def fromConfig(config: Config = ConfigFactory.load()): IO[Configuration] =
    ConfigSource.fromConfig(config).loadF()
