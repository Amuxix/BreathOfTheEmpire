package discord

import cats.instances.list.*
import discord.DiscordID
import pureconfig.ConfigReader
import pureconfig.error.ConfigReaderFailures

given ConfigReader[(PublishCategory, DiscordID)]                    = ConfigReader.derived
given ConfigReader[(DiscordID, List[(PublishCategory, DiscordID)])] = ConfigReader.derived

case class Configuration(
  token: String,
  windsOfFortuneChannels: List[DiscordID],
  windsOfWarChannels: List[DiscordID],
  diplomacyChannels: List[DiscordID],
  appraisalsChannels: List[DiscordID],
  mandatesChannels: List[DiscordID],
  motionsChannels: List[DiscordID],
  magicChannels: List[DiscordID],
  itemsChannels: List[DiscordID],
  maxDescriptionLength: Int,
  tags: List[(DiscordID, List[(PublishCategory, DiscordID)])],
) derives ConfigReader:
  val tagMap: Map[DiscordID, Map[PublishCategory, DiscordID]] =
    tags.map((channel, tagsByCategory) => channel -> tagsByCategory.toMap).toMap
