package discord

import cats.instances.list.*
import discord.DiscordID
import empire.PublishCategory
import pureconfig.ConfigReader
import pureconfig.error.ConfigReaderFailures

given ConfigReader[(PublishCategory, DiscordID)]                    = ConfigReader.derived
given ConfigReader[(DiscordID, List[(PublishCategory, DiscordID)])] = ConfigReader.derived

//case class TagEntry(channel: DiscordID, tagsByCategory: List[(PublishCategory, DiscordID)]) derives ConfigReader

//case class TagEntry(channel: DiscordID, tagsByCategory: List[TagsByCategory]) derives ConfigReader
//case class TagsByCategory(category: PublishCategory, tagID: DiscordID) derives ConfigReader

/*
given ConfigReader[(PublishCategory, DiscordID)] = ConfigReader.derived

given listT: ConfigReader[List[(PublishCategory, DiscordID)]] = ConfigReader.derived

given ConfigReader[(DiscordID, List[(PublishCategory, DiscordID)])] = ConfigReader.derived

given ConfigReader[List[(DiscordID, List[(PublishCategory, DiscordID)])]] = ConfigReader.derived

given ConfigReader[Map[DiscordID, Map[PublishCategory, DiscordID]]] =
ConfigReader[List[(DiscordID, List[(PublishCategory, DiscordID)])]].map(_.toMap.view.mapValues(_.toMap).toMap)
 */

case class Configuration(
  token: String,
  windsChannels: List[DiscordID],
  mandatesChannels: List[DiscordID],
  motionsChannels: List[DiscordID],
  ritualsChannels: List[DiscordID],
  othersChannels: List[DiscordID],
  tags: List[(DiscordID, List[(PublishCategory, DiscordID)])],
) derives ConfigReader:
  val tagMap: Map[DiscordID, Map[PublishCategory, DiscordID]] =
    // tags.map(entry => (entry.channel, entry.tagsByCategory.map(sub => sub.category -> sub.tagID).toMap)).toMap
    // tags.map(entry => (entry.channel, entry.tagsByCategory.toMap)).toMap
    tags.map((channel, tagsByCategory) => channel -> tagsByCategory.toMap).toMap
