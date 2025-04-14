package discord

import discord.DiscordID
import pureconfig.ConfigReader

case class Configuration(
  token: String,
  windsChannels: List[DiscordID],
  mandatesChannels: List[DiscordID],
  motionsChannels: List[DiscordID],
  ritualsChannels: List[DiscordID],
  othersChannels: List[DiscordID],
) derives ConfigReader
