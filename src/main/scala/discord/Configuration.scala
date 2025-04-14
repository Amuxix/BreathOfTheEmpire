package discord

import discord.DiscordID
import pureconfig.ConfigReader

case class DiscordConfiguration(
  token: String,
  windsChannel: DiscordID,
  mandatesChannel: DiscordID,
  motionsChannel: DiscordID,
  ritualsChannel: DiscordID,
  itemsChannel: DiscordID,
) derives ConfigReader
