package discord

import cats.effect.IO
import net.dv8tion.jda.api.entities.Guild
import net.dv8tion.jda.api.entities.channel.concrete.ForumChannel as JDAForumChannel
import net.dv8tion.jda.api.entities.channel.forums.ForumTagSnowflake
import net.dv8tion.jda.api.utils.messages.MessageCreateBuilder

import scala.compiletime.asMatchable

class ForumChannel(channel: JDAForumChannel, guild: Guild) extends Channel(channel, guild):
  def createForumPost(title: String, body: String, tagIDs: DiscordID*): IO[Unit] =
    val messageCreate = new MessageCreateBuilder()
      .setContent(body)
      .build()
    val tag           = tagIDs.map(id => ForumTagSnowflake.fromId(id.toLong))
    channel.createForumPost(title, messageCreate).setTags(tag*).toIO.void
