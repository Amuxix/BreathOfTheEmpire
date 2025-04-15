package discord

import net.dv8tion.jda.api.entities.Guild
import net.dv8tion.jda.api.entities.channel.{Channel as JDAChannel, ChannelType}
import net.dv8tion.jda.api.entities.channel.concrete.{ForumChannel as JDAForumChannel, TextChannel as JDATextChannel}

import scala.compiletime.asMatchable

open class Channel(channel: JDAChannel, val guild: Guild):
  lazy val discordID: DiscordID = DiscordID(channel.getIdLong)
  lazy val mention: String      = channel.getAsMention
  lazy val name: String         = channel.getName

  def toSpecific: Channel = channel.getType match
    case ChannelType.TEXT  => TextChannel(channel.asInstanceOf[JDATextChannel], guild)
    case ChannelType.FORUM => ForumChannel(channel.asInstanceOf[JDAForumChannel], guild)
    case _                 => throw new Exception("Channel type not supported")

  override def toString: String = mention

  override def equals(other: Any): Boolean = other.asMatchable match
    case that: Channel => discordID == that.discordID
    case _             => false

  override def hashCode(): Int =
    val state = Seq(discordID)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
