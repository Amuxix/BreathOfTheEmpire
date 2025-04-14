package discord

import cats.Show
import cats.effect.IO
import net.dv8tion.jda.api.entities.{Guild, MessageEmbed}
import net.dv8tion.jda.api.entities.channel.middleman.MessageChannel
import net.dv8tion.jda.api.utils.FileUpload

import java.io.File
import scala.compiletime.asMatchable

class Channel(channel: MessageChannel, val guild: Guild):
  lazy val discordID: DiscordID = DiscordID(channel.getIdLong)
  lazy val mention: String      = channel.getAsMention
  lazy val name: String         = channel.getName

  def sendMessage(string: String): IO[Message]    =
    channel.sendMessage(string).toIO.map(new Message(_))
  def sendEmbed(embed: MessageEmbed): IO[Message] = channel.sendMessageEmbeds(embed).toIO.map(new Message(_))
  def sendFile(file: File): IO[Message]           = channel
    .sendFiles(FileUpload.fromData(file))
    .toIO
    .map(new Message(_))

  override def toString: String = mention

  override def equals(other: Any): Boolean = other.asMatchable match
    case that: Channel => discordID == that.discordID
    case _             => false

  override def hashCode(): Int =
    val state = Seq(discordID)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)

object Channel:
  given Show[Channel] = Show.fromToString
