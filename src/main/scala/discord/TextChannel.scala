package discord

import cats.effect.IO
import net.dv8tion.jda.api.entities.{Guild, MessageEmbed}
import net.dv8tion.jda.api.entities.channel.concrete.TextChannel as JDATextChannel
import net.dv8tion.jda.api.utils.FileUpload

import java.io.File
import scala.compiletime.asMatchable

class TextChannel(channel: JDATextChannel, guild: Guild) extends Channel(channel, guild):
  def sendMessage(string: String): IO[Message]    =
    channel.sendMessage(string).toIO.map(new Message(_))
  def sendEmbed(embed: MessageEmbed): IO[Message] = channel.sendMessageEmbeds(embed).toIO.map(new Message(_))
  def sendFile(file: File): IO[Message]           = channel
    .sendFiles(FileUpload.fromData(file))
    .toIO
    .map(new Message(_))
