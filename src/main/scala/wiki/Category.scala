package wiki

import cats.syntax.option.*

import scala.util.Try

trait MainCategory(val order: Int = 1)
trait ExtraCategory

enum WikiCategory(val customName: Option[String] = None):
  case SenateMotion        extends WikiCategory with MainCategory
  case FailedSenateMotion  extends WikiCategory(customName = "Failed".some) with ExtraCategory
  case VetoedSenateMotion  extends WikiCategory(customName = "Vetoed".some) with ExtraCategory
  case Mandate             extends WikiCategory with MainCategory
  case Rituals             extends WikiCategory with MainCategory(2)
  case SpringRitual        extends WikiCategory with MainCategory
  case SummerRitual        extends WikiCategory with MainCategory
  case AutumnRitual        extends WikiCategory with MainCategory
  case DayRitual           extends WikiCategory with MainCategory
  case NightRitual         extends WikiCategory with MainCategory
  case WinterRitual        extends WikiCategory with MainCategory
  case MilitaryCouncil     extends WikiCategory(customName = "Wind of War".some) with MainCategory
  case WindsOfFortune      extends WikiCategory("Wind of Fortune".some) with MainCategory(2)
  case TradeWinds          extends WikiCategory(customName = "Wind of Trade".some) with MainCategory
  case WindsOfMagic        extends WikiCategory(customName = "Wind of Magic".some) with MainCategory
  case UrizenLore          extends WikiCategory with ExtraCategory
  case Enchantment         extends WikiCategory with ExtraCategory
  case Curse               extends WikiCategory with ExtraCategory
  case Warfare             extends WikiCategory with ExtraCategory
  case MagicItems          extends WikiCategory(customName = "Magic Item".some) with MainCategory(2)
  case ArcaneImplements    extends WikiCategory(customName = "Arcane Implement".some) with MainCategory
  case ArcaneWeapons       extends WikiCategory(customName = "Arcane Weapon".some) with MainCategory
  case Bows                extends WikiCategory(customName = "Bow".some) with MainCategory
  case Daggers             extends WikiCategory(customName = "Dagger".some) with MainCategory
  case Foci                extends WikiCategory(customName = "Focus".some) with MainCategory
  case Gonfalon            extends WikiCategory(customName = "Gonfalon".some) with MainCategory
  case GreatWeapons        extends WikiCategory(customName = "Great Weapon".some) with MainCategory
  case HeavyArmour         extends WikiCategory(customName = "Heavy Armour".some) with MainCategory
  case Icons               extends WikiCategory(customName = "Icon".some) with MainCategory
  case Jewellery           extends WikiCategory(customName = "Jewellery".some) with MainCategory
  case LightArmour         extends WikiCategory(customName = "Light Armour".some) with MainCategory
  case MageArmour          extends WikiCategory(customName = "Mage Armour".some) with MainCategory
  case MageRobes           extends WikiCategory(customName = "Mage Robe".some) with MainCategory
  case MagicStandards      extends WikiCategory(customName = "Magic Standard".some) with MainCategory
  case MediumArmour        extends WikiCategory(customName = "Medium Armour".some) with MainCategory
  case MusicalInstruments  extends WikiCategory(customName = "Musical Instrument".some) with MainCategory
  case `One-handedSpears`  extends WikiCategory(customName = "One-handed Spear".some) with MainCategory
  case `One-handedWeapons` extends WikiCategory(customName = "One-handed Weapon".some) with MainCategory
  case PairedWeapons       extends WikiCategory(customName = "Paired Weapon".some) with MainCategory
  case Paraphernalia       extends WikiCategory(customName = "Paraphernalia".some) with MainCategory
  case Polearms            extends WikiCategory(customName = "Polearm".some) with MainCategory
  case Regalia             extends WikiCategory(customName = "Regalia".some) with MainCategory
  case Reliquaries         extends WikiCategory(customName = "Reliquary".some) with MainCategory
  case RitualMasks         extends WikiCategory(customName = "Ritual Mask".some) with MainCategory
  case RitualStaves        extends WikiCategory(customName = "Ritual Staff".some) with MainCategory
  case Rods                extends WikiCategory(customName = "Rod".some) with MainCategory
  case Shields             extends WikiCategory(customName = "Shield".some) with MainCategory
  case Staffs              extends WikiCategory(customName = "Staff".some) with MainCategory
  case Tools               extends WikiCategory(customName = "Tool".some) with MainCategory
  case Vestments           extends WikiCategory(customName = "Vestment".some) with MainCategory
  case Wands               extends WikiCategory(customName = "Wand".some) with MainCategory
  case `Runesmith'sLaw`    extends WikiCategory(customName = "Runesmith's Law".some) with ExtraCategory
  case Tonics              extends WikiCategory(customName = "Tonic".some) with MainCategory

  lazy val show: String = toString.replaceAll("([a-z])([A-Z])", "$1 $2")
  lazy val name: String = customName.getOrElse(show)

object WikiCategory:
  def fromString(string: String): Either[String, WikiCategory] =
    Try(WikiCategory.valueOf(string.replaceAll(" ", ""))).toOption
      .toRight(s"Invalid Category `$string` please use one of ${WikiCategory.values.map(_.show).mkString(", ")}.")
end WikiCategory
