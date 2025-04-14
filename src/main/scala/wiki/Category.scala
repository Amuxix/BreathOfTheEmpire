package wiki

import cats.syntax.option.*

import scala.util.Try

sealed trait MainCategory(val order: Int = 1)
sealed trait ExtraCategory

enum Category(val customName: Option[String] = None):
  case SenateMotion        extends Category with MainCategory
  case FailedSenateMotion  extends Category(customName = "Failed".some) with ExtraCategory
  case VetoedSenateMotion  extends Category(customName = "Vetoed".some) with ExtraCategory
  case Mandate             extends Category with MainCategory
  case Rituals             extends Category with MainCategory(2)
  case SpringRitual        extends Category with MainCategory
  case SummerRitual        extends Category with MainCategory
  case AutumnRitual        extends Category with MainCategory
  case DayRitual           extends Category with MainCategory
  case NightRitual         extends Category with MainCategory
  case WinterRitual        extends Category with MainCategory
  case MilitaryCouncil     extends Category(customName = "Wind of War".some) with MainCategory
  case WindsOfFortune      extends Category("Wind of Fortune".some) with MainCategory(2)
  case TradeWinds          extends Category(customName = "Wind of Trade".some) with MainCategory
  case WindsOfMagic        extends Category(customName = "Wind of Magic".some) with MainCategory
  case UrizenLore          extends Category with ExtraCategory
  case Enchantment         extends Category with ExtraCategory
  case Curse               extends Category with ExtraCategory
  case Warfare             extends Category with ExtraCategory
  case MagicItems          extends Category(customName = "Magic Item".some) with MainCategory(2)
  case ArcaneImplements    extends Category(customName = "Arcane Implement".some) with MainCategory
  case ArcaneWeapons       extends Category(customName = "Arcane Weapon".some) with MainCategory
  case Bows                extends Category(customName = "Bow".some) with MainCategory
  case Daggers             extends Category(customName = "Dagger".some) with MainCategory
  case Foci                extends Category(customName = "Focus".some) with MainCategory
  case Gonfalon            extends Category(customName = "Gonfalon".some) with MainCategory
  case GreatWeapons        extends Category(customName = "Great Weapon".some) with MainCategory
  case HeavyArmour         extends Category(customName = "Heavy Armour".some) with MainCategory
  case Icons               extends Category(customName = "Icon".some) with MainCategory
  case Jewellery           extends Category(customName = "Jewellery".some) with MainCategory
  case LightArmour         extends Category(customName = "Light Armour".some) with MainCategory
  case MageArmour          extends Category(customName = "Mage Armour".some) with MainCategory
  case MageRobes           extends Category(customName = "Mage Robe".some) with MainCategory
  case MagicStandards      extends Category(customName = "Magic Standard".some) with MainCategory
  case MediumArmour        extends Category(customName = "Medium Armour".some) with MainCategory
  case MusicalInstruments  extends Category(customName = "Musical Instrument".some) with MainCategory
  case `One-handedSpears`  extends Category(customName = "One-handed Spear".some) with MainCategory
  case `One-handedWeapons` extends Category(customName = "One-handed Weapon".some) with MainCategory
  case PairedWeapons       extends Category(customName = "Paired Weapon".some) with MainCategory
  case Paraphernalia       extends Category(customName = "Paraphernalia".some) with MainCategory
  case Polearms            extends Category(customName = "Polearm".some) with MainCategory
  case Regalia             extends Category(customName = "Regalia".some) with MainCategory
  case Reliquaries         extends Category(customName = "Reliquary".some) with MainCategory
  case RitualMasks         extends Category(customName = "Ritual Mask".some) with MainCategory
  case RitualStaves        extends Category(customName = "Ritual Staff".some) with MainCategory
  case Rods                extends Category(customName = "Rod".some) with MainCategory
  case Shields             extends Category(customName = "Shield".some) with MainCategory
  case Staffs              extends Category(customName = "Staff".some) with MainCategory
  case Tools               extends Category(customName = "Tool".some) with MainCategory
  case Vestments           extends Category(customName = "Vestment".some) with MainCategory
  case Wands               extends Category(customName = "Wand".some) with MainCategory
  case `Runesmith'sLaw`    extends Category(customName = "Runesmith's Law".some) with ExtraCategory
  case Tonics              extends Category(customName = "Potion Recipe".some) with MainCategory

  lazy val show: String = toString.replaceAll("([a-z])([A-Z])", "$1 $2")
  lazy val name: String = customName.getOrElse(show)

object Category:
  def fromString(string: String): Either[String, Category] =
    Try(Category.valueOf(string.replaceAll(" ", ""))).toOption
      .toRight(s"Invalid Category `$string` please use one of ${Category.values.map(_.show).mkString(", ")}.")
end Category
