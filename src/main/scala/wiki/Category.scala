package wiki

import cats.syntax.option.*

sealed trait MainCategory
sealed trait ExtraCategory

enum Category(val customName: Option[String] = None):
  case MilitaryCouncil     extends Category("Wind of War".some) with MainCategory
  case WindsOfMagic        extends Category("Wind of Magic".some) with MainCategory
  case TradeWinds          extends Category("Wind of Trade".some) with MainCategory
  case WindsOfFortune      extends Category("Wind of Fortune".some) with MainCategory
  case Tonics              extends Category("Potion Recipe".some) with MainCategory
  case Rituals             extends Category("Ritual".some) with MainCategory
  case SpringRitual        extends Category("Spring".some) with ExtraCategory
  case SummerRitual        extends Category("Summer".some) with ExtraCategory
  case AutumnRitual        extends Category("Autumn".some) with ExtraCategory
  case DayRitual           extends Category("Day".some) with ExtraCategory
  case NightRitual         extends Category("Night".some) with ExtraCategory
  case WinterRitual        extends Category("Winter".some) with ExtraCategory
  case UrizenLore          extends Category with ExtraCategory
  case Enchantment         extends Category with ExtraCategory
  case Curse               extends Category with ExtraCategory
  case Warfare             extends Category with ExtraCategory
  case MagicItems          extends Category("Magic Item".some) with MainCategory
  case ArcaneImplements    extends Category("Arcane Implement".some) with ExtraCategory
  case ArcaneWeapons       extends Category("Arcane Weapon".some) with ExtraCategory
  case Bows                extends Category("Bow".some) with ExtraCategory
  case Daggers             extends Category("Dagger".some) with ExtraCategory
  case Foci                extends Category("Focus".some) with ExtraCategory
  case Gonfalon            extends Category with ExtraCategory
  case GreatWeapons        extends Category("Great Weapon".some) with ExtraCategory
  case HeavyArmour         extends Category with ExtraCategory
  case Icons               extends Category("Icon".some) with ExtraCategory
  case Jewellery           extends Category with ExtraCategory
  case LightArmour         extends Category with ExtraCategory
  case MageArmour          extends Category with ExtraCategory
  case MageRobes           extends Category("Mage Robe".some) with ExtraCategory
  case MagicStandards      extends Category("Magic Standard".some) with ExtraCategory
  case MediumArmour        extends Category with ExtraCategory
  case MusicalInstruments  extends Category("Musical Instrument".some) with ExtraCategory
  case `One-handedSpears`  extends Category("One-handed Spear".some) with ExtraCategory
  case `One-handedWeapons` extends Category("One-handed Weapon".some) with ExtraCategory
  case PairedWeapons       extends Category("Paired Weapon".some) with ExtraCategory
  case Paraphernalia       extends Category with ExtraCategory
  case Polearms            extends Category("Polearm".some) with ExtraCategory
  case Regalia             extends Category with ExtraCategory
  case Reliquaries         extends Category("Reliquary".some) with ExtraCategory
  case RitualMasks         extends Category("Ritual Mask".some) with ExtraCategory
  case RitualStaves        extends Category("Ritual Staff".some) with ExtraCategory
  case Rods                extends Category("Rod".some) with ExtraCategory
  case Shields             extends Category("Shield".some) with ExtraCategory
  case Staffs              extends Category("Staff".some) with ExtraCategory
  case Tools               extends Category("Tool".some) with ExtraCategory
  case Vestments           extends Category("Vestment".some) with ExtraCategory
  case Wands               extends Category("Wand".some) with ExtraCategory
  case `Runesmith'sLaw`    extends Category with ExtraCategory
  case SenateMotion        extends Category with MainCategory
  case FailedSenateMotion  extends Category("Failed".some) with ExtraCategory
  case VetoedSenateMotion  extends Category("Vetoed".some) with ExtraCategory
  case Mandate             extends Category with MainCategory

  lazy val show: String = toString.replaceAll("([a-z])([A-Z])", "$1 $2")
  lazy val name: String = customName.getOrElse(show)

object Category:
  private lazy val valueMap = Category.values.map(v => v.toString.toLowerCase -> v).toMap

  def fromString(string: String): Option[Category] =
    valueMap
      .get(string.toLowerCase.replaceAll(" ", ""))
