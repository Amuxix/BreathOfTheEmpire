package wiki

import cats.syntax.option.*

sealed trait MainCategory
sealed trait ExtraCategory
sealed trait CustomName(val customName: String):
  this: Category =>
  override lazy val name: String = customName

enum Category:
  case MilitaryCouncil      extends Category with MainCategory with CustomName("Wind of War")
  case WindsOfMagic         extends Category with MainCategory with CustomName("Wind of Magic")
  case TradeWinds           extends Category with MainCategory with CustomName("Wind of Trade")
  case WindsOfFortune       extends Category with MainCategory with CustomName("Wind of Fortune")
  case ForeignNations       extends Category with ExtraCategory
  case Axos                 extends Category with ExtraCategory
  case Asavea               extends Category with ExtraCategory
  case Commonwealth         extends Category with ExtraCategory
  case Faraden              extends Category with ExtraCategory
  case IronConfederacy      extends Category with ExtraCategory with CustomName("Confederacy")
  case Otkodov              extends Category with ExtraCategory
  case PrincipalitiesOfJarm extends Category with ExtraCategory with CustomName("Jarm")
  case SarcophanDelves      extends Category with ExtraCategory with CustomName("Delves")
  case Skoura               extends Category with ExtraCategory
  case SumaahRepublic       extends Category with ExtraCategory with CustomName("Sumaah")
  case Barbarians           extends Category with ExtraCategory
  case Druj                 extends Category with ExtraCategory
  case Grendel              extends Category with ExtraCategory
  case Jotun                extends Category with ExtraCategory
  case Thule                extends Category with ExtraCategory
  case Tonics               extends Category with MainCategory with CustomName("Potion Recipe")
  case Rituals              extends Category with MainCategory with CustomName("Ritual")
  case SpringRitual         extends Category with ExtraCategory with CustomName("Spring")
  case SummerRitual         extends Category with ExtraCategory with CustomName("Summer")
  case AutumnRitual         extends Category with ExtraCategory with CustomName("Autumn")
  case DayRitual            extends Category with ExtraCategory with CustomName("Day")
  case NightRitual          extends Category with ExtraCategory with CustomName("Night")
  case WinterRitual         extends Category with ExtraCategory with CustomName("Winter")
  case UrizenLore           extends Category with ExtraCategory
  case Enchantment          extends Category with ExtraCategory
  case Curse                extends Category with ExtraCategory
  case Warfare              extends Category with ExtraCategory
  case MagicItems           extends Category with MainCategory with CustomName("Magic Item")
  case ArcaneImplements     extends Category with ExtraCategory with CustomName("Arcane Implement")
  case ArcaneWeapons        extends Category with ExtraCategory with CustomName("Arcane Weapon")
  case Bows                 extends Category with ExtraCategory with CustomName("Bow")
  case Daggers              extends Category with ExtraCategory with CustomName("Dagger")
  case Foci                 extends Category with ExtraCategory with CustomName("Focus")
  case Gonfalon             extends Category with ExtraCategory
  case GreatWeapons         extends Category with ExtraCategory with CustomName("Great Weapon")
  case HeavyArmour          extends Category with ExtraCategory
  case Icons                extends Category with ExtraCategory with CustomName("Icon")
  case Jewellery            extends Category with ExtraCategory
  case LightArmour          extends Category with ExtraCategory
  case MageArmour           extends Category with ExtraCategory
  case MageRobes            extends Category with ExtraCategory with CustomName("Mage Robe")
  case MagicStandards       extends Category with ExtraCategory with CustomName("Magic Standard")
  case MediumArmour         extends Category with ExtraCategory
  case MusicalInstruments   extends Category with ExtraCategory with CustomName("Musical Instrument")
  case `One-handedSpears`   extends Category with ExtraCategory with CustomName("One-handed Spear")
  case `One-handedWeapons`  extends Category with ExtraCategory with CustomName("One-handed Weapon")
  case PairedWeapons        extends Category with ExtraCategory with CustomName("Paired Weapon")
  case Paraphernalia        extends Category with ExtraCategory
  case Polearms             extends Category with ExtraCategory with CustomName("Polearm")
  case Regalia              extends Category with ExtraCategory
  case Reliquaries          extends Category with ExtraCategory with CustomName("Reliquary")
  case RitualMasks          extends Category with ExtraCategory with CustomName("Ritual Mask")
  case RitualStaves         extends Category with ExtraCategory with CustomName("Ritual Staff")
  case Rods                 extends Category with ExtraCategory with CustomName("Rod")
  case Shields              extends Category with ExtraCategory with CustomName("Shield")
  case Staffs               extends Category with ExtraCategory with CustomName("Staff")
  case Tools                extends Category with ExtraCategory with CustomName("Tool")
  case Vestments            extends Category with ExtraCategory with CustomName("Vestment")
  case Wands                extends Category with ExtraCategory with CustomName("Wand")
  case `Runesmith'sLaw`     extends Category with ExtraCategory
  case SenateMotion         extends Category with MainCategory
  case FailedSenateMotion   extends Category with ExtraCategory with CustomName("Failed")
  case VetoedSenateMotion   extends Category with ExtraCategory with CustomName("Vetoed")
  case Mandate              extends Category with MainCategory

  lazy val name: String = toString.replaceAll("([a-z])([A-Z])", "$1 $2")

object Category:
  private lazy val valueMap = Category.values.map(v => v.toString.toLowerCase -> v).toMap

  def fromString(string: String): Option[Category] =
    valueMap
      .get(string.toLowerCase.replaceAll(" ", ""))
