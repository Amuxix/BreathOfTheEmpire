package wiki

import wiki.LinkHelper.withoutLinks
import wiki.Section.OtherNations

sealed trait Main
sealed trait Extra
sealed trait Text
sealed trait CustomName(val customName: String):
  this: Category =>
  override lazy val name: String = customName

enum Section:
  case Wind, Senate, Nations, OtherNations, Eternals, Rituals, TheWay, MagicItems, Spirits

enum Category(val section: Section):
  case MilitaryCouncil     extends Category(Section.Wind) with Main with CustomName("Wind of War")
  case ImperialAddress     extends Category(Section.Wind) with Main
  case WindsOfMagic        extends Category(Section.Wind) with Main with CustomName("Wind of Magic")
  case TradeWinds          extends Category(Section.Wind) with Main with CustomName("Wind of Trade")
  case Appraisal           extends Category(Section.Wind) with Main
  case ForeignNations      extends Category(Section.Wind) with Main
  case Plenipotentiary     extends Category(Section.Wind) with Main
  case WindsOfFortune      extends Category(Section.Wind) with Main with CustomName("Wind of Fortune")
  // Empire nations
  case Dawn                extends Category(Section.Nations) with Text
  case Highguard           extends Category(Section.Nations) with Text
  case ImperialOrcs        extends Category(Section.Nations) with Text
  case League              extends Category(Section.Nations) with Text
  case Marches             extends Category(Section.Nations) with Text
  case Navarr              extends Category(Section.Nations) with Text
  case Urizen              extends Category(Section.Nations) with Text
  case Varushka            extends Category(Section.Nations) with Text
  case Wintermark          extends Category(Section.Nations) with Text
  case BrassCoast          extends Category(Section.Nations) with Text
  // Other Nations
  case Axos                extends Category(OtherNations) with Extra with Text
  case Asavea              extends Category(OtherNations) with Extra with Text
  case Commonwealth        extends Category(OtherNations) with Extra with Text
  case Faraden             extends Category(OtherNations) with Extra with Text
  case IronConfederacy     extends Category(OtherNations) with Extra with Text
  case Otkodov             extends Category(OtherNations) with Extra with Text
  case Jarm                extends Category(OtherNations) with Extra with Text with CustomName("Principalities of Jarm")
  case Sarcophan           extends Category(OtherNations) with Extra with Text with CustomName("Sarcophan Delves")
  case Skoura              extends Category(OtherNations) with Extra with Text
  case Sumaah              extends Category(OtherNations) with Extra with Text with CustomName("Sumaah Republic")
  case Tsark               extends Category(OtherNations) with Extra with Text
  case Barbarians          extends Category(OtherNations) with Extra
  case Druj                extends Category(OtherNations) with Extra with Text
  case Grendel             extends Category(OtherNations) with Extra with Text
  case Jotun               extends Category(OtherNations) with Extra with Text
  case Thule               extends Category(OtherNations) with Extra with Text
  // Senate
  case SenateMotion        extends Category(Section.Senate) with Main
  case FailedSenateMotion  extends Category(Section.Senate) with Extra with CustomName("Failed")
  case VetoedSenateMotion  extends Category(Section.Senate) with Extra with CustomName("Vetoed")
  case Mandate             extends Category(Section.Senate) with Main
  // Rituals
  case Rituals             extends Category(Section.Rituals) with Main with CustomName("Ritual")
  case SpringRitual        extends Category(Section.Rituals) with Extra with CustomName("Spring")
  case SummerRitual        extends Category(Section.Rituals) with Extra with CustomName("Summer")
  case AutumnRitual        extends Category(Section.Rituals) with Extra with CustomName("Autumn")
  case DayRitual           extends Category(Section.Rituals) with Extra with CustomName("Day")
  case NightRitual         extends Category(Section.Rituals) with Extra with CustomName("Night")
  case WinterRitual        extends Category(Section.Rituals) with Extra with CustomName("Winter")
  case UrizenLore          extends Category(Section.Rituals) with Extra
  case Enchantment         extends Category(Section.Rituals) with Extra
  case Curse               extends Category(Section.Rituals) with Extra
  case Warfare             extends Category(Section.Rituals) with Extra
  // The Way
  case TheWay              extends Category(Section.TheWay) with Extra with Text with CustomName("The Way")
  case Ambition            extends Category(Section.TheWay) with Extra with Text
  case Courage             extends Category(Section.TheWay) with Extra with Text
  case Loyalty             extends Category(Section.TheWay) with Extra with Text
  case Pride               extends Category(Section.TheWay) with Extra with Text
  case Prosperity          extends Category(Section.TheWay) with Extra with Text
  case Vigilance           extends Category(Section.TheWay) with Extra with Text
  case Wisdom              extends Category(Section.TheWay) with Extra with Text
  // Magic Items
  case MagicItems          extends Category(Section.MagicItems) with Main with CustomName("Magic Item")
  case Tonics              extends Category(Section.MagicItems) with Main with CustomName("Potion Recipe")
  case ArcaneImplements    extends Category(Section.MagicItems) with Extra with CustomName("Arcane Implement")
  case ArcaneWeapons       extends Category(Section.MagicItems) with Extra with CustomName("Arcane Weapon")
  case Bows                extends Category(Section.MagicItems) with Extra with CustomName("Bow")
  case Daggers             extends Category(Section.MagicItems) with Extra with CustomName("Dagger")
  case Foci                extends Category(Section.MagicItems) with Extra with CustomName("Focus")
  case Gonfalon            extends Category(Section.MagicItems) with Extra
  case GreatWeapons        extends Category(Section.MagicItems) with Extra with CustomName("Great Weapon")
  case HeavyArmour         extends Category(Section.MagicItems) with Extra
  case Icons               extends Category(Section.MagicItems) with Extra with CustomName("Icon")
  case Jewellery           extends Category(Section.MagicItems) with Extra
  case LightArmour         extends Category(Section.MagicItems) with Extra
  case MageArmour          extends Category(Section.MagicItems) with Extra
  case MageRobes           extends Category(Section.MagicItems) with Extra with CustomName("Mage Robe")
  case MagicStandards      extends Category(Section.MagicItems) with Extra with CustomName("Magic Standard")
  case MediumArmour        extends Category(Section.MagicItems) with Extra
  case MusicalInstruments  extends Category(Section.MagicItems) with Extra with CustomName("Musical Instrument")
  case `One-handedSpears`  extends Category(Section.MagicItems) with Extra with CustomName("One-handed Spear")
  case `One-handedWeapons` extends Category(Section.MagicItems) with Extra with CustomName("One-handed Weapon")
  case PairedWeapons       extends Category(Section.MagicItems) with Extra with CustomName("Paired Weapon")
  case Paraphernalia       extends Category(Section.MagicItems) with Extra
  case Polearms            extends Category(Section.MagicItems) with Extra with CustomName("Polearm")
  case Regalia             extends Category(Section.MagicItems) with Extra
  case Reliquaries         extends Category(Section.MagicItems) with Extra with CustomName("Reliquary")
  case RitualMasks         extends Category(Section.MagicItems) with Extra with CustomName("Ritual Mask")
  case RitualStaves        extends Category(Section.MagicItems) with Extra with CustomName("Ritual Staff")
  case Rods                extends Category(Section.MagicItems) with Extra with CustomName("Rod")
  case Shields             extends Category(Section.MagicItems) with Extra with CustomName("Shield")
  case Staffs              extends Category(Section.MagicItems) with Extra with CustomName("Staff")
  case Tools               extends Category(Section.MagicItems) with Extra with CustomName("Tool")
  case Vestments           extends Category(Section.MagicItems) with Extra with CustomName("Vestment")
  case Wands               extends Category(Section.MagicItems) with Extra with CustomName("Wand")
  case `Runesmith'sLaw`    extends Category(Section.MagicItems) with Extra
  // Eternals
  case Arhallogen          extends Category(Section.Eternals) with Text
  case IrraHarah           extends Category(Section.Eternals) with Text
  case Llofir              extends Category(Section.Eternals) with Text
  case Ossegrahn           extends Category(Section.Eternals) with Text
  case Siakha              extends Category(Section.Eternals) with Text
  case `Yaw'nagrah`        extends Category(Section.Eternals) with Text
  case Adamant             extends Category(Section.Eternals) with Text
  case Barien              extends Category(Section.Eternals) with Text
  case CathanCanae         extends Category(Section.Eternals) with Text
  case Eleonaris           extends Category(Section.Eternals) with Text
  case Hayaak              extends Category(Section.Eternals) with Text
  case Jaheris             extends Category(Section.Eternals) with Text
  case Meraud              extends Category(Section.Eternals) with Text
  case Rhianos             extends Category(Section.Eternals) with Text
  case Basileia            extends Category(Section.Eternals) with Text
  case Callidus            extends Category(Section.Eternals) with Text
  case Ephisis             extends Category(Section.Eternals) with Text
  case Estavus             extends Category(Section.Eternals) with Text
  case Lictors             extends Category(Section.Eternals) with Text
  case Mazen               extends Category(Section.Eternals) with Text
  case Prospero            extends Category(Section.Eternals) with Text
  case Sinokenon           extends Category(Section.Eternals) with Text
  case Agramant            extends Category(Section.Eternals) with Text
  case Kaela               extends Category(Section.Eternals) with Text
  case Skathe              extends Category(Section.Eternals) with Text
  case Sorin               extends Category(Section.Eternals) with Text
  case Surut               extends Category(Section.Eternals) with Text
  case Tharim              extends Category(Section.Eternals) with Text
  case ThriceCursedCourt   extends Category(Section.Eternals) with Text with CustomName("Thrice-cursed Court")
  case WiseRangara         extends Category(Section.Eternals) with Text
  case Zakalwe             extends Category(Section.Eternals) with Text
  case ColdSun             extends Category(Section.Eternals) with Text
  case Kimus               extends Category(Section.Eternals) with Text
  case Leviathan           extends Category(Section.Eternals) with Text
  case Phaleron            extends Category(Section.Eternals) with Text
  case Roshanwe            extends Category(Section.Eternals) with Text
  case Sung                extends Category(Section.Eternals) with Text
  case Ylenwe              extends Category(Section.Eternals) with Text
  case Azoth               extends Category(Section.Eternals) with Text
  case Janon               extends Category(Section.Eternals) with Text
  case Lashonar            extends Category(Section.Eternals) with Text
  case Sadogua             extends Category(Section.Eternals) with Text
  case Soghter             extends Category(Section.Eternals) with Text
  case WhisperGallery      extends Category(Section.Eternals) with Text
  // Varushkan Sovereigns
  case CharnelLord         extends Category(Section.Spirits) with Text
  case NightBelow          extends Category(Section.Spirits) with Text
  case `Dho'uala`          extends Category(Section.Spirits) with Text
  // Heralds
  case Elioe               extends Category(Section.Spirits) with Text
  case Lioc                extends Category(Section.Spirits) with Text
  case Pollaman            extends Category(Section.Spirits) with Text
  case Arannia             extends Category(Section.Spirits) with Text
  case Melchiore           extends Category(Section.Spirits) with Text
  case Simone              extends Category(Section.Spirits) with Text
  case Dreyfus             extends Category(Section.Spirits) with Text
  case Numis               extends Category(Section.Spirits) with Text
  case Malleas             extends Category(Section.Spirits) with Text
  case Zand                extends Category(Section.Spirits) with Text
  case Mahine              extends Category(Section.Spirits) with Text
  case Esmeray             extends Category(Section.Spirits) with Text
  case Revel               extends Category(Section.Spirits) with Text
  case Temper              extends Category(Section.Spirits) with Text
  // Other Spirits
  case Vallorn             extends Category(Section.Spirits) with Text
  case Sydanjaa            extends Category(Section.Spirits) with Text
  case Volodny             extends Category(Section.Spirits) with Text

  lazy val name: String = toString.replaceAll("([a-z])([A-Z])", "$1 $2").replaceAll("Of", "of")

object Category:
  private lazy val valueMap = Category.values.map(v => v.toString.toLowerCase -> v).toMap

  def fromString(string: String): Option[Category] =
    valueMap
      .get(string.toLowerCase.replaceAll(" ", ""))

  lazy val categoryRules: List[CategoryRule] = Rules.allRules.collect { case categoryRule: CategoryRule =>
    categoryRule
  }

  extension (text: String)
    def extractCategories: List[(Category & Text, Int)] =
      val noLinks = text.withoutLinks
      categoryRules
        .foldLeft(List.empty[(Category & Text, Int)]) { case (current, rule) =>
          val allMatches = rule.pattern.findAllMatchIn(noLinks).toList
          val matches    = allMatches.size
          if matches > 0 then
            println(s"found ${rule.category.name}: ${allMatches.map(_.toString).distinct}")
            current :+ (rule.category -> matches)
          else current
        }

  def sort(categories: List[(Category & (Extra | Text), Int)]): List[Category & (Extra | Text)] =
    categories
      .groupBy(_.head)
      .toList
      .map((category, list) => (category, list.map(_(1)).sum))
      // .sortBy((category, occurrences) => (category.section.ordinal, -occurrences))
      .sortBy(_(1))(using Ordering[Int].reverse)
      .map(_(0))
