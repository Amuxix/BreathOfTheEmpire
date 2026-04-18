package wiki

sealed trait Main
sealed trait Extra
sealed trait Text
sealed trait CustomName(val customName: String):
  this: Category =>
  override lazy val name: String = customName

enum Category:
  case MilitaryCouncil     extends Category with Main with CustomName("Wind of War")
  case WindsOfMagic        extends Category with Main with CustomName("Wind of Magic")
  case TradeWinds          extends Category with Main with CustomName("Wind of Trade")
  case Appraisal           extends Category with Main
  case ForeignNations      extends Category with Main
  case Plenipotentiary     extends Category with Main
  case WindsOfFortune      extends Category with Main with CustomName("Wind of Fortune")
  // Empire nations
  case Dawn                extends Category with Text
  case Highguard           extends Category with Text
  case ImperialOrcs        extends Category with Text
  case League              extends Category with Text
  case Marches             extends Category with Text
  case Navarr              extends Category with Text
  case Urizen              extends Category with Text
  case Varushka            extends Category with Text
  case Wintermark          extends Category with Text
  case BrassCoast          extends Category with Text
  // Other Nations
  case Axos                extends Category with Extra with Text
  case Asavea              extends Category with Extra with Text
  case Commonwealth        extends Category with Extra with Text
  case Faraden             extends Category with Extra with Text
  case IronConfederacy     extends Category with Extra with Text
  case Otkodov             extends Category with Extra with Text
  case Jarm                extends Category with Extra with Text with CustomName("Principalities of Jarm")
  case Sarcophan           extends Category with Extra with Text with CustomName("Sarcophan Delves")
  case Skoura              extends Category with Extra with Text
  case Sumaah              extends Category with Extra with Text with CustomName("Sumaah Republic")
  case Tsark               extends Category with Extra with Text
  case Barbarians          extends Category with Extra
  case Druj                extends Category with Extra with Text
  case Grendel             extends Category with Extra with Text
  case Jotun               extends Category with Extra with Text
  case Thule               extends Category with Extra with Text
  // Senate
  case SenateMotion        extends Category with Main
  case FailedSenateMotion  extends Category with Extra with CustomName("Failed")
  case VetoedSenateMotion  extends Category with Extra with CustomName("Vetoed")
  case Mandate             extends Category with Main
  // Rituals
  case Rituals             extends Category with Main with CustomName("Ritual")
  case SpringRitual        extends Category with Extra with CustomName("Spring")
  case SummerRitual        extends Category with Extra with CustomName("Summer")
  case AutumnRitual        extends Category with Extra with CustomName("Autumn")
  case DayRitual           extends Category with Extra with CustomName("Day")
  case NightRitual         extends Category with Extra with CustomName("Night")
  case WinterRitual        extends Category with Extra with CustomName("Winter")
  case UrizenLore          extends Category with Extra
  case Enchantment         extends Category with Extra
  case Curse               extends Category with Extra
  case Warfare             extends Category with Extra
  // Magic Items
  case MagicItems          extends Category with Main with CustomName("Magic Item")
  case Tonics              extends Category with Main with CustomName("Potion Recipe")
  case ArcaneImplements    extends Category with Extra with CustomName("Arcane Implement")
  case ArcaneWeapons       extends Category with Extra with CustomName("Arcane Weapon")
  case Bows                extends Category with Extra with CustomName("Bow")
  case Daggers             extends Category with Extra with CustomName("Dagger")
  case Foci                extends Category with Extra with CustomName("Focus")
  case Gonfalon            extends Category with Extra
  case GreatWeapons        extends Category with Extra with CustomName("Great Weapon")
  case HeavyArmour         extends Category with Extra
  case Icons               extends Category with Extra with CustomName("Icon")
  case Jewellery           extends Category with Extra
  case LightArmour         extends Category with Extra
  case MageArmour          extends Category with Extra
  case MageRobes           extends Category with Extra with CustomName("Mage Robe")
  case MagicStandards      extends Category with Extra with CustomName("Magic Standard")
  case MediumArmour        extends Category with Extra
  case MusicalInstruments  extends Category with Extra with CustomName("Musical Instrument")
  case `One-handedSpears`  extends Category with Extra with CustomName("One-handed Spear")
  case `One-handedWeapons` extends Category with Extra with CustomName("One-handed Weapon")
  case PairedWeapons       extends Category with Extra with CustomName("Paired Weapon")
  case Paraphernalia       extends Category with Extra
  case Polearms            extends Category with Extra with CustomName("Polearm")
  case Regalia             extends Category with Extra
  case Reliquaries         extends Category with Extra with CustomName("Reliquary")
  case RitualMasks         extends Category with Extra with CustomName("Ritual Mask")
  case RitualStaves        extends Category with Extra with CustomName("Ritual Staff")
  case Rods                extends Category with Extra with CustomName("Rod")
  case Shields             extends Category with Extra with CustomName("Shield")
  case Staffs              extends Category with Extra with CustomName("Staff")
  case Tools               extends Category with Extra with CustomName("Tool")
  case Vestments           extends Category with Extra with CustomName("Vestment")
  case Wands               extends Category with Extra with CustomName("Wand")
  case `Runesmith'sLaw`    extends Category with Extra
  // Eternals
  case Arhallogen          extends Category with Text
  case IrraHarah           extends Category with Text
  case Llofir              extends Category with Text
  case Ossegrahn           extends Category with Text
  case Siakha              extends Category with Text
  case `Yaw'nagrah`        extends Category with Text
  case Adamant             extends Category with Text
  case Barien              extends Category with Text
  case CathanCanae         extends Category with Text
  case Eleonaris           extends Category with Text
  case Hayaak              extends Category with Text
  case Jaheris             extends Category with Text
  case Meraud              extends Category with Text
  case Rhianos             extends Category with Text
  case Basileia            extends Category with Text
  case Callidus            extends Category with Text
  case Ephisis             extends Category with Text
  case Estavus             extends Category with Text
  case Lictors             extends Category with Text
  case Mazen               extends Category with Text
  case Prospero            extends Category with Text
  case Sinokenon           extends Category with Text
  case Agramant            extends Category with Text
  case Kaela               extends Category with Text
  case Skathe              extends Category with Text
  case Sorin               extends Category with Text
  case Surut               extends Category with Text
  case Tharim              extends Category with Text
  case ThriceCursedCourt   extends Category with Text with CustomName("Thrice-cursed Court")
  case WiseRangara         extends Category with Text
  case Zakalwe             extends Category with Text
  case ColdSun             extends Category with Text
  case Kimus               extends Category with Text
  case Leviathan           extends Category with Text
  case Phaleron            extends Category with Text
  case Roshanwe            extends Category with Text
  case Sung                extends Category with Text
  case Ylenwe              extends Category with Text
  case Azoth               extends Category with Text
  case Janon               extends Category with Text
  case Lashonar            extends Category with Text
  case Murit               extends Category with Text
  case Sadogua             extends Category with Text
  case Soghter             extends Category with Text
  case WhisperGallery      extends Category with Text
  // Varushkan Sovereigns
  case CharnelLord         extends Category with Text
  case NightBelow          extends Category with Text
  case `Dho'uala`          extends Category with Text
  // Heralds
  case Elioe               extends Category with Text
  case Lioc                extends Category with Text
  case Pollaman            extends Category with Text
  case Arannia             extends Category with Text
  case Melchiore           extends Category with Text
  case Simone              extends Category with Text
  case Dreyfus             extends Category with Text
  case Numis               extends Category with Text
  case Malleas             extends Category with Text
  case Zand                extends Category with Text
  case Mahine              extends Category with Text
  case Esmeray             extends Category with Text
  case Revel               extends Category with Text
  case Temper              extends Category with Text
  // Other Spirits
  case Sydanjaa            extends Category with Text
  case Volodny             extends Category with Text

  lazy val name: String = toString.replaceAll("([a-z])([A-Z])", "$1 $2").replaceAll("Of", "of")

object Category:
  private lazy val valueMap = Category.values.map(v => v.toString.toLowerCase -> v).toMap

  def fromString(string: String): Option[Category] =
    valueMap
      .get(string.toLowerCase.replaceAll(" ", ""))
