package wiki

sealed trait MainCategory
sealed trait ExtraCategory
sealed trait TextCategory
sealed trait CustomName(val customName: String):
  this: Category =>
  override lazy val name: String = customName

enum Category:
  case MilitaryCouncil      extends Category with MainCategory with CustomName("Wind of War")
  case WindsOfMagic         extends Category with MainCategory with CustomName("Wind of Magic")
  case TradeWinds           extends Category with MainCategory with CustomName("Wind of Trade")
  case Appraisal            extends Category with MainCategory
  case ForeignNations       extends Category with MainCategory
  case Plenipotentiary      extends Category with MainCategory
  case Axos                 extends Category with ExtraCategory with TextCategory
  case Asavea               extends Category with ExtraCategory with TextCategory
  case Commonwealth         extends Category with ExtraCategory with TextCategory
  case Faraden              extends Category with ExtraCategory with TextCategory
  case IronConfederacy      extends Category with ExtraCategory with TextCategory
  case Otkodov              extends Category with ExtraCategory with TextCategory
  case Jarm                 extends Category with ExtraCategory with TextCategory with CustomName("Principalities of Jarm")
  case Sarcophan            extends Category with ExtraCategory with TextCategory with CustomName("Sarcophan Delves")
  case Skoura               extends Category with ExtraCategory with TextCategory
  case Sumaah               extends Category with ExtraCategory with TextCategory with CustomName("Sumaah Republic")
  case Tsark                extends Category with ExtraCategory with TextCategory
  case Barbarians           extends Category with ExtraCategory
  case Druj                 extends Category with ExtraCategory with TextCategory
  case Grendel              extends Category with ExtraCategory with TextCategory
  case Jotun                extends Category with ExtraCategory with TextCategory
  case Thule                extends Category with ExtraCategory with TextCategory
  // Empire nations
  case Dawn                 extends Category with TextCategory
  case Highguard            extends Category with TextCategory
  case ImperialOrcs         extends Category with TextCategory
  case TheLeague            extends Category with TextCategory
  case TheMarches           extends Category with TextCategory
  case Navarr               extends Category with TextCategory
  case Urizen               extends Category with TextCategory
  case Varushka             extends Category with TextCategory
  case Wintermark           extends Category with TextCategory
  case TheBrassCoast        extends Category with TextCategory
  // Eternals
  case Arhallogen           extends Category with TextCategory
  case IrraHarah            extends Category with TextCategory
  case Llofir               extends Category with TextCategory
  case Ossegrahn            extends Category with TextCategory
  case Siakha               extends Category with TextCategory
  case `Yaw'nagrah`         extends Category with TextCategory
  case Adamant              extends Category with TextCategory
  case Barien               extends Category with TextCategory
  case CathanCanae          extends Category with TextCategory
  case Eleonaris            extends Category with TextCategory
  case Hayaak               extends Category with TextCategory
  case Jaheris              extends Category with TextCategory
  case Meraud               extends Category with TextCategory
  case Rhianos              extends Category with TextCategory
  case Basileia             extends Category with TextCategory
  case Callidus             extends Category with TextCategory
  case Ephisis              extends Category with TextCategory
  case Estavus              extends Category with TextCategory
  case TheLictors           extends Category with TextCategory
  case Mazen                extends Category with TextCategory
  case Prospero             extends Category with TextCategory
  case Sinokenon            extends Category with TextCategory
  case Agramant             extends Category with TextCategory
  case Kaela                extends Category with TextCategory
  case Skathe               extends Category with TextCategory
  case Sorin                extends Category with TextCategory
  case Surut                extends Category with TextCategory
  case Tharim               extends Category with TextCategory
  case TheThriceCursedCourt extends Category with TextCategory with CustomName("The Thrice-cursed Court")
  case WiseRangara          extends Category with TextCategory
  case Zakalwe              extends Category with TextCategory
  case ColdSun              extends Category with TextCategory
  case Kimus                extends Category with TextCategory
  case Leviathan            extends Category with TextCategory
  case Phaleron             extends Category with TextCategory
  case Roshanwe             extends Category with TextCategory
  case Sung                 extends Category with TextCategory
  case Ylenwe               extends Category with TextCategory
  case Azoth                extends Category with TextCategory
  case Janon                extends Category with TextCategory
  case Lashonar             extends Category with TextCategory
  case Murit                extends Category with TextCategory
  case Sadogua              extends Category with TextCategory
  case Soghter              extends Category with TextCategory
  case TheWhisperGallery    extends Category with TextCategory
  // Varushkan Sovereigns
  case TheCharnelLord       extends Category with TextCategory
  case TheNightBelow        extends Category with TextCategory
  case TheHowlingQueen      extends Category with TextCategory
  case ThePaleLady          extends Category with TextCategory
  case TheFatherOfWolves    extends Category with TextCategory
  case TheLullaby           extends Category with TextCategory
  case TheThinMan           extends Category with TextCategory
  case TheTallowman         extends Category with TextCategory
  case TheShadowsmith       extends Category with TextCategory
  case `Dho'uala`           extends Category with TextCategory
  // Heralds
  case Elioe                extends Category with TextCategory
  case Lioc                 extends Category with TextCategory
  case Pollaman             extends Category with TextCategory
  case Arannia              extends Category with TextCategory
  case Melchiore            extends Category with TextCategory
  case Simone               extends Category with TextCategory
  case Dreyfus              extends Category with TextCategory
  case Numis                extends Category with TextCategory
  case Malleas              extends Category with TextCategory
  case Zand                 extends Category with TextCategory
  case Mahine               extends Category with TextCategory
  case Esmeray              extends Category with TextCategory
  case Revel                extends Category with TextCategory
  case Temper               extends Category with TextCategory
  // Other Spirits
  case Sydanjaa             extends Category with TextCategory
  // Rest
  case WindsOfFortune       extends Category with MainCategory with CustomName("Wind of Fortune")
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

  lazy val name: String = toString.replaceAll("([a-z])([A-Z])", "$1 $2").replaceAll("Of", "of")

object Category:
  private lazy val valueMap = Category.values.map(v => v.toString.toLowerCase -> v).toMap

  def fromString(string: String): Option[Category] =
    valueMap
      .get(string.toLowerCase.replaceAll(" ", ""))
