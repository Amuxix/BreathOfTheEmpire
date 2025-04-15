package empire

import pureconfig.generic.derivation.EnumConfigReader
import wiki.{Category, MainCategory}

enum PublishCategory derives EnumConfigReader:
  case WindOfFortune, WindOfWar, Mandate, Motion, Ritual, Other

object PublishCategory:
  def fromMainCategory(category: MainCategory): PublishCategory = category match
    case Category.TradeWinds      => WindOfFortune
    case Category.WindsOfMagic    => WindOfFortune
    case Category.WindsOfFortune  => WindOfFortune
    case Category.MilitaryCouncil => WindOfWar
    case Category.Mandate         => Mandate
    case Category.SenateMotion    => Motion
    case Category.Rituals         => Ritual
    case _                        => Other
