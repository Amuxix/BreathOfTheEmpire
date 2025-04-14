package empire
import wiki.{Category, MainCategory}

enum PublishCategory:
  case Wind, Mandate, Motion, Ritual, Other

object PublishCategory:
  def fromMainCategory(category: MainCategory): PublishCategory = category match
    case Category.TradeWinds      => Wind
    case Category.WindsOfMagic    => Wind
    case Category.WindsOfFortune  => Wind
    case Category.MilitaryCouncil => Wind
    case Category.Mandate         => Mandate
    case Category.SenateMotion    => Motion
    case Category.Rituals         => Ritual
    case _                        => Other
