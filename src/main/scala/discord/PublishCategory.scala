package discord

import pureconfig.generic.derivation.EnumConfigReader

enum PublishCategory derives EnumConfigReader:
  case WindOfFortune, WindOfWar, Mandate, Motion, Ritual, Other
