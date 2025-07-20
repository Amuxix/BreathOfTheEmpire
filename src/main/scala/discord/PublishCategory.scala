package discord

import pureconfig.generic.derivation.EnumConfigReader

enum PublishCategory derives EnumConfigReader:
  case WindOfFortune, WindOfWar, Diplomacy, Appraisal, Mandate, Motion, Magic, Item
