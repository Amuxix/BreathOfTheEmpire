package wiki

import org.http4s.Uri

import scala.util.matching.Regex

object LinkEnricher:
  case class Rule(pattern: Regex, page: String, category: Option[Category & Text] = None)

  private def insensitiveWord(term: String): Regex =
    s"(?i)\\b$term\\b".r

  private def rulesFromPattern(entries: (String, String) | (String, String, Category & Text)*): List[Rule] =
    entries.toList.map {
      case (pattern, page)           => Rule(insensitiveWord(pattern), page)
      case (pattern, page, category) => Rule(insensitiveWord(pattern), page, Some(category))
    }

  private def quoted(name: String): Regex = insensitiveWord(s"${Regex.quote(name)}s?")

  private def rulesFromName(entries: String | (String, Category & Text)*): List[Rule] =
    entries.toList.map {
      case name: String     => Rule(quoted(name), name)
      case (name, category) => Rule(quoted(name), name, Some(category))
    }

  // format: off
  private val eternals = rulesFromName(
    // Spring
    "Arhallogen"             -> Category.Arhallogen,
    "Irra Harah"             -> Category.IrraHarah,
    "Llofir"                 -> Category.Llofir,
    "Ossegrahn"              -> Category.Ossegrahn,
    "Siakha"                 -> Category.Siakha,
    "Yaw'nagrah"             -> Category.`Yaw'nagrah`,
    // Summer
    "Adamant"                -> Category.Adamant,
    "Barien"                 -> Category.Barien,
    "Cathan Canae"           -> Category.CathanCanae,
    "Eleonaris"              -> Category.Eleonaris,
    "Hayaak"                 -> Category.Hayaak,
    "Jaheris"                -> Category.Jaheris,
    "Meraud"                 -> Category.Meraud,
    "Rhianos"                -> Category.Rhianos,
    // Autumn
    "Basileia"               -> Category.Basileia,
    "Callidus"               -> Category.Callidus,
    "Ephisis"                -> Category.Ephisis,
    "Estavus"                -> Category.Estavus,
    "Mazen"                  -> Category.Mazen,
    "Prospero"               -> Category.Prospero,
    "Sinokenon"              -> Category.Sinokenon,
    // Winter
    "Agramant"               -> Category.Agramant,
    "Kaela"                  -> Category.Kaela,
    "Skathe"                 -> Category.Skathe,
    "Sorin"                  -> Category.Sorin,
    "Surut"                  -> Category.Surut,
    "Tharim"                 -> Category.Tharim,
    "The Thrice-cursed Court" -> Category.ThriceCursedCourt,
    "Wise Rangara"           -> Category.WiseRangara,
    "Zakalwe"                -> Category.Zakalwe,
    // Day
    "Cold Sun"               -> Category.ColdSun,
    "Kimus"                  -> Category.Kimus,
    "Leviathan"              -> Category.Leviathan,
    "Phaleron"               -> Category.Phaleron,
    "Roshanwe"               -> Category.Roshanwe,
    "Sung"                   -> Category.Sung,
    "Ylenwe"                 -> Category.Ylenwe,
    // Night
    "Azoth"                  -> Category.Azoth,
    "Janon"                  -> Category.Janon,
    "Lashonar"               -> Category.Lashonar,
    "Murit"                  -> Category.Murit,
    "Sadogua"                -> Category.Sadogua,
    "Soghter"                -> Category.Soghter,
  ) ++ rulesFromPattern(
    ("(The )?Lictors", "Lictors", Category.Lictors),
    ("(The )?Whisper Gallery", "Whisper Gallery", Category.WhisperGallery),
  )

  private val nations = rulesFromName(
    "Dawn"          -> Category.Dawn,
    "Highguard"     -> Category.Highguard,
    "Imperial Orcs" -> Category.ImperialOrcs,
    "The League"    -> Category.League,
    "The Marches"   -> Category.Marches,
    "Navarr"        -> Category.Navarr,
    "Urizen"        -> Category.Urizen,
    "Varushka"      -> Category.Varushka,
    "Wintermark"    -> Category.Wintermark,
  ) ++ rulesFromPattern(
    ("(The )?Brass Coast", "The Brass Coast", Category.BrassCoast),
  )

  private val foreignNations = rulesFromName(
    "Axos"              -> Category.Axos,
    "Asavea"            -> Category.Asavea,
    "Commonwealth"      -> Category.Commonwealth,
    "Faraden"           -> Category.Faraden,
    "Iron Confederacy"  -> Category.IronConfederacy,
    "Skoura"            -> Category.Skoura,
    "Otkodov"           -> Category.Otkodov,
    "Tsark"             -> Category.Tsark,
  ) ++ rulesFromPattern(
    ("(Principalities of )?Jarm", "Principalities of Jarm", Category.Jarm),
    ("Sarcophan( Delves)?", "Sarcophan Delves", Category.Sarcophan),
    ("Sumaah( Republic)?", "Sumaah Republic", Category.Sumaah),
  )

  private val barbarianNations = rulesFromName(
    "Druj"    -> Category.Druj,
    "Grendel" -> Category.Grendel,
    "Jotun"   -> Category.Jotun,
    "Thule"   -> Category.Thule,
  )

  private val regions = rulesFromName(
    // Brass Coast
    "Kahraman", "Madruga", "Segura", "Feroz",
    // Dawn
    "Astolat", "Semmerholm", "Weirwater",
    // Highguard
    "Bastion", "Casinea", "Necropolis", "Reikos",
    // Imperial Orcs
    "Skarsind",
    // The League
    "Holberg", "Sarvos", "Tassato", "Temeschwar",
    // The Marches
    "Mitwold", "Mournwold", "Bregasland", "Upwold",
    // Navarr
    "Hercynia", "Miaren", "Therunin", "Liathaven",
    // Urizen
    "Morrow", "Redoubt", "Zenith", "Spiral",
    // Varushka
    "Karov", "Karsk", "Miekarova", "Volodmartz",
    // Wintermark
    "Hahnmark", "Sermersuaq", "Kallavesa", "Skallahn",
    // Other
    "The Barrens", "Ossium", "Mareave", "Sarangrave",
  ) ++ rulesFromPattern(
    ("Broc[eé]liande", "Brocéliande"),
  )

  private val sovereignty = rulesFromName(
    "Imperial Senate", "Imperial Synod", "Imperial Conclave",
    "Imperial Bourse", "Imperial Military Council", "The Throne",
    // Conclave Orders
    "The Celestial Arch", "The Sevenfold Path", "The Rod and Shield",
    "The Golden Pyramid", "The Shuttered Lantern", "The Unfettered Mind", "The Silver Chalice",
    // Archmage titles
    "Archmage of Spring", "Archmage of Summer", "Archmage of Autumn",
    "Archmage of Winter", "Archmage of Day", "Archmage of Night",
  )

  private val springRituals = rulesFromName(
    "Blood of the Hydra", "The Hands of Sacred Life", "Hearthfire Circle",
    "Chirurgeon's Healing Touch", "Fountain of Life", "Vitality of Rushing Water",
    "Irrepressible Monkey Spirit", "Skin of Bark, Blood of Amber", "Rivers of Life",
    "Blessing of New Spring", "Fan the Flame of New Life", "Churning Cauldron of Bravash",
    "Rampant Growth", "Hallow of the Green World", "Fire in the Blood",
    "Touch of Vile Humours", "Unending Cascade of Blood's Fire", "Curse of Gangrenous Flesh",
    "Fetid Breath of Teeming Plague", "Rivers Run Red", "Call Down Lightning's Wrath",
    "Blood and Salt", "Merciless Wrath of the Reaver", "Foam and Spittle of the Furious Sea",
    "Thunderous Deluge", "Turns the Circle", "The Forest Remains",
    "Rising Roots that Rend Stone", "Thunderous Tread of the Trees",
  )

  private val summerRituals = rulesFromName(
    "Renewed Strength of the New Day", "Tenacity of Jotra", "Swan's Cruel Wing",
    "Swift Leaping Hare", "Hammer of Thunder", "Strength of the Bull",
    "Crimson Ward of Summer Stars", "Devastating Maul of Inga Tarn", "Talon of the Gryphon",
    "Might of the Myrmidon", "Unbreakable Behemoth's Strength", "Chasuble of Majesty",
    "Swords in the Noonday Sun", "Empower", "Glorious Crown of Enchantment",
    "Sutannir's Promise", "Prowess of the Golden Magician", "Mantle of Lordly Might",
    "Champions Shining Resolve", "Raise the Standard of War", "Challenge the Iron Duke",
    "Glory to the Sovereign", "Delve Deep, Beneath the Mountain", "Strong Ox, Golden Sun",
    "Thundering Roar of the Lion-bound Horn", "Stout Resolve of the Unyielding",
    "Vigour of Youth", "The Sound of Drums", "Unbreakable Spirit, Unbreakable Blades",
    "Stalwart Stand on Solid Ground", "Frozen Citadel of Cathan Canae",
  )

  private val autumnRituals = rulesFromName(
    "Streams of Silver", "Rivers of Gold", "Chamber of Pallas", "Winds of Fortune",
    "The Lure of Distant Shores", "Gathering the Harvest", "Gift of the Wily Broker",
    "Ephisis' Scale", "Before the Throne of Estavus", "Twist of Moebius",
    "Like Water Through Your Fingers", "Balanced Scales of Lann", "The Conspirator's Cloak",
    "Mantle of the Golden Orator", "Create Bond", "Arcane Mark", "Mark of Fellowship",
    "Mark of Ownership", "Ties that Bind", "Scrivener's Bloodmark",
    "Shared Mastery of the Magician's Guild", "Barked Command of the Iron Serjant",
    "Circle of Gold", "Sum of the Parts", "Bound by Common Cause",
    "Stance of the Constricting Scourge", "Brazen Claws of the Lictor",
    "Inescapable Chains of Bitter Glass", "Entangle", "Hand of the Maker",
    "The Anvil of Estavus", "The Blade Bites Back", "Smooth Hands Shape the World",
    "Timeless Hammer Rhythm", "Shadow of the Bronze Colossus", "Call Winged Messenger",
    "Find the Best Path", "The Ambassadorial Gatekeeper",
  )

  private val winterRituals = rulesFromName(
    "Withering Touch of Frost", "Crumbling Flesh and Withering Limbs",
    "Hungry Grasp of Despair", "Naeve's Twisting Blight", "Inevitable Collapse into Ruin",
    "Howling Despite of the Yawning Maw", "The Grave's Treacherous Edge",
    "Black Iron Blade", "Words of Ending", "Ravenous Tongue of Entropy",
    "Devastating Scythe of Anguish and Loss", "Wind of Mundane Silence",
    "Wither the Seed", "Coil of the Black Leech",
    "Icy Maw Devours the Spark of Essence", "Gnawing, Endless Hunger",
    "Dreamscape of the Endless Hunt", "An Echo of Life Remains",
    "Wisdom of the Balanced Blade", "Ruthless Vigilance, Healthy Crop",
    "Whispers through the Black Gate", "Tribute to the Thrice-Cursed Court",
    "Pakaanan's Iron Shutters", "Hold Back Frozen Hunger",
    "There Is No Welcome Here", "Retreat to the White Caves", "Ward of the Black Waste",
    "Clarion Call of Ivory and Dust", "Pallid Flesh of the Dead King",
    "Sorin's Rite of Agony", "Traitor's Fate", "Freezing Brand of Irremais",
    "Winter's Ghosts", "Hunger of the Draughir", "Unyielding Constitution",
    "Fight Tooth and Nail", "Unending Onslaught", "Wolves of the Hungry Sea",
    "Last Breath Echoes", "Fallow Fields and Dried Meat",
    "Mark the Flesh Incorruptible", "Quickening Cold Meat",
  )

  private val dayRituals = rulesFromName(
    "Bright Lantern of Ophis", "Revelation of the Jewel's Sparkling Heart",
    "Skein of Years", "Clear Lens of the Eternal River", "Sular's Promise",
    "Eyes of the Sun and Moon", "The Eye of the High Places",
    "Gralka's Gift to the Lost Seeker", "Bright Eyes Gleam in the Depths",
    "Detect Magic", "Piercing Light of Revelation", "Swim Leviathan's Depth",
    "All the World in a Grain of Sand", "Revelatory Light of the Empyrean Spheres",
    "Solace of Chimes", "Crystal Clarity of the Rational Soul",
    "Illuminate the Higher Mind", "Ascendance of the Highest Mind",
    "Transcendent Mastery", "Peregrine and Ichimos",
    "Clarity of the Master Strategist", "Ascetic Star of Atun",
    "Distillation of Diverse Parts", "Horizon's Razor Edge", "Sign of Aesh",
    "Standing at the Threshold", "Crystalline Focus of Aesh",
    "Alignment of Mind and Blade", "Kimus' Glaring Eye", "Repel",
  )

  private val nightRituals = rulesFromName(
    "Cast Off The Chain of Memory", "Infant Starts with a Blank Slate",
    "Incantation's Mystic Mask", "Masque of the Blinded Weaver",
    "Secrets for the Shadow Courier", "Secrets of the Empty Heart",
    "Riddle Hides the Reward", "The Cuckoo's Egg", "Drawing the Penumbral Veil",
    "Shroud of Mist and Shadow", "Vale of Shadows", "Thief's Arcane Gambit",
    "The Eight-spoked Wheel", "The Retrograde Wheel", "The Twilight Masquerade",
    "Conclave of Trees and Shadow", "Align the Celestial Net",
    "Transmogrification of the Soul's Echo", "Dripping Echoes of the Fen",
    "Distil the Serpent's Stone", "Amalgamation of Silver and Gold",
    "Emulsion of Lye and Lightning", "Signs and Portents",
    "Verdant Bounty of the Twilight Bayou", "Whispering Shadow Courtiers",
    "Shadowed Glass of Sung", "Wondrous Forests of the Night",
    "Secrets of Skilful Artifice", "Silver Tongue of Virtue",
    "Rule of Risk and Reward", "Sift the Dreamscape's Sands", "Freedom of the Soul",
    "The Chamber of Delights", "Embrace the Living Flame", "Unfettered Anarchy",
    "Still Waters, Running Deep",
  )

  private val bourseResources = rulesFromName("Mithril", "Weirwood", "White Granite", "Ilium")

  private val herbs = rulesFromName(
    "True Vervain", "Cerulean Mazzarine", "Imperial Roseweald",
    "Marrowort", "Bladeroot", "Realmsroot",
  )

  private val metals = rulesFromName("Green Iron", "Orichalcum", "Tempest Jade", "Weltsilver")

  private val forestMaterials = rulesFromName("Ambergelt", "Beggar's Lye", "Dragonbone", "Iridescent Gloaming")

  private val materials = rulesFromName("Liao", "True Liao") ++ rulesFromPattern(
    ("Crystal Mana|Mana Crystals?", "Crystal Mana"),
  )

  private val titles = rulesFromName(
    "Senator", "General", "Cardinal", "Grandmaster", "Ambassador", "Gatekeeper", "Warmage",
  )

  private val locations = rulesFromName("Anvil") ++ rulesFromPattern(
    ("(The )?Sentinel Gate", "Sentinel Gate"),
  )

  private val varushkanSovereigns = rulesFromPattern(
    ("(The )?(Charnel Lord|Lord of the Crows|Battlefed|Moundshroud|King Beneath the Hill|Branocbound|Irontooth|Boyar of the Broken Barrow)", "Charnel Lord", Category.CharnelLord),
    ("(The )?(Night Below|Blackdamp|Waiting Dark|Grasping Dark|Thieving Dark|Fly-the-light|Shadow-in-shadows|Eyes in the Night|Dark-Below-The-Mountain|Extinguished Sun|Shadowcreeper|Watcher-in-the-Corner|Nights-haven|Black-heart|Abyssal Lady)", "Night Below", Category.NightBelow),
    ("Dho'uala|Spirit of Dark Water|Light-in-the-Depths|Netripper|Undertow|Queen of the Drowned|She-Who-Keeps-What-She-Catches|Lady of the Semmerlak", "Dho'uala", Category.`Dho'uala`),
  )

  private val volodny = rulesFromPattern(
    ("(The )?Volodny",       "Volodny", Category.Volodny),
    ("Bas Celik",            "Volodny", Category.Volodny),
    ("Koshiev the White",    "Volodny", Category.Volodny),
    ("Górować",              "Volodny", Category.Volodny),
    ("(The )?Shadowsmith",   "Volodny", Category.Volodny),
    ("Kareina of the Swans", "Volodny", Category.Volodny),
    ("Breknia",              "Volodny", Category.Volodny),
  )

  private val heralds = rulesFromName(
    // Heralds of Phaleron (Day)
    "Elioe"     -> Category.Elioe,
    "Lioc"      -> Category.Lioc,
    "Pollaman"  -> Category.Pollaman,
    "Arannia"   -> Category.Arannia,
    "Melchiore" -> Category.Melchiore,
    // Heralds of Prospero (Autumn)
    "Simone"    -> Category.Simone,
    "Dreyfus"   -> Category.Dreyfus,
    // Heralds of Callidus (Autumn)
    "Numis"     -> Category.Numis,
    "Malleas"   -> Category.Malleas,
    "Zand"      -> Category.Zand,
    // Heralds of Jaheris (Summer)
    "Mahine"    -> Category.Mahine,
    "Esmeray"   -> Category.Esmeray,
    // Heralds of Barien (Summer)
    "Revel"     -> Category.Revel,
    "Temper"    -> Category.Temper,
  )

  private val otherSpirits = rulesFromName(
    // Wintermark
    "Sydanjaa" -> Category.Sydanjaa,
  )

  private val magicItems = rulesFromName(
    // Daggers
    "Scorpion's Sting",
    // One-handed Weapons
    "Apprentice's Blade", "Biting Blade", "Triumphant Blade", "Duelist's Scales",
    "Vorpal Sword", "Thresher's Cudgel", "Thundering Mace", "Ironbound Axe",
    "Shieldbreaker", "Bravo's Blade", "Redwall's Wrath", "Stalwart's Sword",
    // Great Weapons
    "Butcher's Cleaver", "Reaving Mattock", "Dawn's Glory", "Giant's Maul",
    "Landsknecht's Zweihänder", "Splintering Hammer", "Trollslayer's Crescent",
    "Trollhammer", "Captain's Command", "Woodcutter's Axe",
    // One-handed Spears
    "Razorleaf Hasta", "Stumbleroot Spear", "Blackwillow Twist", "Ironbrand Thorn",
    "Bloodsteel Barb", "Oathsworn Spine", "Sydanjaa's Call", "Winternight Lifestealer",
    "Terunael Warlord", "Thorn's Mark",
    // Paired Weapons
    "Rake's Progress", "Brother Blades", "Earthquake Drummers", "Bear Claws",
    // Polearms
    "Farmer's Scythe", "Sanguine Spear", "Butcher's Bill", "Bolstering Bill",
    "Warden's Bardiche", "Blacksmith's Wage", "Barbed Spear", "Magistrate's Grasp",
    "Fell Iron Fury", "Bullroarer's Shout", "Labyrinth's Gate",
    // Bows
    "Martán's Mark", "Vowkeeper", "Wayfinder",
    // Arcane Weapons
    "Mountebank's Surprise", "Shears of Winter", "Warden's Fists", "Arms of the Warwitch",
    "Jade Hammers", "Binding Threads", "Children of Thunder", "Trodwalker's Readiness",
    "Thorns of the Rose",
    // Wands
    "Acolyte's Mercy", "Mazzarine Spindle", "Runesmith's Gavel", "Swanfeather Schema",
    "Witchwood Wand", "Altruist's Recompense", "Redsteel Chisel", "Ambergelt Baton",
    "Boneweaver", "Bloodwoven Braid", "Seer's Probe", "Scrivener's Seal",
    "Woundbinder", "Grimnir's Hearthfire", "Sanguine Thorn",
    // Rods
    "Forlorn Hope", "Storm Sceptre", "Tactician's Demand", "Yeoman's Bounty",
    "Whelpmaster's Fang", "Windreaping Sickle", "Bloodsilver Spike", "Quiet Word",
    "Tumultuous Gyre", "Neophyte's Aid", "Witches' Hammer", "Blood-dimming Tide",
    "Roaring Chimera Rod", "Stormweaver", "Radiant Torrent", "Sceptre of the Necropolis",
    // Staffs
    "Agramant's Bargain", "Lamia's Whisper", "Landskeeper's Oath", "Pugilist's Shillelagh",
    "Unseen Encasement", "Shadowbound Donjon", "Amberglass Chain", "Death's Door",
    "Enfeebling Echo", "Furrowed Wake", "Staff of Power", "Ethereal Manacle",
    "Staff of Command", "Staff of Life", "Suzerain's Command", "Caress of Arhallogen",
    "Staff of Imperial Mastery",
    // Ritual Staves
    "Choleric Staff", "Enigmatic Staff", "Luminous Staff", "Melancholic Staff",
    "Phlegmatic Staff", "Sanguine Staff", "Staff of the Magi",
    // Icons
    "Icon of the Judge", "Icon of the Justicar", "Icon of the Pilgrim", "Icon of the Seer",
    "Icon of the Smith", "Icon of the Steward", "Icon of the Witness", "Icon of the Devoted",
    "Icon of the Driven", "Icon of the Industrious", "Icon of the Insightful",
    "Icon of the Proud", "Icon of the Righteous", "Icon of the Watchful",
    // Musical Instruments
    "Celebrant's Fiddle", "Goldwood Pipes", "Heroes' Horn", "Horn Resounding",
    "Silver Gate", "Thundering Drum", "Watchers' Flute",
    // Light Armour
    "Stoutheart Gambeson", "Soldier's Harness", "Jack of Irons", "Winter's Breath",
    "Runemark Shirt", "Blood-sweat Hauberk", "Ivory Aketon", "Spiritskin Coat",
    "Mediators Purse",
    // Medium Armour
    "Marauder's Cote", "Bloodamber Hauberk", "Boarskin Vest", "Cerulean Protector",
    "Scrivener's Guard", "Runebound Ward", "Wardensweave", "Tombshroud Guardian",
    "Baersark's Rage",
    // Heavy Armour
    "Warrior's Plate", "Enduring Breastplate", "Goldentide Mail", "Knightly Redoubt",
    "Defiant Steel", "Winterborn Warmail", "Runeplate", "Goldenfire Scale",
    "Gryphonsoul Aegis", "Hero's Hauberk",
    // Mage Robes
    "Crystaltender's Vestment", "Volhov's Robe", "Shimmergold Coat", "Opaline Coat",
    "Trollsweave Vest", "Robe of Blood and Bone", "Glamourweave Robe", "Ashen Mantle",
    "Bishop's Ensemble", "Captain's Garb", "Doctor's Attire", "Mountebank's Guise",
    "Prince's Raiment", "Semblance of the Witch", "Moonsilver Doublet",
    // Mage Armour
    "Hero's Girdle", "Splintering Gorget", "Wyvernsting Spaulders", "Warmage's Belt",
    "Healer's Harness", "Sunfire Pectoral", "Bloodfeather Harness",
    "Vambraces of Regeneration", "Twilight Pauldrons", "Gravedigger's Vest",
    "Forge of Isenbrad", "Battlesmith's Panoply", "Mountainfall Bracers",
    // Vestments
    "Mendicant Cassock", "Wayfarer's Robe", "Alabaster Cerement", "Keeper's Habit",
    "Troubadour's Tunic", "Labyrinthine Vestments",
    // Shields
    "Pilgrim's Shield", "Oakheart Shield", "Stormguard Bulwark", "Warcaster's Oath",
    "Burnished Rampart", "Champion's Bastion", "Runesmith's Glare", "Virtuous Ward",
    // Jewellery
    "Circlet of Falling Snow", "Dragonbone Symbol", "Bondring",
    "Shackle of the Unvirtuous", "Alder's Edge", "Greensteel Bracelets",
    "Chrysalis Pendant", "Troubadour's Ring", "Pauper's Key", "Bloodfire Periapt",
    "Abraxus Stone", "Lowther's Gaze", "Circlet of Command",
    // Ritual Masks
    "Straw Mask", "Corsair's Bloody Mask", "Mask of Gold and Lead",
    "Mercantilist's Mask", "Captain's Mask", "Mask of the Mountebank", "Strigine Mask",
    // Foci
    "Cowl of Ashes", "Ring of Adversity", "Ring of Triumph", "Atun's Ring",
    "Ring of the Rainbow Sage", "The Crimson Feast", "Four Spirit's Mask",
    "Icewalker's Anvil", "Kingfisher's Promise",
    // Tools
    "Escharotic Cauldron", "Wyrmstone Mortar", "Phial of the Sun", "Lodestone Shears",
    "Bloodcloak", "Isenbrad's Legacy", "Limbsetter's Point",
    // Regalia
    "Guided Path", "Beneficent Sigil", "Sungold Basin", "Blessing Wheel",
    "Brazier of Benediction", "Pilgrim's Cup", "Fireglass", "Cowl of Judgement",
    "Silent Bell", "Skein Bowl",
    // Magic Standards
    "Banner of the Bold", "Chirurgeon's Ensign", "Thaneshall Banner", "Sunfire Pennant",
    "Titan's Battlemark", "Thorn's Dance", "Celestial Sigil", "Loyal Stanchion",
    "Warsmith's Shingle", "Three Tears",
    // Gonfalon
    "Mercenary Banner",
    // Paraphernalia
    "Amberglass Orb", "Burnished Orb", "Dragonbone Orb", "Greensteel Orb",
    "Radiant Orb", "Twilight Orb", "Vitriolic Orb", "Cartographer's Eye",
    "Orator's Chalice", "The Barren Land", "The Empty Horizon", "The Fields of Glory",
    "The Flickering Flame", "The Green World", "The Iron Labyrinth",
    "Web of Celestial Attunement", "The Eternal Gambit", "The Fountain of Thorns",
    "The Key and the Gate", "The Mountainous Oak", "The Spider's Web",
    "The Drowned Threshold", "The Vagabond Wyrm", "The Syphon of Stars",
    // Reliquaries
    "Litany of the Labyrinth", "Wayfarer's Pyx", "Bishop's Chalice", "Almery of Silence",
  )
  // format: on

  val rules: List[Rule] = List(
    eternals,
    nations,
    foreignNations,
    barbarianNations,
    regions,
    sovereignty,
    titles,
    locations,
    springRituals,
    summerRituals,
    autumnRituals,
    winterRituals,
    dayRituals,
    nightRituals,
    bourseResources,
    metals,
    forestMaterials,
    herbs,
    materials,
    varushkanSovereigns,
    volodny,
    heralds,
    otherSpirits,
    magicItems,
  ).flatten.sortBy(_.page.length)(using Ordering[Int].reverse)

  private val linkPattern: Regex = """\[[^\]]*\]\(<[^>]*>\)""".r

  def enrich(
    text: String,
    pageUri: String => Uri,
    rules: List[Rule] = rules,
  ): (String, List[Category & Text]) =
    val initialLinks            = linkPattern.findAllMatchIn(text).map(m => (m.start, m.end)).toList
    val (result, _, categories) = rules
      .foldLeft((text, initialLinks, List.empty[Category & Text])) {
        case ((current, links, cats), Rule(pattern, page, category)) =>
          pattern
            .findAllMatchIn(current)
            .find(m => !links.exists((s, e) => m.start >= s && m.end <= e))
            .fold((current, links, cats)) { m =>
              val uri          = pageUri(page).renderString
              val replacement  = s"[${m.matched}](<$uri>)"
              val delta        = replacement.length - (m.end - m.start)
              val shiftedLinks = links.map {
                case (s, e) if s >= m.end => (s + delta, e + delta)
                case link                 => link
              }
              (
                current.patch(m.start, replacement, m.end - m.start),
                (m.start, m.start + replacement.length) :: shiftedLinks,
                cats ++ category.toList,
              )
            }
      }
    (result, categories)
