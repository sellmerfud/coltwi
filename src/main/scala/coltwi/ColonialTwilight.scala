
//   _____      _             _       _   _______       _ _ _       _     _
//  / ____|    | |           (_)     | | |__   __|     (_) (_)     | |   | |
// | |     ___ | | ___  _ __  _  __ _| |    | |_      ___| |_  __ _| |__ | |_
// | |    / _ \| |/ _ \| '_ \| |/ _` | |    | \ \ /\ / / | | |/ _` | '_ \| __|
// | |___| (_) | | (_) | | | | | (_| | |    | |\ V  V /| | | | (_| | | | | |_
//  \_____\___/|_|\___/|_| |_|_|\__,_|_|    |_| \_/\_/ |_|_|_|\__, |_| |_|\__|
//                                                             __/ |
//                                                            |___/
// A scala implementation of the solo AI for the game 
// Colonial Twilight, designed by Brian Train and
// Published by GMT Games
// 
// Copyright (c) 2017 Curt Sellmer
//
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

package coltwi

import java.io.IOException
import scala.util.Random.{shuffle, nextInt}
import scala.annotation.tailrec
import scala.util.Properties.{lineSeparator, isWin}
import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine
import scala.language.implicitConversions
import scenarios._
import FUtil.Pathname

object ColonialTwilight {
  
  val SOFTWARE_VERSION = "2.7"
  val INTEGER = """(\d+)""".r
  
  def dieRoll = if (true) nextInt(6) + 1
                else (askOneOf("Enter die roll: ", 1 to 6, allowAbort = false) map (_.toInt)).get
  
  sealed trait Role {
    val name: String
    val sortOrder: Int
    override def toString() = name
  }
    
  case object Gov extends Role {
    val name = "Government"
    val sortOrder = 1
  }
  case object Fln  extends Role {
    val name = "FLN"
    val sortOrder = 2
  }
  
  object Role {
    def apply(name: String): Role = name.toLowerCase match {
      case x if x == Gov.name.toLowerCase => Gov
      case x if x == Fln.name.toLowerCase => Fln
      case _ => throw new IllegalArgumentException(s"Invalid role name: $name")
    }
    implicit val RoleOrdering = Ordering.by { role: Role => role.sortOrder }
    lazy val all = List(Gov, Fln)
    lazy val names = all map (_.name)
    lazy val maxNameLen = (names map (_.length)).max
    def opposite(role: Role) = if (role == Gov) Fln else Gov
    
  }
  

  sealed trait FlnOp
  object Rally  extends FlnOp { override def toString() = "Rally"}
  object March  extends FlnOp { override def toString() = "March"}
  object Attack extends FlnOp { override def toString() = "Attack"}
  object Terror extends FlnOp { override def toString() = "Terror"}
  
  sealed trait FlnSpecial
  object Extort  extends FlnSpecial { override def toString() = "Extort"}
  object Subvert extends FlnSpecial { override def toString() = "Subvert"}
  object Ambush  extends FlnSpecial { override def toString() = "Ambush"}
  
  sealed trait EventSelection
  case object NoEvent  extends EventSelection
  case object Unshaded extends EventSelection
  case object Shaded   extends EventSelection
  
  type BotEventSelector = () => EventSelection  // Used to see if Bot will execute the event.
  type CardEvent        = Role => Unit
  
  // For cards that have only one event:
  //  dual == false and the event condtions and execution use the `unshaded` fields
  class Card(
    val number: Int,
    val name: String,
    val dual: Boolean,
    val markedForFLN: Boolean,  // Play the event if it is effective (no die < 5) necessary
    val flnAlwaysPlay: Boolean, // Capabilities and some Fln marked events are always played even if not immediately effective
    val botEventSelection: BotEventSelector,
    val executeUnshaded: CardEvent,
    val executeShaded: CardEvent) {

    def numAndName = s"#$number $name"
    override def toString() = numAndName
  }

  // Sort by card number
  implicit val CardOrdering = new Ordering[Card] {
    def compare(x: Card, y: Card) = x.number compare y.number
  }
  
  val PivotalMoroccoTunisiaIndepdent = 61
  val PivotalSuezCrisis              = 62
  val PivotalOAS                     = 63
  val PivotalMobilization            = 64
  val PivotalRecallDeGaulle          = 65
  val PivotalCoupdEtat               = 66
  
  object deck {
    val deckMap = Cards.deckMap
    val PropCards       = Range.inclusive(67, 71).toSet
    val GovPivotalCards = Set(PivotalMobilization, PivotalRecallDeGaulle, PivotalCoupdEtat)
    val FlnPivotalCards = Set(PivotalMoroccoTunisiaIndepdent, PivotalSuezCrisis, PivotalOAS)
    def isValidCardNumber(num: Int) = deckMap contains num
    def isPropagandaCard(num: Int)  = PropCards contains num
    def isGovPivotalCard(num: Int)  = GovPivotalCards contains num
    def isFlnPivotalCard(num: Int)  = FlnPivotalCards contains num
    def isPivotalCard(num: Int)     = isGovPivotalCard(num) || isFlnPivotalCard(num)
    def apply(num: Int): Card       = deckMap(num)
    def cards: List[Card]           = deckMap.valuesIterator.toList.sorted
  }
    
  // Track names
  val FranceTrackName     = "France track"
  val BorderZoneTrackName = "Border zone track"
  
  // Cities, Sectors, Countries
  val Barika           = "Barika"             // Sector I-1, pop 1, mountains
  val Batna            = "Batna"              // Sector I-2, pop 0, mountains
  val Biskra           = "Biskra"             // Sector I-3, pop 0, plains
  val OumElBouaghi     = "Oum El Bouaghi"     // Sector I-4, pop 0, mountains
  val Tebessa          = "Tebessa"            // Sector I-5, pop 1, mountains
  val Negrine          = "Negrine"            // Sector I-6, pop 0, mountains
  
  val Constantine      = "Constantine"        // City in Wilaya II, pop 2
  val Setif            = "Setif"              // Sector II-1, pop 1, mountains
  val Philippeville    = "Philippeville"      // Sector II-2, pop 2, mountains
  val SoukAhras        = "Souk Ahras"         // Sector II-3, pop 2, plains
  
  val TiziOuzou        = "Tizi Ouzou"         // Sector III-1, pop 2, mountains
  val BordjBouArreridj = "Bordj Bou Arreridj" // Sector III-2, pop 1, mountains
  val Bougie           = "Bougie"             // Sector III-3, pop 2, mountains
  
  val Algiers          = "Algiers"            // City in Wilaya IV, pop 3
  val Medea            = "Medea"              // Sector IV-1, pop 2, mountains
  val OrleansVille     = "OrleansVille"       // Sector IV-2, pop 2, mountains
  
  val Oran             = "Oran"               // City in Wilaya V, pop 2
  val Mecheria         = "Mecheria"           // Sector V-1, pop 0, mountains
  val Tlemcen          = "Tlemcen"            // Sector V-2, pop 1, plains
  val SidiBelAbbes     = "Sidi Bel Abbes"     // Sector V-3, pop 1, plains
  val Mostaganem       = "Mostaganem"         // Sector V-4, pop 2, mountains
  val Saida            = "Saida"              // Sector V-5, pop 0, mountains
  val Mascara          = "Mascara"            // Sector V-6, pop 0, mountains
  val Tiaret           = "Tiaret"             // Sector V-7, pop 0, mountains
  val AinSefra         = "Ain Sefra"          // Sector V-8, pop 0, plains
  val Laghouat         = "Laghouat"           // Sector V-9, pop 0, plains
  
  val SidiAissa        = "Sidi Aissa"         // Sector VI-1, pop 0, mountains
  val AinOussera       = "Ain Oussera"        // Sector VI-2, pop 1, mountains
   
  val Morocco          = "Morocco"            // Country, pop 1, plains
  val Tunisia          = "Tunisia"            // Country, pop 1, plains
  
  
  // A space name and a list of adjacent space names
  val adjacencyMap: Map[String, Set[String]] = Map(
    Barika           -> Set(Biskra, SidiAissa, BordjBouArreridj, Setif, Philippeville, OumElBouaghi, Batna),
    Batna            -> Set(Biskra, Barika, OumElBouaghi, Negrine),
    Biskra           -> Set(Laghouat, SidiAissa, Barika, Batna, Negrine, Tunisia),
    OumElBouaghi     -> Set(Batna, Barika, Philippeville, SoukAhras, Tebessa, Negrine),
    Tebessa          -> Set(Negrine, OumElBouaghi, SoukAhras, Tunisia),
    Negrine          -> Set(Biskra, Batna, OumElBouaghi, Tebessa, Tunisia),
    Constantine      -> Set(Setif, Philippeville),
    Setif            -> Set(Constantine, Philippeville, Barika, BordjBouArreridj, Bougie),
    Philippeville    -> Set(Constantine, SoukAhras, OumElBouaghi, Barika, Setif),
    SoukAhras        -> Set(Philippeville, Tunisia, Tebessa, OumElBouaghi),
    TiziOuzou        -> Set(Bougie, BordjBouArreridj, Medea),
    BordjBouArreridj -> Set(Bougie, Setif, Barika, SidiAissa, Medea, TiziOuzou),
    Bougie           -> Set(TiziOuzou, Setif, BordjBouArreridj),
    Algiers          -> Set(Medea),
    Medea            -> Set(Algiers, TiziOuzou, BordjBouArreridj, SidiAissa, AinOussera, OrleansVille),
    OrleansVille     -> Set(Medea, AinOussera, Tiaret, Mostaganem),
    Oran             -> Set(SidiBelAbbes),
    Mecheria         -> Set(Morocco, Tlemcen, Saida, AinSefra),
    Tlemcen          -> Set(Morocco, SidiBelAbbes, Saida, Mecheria),
    SidiBelAbbes     -> Set(Oran, Tlemcen, Mostaganem, Mascara, Saida),
    Mostaganem       -> Set(SidiBelAbbes, OrleansVille, Tiaret, Mascara),
    Saida            -> Set(Mecheria, Tlemcen, SidiBelAbbes, Mascara, AinSefra),
    Mascara          -> Set(Saida, SidiBelAbbes, Mostaganem, Tiaret, AinSefra),
    Tiaret           -> Set(Mascara, Mostaganem, OrleansVille, AinOussera, AinSefra),
    AinSefra         -> Set(Morocco, Mecheria, Saida, Mascara, Tiaret, AinOussera, Laghouat),
    Laghouat         -> Set(AinSefra, AinOussera, SidiAissa, Biskra),
    SidiAissa        -> Set(Biskra, Laghouat, AinOussera, Medea, BordjBouArreridj, Barika),
    AinOussera       -> Set(Laghouat, AinSefra, Tiaret, OrleansVille, Medea, SidiAissa),
    Morocco          -> Set(Tlemcen, Mecheria, AinSefra),
    Tunisia          -> Set(SoukAhras, Tebessa, Negrine, Biskra))
  
  def getAdjacent(name: String): Set[String] = adjacencyMap(name)
  def areAdjacent(name1: String, name2: String) = getAdjacent(name1) contains name2
  
  
  // Markers to place in spaces
  val Plus1PopMarker  = "+1 Population"
  val Plus1BaseMarker = "+1 Base"
  val ResettledMarker = "Resettled"
  val TerrorMarker    = "Terror"
  
  // Capability Markers
  val CapRevenge         = "FLN:Overkill - Revenge"                       // Place 1 Guerrilla after assault
  val CapOverkill        = "Gov:Overkill - Let God sort 'em out"          // Neutralize removes 4 pieces
  val CapScorch          = "FLN:Napalm - Scorch the countryside"          // Assault costs 3 resources/space
  val CapNapalm          = "Gov:Napalm - Effective"                       // Assault kill 1:1 in mountain spaces
  val CapEffectiveBomber = "FLN:Taleb Bomber - Effective"                 // City Terror costs zero resources
  val CapAmateurBomber   = "Gov:Taleb Bomber - Amateur"                   // City Terror msut activate 2 guerrillas
  val CapXWilayaCoord    = "FLN:Covert Movement - Cross Wilaya Coordination"  // Redeploy to any base (regardless of Wilaya)
  val CapDeadZones       = "Gov:Covert Movement - Dead Zones"             // March is 1 space only
  val CapFlnSaS          = "FLN:SAS - Caution"                            // Assault is 1 space only
  val CapGovSaS          = "Gov:SAS - Hearts & Minds"                     // Train may pacify in 1 or 2 spaces
  val CapFlnCommandos    = "FLN:Commandos - Zonal Commandos"              // Ambush does not activate a guerrilla
  val CapGovCommandos    = "Gov:Commandos - Commandos de Chasse"          // Each Algerian cube in mountain Sweep/Garrison activates 1 guerrilla
  val CapTorture         = "Dual: Torture"

  val AllCapabilities = List(
    CapRevenge, CapOverkill, CapScorch, CapNapalm, CapEffectiveBomber, CapAmateurBomber, CapXWilayaCoord,
    CapDeadZones, CapFlnSaS, CapGovSaS, CapFlnCommandos, CapGovCommandos, CapTorture)
    
  
  // Momentum markers  
  val MoBalkyConscripts      = "FLN: Balky Conscripts"
  val MoPeaceOfTheBrave      = "Gov: Peace Of The Brave"
  val MoCrossBorderAirStrike = "FLN: Cross-border air strike"
  val MoMoudjahidine         = "FLN: Moudjahidine"
  val MoBananes              = "Gov: Bananes"
  val MoVentilos             = "Gov: Ventilos"
  val MoTheCallUp            = "FLN: The Call Up"
  val MoIntimidation         = "Gov: Intimidation"
  val MoStrategicMovement    = "FLN: Strategic Movement"
  val MoParanoia             = "Gov: Paranoia"
  val MoChallePlanGov        = "Gov: Challe Plan"
  val MoChallePlanFln        = "FLN: Challe Plan"
  val MoMoghazni             = "Gov: Moghazni"
  val MoPopulationControl    = "Gov: Population Control"
  val MoHardenedAttitudes    = "Dual: Hardened Attitudes"
  val MoPeaceTalks           = "Dual: Peace Talks"

  val AllMomentum = List(
    MoBalkyConscripts, MoPeaceOfTheBrave, MoCrossBorderAirStrike, MoMoudjahidine, MoBananes,
    MoVentilos, MoTheCallUp, MoIntimidation, MoStrategicMovement, MoParanoia, MoChallePlanGov,
    MoChallePlanFln, MoMoghazni, MoPopulationControl, MoHardenedAttitudes, MoPeaceTalks)
  
  sealed trait PieceType {
    val singular: String
    val plural: String
    override def toString() = plural
  }
  case object FrenchTroops     extends PieceType { val singular = "French troop";          val plural = "French troops"}
  case object FrenchPolice     extends PieceType { val singular = "French police";         val plural = "French police"}
  case object AlgerianTroops   extends PieceType { val singular = "Algerian troop";        val plural = "Algerian troops"}
  case object AlgerianPolice   extends PieceType { val singular = "Algerian police";       val plural = "Algerian police"}
  case object HiddenGuerrillas extends PieceType { val singular = "Underground guerrilla"; val plural = "Underground guerrillas"}
  case object ActiveGuerrillas extends PieceType { val singular = "Active guerrilla";      val plural = "Active guerrillas"}
  case object GovBases         extends PieceType { val singular = "Government Base";       val plural = "Government Bases"}
  case object FlnBases         extends PieceType { val singular = "FLN Base";              val plural = "FLN Bases"}
  val AllPieceTypes = List(FrenchTroops, FrenchPolice, AlgerianTroops, AlgerianPolice,
                      HiddenGuerrillas, ActiveGuerrillas, GovBases, FlnBases)
  
  val FLNPieces      = Set[PieceType](HiddenGuerrillas, ActiveGuerrillas, FlnBases)
  
  val TROOPS         = List(FrenchTroops, AlgerianTroops)
  val POLICE         = List(FrenchPolice, AlgerianPolice)
  val FRENCH_CUBES   = List(FrenchPolice, FrenchTroops)
  val ALGERIAN_CUBES = List(AlgerianPolice, AlgerianTroops)
  val CUBES          = FRENCH_CUBES ::: ALGERIAN_CUBES
  val GUERRILLAS     = List(HiddenGuerrillas, ActiveGuerrillas)
  val FRENCH_PIECES  = List(FrenchPolice, FrenchTroops, GovBases)
  val GOV_PIECES     = FRENCH_PIECES ::: ALGERIAN_CUBES
  
  
  def owner(t: PieceType) = if (FLNPieces contains t) Fln else Gov
    
  // Class used to keep track of the pieces in a particular space
  case class Pieces(
    frenchTroops:Int = 0,
    frenchPolice: Int = 0,
    algerianTroops: Int = 0,
    algerianPolice: Int = 0,
    hiddenGuerrillas: Int = 0,
    activeGuerrillas: Int = 0,
    govBases: Int = 0,
    flnBases: Int = 0) {
    
    val totalBases      = govBases         + flnBases
    val totalGuerrillas = hiddenGuerrillas + activeGuerrillas
    val totalTroops     = frenchTroops     + algerianTroops
    val totalPolice     = frenchPolice     + algerianPolice
    val totalCubes      = totalTroops      + totalPolice
    val totalGov        = totalCubes       + govBases
    val totalFln        = totalGuerrillas  + flnBases
    val total           = totalGov         + totalFln
    
    val algerianCubes = algerianTroops + algerianPolice
    val frenchCubes   = frenchTroops   + frenchPolice
    
    def stringItems = AllPieceTypes filter (numOf(_) > 0) map (t => amtPiece(numOf(t), t))
    
    override def toString() = if (total == 0) "none" else stringItems.mkString(", ")
    
    def numOf(pieceType: PieceType): Int = pieceType match {
      case FrenchTroops     => frenchTroops
      case FrenchPolice     => frenchPolice
      case AlgerianTroops   => algerianTroops
      case AlgerianPolice   => algerianPolice
      case HiddenGuerrillas => hiddenGuerrillas
      case ActiveGuerrillas => activeGuerrillas
      case GovBases         => govBases
      case FlnBases         => flnBases
    }
    
    def totalOf(pieceTypes: Seq[PieceType]) = pieceTypes.foldLeft(0) { (num, piece) => num + numOf(piece) }
    
    // Return true of this Pieces instance contains at least all of the other pieces.
    def contains(other: Pieces): Boolean = AllPieceTypes forall (t => numOf(t) >= other.numOf(t))
    
    def set(num: Int, pieceType: PieceType): Pieces = pieceType match {
      case FrenchTroops     => copy(frenchTroops     = num)
      case FrenchPolice     => copy(frenchPolice     = num)
      case AlgerianTroops   => copy(algerianTroops   = num)
      case AlgerianPolice   => copy(algerianPolice   = num)
      case HiddenGuerrillas => copy(hiddenGuerrillas = num)
      case ActiveGuerrillas => copy(activeGuerrillas = num)
      case GovBases         => copy(govBases         = num)
      case FlnBases         => copy(flnBases         = num)
    }
    
    def add(num: Int, pieceType: PieceType): Pieces = pieceType match {
      case FrenchTroops     => copy(frenchTroops     = frenchTroops + num)
      case FrenchPolice     => copy(frenchPolice     = frenchPolice + num)
      case AlgerianTroops   => copy(algerianTroops   = algerianTroops + num)
      case AlgerianPolice   => copy(algerianPolice   = algerianPolice + num)
      case HiddenGuerrillas => copy(hiddenGuerrillas = hiddenGuerrillas + num)
      case ActiveGuerrillas => copy(activeGuerrillas = activeGuerrillas + num)
      case GovBases         => copy(govBases         = govBases + num)
      case FlnBases         => copy(flnBases         = flnBases + num)
    }
    
    def remove(num: Int, pieceType: PieceType): Pieces = pieceType match {
      case FrenchTroops     => copy(frenchTroops     = (frenchTroops - num) max 0)
      case FrenchPolice     => copy(frenchPolice     = (frenchPolice - num) max 0)
      case AlgerianTroops   => copy(algerianTroops   = (algerianTroops - num) max 0)
      case AlgerianPolice   => copy(algerianPolice   = (algerianPolice - num) max 0)
      case HiddenGuerrillas => copy(hiddenGuerrillas = (hiddenGuerrillas - num) max 0)
      case ActiveGuerrillas => copy(activeGuerrillas = (activeGuerrillas - num) max 0)
      case GovBases         => copy(govBases         = (govBases - num) max 0)
      case FlnBases         => copy(flnBases         = (flnBases - num) max 0)
    }
    
    def only(pieceTypes: Seq[PieceType]): Pieces = 
      pieceTypes.foldLeft(Pieces()) { (pieces, t) => pieces.add(numOf(t), t) }
      
    def only(pieceType: PieceType): Pieces = only(Seq(pieceType))
    
    def + (added: Pieces): Pieces = Pieces(
      frenchTroops     = frenchTroops     + added.frenchTroops,
      frenchPolice     = frenchPolice     + added.frenchPolice,
      algerianTroops   = algerianTroops   + added.algerianTroops,
      algerianPolice   = algerianPolice   + added.algerianPolice,
      hiddenGuerrillas = hiddenGuerrillas + added.hiddenGuerrillas,
      activeGuerrillas = activeGuerrillas + added.activeGuerrillas,
      govBases         = govBases         + added.govBases,
      flnBases         = flnBases         + added.flnBases)
      
    def - (removed: Pieces): Pieces = Pieces(
      frenchTroops     = (frenchTroops     - removed.frenchTroops) max 0,
      frenchPolice     = (frenchPolice     - removed.frenchPolice) max 0,
      algerianTroops   = (algerianTroops   - removed.algerianTroops) max 0,
      algerianPolice   = (algerianPolice   - removed.algerianPolice) max 0,
      hiddenGuerrillas = (hiddenGuerrillas - removed.hiddenGuerrillas) max 0,
      activeGuerrillas = (activeGuerrillas - removed.activeGuerrillas) max 0,
      govBases         = (govBases         - removed.govBases) max 0,
      flnBases         = (flnBases         - removed.flnBases) max 0)
    
    def activateGuerrillas(num: Int): Pieces = {
      assert(hiddenGuerrillas >= num, "Not enough undergound guerrillas")
      this - Pieces(hiddenGuerrillas = num) + Pieces(activeGuerrillas = num)
    }
    
    def hideGuerrillas(num: Int): Pieces = {
      assert(activeGuerrillas >= num, "Not enough active guerrillas")
      this - Pieces(activeGuerrillas = num) + Pieces(hiddenGuerrillas = num)
    }
    
    // Convert to a list where each piece is wrapped in its own Pieces object.
    def explode(order: List[PieceType] = AllPieceTypes): List[Pieces] = {
      order flatMap { pieceType => List.fill(numOf(pieceType))(Pieces().set(1, pieceType)) }
    }
  }
  
  object Pieces {
    def combined(collection: Seq[Pieces]) =
      collection.foldLeft(Pieces()) { (combined, pieces) => combined + pieces }
  }
  
  // Convenience wo that we can use for example
  //    sp.hiddenGuerrillas in place of sp.pieces.hiddenGuerrillas
  implicit def spacePieces(sp: Space): Pieces = sp.pieces

  // Types of map spaces
  sealed trait SpaceType
  case object City            extends SpaceType
  case object Sector          extends SpaceType
  case object Country         extends SpaceType
  
  object SpaceType {
    val AllTypes = List(City, Sector, Country)
    def apply(name: String): SpaceType = AllTypes find (t => t.toString.toLowerCase == name.toLowerCase) getOrElse {
      throw new IllegalArgumentException(s"Invalid SpaceType name: $name")
    }
  }
  
  sealed trait Control
  case object Uncontrolled  extends Control { override def toString() = "Uncontrolled"}
  case object GovControl    extends Control { override def toString() = "Government control"}
  case object FlnControl    extends Control { override def toString() = "FLN control"}
  
  sealed trait SupportValue
  case object Neutral    extends SupportValue
  case object Support    extends SupportValue
  case object Oppose     extends SupportValue
  
  object SupportValue {
    val AllSupport = List(Neutral, Support, Oppose)
    def apply(name: String): SupportValue = AllSupport find (t => t.toString.toLowerCase == name.toLowerCase) getOrElse {
      throw new IllegalArgumentException(s"Invalid SupportValue name: $name")
    }
  }
  
  sealed trait Terrain
  case object Mountains extends Terrain
  case object Plains    extends Terrain
  case object Urban     extends Terrain
  
  object Terrain {
    val AllTerrains = List(Mountains, Plains, Urban)
    def apply(name: String): Terrain = AllTerrains find (t => t.toString.toLowerCase == name.toLowerCase) getOrElse {
      throw new IllegalArgumentException(s"Invalid Terrain name: $name")
    }
  }
  
  // In Wilaya order, Cities first within the Wilaya.
  // Morocco and Tunisia last
  implicit val SpaceOrdering = new Ordering[Space] {
    private val SECTOR = """([^-]+)-(\d+)""".r
    
    def roman(x: String) = x match {
      case "VI" => 6
      case "V"  => 5
      case "IV" => 4
      case x    => x.length
    }
    
    def weight(s: Space) = s.name match {
      case Morocco => 100
      case Tunisia => 200
      case _ =>
        s.zone match {
          case SECTOR(z, n) => roman(z) * 10 + n.toInt
          case z            => roman(z) * 10 // city
        }
    }
    
    def compare(x: Space, y: Space) = weight(x) compare weight(y)
  }
  
  
  case class Space(
    name:           String,
    spaceType:      SpaceType,   // City, Sector, Country
    zone:           String,      // City: "V", Sector: "V-1", Country: ""
    terrain:        Terrain,
    basePop:        Int,
    coastal:        Boolean      = false,
    support:        SupportValue = Neutral,
    pieces:         Pieces       = Pieces(),
    markers:        List[String] = Nil) {
    
    val nameAndZone = {
      val z = if (spaceType == Sector) s" $zone" else ""
      s"$name$z"
    }
    
    override def toString() = nameAndZone      
    
    private val ZONE = """([^-]+)-.*""".r
    
    def wilaya = zone match {
      case ""      => ""  // Country
      case ZONE(w) => w   // Sector
      case x       => x   // City
    }
    
    def isCity    = spaceType == City
    def isSector  = spaceType == Sector
    def isCountry = spaceType == Country
    
    def isMountains = terrain == Mountains
    def isPlains    = terrain == Plains
    def isUrban     = terrain == Urban
    
    def hasMarker(name: String) = markers contains name
    // Number of terror markers
    def terror = markers count (_ == TerrorMarker)
    def hasTerror = terror > 0
    def isResettled = hasMarker(ResettledMarker)
    
    def addMarker(m: String, num: Int = 1): Space = copy(markers = m :: markers)
    def removeMarker(m: String, num: Int = 1): Space = {
      val (matched, other) = markers.partition(_ == m)
      copy(markers = matched.drop(num) ::: other)
    }
    
    // As per the Errata, border sectors do not come into play until
    // Morocco and Tunisia become independent.
    def isBorderSector = game.moroccoTunisiaIndependent &&
                         (areAdjacent(name, Morocco) || areAdjacent(name, Tunisia))
    
    def population = if (isResettled) 0
                     else if (hasMarker(Plus1PopMarker)) basePop + 1
                     else basePop
    
    def maxBases = if (hasMarker(Plus1BaseMarker)) 3 else 2
    def canTakeBase = pieces.totalBases < maxBases
    def hasGovBase = pieces.govBases > 0
    def hasFlnBase = pieces.flnBases > 0

    def control = (pieces.totalGov - pieces.totalFln) match {
      case 0          => Uncontrolled
      case x if x > 0 => GovControl
      case _          => FlnControl
    }
    
    def isUncontrolled  = control == Uncontrolled
    def isGovControlled = control == GovControl
    def isFlnControlled = control == FlnControl
    
    def isNeutral = support == Neutral
    def isSupport = support == Support
    def isOppose  = support == Oppose
    
    def supportValue = if (isSupport) population else 0
    def opposeValue  = if (isOppose)  population else 0
      
    def sweepHasEffect = pieces.hiddenGuerrillas > 0 &&
                         ((isMountains && pieces.totalCubes > 1) || (pieces.totalCubes > 0))
                       
    def prohibitedTrainSector = momentumInPlay(MoHardenedAttitudes) && isSector && !isSupport  
    
    def normalTrain         = isCity || hasGovBase
    def recallDeGaulleTrain = game.recallDeGaulleInEffect && isGovControlled && pieces.totalTroops > 0 && pieces.totalPolice > 0
    def moghazniTrain       = momentumInPlay(MoMoghazni) && isSector && isSupport && isGovControlled
    
    def canTrain          = (normalTrain || recallDeGaulleTrain || moghazniTrain) && !prohibitedTrainSector
    def moghazniTrainOnly = moghazniTrain && !(normalTrain || recallDeGaulleTrain)
  }
  
  // Default empty spaces
  val DefaultBarika           = Space(Barika, Sector, "I-1", Mountains, 1)
  val DefaultBatna            = Space(Batna, Sector, "I-2", Mountains, 0)
  val DefaultBiskra           = Space(Biskra, Sector, "I-3", Plains, 0)
  val DefaultOumElBouaghi     = Space(OumElBouaghi, Sector, "I-4", Mountains, 0)
  val DefaultTebessa          = Space(Tebessa, Sector, "I-5", Mountains, 1)
  val DefaultNegrine          = Space(Negrine, Sector, "I-6", Mountains, 0)
  val DefaultConstantine      = Space(Constantine, City, "II", Urban, 2)
  val DefaultSetif            = Space(Setif, Sector, "II-1", Mountains, 1, coastal = true)
  val DefaultPhilippeville    = Space(Philippeville, Sector, "II-2", Mountains, 2, coastal = true)
  val DefaultSoukAhras        = Space(SoukAhras, Sector, "II-3", Plains, 2, coastal = true)
  val DefaultTiziOuzou        = Space(TiziOuzou, Sector, "III-1", Mountains, 2, coastal = true)
  val DefaultBordjBouArreridj = Space(BordjBouArreridj, Sector, "III-2", Mountains, 1)
  val DefaultBougie           = Space(Bougie, Sector, "III-3", Mountains, 2, coastal = true)
  val DefaultAlgiers          = Space(Algiers, City, "IV", Urban, 3, coastal = true)
  val DefaultMedea            = Space(Medea, Sector, "IV-1", Mountains, 2, coastal = true)
  val DefaultOrleansVille     = Space(OrleansVille, Sector, "IV-2", Mountains, 2, coastal = true)
  val DefaultOran             = Space(Oran, City, "V", Urban, 2, coastal = true)
  val DefaultMecheria         = Space(Mecheria, Sector, "V-1", Mountains, 0)
  val DefaultTlemcen          = Space(Tlemcen, Sector, "V-2", Plains, 1, coastal = true)
  val DefaultSidiBelAbbes     = Space(SidiBelAbbes, Sector, "V-3", Plains, 1, coastal = true)
  val DefaultMostaganem       = Space(Mostaganem, Sector, "V-4", Mountains, 2, coastal = true)
  val DefaultSaida            = Space(Saida, Sector, "V-5", Mountains, 0)
  val DefaultMascara          = Space(Mascara, Sector, "V-6", Mountains, 0)
  val DefaultTiaret           = Space(Tiaret, Sector, "V-7", Mountains, 0)
  val DefaultAinSefra         = Space(AinSefra, Sector, "V-8", Plains, 0)
  val DefaultLaghouat         = Space(Laghouat, Sector, "V-9", Plains, 0)
  val DefaultSidiAissa        = Space(SidiAissa, Sector, "VI-1", Mountains, 0)
  val DefaultAinOussera       = Space(AinOussera, Sector, "VI-2", Mountains, 1)
  val DefaultMorocco          = Space(Morocco, Country, "", Plains, 1, coastal = true)
  val DefaultTunisia          = Space(Tunisia, Country, "", Plains, 1, coastal = true)
  
  
  val DefaultSpaces = List(
    DefaultBarika, DefaultBatna, DefaultBiskra, DefaultOumElBouaghi, DefaultTebessa, DefaultNegrine,
    DefaultConstantine, DefaultSetif, DefaultPhilippeville, DefaultSoukAhras, DefaultTiziOuzou,
    DefaultBordjBouArreridj, DefaultBougie, DefaultAlgiers, DefaultMedea, DefaultOrleansVille,
    DefaultOran, DefaultMecheria, DefaultTlemcen, DefaultSidiBelAbbes, DefaultMostaganem,
    DefaultSaida, DefaultMascara, DefaultTiaret, DefaultAinSefra, DefaultLaghouat, DefaultSidiAissa,
    DefaultAinOussera, DefaultMorocco, DefaultTunisia)  
    
  val SpaceNames = (DefaultSpaces map (_.name)).sorted
  val BorderCountryNames = List(Morocco, Tunisia)

  // Morocco/Tunisia have blank wilayas
  def inSameWilaya(src: String, dest: String) = (game getSpace src).wilaya == (game getSpace dest).wilaya
  
  val EdgeTrackMax             = 50
  val FranceTrackMax           =  5  // 0(A) to 5(F)
  val BorderZoneTrackMax       =  4  // 0 to 4
  val FrenchTroopsManifest     =  9
  val FrenchPoliceManifest     = 21
  val AlgerianTroopsManifest   =  3
  val AlgerianPoliceManifest   =  7
  val GuerrillasManifest       = 30
  val GovBasesManifest         =  6
  val FlnBasesManifest         = 15
  val TerrorMarkerManifest     = 12
  val ResettledMarkerManifest  =  7
  val Plus1PopMarkerManifest   =  2
  val Plus1BaseMarkerManifest  =  2
  
  
  class FranceTrackEntry(val name: String, val commit: Int, val resource: Int)
  val FranceTrack: Vector[FranceTrackEntry] = Vector(
    new FranceTrackEntry("A", 0, 1),
    new FranceTrackEntry("B", 1, 2),
    new FranceTrackEntry("C", 2, 3),
    new FranceTrackEntry("D", 2, 4),
    new FranceTrackEntry("E", 3, 5),
    new FranceTrackEntry("F", 3, 6)
  )
  
  def franceTrackFromLetter(letter: Char): Int = {
    assert(letter >= 'A' && letter <= 'F', "France track must be initialized to a value A through F")
    letter - 'A'
  }
  
  trait Scenario {
    val name: String
    val numberOfPropCards: Int
    val resources: Resources
    val commitment: Int
    val franceTrack: Int
    val borderZoneTrack: Int
    val outOfPlay: Pieces
    val pivotalCardsPlayed: Set[Int]
    val spaces: List[Space]
    
    // Override this if the scenario requires any special setup
    val additionalSetup: () => Unit = () => ()
  }
  
  case class GameParameters(scenarioName: String,
                            finalPropSupport: Boolean = false, // Allow support in final Prop round (optional rule)
                            botDebug: Boolean = false)
  
  sealed trait Action {
    val name: String
  }
  case object Pass               extends Action    { val name = "Pass";               override def toString() = "Pass" }
  case object Event              extends Action    { val name = "Event";              override def toString() = "Execute Event" }
  case object ExecOpPlusActivity extends Action    { val name = "ExecOpPlusActivity"; override def toString() = "Execute Op & Special Activity" }
  case object ExecLimitedOp      extends Action    { val name = "ExecLimitedOp";      override def toString() = "Execute Limited Op" }
  case object ExecOpOnly         extends Action    { val name = "ExecOpOnly";         override def toString() = "Execute Op Only" }
  
  object Action {
    val AllActions = List(Pass, Event, ExecOpPlusActivity, ExecLimitedOp, ExecOpOnly)
    def apply(name: String): Action = AllActions find (a => a.name.toLowerCase == name.toLowerCase) getOrElse {
      throw new IllegalArgumentException(s"Invalid Action name: $name")
    }
  }
  
  val secondActions: Map[Action, List[Action]] = Map(
    Pass               -> List(Event, ExecOpPlusActivity, ExecOpOnly, ExecLimitedOp, Pass),
    Event              -> List(ExecOpPlusActivity, Pass),
    ExecOpPlusActivity -> List(Event, ExecLimitedOp, Pass),
    ExecOpOnly         -> List(ExecLimitedOp, Pass),
    ExecLimitedOp      -> List(ExecOpPlusActivity, ExecOpOnly, Pass)
  )
  
  case class SequenceOfPlay(
    firstEligible: Role = Fln,
    secondEligible: Role = Gov,
    firstAction: Option[Action] = None,
    secondAction: Option[Action] = None) {

    def numActed = firstAction.size + secondAction.size
    
    def availableActions: List[Action] = {
      assert(secondAction.isEmpty, "availableActions called after two actions taken")
      firstAction match {
        case Some(first) => secondActions(first)
        case None        => List(Event, ExecOpPlusActivity, ExecOpOnly, ExecLimitedOp, Pass)
      }
    }
      
    def nextAction(action: Action): SequenceOfPlay = numActed match {
      case 0 => copy(firstAction  = Some(action))
      case 1 => copy(secondAction = Some(action))
      case _ => throw new IllegalStateException("nextAction(): Two actions have already occurred on the current card")
    }
    
    def reset(): SequenceOfPlay = {
      val retainInitiative: Set[Action] = Set(Pass, Event, ExecLimitedOp)
      assert(firstAction.nonEmpty && secondAction.nonEmpty, s"cannot reset sequence of play until two actions have occurred")
      if (retainInitiative contains firstAction.get)
        SequenceOfPlay(firstEligible, secondEligible)
      else
        SequenceOfPlay(secondEligible, firstEligible)
    }
  }
      
  case class Resources(gov: Int = 0, fln: Int = 0) {
    def apply(role: Role) = role match {
      case Gov => gov
      case Fln => fln
    }
    
    def update(role: Role, value: Int): Resources = role match {
      case Gov => copy(gov = value)
      case Fln => copy(fln = value)
    }
    
    def increase(role: Role, amount: Int): Resources = role match {
      case Gov => copy(gov = (gov + amount) min EdgeTrackMax)
      case Fln => copy(fln = (fln + amount) min EdgeTrackMax)
    }
    
    def decrease(role: Role, amount: Int): Resources = role match {
      case Gov => copy(gov = (gov - amount) max 0)
      case Fln => copy(fln = (fln - amount) max 0)
    }
  }
  
  case class GameState(
    params: GameParameters,
    turn: Int,
    numberOfPropCards: Int,
    spaces: List[Space],
    franceTrack: Int                 = 0,
    borderZoneTrack: Int             = 0,
    commitment: Int                  = 0,
    resources: Resources             = Resources(),
    outOfPlay: Pieces                = Pieces(),
    casualties: Pieces               = Pieces(),
    sequence: SequenceOfPlay         = SequenceOfPlay(),
    capabilities: List[String]       = Nil,
    momentum: List[String]           = Nil,
    currentCard: Option[Int]         = None,
    previousCard: Option[Int]        = None,
    propCardsPlayed: Int             = 0,
    pivotalCardsPlayed: Set[Int]     = Set.empty,  // Coup d'Etat will be removed after each propaganda round
    coupdEtatPlayedOnce: Boolean     = false,
    recallDeGaulleCancelled: Boolean = false,
    history: Vector[String]          = Vector.empty) {
    
    val algerianSpaces = spaces filterNot (_.isCountry)
    val countrySpaces  = spaces filter (_.isCountry)
    
    def wilayaSpaces(wilaya: String) = algerianSpaces filter (_.wilaya == wilaya)
    
    val franceTrackLetter = ('A' + franceTrack).toChar
    
    def moroccoTunisiaIndependent = pivotalCardsPlayed(PivotalMoroccoTunisiaIndepdent)
    def recallDeGaulleInEffect    = pivotalCardsPlayed(PivotalRecallDeGaulle) && !recallDeGaulleCancelled

    def isFinalCampaign = propCardsPlayed == (numberOfPropCards - 1)
    def govPivotalAvailable = deck.GovPivotalCards -- pivotalCardsPlayed
    def flnPivotalAvailable = deck.FlnPivotalCards -- pivotalCardsPlayed
    
    def govPivotalPlayable: Set[Int] = govPivotalAvailable filter {
      case PivotalCoupdEtat      => true
      case PivotalMobilization   => flnScore >= 15
      case PivotalRecallDeGaulle => coupdEtatPlayedOnce && !recallDeGaulleCancelled
      case n => throw new IllegalStateException(s"Non gov pivotal card: $n") 
    }
    
    // Count the total number of something in each space on the map
    def totalOnMap(numberPer: Space => Int) = 
      spaces.foldLeft(0) { (total, space) => total + numberPer(space) }
    
    // Count the number a type of piece that is in the map, casualties, or out or play
    def piecesInUse(numberPer: Pieces => Int) =
      totalOnMap(space => numberPer(space.pieces)) + numberPer(casualties) + numberPer(outOfPlay)
      
    def terrorMarkersAvailable    = TerrorMarkerManifest    - totalOnMap(_.terror) 
    def frenchTroopsAvailable     = FrenchTroopsManifest    - piecesInUse(_.frenchTroops)
    def frenchPoliceAvailable     = FrenchPoliceManifest    - piecesInUse(_.frenchPolice)
    def algerianTroopsAvailable   = AlgerianTroopsManifest  - piecesInUse(_.algerianTroops)
    def algerianPoliceAvailable   = AlgerianPoliceManifest  - piecesInUse(_.algerianPolice)
    def guerrillasAvailable       = GuerrillasManifest      - piecesInUse(_.totalGuerrillas)
    def govBasesAvailable         = GovBasesManifest        - piecesInUse(_.govBases)
    def flnBasesAvailable         = FlnBasesManifest        - piecesInUse(_.flnBases)
    def resttledMarkersAvailable  = ResettledMarkerManifest - resettledSectors
    def plus1PopMarkersAvailable  = Plus1PopMarkerManifest  - plus1PopCities
    def plus1BaseMarkersAvailable = Plus1BaseMarkerManifest - plus1BaseCountries

    def availablePieces = Pieces(
      frenchTroopsAvailable,
      frenchPoliceAvailable,
      algerianTroopsAvailable,
      algerianPoliceAvailable,
      guerrillasAvailable,
      0,  // No active guerrillas in available box
      govBasesAvailable,
      flnBasesAvailable)
    
    def resettledSectors   = spaces count (_.isResettled)
    def plus1PopCities     = spaces count (_.hasMarker(Plus1PopMarker))
    def plus1BaseCountries = spaces count (_.hasMarker(Plus1BaseMarker))
    
    // Does at least one space meet the given condtion?
    def hasSpace(test: (Space) => Boolean) = spaces exists test
    def hasAlgerianSpace(test: (Space) => Boolean) = algerianSpaces exists test
    def getSpace(name: String) = (spaces find (_.name == name)).get
    def updateSpace(changed: Space): GameState =
      this.copy(spaces = changed :: (spaces filterNot (_.name == changed.name)))
    
    def updatePieces(space: Space, pieces: Pieces): GameState = updateSpace(space.copy(pieces = pieces))
    
    def isPropRound = currentCard map deck.isPropagandaCard getOrElse false
    def isConsecutivePropCard = isPropRound && (previousCard map deck.isPropagandaCard getOrElse false)
    
    def govScore  = totalOnMap(_.supportValue) + commitment
    def govMargin = govScore - 35
    def flnScore  = totalOnMap(_.opposeValue)  + totalOnMap(_.flnBases)
    def flnMargin = flnScore - 30
    
    def scenarioSummary: Seq[String] = {
      val b = new ListBuffer[String]
      b += s"Scenario: ${params.scenarioName}"
      b += separator()
      if (game.params.finalPropSupport) {
        b += "Optional Rules:"
        b += "8.5.1 - Conduct Support Phase in final Propaganda round"
      }
      b.toList
    }
    
    def franceTrackDisplay: String = {
      val entry = FranceTrack(franceTrack)
      s"${entry.name} (Commitment -${entry.commit}, Resource +${entry.resource})"
    }  
    
    def statusSummary: Seq[String] = {
      val (gov, fln) = (govScore, flnScore)
      val b = new ListBuffer[String]
      
      val cardsPlayed         = if (currentCard.nonEmpty) turn else turn - 1
      val nextCardsPile       = (cardsPlayed / 13) + 1
      val propCardOutstanding = propCardsPlayed < nextCardsPile
      val cardsRemaining      = nextCardsPile * 13 - cardsPlayed  // remaining in current campaign
      val (propCardChance, propCardFraction) = if (propCardOutstanding)
        (1.0/cardsRemaining, s"  (1/$cardsRemaining)")
      else
        (0.0, s"  (${amountOf(13 - cardsPlayed % 13, "safe card")})")
      b += "Status"
      b += separator()
      b += f"Support + Commit : ${gov}%2d (${govMargin}%+3d)"
      b += f"Oppose  + Bases  : ${fln}%2d (${flnMargin}%+3d)"
      b += separator()
      b += f"Gov resources    : ${resources(Gov)}%2d"
      b += f"FLN resources    : ${resources(Fln)}%2d"
      b += separator()
      b += f"Gov commitment   : ${commitment}%2d"
      b += f"Resettled sectors: ${resettledSectors}%2d"
      b += separator()
      b += s"France track     : ${franceTrackDisplay}"
      b += f"Border zone track: ${borderZoneTrack}%d"
      b += separator()
      b += s"Campaign         : ${ordinal(propCardsPlayed+1)}${if (propCardsPlayed == numberOfPropCards - 1) " -- Final campaign" else ""}"
      b += f"Prop card chance : $propCardChance%.5f$propCardFraction"
      b.toList
    }
    
    def availablePiecesSummary: Seq[String] = {
      val b = new ListBuffer[String]
      b += "Available Pieces"
      b += separator()
      b += f"French Troops   : ${frenchTroopsAvailable}%2d"
      b += f"French Police   : ${frenchPoliceAvailable}%2d"
      b += f"Algerian Troops : ${algerianTroopsAvailable}%2d"
      b += f"Algerian Police : ${algerianPoliceAvailable}%2d"
      b += f"Government Bases: ${govBasesAvailable}%2d"
      b += separator()
      b += f"FLN guerrillas  : ${guerrillasAvailable}%2d"
      b += f"FLN Bases       : ${flnBasesAvailable}%2d"
      b += separator()
      b += f"Terror markers  : ${terrorMarkersAvailable}%2d"
      b.toList
    }
    
    def casualtiesSummary: Seq[String] = {
      val b = new ListBuffer[String]
      b += "Casualties"
      b += separator()
      b += f"French Troops   : ${casualties.frenchTroops}%2d"
      b += f"French Police   : ${casualties.frenchPolice}%2d"
      b += f"Algerian Troops : ${casualties.algerianTroops}%2d"
      b += f"Algerian Police : ${casualties.algerianPolice}%2d"
      b += f"Government Bases: ${casualties.govBases}%2d"
      b += separator()
      b += f"FLN guerrillas  : ${casualties.totalGuerrillas}%2d"
      b += f"FLN Bases       : ${casualties.flnBases}%2d"
      b.toList
    }
    
    def outOfPlaySummary: Seq[String] = {
      val b = new ListBuffer[String]
      b += "Out of Play"
      b += separator()
      b += f"French Troops   : ${outOfPlay.frenchTroops}%2d"
      b += f"French Police   : ${outOfPlay.frenchPolice}%2d"
      b += f"Government Bases: ${outOfPlay.govBases}%2d"
      b += separator()
      b += f"FLN guerrillas  : ${outOfPlay.totalGuerrillas}%2d"
      b.toList
    }
    
    def eventSummary: Seq[String] = {
      val b = new ListBuffer[String]
      b += "Active Events"
      b += separator()
      val pivotals = for {
        num  <- pivotalCardsPlayed.toList.sorted
        card  = deck(num)
        extra = if (num == PivotalRecallDeGaulle && recallDeGaulleCancelled) " (Cancelled)" else ""
      } yield s"$card$extra"
      
      wrap("Pivotal     : ", pivotals)     foreach (l => b += l)
      wrap("Capabilities: ", capabilities) foreach (l => b += l)
      wrap("Momentum    : ", momentum)     foreach (l => b += l)
      b.toList
    }
    
    def sequenceSummary: Seq[String] = {
      val card = currentCard map (n => deck(n).toString) getOrElse "none"
      def actor(role: Role, action: Option[Action]): String = {
        val desc = action map (_.toString) getOrElse "Has not acted"
        s"${padLeft(role, Role.maxNameLen)} ($desc)"
      }
      val b = new ListBuffer[String]
      b += "Sequence of Play"
      b += separator()
      b += s"Current card  : $card"
      b += s"1st eligible  : ${actor(sequence.firstEligible, sequence.firstAction)}"
      b += s"2nd eligible  : ${actor(sequence.secondEligible, sequence.secondAction)}"
      b.toList
    }
  
    def spaceSummary(name: String): Seq[String] = {
      val sp = getSpace(name)
      val b = new ListBuffer[String]
      val desc = if (sp.isCity) "City"
                 else if (sp.isCountry) "Country, plains"
                 else s"Sector, ${sp.terrain}"
      val popExtra = if (sp.isResettled) " (Resettled)"
                     else if (sp.hasMarker(Plus1PopMarker)) " (+1 Population)"
                     else ""
      b += ""
      b += s"${sp.nameAndZone}  ($desc)"
      b += separator()
      b += s"Population: ${sp.population}$popExtra" 
      wrap("Status    : ", Seq(sp.control.toString, sp.support.toString)) foreach (l => b += l)
      wrap("Pieces    : ", sp.stringItems)  foreach (l => b += l)
      if (sp.markers.nonEmpty)
        wrap("Markers   : ", sp.markers.sorted)  foreach (l => b += l)
      b.toList
    }
  }
  
  // Current game state.
  var game: GameState = _
  
  def displayGameStateDifferences(from: GameState, to: GameState): Unit = {
    var headerShown = false
    val b = new ListBuffer[String]
    def showHeader(): Unit = if (!headerShown) {
      headerShown = true
      println()
      println(separator(char = '='))
      println("The following changes should be made to the game board")
      println(separator(char = '='))
    }
    
    def showMarker(marker: String, oldValue: Any, newValue: Any, displayValue: Any = null): Unit = if (oldValue != newValue) {
      val display = if (displayValue == null) newValue.toString else displayValue.toString
      b += s"Set $marker to: $display"
    }
    def showPieces(label: String, oldPieces: Pieces, newPieces: Pieces): Unit = if (oldPieces != newPieces) {
      wrap(label, newPieces.stringItems) foreach (x => b += x)
    }
    def showList(label: String, oldList: Seq[String], newList: Seq[String]): Unit = if (oldList.sorted != newList.sorted) {
      wrap(label, newList.sorted) foreach (x => b += x)
    }
    
    showMarker("France Track marker",    from.franceTrack, to.franceTrack, FranceTrack(to.franceTrack).name)
    showMarker("Border Zone marker",     from.borderZoneTrack, to.borderZoneTrack)
    showMarker("Government commitment", from.commitment, to.commitment)
    showMarker("Government resources",  from.resources(Gov), to.resources(Gov))
    showMarker("FLN resources",         from.resources(Fln), to.resources(Fln))
    
    if (from.sequence != to.sequence) {
      val seq = to.sequence
      val first = seq.firstAction match {
        case None         => s"Place ${seq.firstEligible} cylinder in: 1st eligible box"
        case Some(action) => s"Place ${seq.firstEligible} cylinder in: $action box"
      }
      val second = seq.secondAction match {
        case None         => s"Place ${seq.secondEligible} cylinder in: 2nd eligible box"
        case Some(action) => s"Place ${seq.secondEligible} cylinder in: $action box"
      }
      b += ""
      b += first
      b += second
    }
    
    showPieces("Out of play: ", from.outOfPlay, to.outOfPlay)
    showPieces("Casualties : ", from.casualties, to.casualties)
    showPieces("Available  : ", from.availablePieces, to.availablePieces)
    
    showList("Pivotal in play     : ", from.pivotalCardsPlayed.toList map (deck(_).numAndName),
                                       to.pivotalCardsPlayed.toList map (deck(_).numAndName))
    showList("Capabilities in play: ", from.capabilities, to.capabilities)
    showList("Momentum in play    : ", from.momentum, to.momentum)
    
    if (b.nonEmpty) {
      showHeader()
      b foreach println
    }
    
    for (name <- spaceNames(from.spaces).sorted) {
      b.clear()
      val (fromSp, toSp) = (from.getSpace(name), to.getSpace(name))
      showMarker("Support marker", fromSp.support, toSp.support)
      showMarker("Control marker", fromSp.control, toSp.control)
      showPieces("Pieces: ", fromSp.pieces, toSp.pieces)
      showList("Markers: ", fromSp.markers, toSp.markers)
      if (b.nonEmpty) {
        showHeader()
        b.prepend(s"\n${fromSp.nameAndZone} changes:\n${separator()}")
        b foreach println
      }
    }
  }
  
  def spaceNames(spaces: Iterable[Space]): List[String] = (spaces map (_.name)).toList.sorted
  
  def spaces(names: Iterable[String]): List[Space] = (names map game.getSpace).toList.sortBy(_.name)
  
  def capabilityInPlay(cap: String) = game.capabilities contains cap
  
  def playCapability(cap: String): Unit = {
    game = game.copy(capabilities = cap :: game.capabilities)
    log(s"Capability is now in play: $cap")
  }
  
  def removeCapabilityFromPlay(cap: String): Unit = {
    if (game.capabilities contains cap) {
      game = game.copy(capabilities = game.capabilities filterNot (_ == cap))
      log(s"Remove capability '$cap' from play")
    }
  }
  
  def momentumInPlay(mo: String) = game.momentum contains mo
  
  def playMomentum(mo: String): Unit = {
    game = game.copy(momentum = mo :: game.momentum)
    log(s"Momentum event is now in play: $mo")
  }
  
  def activateHiddenGuerrillas(spaceName: String, num: Int): Unit = if (num > 0) {
    val sp = game.getSpace(spaceName)
    game = game.updatePieces(sp, sp.activateGuerrillas(num))
    log(s"Flip ${hiddenG(num)} to active in $spaceName")
  }
  
  def hideActiveGuerrillas(spaceName: String, num: Int): Unit = if (num > 0) {
    val sp = game.getSpace(spaceName)
    game = game.updatePieces(sp, sp.hideGuerrillas(num))
    log(s"Flip ${activeG(num)} to underground in $spaceName")
  }

  def increaseResources(role: Role, amount: Int): Unit = if (amount > 0) {
    game = game.copy(resources = game.resources.increase(role, amount))
    log(s"Increase ${role} resources by +$amount to ${game.resources(role)}")
  }
  
  def decreaseResources(role: Role, amount: Int): Unit = if (amount > 0) {
    game = game.copy(resources = game.resources.decrease(role, amount))
    log(s"Decrease ${role} resources by -$amount to ${game.resources(role)}")
  }
  
  def increaseCommitment(amount: Int): Unit = if (amount > 0) {
    game = game.copy(commitment = (game.commitment + amount) min EdgeTrackMax)
    log(s"Increase Government commitment by +$amount to ${game.commitment}")
    log(s"Move 'Support+Commit' marker to ${game.govScore}")
  }
  
  def decreaseCommitment(amount: Int): Unit = if (amount > 0) {
    game = game.copy(commitment = (game.commitment - amount) max 0)
    log(s"Decrease Government commitment by -$amount to ${game.commitment}")
    log(s"Move 'Support+Commit' marker to ${game.govScore}")
  }
  
  def increaseFranceTrack(num: Int = 1): Unit = if (num > 0) {
    game = game.copy(franceTrack = (game.franceTrack + num) min FranceTrackMax)
    val entry = FranceTrack(game.franceTrack)
    log(s"Shift the France track right ${amountOf(num, "space")} to '${entry.name}'")
  }
  
  def decreaseFranceTrack(num: Int = 1): Unit = if (num > 0) {
    game = game.copy(franceTrack = (game.franceTrack - num) max 0)
    val entry = FranceTrack(game.franceTrack)
    log(s"Shift the France track left ${amountOf(num, "space")} to '${entry.name}'")
  }
  
  def increaseBorderZoneTrack(num: Int = 1): Unit = if (num > 0) {
    game = game.copy(borderZoneTrack = (game.borderZoneTrack + num) min BorderZoneTrackMax)
    log(s"Increase the Border zone track ${amountOf(num, "space")} to '${game.borderZoneTrack}'")
  }
  
  def decreaseBorderZoneTrack(num: Int = 1): Unit = if (num > 0) {
    game = game.copy(borderZoneTrack = (game.borderZoneTrack - num) max 0)
    log(s"Decrease the Border zone track ${amountOf(num, "space")} to '${game.borderZoneTrack}'")
  }
  
  def addTerror(name: String, num: Int): Unit = if (num > 0) {
    val sp = game.getSpace(name)
    assert(game.terrorMarkersAvailable >= num, "addTerror: not enough available markers")
    game = game.updateSpace(sp.addMarker(TerrorMarker, num))
    log(s"Add ${amountOf(num, "terror marker")} to $name")
  }
  
  def removeTerror(name: String, num: Int): Unit = if (num > 0) {
    val sp = game.getSpace(name)
    assert(sp.terror >= num, "removeTerror: not enought markers in space")
    game = game.updateSpace(sp.removeMarker(TerrorMarker, num))
    log(s"Remove ${amountOf(num, "terror marker")} from $name")
  }
  
  def addResettledMarker(name: String): Unit = {
    val sp = game.getSpace(name)
    assert(game.resttledMarkersAvailable > 0, "addResettledMarker: no resettled markers available")
    val updated = sp.addMarker(ResettledMarker).copy(support = Neutral)
    game = game.updateSpace(updated)
    log(s"Add Resettled marker to $name")
    logSupportChange(sp, updated)
  }
  
  def addBaseMarker(name: String): Unit = {
    assert(name == Morocco || name == Tunisia, "addBaseMarker: only applies to Morocco and Tunisia")
    val sp = game.getSpace(name)
    if (sp.hasMarker(Plus1BaseMarker))
      log(s"$name already has an extra base marker")
    else {
      game = game.updateSpace(sp.addMarker(Plus1BaseMarker))
      log(s"Add Base marker to $name")
    }
  }

  def addPlus1PopMarker(name: String): Unit = {
    val sp = game.getSpace(name)
    assert(sp.isCity, "addPlus1PopMarker: only applies to cities")
    if (game.plus1PopMarkersAvailable > 0) {
      game = game.updateSpace(sp.addMarker(Plus1PopMarker))
      log(s"Add '+1 Pop' marker to $name")
    }
    else
      log("There are no '+1 Pop' markers available")
  }

  def setSupport(name: String, newLevel: SupportValue): Unit = {
    val sp = game.getSpace(name)
    if (sp.support != newLevel) {
      val updated = sp.copy(support = newLevel)
      game = game.updateSpace(updated)
      logSupportChange(sp, updated)
    }
  }
  
  def increaseSupport(name: String, num: Int): Unit = if (num > 0) {
    val sp = game.getSpace(name)
    val newLevel = (sp.support, num) match {
      case (Neutral, 1) => Support
      case (Oppose,  1) => Neutral
      case (Oppose,  2) => Support
      case (s, n)       => throw new IllegalStateException(s"Cannot increase support from $s by $n steps")
    }
    val updated = sp.copy(support = newLevel)
    game = game.updateSpace(updated)
    logSupportChange(sp, updated)
  }
  
  def decreaseSupport(name: String, num: Int): Unit = if (num > 0) {
    val sp = game.getSpace(name)
    val newLevel = (sp.support, num) match {
      case (Neutral, 1) => Oppose
      case (Support, 1) => Neutral
      case (Support, 2) => Oppose
      case (s, n)       => throw new IllegalStateException(s"Cannot decrease support from $s by $n steps")
    }
    val updated = sp.copy(support = newLevel)
    game = game.updateSpace(updated)
    logSupportChange(sp, updated)
  }
  
  
  def logControlChange(orig: Space, updated: Space): Unit = {
    assert(orig.name == updated.name, "logControlChange: not the same space!")
    if (orig.control != updated.control) {
      if (updated.control == Uncontrolled)
        log(s"Remove ${orig.control} marker from ${orig.name}")
      else if (orig.control == Uncontrolled)
        log(s"Place ${updated.control} marker in ${orig.name}")
      else
        log(s"Flip control marker to ${updated.control} in ${orig.name}")
    }
  }
  
  def logSupportChange(orig: Space, updated: Space): Unit = {
    assert(orig.name == updated.name, "logSupportChange: not the same space!")
    if (orig.support != updated.support) {
      if (updated.support == Neutral)
        log(s"Remove ${orig.support} marker from ${orig.name}")
      else if (orig.support == Neutral)
        log(s"Place ${updated.support} marker in ${orig.name}")
      else
        log(s"Flip support marker to ${updated.support} in ${orig.name}")
      // Log change in score
      if (orig.support != Neutral && updated.support != Neutral) {
        log(s"Move 'Support+Commit' marker to ${game.govScore}")
        log(s"Move 'Oppose+Bases' marker to ${game.flnScore}")
      }
      else if (orig.support == Support || updated.support == Support)
        log(s"Move 'Support+Commit' marker to ${game.govScore}")
      else
        log(s"Move 'Oppose+Bases' marker to ${game.flnScore}")
    }
  }

  
  // If num is 1 use the name as is
  // otherwise either use the plural if given or add an 's' to the name.
  def amountOf(num: Int, name: String, plural: Option[String] = None) = 
    (num, plural) match {
      case (1, _)            => s"$num $name"
      case (_, Some(plural)) => s"$num $plural"
      case _                 => s"$num ${name}s"
    }
            
  def amtPiece(num: Int, pieceType: PieceType) = amountOf(num, pieceType.singular, Some(pieceType.plural))
  def amtRes(role: Role)  = amountOf(game.resources(role), "resource")
  def frenchT(num: Int)   = amtPiece(num, FrenchTroops)
  def frenchP(num: Int)   = amtPiece(num, FrenchPolice)
  def algerianT(num: Int) = amtPiece(num, AlgerianTroops)
  def algerianP(num: Int) = amtPiece(num, AlgerianPolice)
  def hiddenG(num: Int)   = amtPiece(num, HiddenGuerrillas)
  def activeG(num: Int)   = amtPiece(num, ActiveGuerrillas)
  def govB(num: Int)      = amtPiece(num, GovBases)
  def flnB(num: Int)      = amtPiece(num, FlnBases)
  
  // Returns comma separated string with last choice separated by "and"
  //    List("apples")                      => "apples"
  //    List("apples", "oranges")           => "apples and oranges"
  //    List("apples", "oranges", "grapes") => "apples, oranges and grapes"
  def andList(x: Seq[Any]) = x match {
    case Seq()     => ""
    case Seq(a)    => a.toString
    case Seq(a, b) => s"${a.toString} and ${b.toString}"
    case _         => x.dropRight(1).mkString(", ") + ", and " + x.last.toString
  }
  
  // Returns comma separated string with last choice separated by "or"
  //    List("apples")                      => "apples"
  //    List("apples", "oranges")           => "apples or oranges"
  //    List("apples", "oranges", "grapes") => "apples, oranges or grapes"
  def orList(x: Seq[Any]) = x match {
    case Seq()     => ""
    case Seq(a)    => a.toString
    case Seq(a, b) => s"${a.toString} or ${b.toString}"
    case _         => x.dropRight(1).mkString(", ") + ", or " + x.last.toString
  }
  
  // Find a match for the given string in the list of options.
  // Any unique prefix of the given options will succeed.
  def matchOne(s: String, options: Seq[String]): Option[String] = {
    if (s == "?") {
      println(s"Enter one of:\n${orList(options)}")
      None
    }
    else {
      val normalized = options map (_.toLowerCase)
      (normalized.distinct filter (_ startsWith s.toLowerCase)) match {
        case Seq() =>
          println(s"'$s' is not valid. Must be one of:\n${orList(options)}")
          None
        case Seq(v)  =>
          Some(options(normalized.indexOf(v)))
        
        case many if many exists (_ == s) =>
          Some(options(normalized.indexOf(s)))
      
        case ambiguous =>
          println(s"'$s' is ambiguous. (${orList(ambiguous)})")
          None
      }
    }
  }
    
  def askOneOf(prompt: String,
               options: Seq[Any],
               initial: Option[String] = None, 
               allowNone: Boolean = false,
               allowAbort: Boolean = true): Option[String] = {
    val choices = if (allowAbort) options ++ List(AbortActionString) else options
    def testResponse(response: Option[String]): Option[String] = {
      response flatMap (s => matchOne(s.trim, choices map (_.toString))) match {
        case None =>
          readLine(prompt) match {
            case null | "" if allowNone => None
            case null | ""              => testResponse(None)
            case input                  => testResponse(Some(input))
          }
        case Some(AbortActionString) if allowAbort => 
          if (askYorN("Really abort (y/n)? ")) throw AbortAction else testResponse(None)
        case s => s
      }
    }
    testResponse(initial)
  }
  
  def askYorN(prompt: String): Boolean = {
    def testResponse(r: String): Option[Boolean] = {
      if (r == null)
        None
      else
        r.trim.toLowerCase match {
          case "n" | "no"  => Some(false)
          case "y" | "yes" => Some(true)
          case _           => None
        }
    }
    
    testResponse(readLine(prompt)) match {
      case Some(result) => result
      case None         => askYorN(prompt)
    }
  }
  
  def askInt(prompt: String, low: Int, high: Int, default: Option[Int] = None, allowAbort: Boolean = true): Int = {
    assert(low <= high, "askInt() low cannot be greater than high")
    if (low == high) {
      println(s"$prompt: $low")
      low
    }
    else {
      val choices = (low to high).toList
      default match {
        case Some(d) =>
          val p = if (choices.size > 3)
            "%s (%d - %d) Default = %d: ".format(prompt, choices.head, choices.last, d)
          else
            "%s (%s) Default = %d: ".format(prompt, orList(choices), d)
          askOneOf(p, choices, allowNone = true, allowAbort = allowAbort) map (_.toInt) match {
            case None    => d
            case Some(x) => x
          }
        case None => 
          val p = if (choices.size > 3)
            "%s (%d - %d): ".format(prompt, choices.head, choices.last)
          else
            "%s (%s): ".format(prompt, orList(choices))
          (askOneOf(p, choices, None, allowAbort = allowAbort) map (_.toInt)).get
      }
    }
  }
  
  // Convenience method for createing choices for the askMenu() function.
  def choice[T](condition: Boolean, value: T, desc: String): Option[(T, String)] =
    if (condition) Some(value -> desc) else None
  
  // Present a numbered menu of choices
  // Allow the user to choose 1 or more choices and return
  // a list of keys to the chosen items.
  // Caller should println() a brief description of what is being chosen.
  // items is a list of (key -> display) for each item in the menu.
  def askMenu[T](items: List[(T, String)], 
             numChoices: Int = 1,
             repeatsOK: Boolean = false,
             allowAbort: Boolean = true): List[T] = {
    def nextChoice(num: Int, itemsRemaining: ListMap[T, String]): List[T] = {
      if (itemsRemaining.isEmpty || num > numChoices)
        Nil
      else if (itemsRemaining.size == 1)
        itemsRemaining.keys.head :: Nil
      else {
        println(separator())
        val indexMap = (itemsRemaining.keys.zipWithIndex map (_.swap)).toMap
        for ((key, i) <- itemsRemaining.keysIterator.zipWithIndex)
          println(s"${i+1}) ${itemsRemaining(key)}")
        val prompt = if (numChoices > 1) s"${ordinal(num)} Selection: "
        else "Selection: "
        val choice = askOneOf(prompt, 1 to itemsRemaining.size, allowAbort = allowAbort).get.toInt
        val index  = choice - 1
        val key    = indexMap(index)
        val remain = if (repeatsOK) itemsRemaining else itemsRemaining - key
        indexMap(index) :: nextChoice(num + 1, remain)
      }
    }
    nextChoice(1, ListMap(items:_*))
  }

  def askCardNumber(prompt: String): Option[Int] = {    
    readLine(prompt).trim match {
      case null => askCardNumber(prompt)
      case "" => None
      case input if ("quit".startsWith(input.toLowerCase)) =>
        if (askYorN("Really quit (y/n)? ")) throw ExitGame else askCardNumber(prompt)
      case INTEGER(num) if deck.isValidCardNumber(num.toInt) => Some(num.toInt)
      case input => 
        println(s"'$input' is not a valid card number")
        askCardNumber(prompt)
    }
  }
  
  def askCandidate(prompt: String, candidates: List[String], allowAbort: Boolean = true): String = {
    assert(candidates.nonEmpty, s"askCandidate(): list of candidates cannot be empty")
    // If only one candidate then don't bother to ask
    candidates match {
      case x :: Nil => println(s"$prompt $x"); x
      case xs       => askOneOf(prompt, xs, allowAbort = allowAbort).get
    }
  }
  
  def askCandidateAllowNone(prompt: String, candidates: List[String]): Option[String] = {
    assert(candidates.nonEmpty, s"askCandidateAllowNone(): list of candidates cannot be empty")
    // If only one candidate then don't bother to ask
    candidates match {
      case x :: Nil => println(s"$prompt $x"); Some(x)
      case xs       => askOneOf(prompt, xs, allowNone = true, allowAbort = false)
    }
  }
  
  val ALL_WILAYAS = Set("I", "II", "III", "IV", "V", "VI")

  def askWilaya(prompt: String = "\nSelect Wilaya: ", allowed: Set[String] = ALL_WILAYAS, allowAbort: Boolean = true): String = {
    val choices = List("I", "II", "III", "IV", "V", "VI") flatMap { wilaya =>
      if (allowed(wilaya)) Some(wilaya -> s"Wilaya $wilaya") else None
    }
    assert(choices.nonEmpty, "askWilaya: allowed set does not contain at least one valid wilaya")
    if (choices.size == 1)
      choices.head._1
    else {
      println(prompt)
      askMenu(choices).head
    }
  }
  
  // Ask the user to select a number of pieces. 
  // The type of pieces allowed for selection may be limited by passing a list of those
  // that are allowed.  An empty list indicates that all types of pieces may be selected.
  def askPieces(pieces: Pieces, num: Int, allowed: Seq[PieceType] = AllPieceTypes,
      heading: Option[String] = None,
      allowAbort: Boolean = true): Pieces = {
    val pieceTypes = allowed filter (pieces.numOf(_) > 0)
    var selected   = Pieces()
    val numPieces  = num min pieces.totalOf(pieceTypes)
    if (numPieces > 0) {
      if (pieceTypes.size == 1)
        selected = selected.set(numPieces, pieceTypes.head)
      else {
        val available = pieces.only(pieceTypes)
        println()
        heading foreach println
        println(s"Select ${amountOf(numPieces, "piece")} among the following:")
        wrap("  ", available.stringItems) foreach println
        println()
      
        def nextType(types: Seq[PieceType]): Unit = {
          val numRemaining = numPieces - selected.total
          if (numRemaining != 0) {
            // If we have to include all remainig pieces, don't bother asking
            if (pieces.totalOf(types) == numRemaining) {
              for (pieceType <- types)
                selected = selected.add(pieces.numOf(pieceType), pieceType)
            }
            else {
              val (pieceType :: rest) = types
              val totalOfRest = pieces.totalOf(rest)
              val minimum = if (totalOfRest < numRemaining) numRemaining - totalOfRest else 0
              val maximum = numRemaining min pieces.numOf(pieceType)
              val n = askInt(s"How many ${pieceType}", minimum, maximum, allowAbort = allowAbort)
              selected = selected.add(n, pieceType)
              nextType(rest)
            }
          }
        }
        nextType(pieceTypes)
      }
    }
    selected
  }
  
  
  // Ask the number of each type of pieces to place in the given space up to the
  // given maximum.
  // If there are not sufficient pieces in the available box, then the user is 
  // asked to remove some from the map.
  // If there are not enough in available or in other spaces on the map, then the
  // piece type is skipped.
  def askPiecesToPlace(spaceName: String, types: List[PieceType], maxToPlace: Int): Pieces = {
    // Get number of each type in available box plus on map (but not in target space)
    val availNum = (types map { pieceType =>
      val num = game.availablePieces.numOf(pieceType) + 
                game.totalOnMap(_.numOf(pieceType)) - 
                game.getSpace(spaceName).numOf(pieceType)
      (pieceType -> num)
    }).toMap
    val maxPieces = maxToPlace min (availNum.values.sum)
    val availTypes = types filter (availNum(_) > 0)
    if (availTypes.isEmpty) {
      println(s"\nThere are no ${orList(types)} available to be placed.")
      Pieces()
    }
    else {
      println(s"\nPlace up to ${amountOf(maxPieces, "piece")} in $spaceName (${andList(availTypes)})")
      
      def nextType(placed: Pieces, remainingTypes: List[PieceType]): Pieces = {
        val maxRemaining = maxPieces - placed.total
        if (maxRemaining == 0 || remainingTypes.isEmpty)
          placed
        else {
          val pieceType = remainingTypes.head
          val maxOfType = availNum(pieceType) min maxRemaining
          if (maxOfType == 0) {
            println(s"\nThere are no ${pieceType} available to be placed.")
            nextType(placed, remainingTypes.tail)
          }
          else {
            val num = askInt(s"\nPlace how many $pieceType? ", 0, maxOfType)
            val numAvail = game.availablePieces.numOf(pieceType)
            val finalNum = if (num <= numAvail)
               num
            else {
              // Ask if they want to voluntarilty remove pieces to make up the difference.
              numAvail match {
                case 0 => println(s"\nThere are no ${pieceType.plural} in the available box")
                case 1 => println(s"\nThere is only 1 ${pieceType.singular} in the available box")
                case n => println(s"\nThere are only ${amtPiece(n, pieceType)} in the available box")
              }
              println()
              if (askYorN("Do you wish to voluntarily remove pieces to make up the difference? (y/n) ")) {
                val numToRemove = askInt("How many pieces do you wish to remove from the map", 0, num - numAvail)
                voluntaryRemoval(numToRemove, pieceType, Set(spaceName))
                numAvail + numToRemove
              }
              else
                numAvail
            }
            nextType(placed.add(finalNum, pieceType), remainingTypes.tail)
          }
        }
      }
      
      nextType(Pieces(), availTypes)
    }
  }

  // Ask the user to remove the given number of pieces of the requested type from the map.
  def voluntaryRemoval(num: Int, pieceType: PieceType, prohibitedSpaces: Set[String]): Unit = if (num > 0) {
    val candidateNames = spaceNames(game.spaces filterNot (sp => prohibitedSpaces(sp.name)) filter (_.numOf(pieceType) > 0))
    def availPieces(names: List[String]) = names.foldLeft(0)((sum, n) => sum + game.getSpace(n).numOf(pieceType))
    assert(availPieces(candidateNames) >= num, "voluntaryRemoval: Not enough pieces on map!")
    
    def nextSpace(removed: Vector[(String, Int)], candidates: List[String]): Vector[(String, Int)] = {
      val removedSoFar = removed.foldLeft(0) { case (sum, (_, n)) => sum + n }
      val numLeft      = num - removedSoFar
      val avail        = availPieces(candidates)
      if (numLeft == 0)
        removed
      else if (avail == numLeft) {
        // Remove all remaining pieces
        removed ++ (candidates map (n => (n -> game.getSpace(n).numOf(pieceType))))
      }
      else {
        val name = askCandidate(s"\nSelect space to remove $pieceType: ", candidates)
        val numInSpace = game.getSpace(name).numOf(pieceType) min numLeft
        val minFromSpace = 1 max (numLeft - (avail - numInSpace))
        val x = if (minFromSpace == numInSpace) numInSpace
                else askInt(s"Remove how many", minFromSpace, numInSpace)
        nextSpace(removed :+ (name -> x), candidates filterNot (_ == name))
      }
    }
    
    val removed = nextSpace(Vector.empty, candidateNames)
    
    log()
    for ((name, number) <- removed; sp = game.getSpace(name)) {
      val updated = sp.copy(pieces = sp.remove(number, pieceType))
      game = game.updateSpace(updated)
      log(s"Remove ${amtPiece(number, pieceType)} from $name to AVAILABLE")
      logControlChange(sp, updated)
    }
  }

  // Place pieces from the AVAILABLE box in the given map space.
  // There must be enough pieces in the available box or an exception is thrown.
  def placePieces(spaceName: String, pieces: Pieces, logControl: Boolean = true): Unit = if (pieces.total > 0) {
    assert(game.availablePieces contains pieces, "Insufficent pieces in the available box")
    
    val sp = game.getSpace(spaceName)
    val updated = sp.copy(pieces = sp.pieces + pieces)
    game = game.updateSpace(updated)
    log(s"\nPlace the following pieces from AVAILABLE into $spaceName:")
    wrap("  ", pieces.stringItems) foreach (log(_))
    if (logControl)
      logControlChange(sp, updated)
    if (pieces.flnBases > 0)
      log(s"Move 'Oppose+Bases' marker to ${game.flnScore}")
      
  }
  
  // Place pieces the available box into the out of play box.
  // There must be enough pieces in the out of play box or an exception is thrown.
  def movePiecesFromAvailableToOutOfPlay(pieces: Pieces): Unit = if (pieces.total > 0) {
    assert(game.availablePieces contains pieces, "Insufficent pieces in the available box")
    
    game = game.copy(outOfPlay = game.outOfPlay + pieces)
    log(s"\nMove the following pieces from AVAILABLE to OUT-OF-PLAY:")
    wrap("  ", pieces.stringItems) foreach (log(_))
  }
  
  // Place pieces from the Out Of Play box into the available box.
  // There must be enough pieces in the out of play box or an exception is thrown.
  def movePiecesFromOutOfPlayToAvailable(pieces: Pieces): Unit = if (pieces.total > 0) {
    assert(game.outOfPlay contains pieces, "Insufficent pieces in the out of play box")
    
    game = game.copy(outOfPlay = game.outOfPlay - pieces)
    log(s"\nMove the following pieces from OUT-OF-PLAY to AVAILABLE:")
    wrap("  ", pieces.stringItems) foreach (log(_))
  }
  
  def movePiecesFromCasualtiesToAvailable(pieces: Pieces): Unit = if (pieces.total > 0) {
    assert(game.casualties contains pieces, "Insufficent pieces in the casualties box")
    
    game = game.copy(casualties = game.casualties - pieces)
    log(s"\nMove the following pieces from CASUALTIES to AVAILABLE:")
    wrap("  ", pieces.stringItems) foreach (log(_))
  }
  
  def movePiecesFromCasualtiesToOutOfPlay(pieces: Pieces): Unit = if (pieces.total > 0) {
    assert(game.casualties contains pieces, "Insufficent pieces in the casualties box")
    
    game = game.copy(outOfPlay = game.outOfPlay + pieces, casualties = game.casualties - pieces)
    log(s"\nMove the following pieces from CASUALTIES to OUT-OF-PLAY:")
    wrap("  ", pieces.stringItems) foreach (log(_))
  }
  
  // Place pieces from the Out Of Play box in the given map space.
  // There must be enough pieces in the out of play box or an exception is thrown.
  def placePiecesFromOutOfPlay(spaceName: String, pieces: Pieces): Unit = if (pieces.total > 0) {
    assert(game.outOfPlay contains pieces, "Insufficent pieces in the out of play box")
    
    val sp = game.getSpace(spaceName)
    val updated = sp.copy(pieces = sp.pieces + pieces)
    game = game.updateSpace(updated).copy(outOfPlay = game.outOfPlay - pieces)
    log(s"\nPlace the following pieces from OUT-OF-PLAY into $spaceName:")
    wrap("  ", pieces.stringItems) foreach (log(_))
    logControlChange(sp, updated)
    if (pieces.flnBases > 0)
      log(s"Move 'Oppose+Bases' marker to ${game.flnScore}")
  }
  
  def removeToAvailable(spaceName: String, pieces: Pieces, logControl: Boolean = true): Unit = if (pieces.total > 0) {
    val sp = game.getSpace(spaceName)
    assert(sp.pieces contains pieces, s"$spaceName does not contain all requested pieces: $pieces")
    val updated = sp.copy(pieces = sp.pieces - pieces)
    game = game.updateSpace(updated)
    log(s"\nMove the following pieces from $spaceName to AVAILABLE:")
    wrap("  ", pieces.stringItems) foreach (log(_))
    if (logControl)
      logControlChange(sp, updated)
    if (pieces.flnBases > 0)
      log(s"Move 'Oppose+Bases' marker to ${game.flnScore}")
  }

  def removeToCasualties(spaceName: String, pieces: Pieces, logControl: Boolean = true): Unit = if (pieces.total > 0) {
    val sp = game.getSpace(spaceName)
    assert(sp.pieces contains pieces, s"$spaceName does not contain all requested pieces: $pieces")
    val updated = sp.copy(pieces = sp.pieces - pieces)
    // Guerrillas in casualties are always hidden.
    val toCasualties = if (pieces.activeGuerrillas == 0)
      pieces
    else
      pieces.remove(pieces.activeGuerrillas, ActiveGuerrillas).add(pieces.activeGuerrillas, HiddenGuerrillas)
    game = game.updateSpace(updated).copy(casualties = game.casualties + toCasualties)
    log(s"\nMove the following pieces from $spaceName to CASUALTIES:")
    wrap("  ", pieces.stringItems) foreach (log(_))
    if (logControl)
      logControlChange(sp, updated)
    if (pieces.flnBases > 0)
      log(s"Move 'Oppose+Bases' marker to ${game.flnScore}")
  }

  def removeToOutOfPlay(spaceName: String, pieces: Pieces): Unit = if (pieces.total > 0) {
    val sp = game.getSpace(spaceName)
    assert(sp.pieces contains pieces, s"$spaceName does not contain all requested pieces: $pieces")
    val updated = sp.copy(pieces = sp.pieces - pieces)
    // Guerrillas in casualties are always hidden.
    val toOop = if (pieces.activeGuerrillas == 0)
      pieces
    else
      pieces.remove(pieces.activeGuerrillas, ActiveGuerrillas).add(pieces.activeGuerrillas, HiddenGuerrillas)
    
    game = game.updateSpace(updated).copy(outOfPlay = game.outOfPlay + toOop)
    log(s"\nMove the following pieces from $spaceName to OUT-OF-PLAY:")
    wrap("  ", pieces.stringItems) foreach (log(_))
    logControlChange(sp, updated)
    if (pieces.flnBases > 0)
      log(s"Move 'Oppose+Bases' marker to ${game.flnScore}")
  }

  // Move the given pieces from the source space to the destination space
  // and log the activity.
  def movePieces(pieces: Pieces, source: String, dest: String, activateGuerrillas: Boolean = false): Unit = {
    val srcSpace = game.getSpace(source)
    val dstSpace = game.getSpace(dest)
    assert(srcSpace.pieces contains pieces, s"$source does not contain all requested pieces: $pieces")
    
    val finalPieces = if (activateGuerrillas) pieces.activateGuerrillas(pieces.hiddenGuerrillas) else pieces
    val updatedSrc = srcSpace.copy(pieces = srcSpace.pieces - pieces)
    val updatedDst = dstSpace.copy(pieces = dstSpace.pieces + finalPieces)
    game = game.updateSpace(updatedSrc).updateSpace(updatedDst)
    log(s"\nMove the following pieces from $source to $dest:")
    wrap("  ", pieces.stringItems) foreach (log(_))
    if (activateGuerrillas && pieces.hiddenGuerrillas > 0)
      log(s"Activate the ${hiddenG(pieces.hiddenGuerrillas)}")
    logControlChange(srcSpace, updatedSrc)
    logControlChange(dstSpace, updatedDst)
  }
  
  // Similar to movePieces() but we do not log any change in control
  // of the space.
  def redeployPieces(pieces: Pieces, source: String, dest: String): Unit = {
    val srcSpace = game.getSpace(source)
    val dstSpace = game.getSpace(dest)
    assert(srcSpace.pieces contains pieces, s"$source does not contain all requested pieces: $pieces")
    
    val updatedSrc = srcSpace.copy(pieces = srcSpace.pieces - pieces)
    val updatedDst = dstSpace.copy(pieces = dstSpace.pieces + pieces)
    game = game.updateSpace(updatedSrc).updateSpace(updatedDst)
    log(s"\nRedeploy the following pieces from $source to $dest:")
    wrap("  ", pieces.stringItems) foreach (log(_))
  }
  
  // Remove combat losses from the board to the appropriate box(es)
  // and log the results.
  // bases are removed to available
  // guerrillas are alternately removed to available and casualties
  // hidden guerrillas may be taken as losses in some cases (capabilities, etc.)

  def removeLosses(spaceName: String, lostPieces: Pieces): Unit = removeLosses(Seq(spaceName -> lostPieces))
  
  def removeLosses(losses: Seq[(String, Pieces)]): Unit = {
    // The removed guerrillas alternately go to AVAILABLE and CASUALTIES.
    // This is cumulative among all spaces.  (This is to support neutralize)
    // During assault, we remove losses one space at a time.
    val totalGuerrillasLost = losses.foldLeft(0) { case (sum, (_, p)) => sum + p.totalGuerrillas }
    var gToAvailable = (totalGuerrillasLost + 1) / 2
    var gToCasualties = totalGuerrillasLost / 2
    
    for ((spaceName, lostPieces) <- losses) {
      // log(s"\nLosses for $spaceName:")
      // wrap("  ", lostPieces.stringItems) foreach (log(_))
      val orig = game.getSpace(spaceName)
      removeToCasualties(spaceName, lostPieces.only(GOV_PIECES), logControl = false)
      decreaseCommitment(lostPieces.numOf(GovBases))
      
      val activeToAvailable  = gToAvailable min lostPieces.activeGuerrillas
      gToAvailable -= activeToAvailable
      val hiddenToAvailable  = gToAvailable min lostPieces.hiddenGuerrillas
      gToAvailable -= hiddenToAvailable
      val activeToCasualties = gToCasualties min (lostPieces.activeGuerrillas - activeToAvailable)
      gToCasualties -= activeToCasualties
      val hiddenToCasualties = gToCasualties min (lostPieces.hiddenGuerrillas - hiddenToAvailable)
      gToCasualties -= hiddenToCasualties

      val flnToAvail = lostPieces.only(FlnBases) + 
                       Pieces(hiddenGuerrillas = hiddenToAvailable,  activeGuerrillas = activeToAvailable)
      val flnToCasua = Pieces(hiddenGuerrillas = hiddenToCasualties, activeGuerrillas = activeToCasualties)
      removeToAvailable(spaceName,  flnToAvail, logControl = false)
      removeToCasualties(spaceName, flnToCasua, logControl = false)
      increaseCommitment(lostPieces.numOf(FlnBases))
      
      val updated = game.getSpace(spaceName)
      logControlChange(orig, updated)
    }
  }
  

  def inspect[T](name: String, value: T): T = {
    println(s"DEBUG: $name == ${value.toString}")
    value
  }
  
  // Format the given sequence of strings in a comma separated list 
  // such that we do not exceed the given number of columns.
  def wrap(prefix: String, values: Seq[String], columns: Int = 78): Seq[String] = {
    val b = new ListBuffer[String]
    val s = new StringBuilder(prefix)
    var first = true
    if (values.isEmpty)
      s.append("none")
    else {
      val margin = " " * prefix.length
      s.append(values.head)
      for (v <- values.tail) {
        s.append(", ")
        if (s.length + v.length < columns)
          s.append(v)
        else {
          b += s.toString
          s.clear()
          s.append(margin).append(v)
        }
      }
    }
    b += s.toString
    b.toList
  }
  
  def pause(): Unit = {
    import scala.util.Properties.isWin
    if (isWin)
      readLine("Press Enter to continue... ")
    else
      readLine("Continue ↩︎ ")
  }
  
  var echoLogging = true
  // Print the line to the console and save it in the game's history.
  def log(line: String = ""): Unit = {
    if (echoLogging)
      println(line)
    game = game.copy(history = game.history :+ line)
  }
  
  def separator(length: Int = 52, char: Char = '-'): String = char.toString * length
  

  def printSummary(summary: Seq[String]): Unit = {
    println()
    summary foreach println
  }
  
  def logSummary(summary: Seq[String]): Unit = {
    log()
    summary foreach (log(_))
  }
  
  def padLeft(x: Any, width: Int) = "%%-%ds".format(width).format(x.toString)
  
  // Get ordinal number.  Good for 1 to 20.
  def ordinal(i: Int): String = i match {
    case 1 => "1st"
    case 2 => "2nd"
    case 3 => "3rd"
    case x if x > 20 => throw new IllegalArgumentException("ordinal() only good for numbers <= 20")
    case x => s"${x}th"
  }
  
  
  def initialGameState(scenario: Scenario, finalSupport: Boolean) = {
    var spaces = DefaultSpaces
    // Apply scenario overrides to countries.
    for (sp <- scenario.spaces)
      spaces = sp :: (spaces filterNot (_.name == sp.name))
    
    GameState(
      GameParameters(scenario.name, finalSupport),
      0, // Turn number, zero indicates start of game.
      scenario.numberOfPropCards,
      spaces,
      scenario.franceTrack,
      scenario.borderZoneTrack,
      scenario.commitment,
      scenario.resources,
      outOfPlay = scenario.outOfPlay,
      pivotalCardsPlayed = scenario.pivotalCardsPlayed)
  }
  
  
  val scenarios = ListMap[String, Scenario](
    "Short"  -> new Short,
    "Medium" -> new Medium,
    "Full"   -> new Full
  )
  val scenarioChoices = scenarios.toList map { case (key, scenario) => key -> scenario.name }
  
  // Case sensitive
  def isValidScenario(name: String) = scenarios contains name
  
  val AbortActionString = "abort action"
  case object ExitGame extends Exception
  case object AbortAction extends Exception
  case object Rollback extends Exception
  
  def main(args: Array[String]): Unit = {
    import scenarios._
    val versionSuffix = if (SOFTWARE_VERSION.startsWith("0")) " - BETA" else ""
    val versionDisplay = s"Colonial Twilight Bot Software (version $SOFTWARE_VERSION$versionSuffix)"
    val versionFlags = Set("-v", "-version", "--version", "version")
    
    if (args.nonEmpty && versionFlags.contains(args(0).toLowerCase)) {
      println(versionDisplay)
      System.exit(0)      
    }
    
    println()
    println(versionDisplay)
    println(separator())
    
    try {
      gamesDir.mkpath()

      askWhichGame() match {
        case Some(name) =>
          loadMostRecent(name)
      
        case None => // Start a new game
          println()
          val scenarioName = {
            // prompt for scenario
            println("Choose a scenario:")
            val choices = scenarioChoices :+ ("quit" -> "Quit")
            askMenu(choices, allowAbort = false).head match {
              case "quit"   => throw ExitGame
              case scenario => scenario
            }
          }
          val scenario = scenarios(scenarioName)
          
          val choices = List(
            false -> "Standard Rules      - No Support Phase in final Propaganda round",
            true  -> "Optional Rule 8.5.1 - Conduct Support Phase in final Propaganda round")
          println()
          println("Choose one:")
          val finalSupport = askMenu(choices, allowAbort = false).head
          
          println()
          gameName = Some(askGameName("Enter a name for your new game: "))

          game = initialGameState(scenario, finalSupport)
          logSummary(game.scenarioSummary)
          log()
          scenario.additionalSetup()
      }
      mainLoop()
    }
    catch {
      case ExitGame => 
    }
  }
  
  // Ask which saved game the user wants to load.
  // Return None if they wish to start a new game.
  def askWhichGame(): Option[String] = {
    val games = savedGames
    if (games.isEmpty)
      None
    else {
      val gameChoices = games.toList map { name =>
        val desc = loadGameDescription(name)
        val suffix = if (desc == "") "" else s": $desc"
        name -> s"Resume '$name'$suffix"
      }
      val choices = ("--new-game--" -> "Start a new game") :: gameChoices ::: List("--quit-game--" -> "Quit")
      println()
      println("Which game would you like to play:")
      askMenu(choices, allowAbort = false).head match {
        case "--new-game--"  => None
        case "--quit-game--" => throw ExitGame
        case name            => Some(name)
      }
    }
  }
  
  // We assume that the current working directory
  // set as the installed directory and thus the game directory
  // is in ./games.  The script used to invoke the program will
  // ensure that is the case.
  val gamesDir = Pathname("./games")
  var gameName: Option[String] = None // The name of sub-directory containing the game files
  
  val TURN = """turn-(\d+)""".r
  
  def gameFilePath(filename: String): Pathname = {
    assert(gameName.nonEmpty, "gameFilePath(): called with gameName not set!")
    gamesDir/gameName.get/filename
  }
  
  def turnFilename(num: Int): String = s"turn-$num"
  
  def gameDescPath(nameOfGame: String): Pathname = gamesDir/nameOfGame/"description"
  
  
  def turnFileNumber(path: Pathname): Int = path.basename.toString match {
    case TURN(n) => n.toInt
    case _ => throw new IllegalArgumentException(s"turnFileNumber(): Invalid turn file: $path")
  }
  
  def saveTurn(): Unit = {
    // saveGameState(gameFilePath(turnFilename(game.turn)), game)
    SavedGame.save(gameFilePath(turnFilename(game.turn)), game)
  }
  
  // Save a brief description of the game.
  // The descriptions are used by the askWhichGame() function.
  def saveGameDescription(turnsCompleted: Int): Unit = {
    val desc = s"${game.params.scenarioName}, ${amountOf(turnsCompleted, "turn")} completed"
    gameDescPath(gameName.get).writeFile(desc)
  }
  
  def loadGameDescription(nameOfGame: String): String = {
    val path = gameDescPath(nameOfGame)
    if (path.exists)
      path.readFile()
    else
      ""
  }
  
  // Load the most recent game file for the given game.
  def loadMostRecent(name: String): Unit = {
    val filename = mostRecentSaveFile(name) getOrElse {
      throw new IllegalStateException(s"No saved file found for game '$name'")
    }
    gameName = Some(name)
    game = SavedGame.load(gameFilePath(filename))
    // game = loadGameState(gameFilePath(filename))
  }

  // Return the list of saved games
  def savedGames: Seq[String] =
    gamesDir.children(withDirectory = false) map (_.toString) filter { name =>
      mostRecentSaveFile(name).nonEmpty 
    }

  // Given a directory for a saved game finds the most recent save file.
  def mostRecentSaveFile(name: String): Option[String] = {
    case class Entry(number: Int, filename: String)
    val dir = gamesDir/name
    if (dir.isDirectory) {
      val entries = dir.children(withDirectory = false) map { child =>
        child.toString match {
          case name @ TURN(n) => Entry(n.toInt, name)
          case _              => Entry(-1, "")
        }
      }
      (entries filter (_.number >= 0)).sortBy(-_.number).headOption map (_.filename)
    }
    else
      None
  }

  val VALID_NAME = """([-A-Za-z0-9_ ]+)""".r

  // Ask the user for a name for their new game.
  def askGameName(prompt: String): String = {
    def getName: String = {
      readLine(prompt) match {
        case null => getName
        case VALID_NAME(name) =>
          if ((gamesDir/name).exists) {
            println(s"A game called '$name' already exists.")
            if (askYorN(s"Do you want to overwrite the existing game (y/n)? ")) {
              (gamesDir/name).rmtree()
              name
            }
            else
              getName
          }
          else
            name
        case name => 
          println("The name must consist of one or more letters, numbers, spaces, dashes or undercores")
          getName
      }
    }
    getName
  }

  def safeToInt(str: String): Option[Int] = try Some(str.toInt) catch { case e: NumberFormatException => None }
  
  // ---------------------------------------------
  // Process all top level user commands.
  @tailrec def mainLoop(): Unit = {
    saveTurn()  // Save the current game state
    saveGameDescription(turnsCompleted = game.turn)
    game = game.copy(turn = game.turn + 1)
    try {
        
      def getNextCard(): Unit = {
        
        val common  = List(ShowCmd, HistoryCmd, RollbackCmd, AdjustCmd)
        val cmds    = List(PlayCardCmd)
        val opts = orList((cmds map (_.name)) :+ "?")
        val prompt  = s"""
                      |>>> Turn ${game.turn}  (Event card not yet played) <<<
                      |${separator()}
                      |Command ($opts): """.stripMargin
        
        val (cmd, param) = askCommand(prompt, cmds ::: common)
        cmd match {
          case PlayCardCmd =>
            val cardNum = param flatMap safeToInt orElse askCardNumber("Enter the card number: ")
            cardNum  match {
              case None => getNextCard()
              case Some(num) if !deck.isValidCardNumber(num) =>
                println(s"'$num' is not a valid card number")
                getNextCard()
              case Some(num) =>
                game = game.copy(currentCard = Some(num))
                log()
                log(s"Turn #${game.turn}")
                log(separator(char = '='))
                log(s"Event card: ${deck(num)}")
                if (!game.isPropRound)
                  log(s"${game.sequence.firstEligible} is first eligible")
                log(separator(char = '='))
            }
          
          case _ =>
            cmd.action(param)
            getNextCard()
        }
      }
      
      getNextCard()
      
      val newSequence = if (game.isPropRound) {
        game = game.copy(propCardsPlayed = game.propCardsPlayed + 1)
        resolvePropagandaCard()
        SequenceOfPlay()
      }
      else {
        resolveEventCard()
        game.sequence.reset()
      }
      
      log()
      log(s"Place the ${newSequence.firstEligible} cylinder in the 1st eligible box")
      if (game.sequence.secondEligible != newSequence.secondEligible)
        log(s"Place the ${newSequence.secondEligible} cylinder in the 2nd eligible box")
      game = game.copy(sequence = newSequence, previousCard = game.currentCard, currentCard = None)
    }
    catch {
      case Rollback => // We continue with the previously saved game state
    }
    mainLoop()
  }
  
  trait Command {
    val name: String
    val desc: String
    val action: (Option[String]) => Unit
  }
  
  object GovCmd extends Command {
    val name = "gov"
    val desc = "Take an action on the current card"
    val action = (_: Option[String]) => Human.act()
  }
  
  object PivotCmd extends Command {
    val name = "pivot"
    val desc = "Play a pivotal event card"
    val action = (_: Option[String]) => doPivot(Gov)
  }
  
  object PlayCardCmd extends Command {
    val name = "card"
    val desc = """Play the next event card
                 |  card #    - Enter the number of the event card
                 |  card      - You will be prompted for the card number
               """.stripMargin
    val action = (param: Option[String]) => ()
  }
  
  object BotCmd extends Command {
    val name = "fln"
    val desc = s"The FLN Bot acts on the current card"
    val action = (_: Option[String]) => Bot.act()
  }

  object ShowCmd extends Command {
    val name = "show"
    val desc = """Display the current game state
                 |  show scenario  - current score and difficulty level
                 |  show status    - current score, resources, etc.
                 |  show available - pieces that are currently available
                 |  show events    - pivotal events, capabilies and momentum events in play
                 |  show sequence  - 1st eligible, 2nd eligible and actions taken
                 |  show all       - entire game state
                 |  show <space>   - state of a single space""".stripMargin
    val action = (param: Option[String]) => showCommand(param)
  }
  
  object HistoryCmd extends Command {
    val name = "history"
    val desc = """Display game history
                 |  history       - Shows the log from the beginning of the current turn
                 |  history -1    - Shows the log from the beginning of the previous turn
                 |  history -n    - Shows the log from the beginning of the turn n turns ago
                 |  history 1     - Shows the log for the first turn
                 |  history n     - Shows the log for the nth turn
                 |  history 1..3  - Shows the log for the first through third turns
                 |  history 5..   - Shows the log from the fifth turn through the end
                 |  history ..5   - Shows the log from the beginning through the fifth turn
                 |  history all   - Shows the entire log
                 |  Any of the above commands may be followed by >filename
                 |  to save the history in a file instead of echoing it to the console""".stripMargin
    val action = (param: Option[String]) => history(param)
  }
  
  object AdjustCmd extends Command {
    val name = "adjust"
    val desc = """Adjust game settings  (Minimal rule checking is applied)
                              |  adjust gov resources - Current government resource level
                              |  adjust fln resources - Current FLN resource level
                              |  adjust commitment    - Government commitment level
                              |  adjust france track  - Current position of the Franch track
                              |  adjust border zone   - Current position of the Border Zone track
                              |  adjust casualties    - Pieces in the casualties box
                              |  adjust pivotal       - Pivotal cards in play
                              |  adjust out of play   - Pieces in the out of play box
                              |  adjust capabilities  - Capabilities currently in play
                              |  adjust momentum      - Momentum events currently in play
                              |  adjust bot debug     - Toggle debug output of bot logic
                              |  adjust <space>       - Space specific settings""".stripMargin
    val action = (param: Option[String]) => adjustSettings(param)
  }
    
  object RollbackCmd extends Command {
    val name = "rollback"
    val desc = "Roll back to the start of any turn"
    val action = (param: Option[String]) => rollback(param)
  }
  
  object QuitCmd extends Command {
    val name = "quit"
    val desc = "Quit the game.  All plays for the current turn will be saved."
    val action = (_: Option[String]) => if (askYorN("Really quit (y/n)? ")) throw ExitGame
  }
  
  object HelpCmd extends Command {
    val name = "help"
    val desc = "List available commands"
    val action = (_: Option[String]) => () // Handled by the command processing routine.
  }
  
  def performPass(role: Role): Unit = {
    increaseResources(role, if (role == Gov) 2 else 1)
  }
  

  def doPivot(role: Role): Unit = {
    val other = Role.opposite(role)
    
    val pivotCard = if (role == Fln) {
      // The FLN bot will only ever play the Morocco and Tunisia Independent pivotal event.
      Some(deck(PivotalMoroccoTunisiaIndepdent))
    }
    else {
      val choices = game.govPivotalPlayable.toList.sorted.map(n => n -> s"Play ${deck(n).numAndName}") ::: 
                    List(0 -> "Do not play a pivotal event")
                    
      askMenu(choices, allowAbort = false).head match {
        case 0 => None
        case n => Some(deck(n))
      }
    }
    
    pivotCard foreach { card =>
      log()
      log(s"The $role plays pivotal event: ${card.numAndName}")
      log(s"Place the ${card.name} card on top of the discard pile")
      log(s"Place the ${role} eligibility cylinder in the ${Event} box")
      if (game.sequence.secondEligible != other)
        log(s"Place $other cylinder on the Second Eligible space on the sequence track")
      log()
      game = game.copy(sequence           = SequenceOfPlay(firstEligible = role, secondEligible = other, firstAction = Some(Event)),
                       previousCard       = game.currentCard,
                       currentCard        = Some(card.number),
                       pivotalCardsPlayed = game.pivotalCardsPlayed + card.number)
      card.executeUnshaded(role)
     
    }
  }
  
  // Allows the user to roll back to the beginning of any turn.
  def rollback(input: Option[String]): Unit = {
    val NUMBER = """(\d+)""".r
    val inputNum = input flatMap {
      case NUMBER(n) if n.toInt > 0 && n.toInt <= game.turn => Some(n.toInt)
      case _ => None
    }
    
    try {
      val turnNumber = inputNum getOrElse {
        println("\nRollback to the beginning of a previous turn")
        askInt("Enter turn number", 1, game.turn)
      }
      
      if (askYorN(s"Are you sure you want to rollback to turn $turnNumber? (y/n) ")) {
        // Games are saved at the end of the turn, so we actually want
        // to load the file with turnNumber -1.
        val newGameState = SavedGame.load(gameFilePath(turnFilename(turnNumber - 1)))
        // val newGameState = loadGameState(gameFilePath(turnFilename(turnNumber - 1)))
        removeTurnFiles(turnNumber)      
        displayGameStateDifferences(game, newGameState)
        game = newGameState
        throw Rollback
      }
    }
    catch {
      case AbortAction =>
    }
  }
  
  // Remove turn files starting with the given number an all 
  // those that follow the number.
  def removeTurnFiles(num: Int = 0): Unit = {
    import Pathname.glob    
    val turnFiles = glob(gamesDir/gameName.get/"turn*")
    turnFiles filter (turnFileNumber(_) >= num) foreach (_.delete())
  }
  

  @tailrec def resolveEventCard(): Unit = {
    val card = deck(game.currentCard.get)
    // Check to see if the Bot will play the "Morocco and Tunisia Independent" pivotal event.
    // FLN Bot never plays Suez Crisis or OAS pivotal events.
    val flnPivots = (game.pivotalCardsPlayed(PivotalMobilization)) &&
                    !game.moroccoTunisiaIndependent                &&
                    !deck.isPivotalCard(card.number)               &&
                    game.sequence.numActed == 0                    &&
                    game.sequence.secondEligible == Fln            &&
                    card.markedForFLN
    
    if (flnPivots)
      doPivot(Fln)
    else {
      val (role, desc) = game.sequence.numActed match {
        case 0 =>
          val role = game.sequence.firstEligible
          (role, s"${role} is up (1st eligible, ${game.resources(role)} resources)")
        case 1 =>
          val role = game.sequence.secondEligible
          (role, s"${role} is up (2nd eligible, ${game.resources(role)} resources)")
        case _ => throw new IllegalStateException("resolveEventCard() two actions already completed")
      }
    
      val govCanPivot = game.sequence.numActed == 0 && game.govPivotalPlayable.nonEmpty
      val common  = List(ShowCmd, HistoryCmd, RollbackCmd, AdjustCmd)
      val cmds = List(
        if (role == Fln) Some(BotCmd) else None,
        if (role == Gov) Some(GovCmd) else None,
        if (govCanPivot) Some(PivotCmd) else None).flatten
      val opts = orList((cmds map (_.name)) :+ "?")
      val prompt  = s"""
                    |>>> Turn ${game.turn}  (${card.numAndName}) <<<
                    |${separator()}
                    |$desc
                    |Command ($opts): """.stripMargin
    
    
      val (cmd, param) = askCommand(prompt, cmds ::: common)
      cmd.action(param)
    }
    
    if (game.sequence.numActed < 2)
      resolveEventCard()
  }
  
  def resolvePropagandaCard(): Unit = {
    val finalPropRound = game.propCardsPlayed == game.numberOfPropCards
    def logGameOver(): Unit = {
      // FLN wins ties
      val winner = if (!finalPropRound || game.flnMargin >= game.govMargin) Fln else Gov
      val totalSupport = game.totalOnMap(_.supportValue)
      val totalOppose  = game.totalOnMap(_.opposeValue)
      val flnBases     = game.totalOnMap(_.flnBases)
      log("\n\n")
      log(separator(char = '='))
      if (finalPropRound)
        log("Final Propaganda card")
      else
        log(s"$Fln has achieved its victory margin")
      log(separator(char = '='))
      log(s"\nGame over: $winner wins!")
      log(separator())
      log(f"Government score: ${game.govScore}%2d (${game.govMargin}%+3d) | support   : $totalSupport%2d, commitment: ${game.commitment}%2d")
      log(f"FLN score       : ${game.flnScore}%2d (${game.flnMargin}%+3d) | opposition: $totalOppose%2d, bases     : $flnBases%2d")
    }
    
    log(separator(char = '='))
    log(s"${ordinal(game.propCardsPlayed)} Propaganda Round")
    log(separator(char = '='))
    
    if (game.isConsecutivePropCard) {
      log("\nConsecutive Propagand cards.  Propaganda round skipped.")
      if (finalPropRound)
        logGameOver()
    }
    else if ((game.propCardsPlayed > 1 && game.flnMargin > 0)) {
      log("\nVictory Phase")
      log(separator())
      logGameOver()
    }
    else {
      log("\nVictory Phase")
      log(separator())
      if (game.propCardsPlayed == 1)
        log("Skipped")
      else
        log("Neither side has achieved vitory")

      log("\nResources Phase")
      log(separator())
      val franceEntry  = FranceTrack(game.franceTrack)
      val totalSupport = if (game.recallDeGaulleInEffect) game.totalOnMap(_.supportValue) else 0
      val frenchAvail  = if (game.recallDeGaulleInEffect) 0 else game.availablePieces.totalOf(FRENCH_PIECES)
      val govBasePop   = game.totalOnMap(sp => if (sp.isSector && sp.isGovControlled && sp.govBases > 0) sp.population else 0)
      val govResAmt    = game.commitment + totalSupport + frenchAvail + govBasePop - game.resettledSectors
      val flnBases     = game.totalOnMap(_.flnBases)
      val flnResAmt    = flnBases + franceEntry.resource - game.borderZoneTrack
      if (govResAmt > 0) increaseResources(Gov, govResAmt) else decreaseResources(Gov, -govResAmt)
      log(f"  Commitment level    : ${game.commitment}%+3d")
      log(f"  Bases at Gov control: ${govBasePop}%+3d")
      if (game.recallDeGaulleInEffect)
        log(f"  Total support       : ${totalSupport}%+3d")
      else
        log(f"  French available    : ${frenchAvail}%+3d")
      log(f"  Resettled sectors   : ${-game.resettledSectors}%+3d")
      log()
      if (flnResAmt > 0) increaseResources(Fln, flnResAmt) else decreaseResources(Fln, -flnResAmt)
      log(f"  FLN bases           : ${flnBases}%+3d")
      log(f"  France Track        : ${franceEntry.resource}%+3d")
      log(f"  Bordor Zone Status  : ${-game.borderZoneTrack}%+3d")
        
      log("\nCommitment Adjustment Phase")
      log(separator())
      val numOop        = game.outOfPlay.totalOf(FRENCH_PIECES)
      val numAvailable  = game.availablePieces.totalOf(FRENCH_PIECES)
      val numCasualties = game.casualties.totalOf(FRENCH_PIECES)
      if (numOop > 0) {
        val num = askInt("\nMove how many French pieces from Out of Play to Available", 0, numOop, allowAbort = false)
        if (num > 0) {
          val pieces = askPieces(game.outOfPlay, num, FRENCH_PIECES, allowAbort = false)
          movePiecesFromOutOfPlayToAvailable(pieces)
          decreaseCommitment((num + 1) / 3)
        }
      }
      if (numAvailable > 0) {
        val num = askInt("\nMove how many French pieces from Available to Out of Play", 0, numAvailable, allowAbort = false)
        if (num > 0) {
          val pieces = askPieces(game.availablePieces, num, FRENCH_PIECES, allowAbort = false)
          movePiecesFromAvailableToOutOfPlay(pieces)
          increaseCommitment(num / 3)
        }
      }
      if (numCasualties > 2 && !game.recallDeGaulleInEffect) {
        log("\nCommitment is affected by French casualties")
        decreaseCommitment(numCasualties / 3)
      }
      if (franceEntry.commit > 0) {
        log("\nCommitment is affected by the France track marker")
        decreaseCommitment(franceEntry.commit)
      }
      
      // Game ends after commitiment adjustment in the final prop round
      if (finalPropRound && !game.params.finalPropSupport)
        logGameOver()
      else {
        log("\nSupport Phase")
        log(separator())
        Human.propSupportPhase()
        Bot.propAgitatePhase()
        
        if (finalPropRound)
          logGameOver()  // Will only get here if game.params.finalPropSupport == false
        else {
          log("\nRedeploy Phase")
          log(separator())
          // Don't adjust control until both sides have finished redeploy!!!
          val preRedeployState = game
          Human.propRedeployPhase(preRedeployState)
          Bot.propRedeployPhase()
          // Now log all of the changes in control.
          log()
          for (sp <- preRedeployState.spaces; updated = game.getSpace(sp.name))
            logControlChange(sp, updated)
      
          log("\nReset Phase")
          log(separator())
          val govCasualtiesOop = (game.casualties.frenchCubes + game.casualties.govBases) / 3
          val flnCasualtiesOop = game.casualties.hiddenGuerrillas / 3

          val govPieces = askPieces(game.casualties, govCasualtiesOop, FRENCH_PIECES, allowAbort = false,
              heading = Some(s"\nYou must move ${amountOf(govCasualtiesOop, "French piece")} from casualties to Out of Play"))
          movePiecesFromCasualtiesToOutOfPlay(govPieces)
          movePiecesFromCasualtiesToAvailable(game.casualties.only(GOV_PIECES))
      
          movePiecesFromCasualtiesToOutOfPlay(Pieces(hiddenGuerrillas = flnCasualtiesOop))
          movePiecesFromCasualtiesToAvailable(game.casualties.only(HiddenGuerrillas))
        
          if (game.franceTrack > 0)
            decreaseFranceTrack(1)
          if (game.borderZoneTrack > 0)
            decreaseBorderZoneTrack(1)
          for (sp <- game.algerianSpaces filter (_.terror > 0))
            removeTerror(sp.name, 1)
          for (sp <- game.spaces filter (_.activeGuerrillas > 0))
            hideActiveGuerrillas(sp.name, sp.activeGuerrillas)
        
          for (m <- game.momentum)
            log(s"Discard momentum card: $m")

          // Remove all momenttum cards
          // Remove the Coup d'etat card from the list of pivotal cards played
          // because it may be played once per campaign.
          game = game.copy(momentum = Nil, 
                          pivotalCardsPlayed = game.pivotalCardsPlayed filterNot (_ == PivotalCoupdEtat))
        }
      }
    }
  }

  
  // We always append the Help and Quit commands.
  // Returns the selected command and any extra user input that was given.
  @tailrec def askCommand(prompt: String, callerCmds: Seq[Command]): (Command, Option[String]) = {
    val cmds = callerCmds ++ List(HelpCmd, QuitCmd)
    readLine(prompt) match {
      case null =>  // EOF, user pressed ctrl-d (ctrl-Z on Windoze)
        println()
        askCommand(prompt, callerCmds)
      case input =>
        val tokens = input.split("\\s+").toList.dropWhile(_ == "")
        tokens match {
          case Nil => askCommand(prompt, callerCmds)  // User did not enter a command
          case verb :: rest =>
            val param = if (rest.nonEmpty) Some(rest.mkString(" ")) else None
            matchOne(verb, cmds map (_.name)) match {
              case None => askCommand(prompt, callerCmds)  // Invalid command entered
              case Some(name) =>
                cmds find (_.name == name) match {
                  case None => throw new IllegalStateException(s"Internal error: Command '$name' is not valid")
                  case Some(cmd) if cmd == HelpCmd =>
                    showHelp(cmds, param)
                    askCommand(prompt, callerCmds)
                  case Some(cmd) => (cmd, param)
                }
            }
        }
    }
  }
  
  def showHelp(cmds: Seq[Command], param: Option[String]): Unit = {
    val names = cmds map (_.name)
    param match {
      case None =>
        println("Available commands: (type help <command> for more detail)")
        println(orList(names))
      case Some(p) =>
        for (name <- matchOne(p, names); cmd <- cmds find (_.name == name))
          println(cmd.desc)
    }
  }


  // Display some or all of the game log.
  // usage:
  //   history        ##  Shows the log from the beginning of the current turn
  //   history -1     ##  Shows the log from the beginning of the previous turn
  //   history -n     ##  Shows the log from the beginning of the turn n turns ago
  //   history 1      ##  Shows the log for the first turn
  //   history n      ##  Shows the log for the nth turn
  //   history 1..3   ##  Shows the log for the first through third turns
  //   history 5..    ##  Shows the log from the fifth turn through the end
  //   history ..5    ##  Shows the log from the beginning through the fifth turn
  //   history all    ##  Shows the entire log
  def history(input: Option[String]): Unit = {
    val POS = """(\d+)""".r
    val NEG = """-(\d+)""".r
    val PRE = """\.\.(\d+)""".r
    val SUF = """(\d+)\.\.""".r
    val RNG = """(\d+)\.\.(\d+)""".r
    val ALL = "all"
    case class Error(msg: String) extends Exception
    try {
      def redirect(tokens: List[String]): Option[String] = {
        tokens match {
          case Nil => None
          case x::xs  if !(x startsWith ">") => None
          case ">":: Nil => throw Error("No filename specified after '>'")
          case ">"::file::xs => Some(file)
          case file::xs => Some(file drop 1)
        }
      }
      
      val tokens = (input getOrElse "" split "\\s+").toList map (_.toLowerCase) dropWhile (_ == "")
      val (param, file) = if (tokens.isEmpty)
        (None, None)
      else if (!(tokens.head startsWith ">"))
          (tokens.headOption, redirect(tokens.tail))
      else
        (None, redirect(tokens))
    
      // We have a special case where the game.turn has been incremented, but if a 
      // card has not yet been played for the current turn, then there will not be
      // at "Turn #" entry in the log for the current turn.
      // In this case we will act as if the turn number is still the previous turn.
      val currentTurn = if (game.history exists (line => line startsWith s"Turn #${game.turn}"))
        game.turn
      else
        game.turn - 1
      def normalize(n: Int) = 0 max n min (currentTurn + 1)
      val START = 0
      val END   = currentTurn + 1
      val (start, end) = param match {
        case None                                   => (currentTurn, currentTurn + 1)
        case Some(POS(n))                           => (normalize(n.toInt), normalize(n.toInt + 1))
        case Some(NEG(n))                           => (normalize(currentTurn - n.toInt), END)
        case Some(PRE(e))                           => (START, normalize(e.toInt + 1))
        case Some(SUF(s))                           => (normalize(s.toInt), END)
        case Some(RNG(s, e)) if (e.toInt < s.toInt) => (normalize(e.toInt), normalize(s.toInt + 1))
        case Some(RNG(s, e))                        => (normalize(s.toInt), normalize(e.toInt + 1))
        case Some("all" | "al" | "a")               => (START, END)
        case Some(p)                                => throw Error(s"Invalid parameter: $p")
      }
      
      val SOT = """Turn #\s*(\d+).*""".r
      def turnIndex(num: Int): Int = {
        val turnMatch = (x: String) => x match {
          case SOT(n) if n.toInt == num => true
          case _ => false
        }
        if (num == 0)             0
        else if (num > currentTurn) game.history.size
        else                        game.history indexWhere turnMatch
      } 
      val ignore = turnIndex(start)
      val length = turnIndex(end) - ignore
      val logs = game.history drop ignore take length
      file match {
        case None => logs foreach println
        case Some(fname) =>
          Pathname(fname).writer { w =>
            logs foreach { log =>
              w.write(log)
              w.write(lineSeparator)
            }
          }
      }
    }
    catch {
      case e: IOException => println(s"IOException: ${e.getMessage}")
      case Error(msg) => println(msg)
    }
  }
  
  def showCommand(param: Option[String]): Unit = {
    val options =  "scenario" :: "status" :: "available" :: "casualties" :: "out of play" ::
                   "events" :: "sequence" :: "all" :: SpaceNames
                  
    askOneOf("Show (? for options): ", options, param, allowNone = true, allowAbort = false) foreach {
      case "scenario"     => printSummary(game.scenarioSummary)
      case "status"       => printSummary(game.statusSummary) 
      case "available"    => printSummary(game.availablePiecesSummary)
      case "casualties"   => printSummary(game.casualtiesSummary)
      case "out of play"  => printSummary(game.outOfPlaySummary)
      case "events"       => printSummary(game.eventSummary)
      case "sequence"     => printSummary(game.sequenceSummary)  // card, 1st eligible, etc.
      case "all"          => printGameState()
      case name           => printSummary(game.spaceSummary(name))
    }
  }
  
  def printGameState(): Unit = {
    printSummary(game.scenarioSummary)
    printSummary(game.statusSummary)
    printSummary(game.availablePiecesSummary)
    printSummary(game.casualtiesSummary)
    printSummary(game.outOfPlaySummary)
    printSummary(game.eventSummary)
    printSummary(game.sequenceSummary)
         
    for (sp <- game.spaces.sorted; line <- game.spaceSummary(sp.name))
      println(line)
  }
  
    
  def adjustSettings(param: Option[String]): Unit = {
    val options = List("gov resources", "fln resources", "commitment", "france track", "border zone",
                       "casualties", "out of play", "pivotal", "capabilities", "momentum", "bot debug",
                       "final prop support"
                       ).sorted ::: SpaceNames

    val choice = askOneOf("[Adjust] (? for list): ", options, param, allowNone = true, allowAbort = false)
    choice foreach {
      case "gov resources"      => adjustGovResources()
      case "fln resources"      => adjustFlnResources()
      case "commitment"         => adjustCommitment()
      case "france track"       => adjustFranceTrack()
      case "border zone"        => adjustBorderZoneTrack()
      case "casualties"         => adjustCasualties()
      case "out of play"        => adjustOutOfPlay()
      case "pivotal"            => adjustPivotal()
      case "capabilities"       => adjustCapabilities()
      case "momentum"           => adjustMomentum()
      case "bot debug"          => adjustBotDebug()
      case "final prop support" => adjustFinalPropSupport()
      case name                 => adjustSpace(name)
    }
  }
  
  def logAdjustment(name: String, oldValue: Any, newValue: Any): Unit = {
    def normalize(value: Any) = value match {
      case None                       => "none"
      case Some(x)                    => x.toString.trim
      case true                       => "yes"
      case false                      => "no"
      case s: Seq[_] if s.isEmpty     => "none"
      case s: Seq[_]                  => s map (_.toString) mkString ", "
      case x if x.toString.trim == "" => "none"
      case x                          => x.toString.trim
    }
    log(s"$name adjusted from [${normalize(oldValue)}] to [${normalize(newValue)}]")
  }
  
  def logAdjustment(spaceName: String, attributeName: String, oldValue: Any, newValue: Any): Unit =
    logAdjustment(s"$spaceName: $attributeName", oldValue, newValue)
  
  def adjustInt(name: String, current: Int, range: Range): Option[Int] = {
    val prompt = s"$name is $current.  Enter new value (${range.min} - ${range.max}) "
    @tailrec def getResponse(): Option[Int] =
      readLine(prompt) match {
        case null | "" => None
        case INTEGER(x) if range contains x.toInt => Some(x.toInt)
        case input =>
          println(s"$input is not valid")
          getResponse()
      }
    getResponse()
  }
  
  def adjustGovResources(): Unit = { 
    val label = "Government resources"
    adjustInt(label, game.resources.gov, 0 to EdgeTrackMax) foreach { value =>
      logAdjustment(label, game.resources.gov, value)
      game = game.copy(resources = game.resources.copy(gov = value))
    }
  }
  
  def adjustFlnResources(): Unit = { 
    val label = "FLN resources"
    adjustInt(label, game.resources.fln, 0 to EdgeTrackMax) foreach { value =>
      logAdjustment(label, game.resources.fln, value)
      game = game.copy(resources = game.resources.copy(fln = value))
    }
  }
  
  def adjustCommitment(): Unit = {
    val label = "Government commitment"
    adjustInt(label, game.commitment, 0 to EdgeTrackMax) foreach { value =>
      logAdjustment(label, game.commitment, value)
      game = game.copy(commitment = value)
      log(s"Move 'Support+Commit' marker to ${game.govScore}")
    }
  }
  
  def adjustFranceTrack() = {
    val current = game.franceTrackLetter
    val options = Seq("A", "B", "C", "D", "E", "F")
    val label = "France Track"
    val prompt = s"$label is: $current.  Enter new value (${orList(options)}) "
    askOneOf(prompt, options, allowNone = true, allowAbort = false) foreach { value =>
      logAdjustment(label, current, value)
      game = game.copy(franceTrack = franceTrackFromLetter(value(0)))
    }
  }
  
  def adjustBorderZoneTrack() = {
    val label = "Border Zone track"
    adjustInt(label, game.borderZoneTrack, 0 to BorderZoneTrackMax) foreach { value =>
      logAdjustment(label, game.borderZoneTrack, value)
      game = game.copy(borderZoneTrack = value)
    }
  }
  
  // Any Troops, Any Police, Gov Bases, Guerrillas
  def adjustCasualties(): Unit = {
    def adjustPiece(pieceType: PieceType): Unit = {
      val pickFrom = game.availablePieces + game.casualties
      val current  = game.casualties.numOf(pieceType)
      val maxNum   = pickFrom.numOf(pieceType)
      if (maxNum == 0) {
        println(s"There are no ${pieceType} available to add to the casualties box.")
        pause()
      }
      else {
        val label = s"Casualties: ${pieceType}"
        adjustInt(label, current, 0 to maxNum) foreach { value =>
          logAdjustment(label, current, value)
          game = game.copy(casualties = game.casualties.set(value, pieceType))
        }
      }
    }
    
    @tailrec def getNextResponse(): Unit = {
      println()
      println("Casualties box")
      println(separator())
      println(game.casualties)
      println()
      println("Available")
      println(separator())
      println(game.availablePieces)
      println()

      val choices = List(
        "french troops", "french police", "algerian troops", "algerian police", 
        "guerrillas", "gov bases")
      askOneOf(s"[Adjust casualty] (? for list): ", choices, allowNone = true, allowAbort = false) match {
        case None =>
        case Some(casualty) =>
          casualty match {
            case "french troops"   => adjustPiece(FrenchTroops)
            case "french police"   => adjustPiece(FrenchPolice)
            case "algerian troops" => adjustPiece(AlgerianTroops)
            case "algerian police" => adjustPiece(AlgerianPolice)
            case "guerrillas"      => adjustPiece(HiddenGuerrillas)
            case "gov bases"       => adjustPiece(GovBases)
          }
          getNextResponse()
      }
    }
    getNextResponse()
  }
  
  // French troops, French police, Gov bases, guerrillas
  def adjustOutOfPlay(): Unit = {
    def adjustPiece(pieceType: PieceType): Unit = {
      val pickFrom = game.availablePieces + game.outOfPlay
      val current  = game.outOfPlay.numOf(pieceType)
      val maxNum   = pickFrom.numOf(pieceType)
      if (maxNum == 0) {
        println(s"There are no ${pieceType} available to add to the out of play box.")
        pause()
      }
      else {
        val label = s"Out of play: ${pieceType}"
        adjustInt(label, current, 0 to maxNum) foreach { value =>
          logAdjustment(label, current, value)
          game = game.copy(outOfPlay = game.outOfPlay.set(value, pieceType))
        }
      }
    }
    
    @tailrec def getNextResponse(): Unit = {
      println()
      println("Out of Play box")
      println(separator())
      println(game.outOfPlay)
      println()
      println("Available")
      println(separator())
      println(game.availablePieces)
      println()

      val choices = List("french troops", "french police", "guerrillas", "gov bases")
      askOneOf(s"[Adjust out of play] (? for list): ", choices, allowNone = true, allowAbort = false) match {
        case None =>
        case Some(piece) =>
          piece match {
            case "french troops" => adjustPiece(FrenchTroops)
            case "french police" => adjustPiece(FrenchPolice)
            case "guerrillas"    => adjustPiece(HiddenGuerrillas)
            case "gov bases"     => adjustPiece(GovBases)
          }
          getNextResponse()
      }
    }
    getNextResponse()
  }
  
  def adjustPivotal(): Unit = {
    // The only pivotal card that the Bot will ever play is 'Morocco and Tunisia Independent'
    // So that is the only Bot card included in this set.
    val AllPivots = Set(PivotalCoupdEtat, PivotalMobilization, PivotalRecallDeGaulle, PivotalMoroccoTunisiaIndepdent)
    var included = game.pivotalCardsPlayed
    var excluded = AllPivots -- included
    
    def nextAdjustment(): Unit = {
      val choices = (included.toList.sorted map (c => c -> s"Remove ${deck(c).numAndName}")) ++
                    (excluded.toList.sorted map (c => c -> s"Add ${deck(c).numAndName}")) :+
                    (0 -> "Finished")
      println("\nPivotal cards that have been played")
      println("Choose one:")
      askMenu(choices, allowAbort = false).head match {
        case 0 =>
        case c =>
          if (included contains c) {
            included = included - c
            excluded = excluded + c
          }
          else {
            included = included + c
            excluded = excluded - c
          }
          nextAdjustment()
      }
    }
    
    def cardList(x: Set[Int]): List[String] = x.toList.sorted map (deck(_).numAndName)
    nextAdjustment()
    if (included != game.pivotalCardsPlayed) {
      logAdjustment("pivotal cards played", cardList(game.pivotalCardsPlayed), cardList(included))
      game = game.copy(pivotalCardsPlayed = included)
    }
  }
  
    
  def adjustCapabilities(): Unit = {
    var included = game.capabilities
    var excluded = AllCapabilities filterNot included.contains
    
    def nextAdjustment(): Unit = {
      val choices = (included.sorted map (c => c -> s"Remove $c")) ++
                    (excluded.sorted map (c => c -> s"Add $c")) :+
                    ("done" -> "Finished")
      println()
      println("Choose one:")
      askMenu(choices, allowAbort = false).head match {
        case "done" =>
        case cap    =>
          if (included contains cap) {
            included = included filterNot (_ == cap)
            excluded = cap :: excluded
          }
          else {
            included = cap :: included
            excluded = excluded filterNot (_ == cap)
          }
          nextAdjustment()
      }
    }

    nextAdjustment()
    if (included != game.capabilities) {
      logAdjustment("capabilities", game.capabilities.sorted, included.sorted)
      game = game.copy(capabilities = included)
    }
  }
  
  def adjustMomentum(): Unit = {
    var included = game.momentum
    var excluded = AllMomentum filterNot included.contains
    
    def nextAdjustment(): Unit = {
      val choices = (included.sorted map (c => c -> s"Remove $c")) ++
                    (excluded.sorted map (c => c -> s"Add $c")) :+
                    ("done" -> "Finished")
      println()
      println("Choose one:")
      askMenu(choices, allowAbort = false).head match {
        case "done" =>
        case event    =>
          if (included contains event) {
            included = included filterNot (_ == event)
            excluded = event :: excluded
          }
          else {
            included = event :: included
            excluded = excluded filterNot (_ == event)
          }
          nextAdjustment()
      }
    }

    nextAdjustment()
    if (included != game.capabilities) {
      logAdjustment("momentum", game.momentum.sorted, included.sorted)
      game = game.copy(momentum = included)
    }
  }
  
  def adjustSpace(name: String): Unit = {
    def getNextResponse(): Unit = {
      val sp = game.getSpace(name)
      println()
      println(separator())
      printSummary(game.spaceSummary(name))
      println()
        
      var choices = List(
        "support", "terror", "french troops", "french police", "algerian troops", "algerian police", 
        "underground guerrillas", "active guerrillas", "gov bases", "fln bases")
      if (sp.isCity) choices = "plus 1 population" :: choices
      if (sp.isSector && sp.basePop == 1) choices = "resettled" :: choices
      if (sp.isCountry) choices = "plus 1 base" :: choices

      askOneOf(s"[$name attribute] (? for list): ", choices.sorted, allowNone = true, allowAbort = false) foreach {
        attribute =>
          attribute match {
            case "support"                => adjustSupport(name)
            case "french troops"          => adjustPieces(name, FrenchTroops)
            case "french police"          => adjustPieces(name, FrenchPolice)
            case "algerian troops"        => adjustPieces(name, AlgerianTroops)
            case "algerian police"        => adjustPieces(name, AlgerianPolice)
            case "underground guerrillas" => adjustPieces(name, HiddenGuerrillas)
            case "active guerrillas"      => adjustPieces(name, ActiveGuerrillas)
            case "gov bases"              => adjustPieces(name, GovBases)
            case "fln bases"              => adjustPieces(name, FlnBases)
            case "terror"                 => adjustTerrorMarkers(name)
            case "resettled"              => adjustResettled(name)
            case "plus 1 population"      => adjustPlusOnePop(name)
            case "plus 1 base"            => adjustPlusOneBase(name)
          }
          getNextResponse()
      }    
    }
    getNextResponse()
  }

  // Adust the number of the given type of piece in the given space.
  def adjustPieces(name: String, pieceType: PieceType): Unit = {
    val sp = game.getSpace(name)
    val pickFrom = game.availablePieces + sp.pieces
    val current  = sp.numOf(pieceType)
    val maxNum   = pieceType match {
      case GovBases         => (sp.maxBases - sp.flnBases) min pickFrom.numOf(pieceType)
      case FlnBases         => (sp.maxBases - sp.govBases) min pickFrom.numOf(pieceType)
      case HiddenGuerrillas => game.guerrillasAvailable + sp.hiddenGuerrillas
      case ActiveGuerrillas => game.guerrillasAvailable + sp.activeGuerrillas
      case _                => pickFrom.numOf(pieceType)
    }
    if (maxNum == 0) {
      println(s"There are no ${pieceType} available to add to $name.")
      pause()
    }
    else {
      val label = pieceType.toString
      adjustInt(label, current, 0 to maxNum) foreach { value =>
        logAdjustment(name, label, current, value)
        game = game.updateSpace(sp.copy(pieces = sp.set(value, pieceType)))
        if (pieceType == FlnBases)
          log(s"Move 'Oppose+Bases' marker to ${game.flnScore}")
      }
    }
  }
  
  def adjustSupport(name: String): Unit = {
    val choices = List(
      Neutral -> "Neutral",
      Support -> "Support",
      Oppose  -> "Opposition")
    val sp = game.getSpace(name)
    println("\nChoose one:")
    val newState = askMenu(choices, allowAbort = false).head
    if (newState != sp.support) {
      logAdjustment(name, "Support", sp.support, newState)
      val updated = sp.copy(support = newState)
      game = game.updateSpace(updated)
      log(s"Move 'Support+Commit' marker to ${game.govScore}")
      log(s"Move 'Oppose+Bases' marker to ${game.flnScore}")    
    }
  }
  
  def adjustTerrorMarkers(name: String): Unit = {
    val sp = game.getSpace(name)
    val avail = sp.terror + game.terrorMarkersAvailable
    
    if (avail == 0) {
      println(s"All terror markers are already on the map.")
      pause()
    }
    else {
      val newTerror = askInt("How many terror markers", 0, avail, allowAbort = false)
      if (newTerror != sp.terror ) {
        logAdjustment(name, "Terror markers", sp.terror, newTerror)
        val markers = List.fill(newTerror)(TerrorMarker) ::: (sp.markers filterNot (_ == TerrorMarker))
        game = game.updateSpace(sp.copy(markers = markers))
      }
    }
  }
  
  def adjustResettled(name: String): Unit = {
    val sp = game.getSpace(name)
    if (!sp.isResettled && game.resttledMarkersAvailable == 0) {
      println(s"All resettled markers are already on the map.")
      pause()
    }
    else {
      val newValue = !sp.isResettled
      logAdjustment(name, "Resettled", sp.isResettled, newValue)
      if (newValue)
        game = game.updateSpace(sp.addMarker(ResettledMarker))
      else
        game = game.updateSpace(sp.removeMarker(ResettledMarker))
    }
  }
  
  // Cities only.
  def adjustPlusOnePop(name: String): Unit = {
    val sp = game.getSpace(name)
    val plusOne = sp.hasMarker(Plus1PopMarker)
    if (!plusOne && game.plus1PopMarkersAvailable == 0) {
      println(s"All +1 Pop markers are already on the map.")
      pause()
    }
    else {
      val newValue = !plusOne
      logAdjustment(name, "+1 Pop", plusOne, newValue)
      if (newValue)
        game = game.updateSpace(sp.addMarker(Plus1PopMarker))
      else
        game = game.updateSpace(sp.removeMarker(Plus1PopMarker))
    }
  }
  
  // Morocco and Tunisia only
  def adjustPlusOneBase(name: String): Unit = {
    val sp = game.getSpace(name)
    val plusOne = sp.hasMarker(Plus1BaseMarker)
    if (!plusOne && game.plus1BaseMarkersAvailable == 0) {
      println(s"All +1 Base markers are already on the map.")
      pause()
    }
    else {
      val newValue = !plusOne
      logAdjustment(name, "+1 Base", plusOne, newValue)
      if (newValue)
        game = game.updateSpace(sp.addMarker(Plus1BaseMarker))
      else
        game = game.updateSpace(sp.removeMarker(Plus1BaseMarker))
    }
  }
  
  def adjustFinalPropSupport(): Unit = {
    val newValue = !game.params.finalPropSupport
    logAdjustment("Final prop support", game.params.finalPropSupport, newValue)
    game = game.copy(params = game.params.copy(finalPropSupport = newValue))
  }
  
  def adjustBotDebug(): Unit = {
    val newValue = !game.params.botDebug
    logAdjustment("Bot debug", game.params.botDebug, newValue)
    game = game.copy(params = game.params.copy(botDebug = newValue))
  }
}

