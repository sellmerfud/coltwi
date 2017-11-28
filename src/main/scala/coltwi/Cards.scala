
// Colonial Twilight
//
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

import scala.util.Random.shuffle
import ColonialTwilight._

object Cards {
  val Single     = false
  val Dual       = true
  val FlnMarked  = true
  val AlwaysPlay = true  // Capabilities and some FLN marked events.

  // Convenience method for adding a card to the deck.
  private def entry(card: Card) = (card.number -> card)
  
  val flnNavySource = (sp: Space) =>
    sp.coastal &&
    !sp.isSupport &&
    Bot.ConsiderMarch.eligibleGuerrillas(sp, true, true).total > 0
    
  val flnNavyDest = (sp: Space) => sp.coastal && sp.isSupport
  
  val moghazniCriteria = (sp: Space) => {
    sp.isSector && sp.isSupport && sp.algerianPolice > 0 &&
    Bot.getGuerrillasToPlace(sp.algerianPolice, sp).total > 0
  }
  
  val thirdForceCriteria = (sp: Space) => sp.isSector && sp.algerianPolice > 0 && !sp.isOppose
  
  val stripeyHoleUnshaded = (sp: Space) => sp.isSector && !sp.isOppose && sp.flnBases > 0

  val stripeyHoleShaded = (sp: Space) => {
    val haveAvail = Bot.getGuerrillasToPlace(2, sp).total > 0
    sp.isSector && sp.isSupport && (haveAvail || game.outOfPlay.totalGuerrillas > 0)
  }
  
  // True if the space or any adjacent space contains an FLN base
  val flnBaseAdjacent = (sp: Space) => {
    ((getAdjacent(sp.name) map game.getSpace) + sp) exists (_.flnBases > 0)
  }
  
  val op744 = (sp: Space) => sp.isMountains && sp.population == 0 && !flnBaseAdjacent(sp)
  
  val soumanConf = (sp: Space) => sp.canTakeBase && sp.flnBases == 0

  
  val OppositionPriorities = List(
    new Bot.CriteriaFilter[Space]("Is at support", _.isSupport),
    new Bot.HighestScorePriority[Space]("Highest population", _.population),
    new Bot.CriteriaFilter[Space]("Gov cannot train", sp => !sp.canTrain))
  
  val deckMap: Map[Int, Card] = Map(
    // ------------------------------------------------------------------------
    entry(new Card(1, "Quadrillage", Dual, false, false,
      () => NoEvent,
      (role: Role) => {
        // Role will always be Gov
      },
      (role: Role) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(2, "Balky Conscripts", Dual, false, false,
      () => NoEvent,
      (role: Role) => {
        // Role will always be Gov
      },
      (role: Role) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(3, "Leadership Snatch", Dual, FlnMarked, false,
      () => Shaded,
      (role: Role) => (),
      (role: Role) => {
         // Widespread rage: Set up to 2 FLN-Controlled spaces to Oppose.
        def setOppose(candidates: List[Space], remaining: Int): Unit = {
          if (remaining > 0 && candidates.nonEmpty) {
            val sp = Bot.topPriority(candidates, OppositionPriorities)
            setSupport(sp.name, Oppose)
            setOppose(candidates filterNot (_.name == sp.name), remaining - 1)
          }
        }
        val criteria = (sp: Space) => sp.population > 0 && sp.isFlnControlled && !sp.isOppose
        setOppose(game.algerianSpaces filter criteria, 2)
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(4, "Oil & Gas Discoveries", Single, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => () // Single event
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(5, "Peace of the Brave", Dual, false, false,
      () => Shaded,
      (role: Role) => (),
      (role: Role) => {
        // Fight like hell: Free Rally in any 2 selectable spaces.
        Bot.turnState = Bot.TurnState(
          specialActivityAllowed = false,
          freeOperation          = true,
          maxSpaces              = Some(2))
        Bot.ConsiderRally.execute() // Rally will only be used if it adds a base and/or shifts the France Track
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(6, "Factionalism", Dual, false, false,
      () => Shaded,
      (role: Role) => (),
      (role: Role) => {
        Bot.turnState = Bot.TurnState(
          specialActivityAllowed = false,
          freeOperation          = true,
          maxSpaces              = Some(1),
          onlyIn                 = spaceNames(game.spaces filter (_.flnBases > 0)).toSet)
        Bot.ConsiderRally.execute() // Rally will only be used if it adds a base and/or shifts the France Track
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(7, "5th Bureau", Dual, FlnMarked, false,
      () => Shaded,
      (role: Role) => (),
      (role: Role) => {
        // Propaganda flop: Shift any 2 Sectors 1 level each towards Opposition. 
       def shiftOppose(candidates: List[Space], remaining: Int): Unit = {
         if (remaining > 0 && candidates.nonEmpty) {
           val sp = Bot.topPriority(candidates, OppositionPriorities)
           decreaseSupport(sp.name, 1)
           shiftOppose(candidates filterNot (_.name == sp.name), remaining - 1)
         }
       }
       val criteria = (sp: Space) => sp.population > 0 && !sp.isOppose
       shiftOppose(game.algerianSpaces filter criteria, 2)
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(8, "Cross-border air strike", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(9, "Beni-Oui-Oui", Single, FlnMarked, false,
      () => Unshaded,
      (role: Role) => if (role == Gov) {
        
      }
      else { // Role == Fln
        // Set any two non-terrorized spaces to Oppose
       def setOppose(candidates: List[Space], remaining: Int): Unit = {
         if (remaining > 0 && candidates.nonEmpty) {
           val sp = Bot.topPriority(candidates, OppositionPriorities)
           setSupport(sp.name, Oppose)
           setOppose(candidates filterNot (_.name == sp.name), remaining - 1)
         }
       }
       val criteria = (sp: Space) => sp.terror == 0 && !sp.isOppose
       setOppose(game.algerianSpaces filter criteria, 2)
      },
      (role: Role) => () // Single event
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(10, "Moudjahidine", Dual, FlnMarked, AlwaysPlay,
      () => Shaded,
      (role: Role) => (),
      (role: Role) => {
        // Sign me up: Until Propaganda Round, treat each Rally in a FLN-Con-trolled
        // space with no Base as if it contained 1 Base.
        playMomentum(MoMoudjahidine)
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(11, "Bananes", Dual, FlnMarked, false,
      () => Shaded,
      (role: Role) => (),
      (role: Role) => {
        // Misguided airstrike incident
        decreaseCommitment(1)
      }
    )),
    
    // ------------------------------------------------------------------------
    // #12 Special Instructions
    // If able, redistribute Underground Guerrillas to most support
    // per March limitations, otherwise choose Op(+Sa)   
    entry(new Card(12, "Ventilos", Dual, FlnMarked, AlwaysPlay,
      () => {
        if ((game.hasSpace(flnNavySource)) && (game.hasAlgerianSpace(flnNavyDest)))
          Shaded
        else
          NoEvent
      },
      (role: Role) => (),
      (role: Role) => {
        val die = dieRoll
        val num = (die + 1) / 2   // Half rounded up
        
        val DestPriorities = List(
          new Bot.CriteriaFilter[Space]("Cities", _.isCity),
          new Bot.HighestScorePriority[Space]("Highest population", _.population))
        
        val sources = game.spaces filter flnNavySource
        val dests   = game.algerianSpaces filter flnNavyDest
        
        val guerrillas = (sources map (sp => (sp.name, Bot.ConsiderMarch.eligibleGuerrillas(sp, true, true)))
                                  sortBy { case (_, pieces) => -pieces.total })
        val total = (guerrillas.foldLeft(0) { case (sum, (_, p)) => sum + p.total }) min num
        val (firstSrc, firstPieces) = guerrillas.head
        
        log(s"Die roll is $die.  $Fln may move up to $num guerrillas among 3 coastal spaces")
        if (total > 1 && firstPieces.total >= total && dests.size > 1) {
          // Move guerrillas from one space to two destinations
          val dest1 = Bot.topPriority(dests, DestPriorities)
          val dest2 = Bot.topPriority(dests filterNot (_.name == dest1.name), DestPriorities)
          movePieces(Pieces(hiddenGuerrillas = if (total == 3) 2 else 1), firstSrc, dest1.name)
          movePieces(Pieces(hiddenGuerrillas = 1), firstSrc, dest2.name)
        }
        else {
          // Move guerrillas to a single destinations
          val dest = Bot.topPriority(dests, DestPriorities)
          
          if (firstPieces.total >= total)
            movePieces(Pieces(hiddenGuerrillas = total), firstSrc, dest.name)
          else {
            val (secondSrc, secondPieces) = guerrillas.drop(1).head
            movePieces(firstPieces, firstSrc, dest.name)
            val from2nd = (total - firstPieces.total) min secondPieces.total
            movePieces(Pieces(hiddenGuerrillas = from2nd), secondSrc, dest.name)
          }
        }
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(13, "SAS", Dual, false, AlwaysPlay,
      () => Shaded,
      (role: Role) => {
        playCapability(CapGovSaS)
      },
      (role: Role) => {
        playCapability(CapFlnSaS)
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(14, "Protest in Paris", Single, FlnMarked, AlwaysPlay,
      () => if (game.franceTrack < FranceTrackMax) Shaded else NoEvent,
      (role: Role) => {
        if (role == Gov)
          decreaseFranceTrack(2)
        else
          increaseFranceTrack(2)
      },
      (role: Role) => () // Single event
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(15, "Jean-Paul Sarte", Dual, false, false,
      () => if (game.commitment > 0) Shaded else NoEvent
      ,
      (role: Role) => {
        // Writes a play, donates royalties: +2 FLN Resources
        increaseResources(Fln, 2)
      },
      (role: Role) => {
        // Signs manifesto: -1 Commitment
        decreaseCommitment(1)
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(16, "NATO", Dual, false, false,
      () => NoEvent,  // Would never be "effective" so never executed
      (role: Role) => (),
      (role: Role) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(17, "Commandos", Dual, false, AlwaysPlay,
      () => Shaded,
      (role: Role) => {
        playCapability(CapGovCommandos)
      },
      (role: Role) => {
        playCapability(CapFlnCommandos)
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(18, "Torture", Single, FlnMarked, AlwaysPlay,
      () => Unshaded,
      (role: Role) => {
        playCapability(CapTorture)
      },
      (role: Role) => () // Single event
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(19, "General Strike", Dual, FlnMarked, false,
      () => Shaded,
      (role: Role) => (),
      (role: Role) => {
        // United Nations resolution condemning violence raises FLN profile: +2 FLN Resources
        increaseResources(Fln, 2)
      }
    )),
    
    // ------------------------------------------------------------------------
    // #20 Special Instructions
    // Choose Op(+SA)
    entry(new Card(20, "Suave qui peut", Single, FlnMarked, false,
      () => NoEvent,  // Bot never plays this event
      (role: Role) => (),
      (role: Role) => () // Single event
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(21, "United Nations Resolution", Dual, false, false,
      () => Shaded,
      (role: Role) => {
        // Mind your own business!
        increaseCommitment(1)
      },
      (role: Role) => {
        // Binding
        decreaseCommitment(1)
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(22, "The Government of USA is Convinced...", Dual, FlnMarked, false,
      () => Shaded,
      (role: Role) => {
        // FLN are Communists
        increaseCommitment(2)
      },
      (role: Role) => {
        // Algeria is entitled to self-determination
        decreaseCommitment(2)
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(23, "Diplomatic Leanings", Dual, FlnMarked, false,
      () => Shaded,
      (role: Role) => (),
      (role: Role) => {
        // Arab Bloc solidarity
        increaseResources(Fln, 6)
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(24, "Economic Development", Dual, FlnMarked, false,
      () => Shaded,
      (role: Role) => (),
      (role: Role) => {
        // Military funds diverted to social enterprises
        decreaseResources(Gov, 6)
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(25, "Purge", Single, false, false,
      () => Unshaded,
      (role: Role) => if (role == Gov) {
        // To be done.
      }
      else {
        val piecesDie = dieRoll
        val numPieces = (piecesDie + 1) / 2
        val resourcesDie = dieRoll
        val numRemoved = 0
        val basePriorities = List(
          new Bot.CriteriaFilter[Space]("Has FLN Base", _.flnBases > 0),
          new Bot.LowestScorePriority[Space]("Least guerrillas", _.totalGuerrillas))
        val otherPriorities = List(
          new Bot.CriteriaFilter[Space]("Guerrilla present", _.totalGuerrillas > 0),
          new Bot.CriteriaFilter[Space]("Gov control", _.isGovControlled),
          new Bot.HighestScorePriority[Space]("Most guerrillas", _.totalGuerrillas))
        
        log(s"Die roll for pieces is $piecesDie.  $Fln may move up to $numPieces enemy pieces")
        def doRemove(candiates: List[Space], pieceType: PieceType): Unit = {
          if (numRemoved < numPieces && candiates.nonEmpty) {
            val sp = if (candiates exists (_.flnBases > 0))
              Bot.topPriority(candiates, basePriorities)
            else
              Bot.topPriority(candiates, otherPriorities)
            val num = (numPieces - numRemoved) min sp.pieces.numOf(pieceType)
            removeToAvailableFrom(sp.name, Pieces().set(num, pieceType))
            doRemove(candiates filterNot(_.name == sp.name), pieceType)
          }
        }
        
        // French before Algerians, then or within that, Troops before Police.
        doRemove(game.algerianSpaces filter (_.frenchTroops   > 0), FrenchTroops)
        doRemove(game.algerianSpaces filter (_.frenchPolice   > 0), FrenchPolice)
        doRemove(game.algerianSpaces filter (_.algerianTroops > 0), AlgerianTroops)
        doRemove(game.algerianSpaces filter (_.algerianPolice > 0), AlgerianPolice)
          
        log(s"Die roll for resources is $resourcesDie")
        decreaseResources(Gov, resourcesDie)
      },
      (role: Role) => () // Single event
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(26, "Casbah", Dual, FlnMarked, AlwaysPlay,    
      () => if (Bot.getGuerrillasToPlace(4, game.getSpace(Algiers)).total > 0) Shaded else NoEvent,
      (role: Role) => (),
      (role: Role) => {
        // Urban uprising
        val toPlace = Bot.getGuerrillasToPlace(4, game.getSpace(Algiers))
        Bot.placeGuerrillas(Algiers, toPlace)
        
        // If Algiers is now FLN controlled free agitate 1 "level"
        // Remove 1 terror marker or
        // if there are no terror markers will shift 1 level toward Oppose
        val sp = game.getSpace(Algiers)
        if (sp.isFlnControlled && sp.terror > 0)
          removeTerror(Algiers, 1)
        else if (sp.isFlnControlled && !sp.isOppose)
          decreaseSupport(Algiers, 1)
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(27, "Covert Movement", Dual, false, AlwaysPlay,
      () => Shaded,
      (role: Role) => {
        playCapability(CapDeadZones)
      },
      (role: Role) => {
        playCapability(CapXWilayaCoord)
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(28, "Atrocities and Reprisals", Single, FlnMarked, AlwaysPlay,
      () => if (game.resources(Fln) > 0 && game.terrorMarkersAvailable > 0 &&
                ((game.commitment > 0 && game.hasAlgerianSpace(!_.isOppose)) || 
                 game.hasAlgerianSpace(_.isSupport)))
              Unshaded
            else
              NoEvent
      ,
      (role: Role) => if (role == Gov) {
        // To do
      }
      else { // role == Fln
        val priorities = List(
          new Bot.CriteriaFilter[Space]("Is at support", _.isSupport),
          new Bot.HighestScorePriority[Space]("Highest population", _.population))
        
        def placeTerror(selected: Set[String], remaining: Int): Unit = {
          if (remaining > 0 && game.resources(Fln) > 0 && game.terrorMarkersAvailable > 0) {
            val criteria = if (game.commitment > 0)
              (sp: Space) => !sp.isOppose
            else
              (sp: Space) => sp.isSupport
            
            (game.algerianSpaces filter criteria) match {
              case Nil =>
              case candidates =>
                val sp = Bot.topPriority(candidates, priorities)
                decreaseResources(Fln, 1)
                addTerror(sp.name, 1)
                setSupport(sp.name, Neutral)
                decreaseCommitment(1)
                placeTerror(selected + sp.name, remaining - 1)
            }
          }
        }
        placeTerror(Set.empty, 2)
      },
      (role: Role) => () // Single event
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(29, "The Call Up", Dual, false, false,
      () => NoEvent,  // Fln does not play unmarked momentum events
      (role: Role) => (),
      (role: Role) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(30, "Change in Tactics", Single, false, AlwaysPlay,
      () => if (game.capabilities exists (_.startsWith("Gov:"))) Unshaded else NoEvent,
      (role: Role) => if (role == Gov) {
        // To be done
      }
      else {
        val cap = shuffle(game.capabilities filter (_.startsWith("Gov:"))).head
        removeCapabilityFromPlay(cap)
      },
      (role: Role) => () // Single event
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(31, "Intimidation", Dual, FlnMarked, false,
      () => Shaded,
      (role: Role) => (),
      (role: Role) => {
        val entry = FranceTrack(game.franceTrack)
        increaseResources(Fln, entry.resource)
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(32, "Teleb the Bomb-maker", Dual, false, AlwaysPlay,
      () => Shaded,
      (role: Role) => {
        playCapability(CapAmateurBomber)
      },
      (role: Role) => {
        playCapability(CapEffectiveBomber)
        
      }
    )),
    
    // ------------------------------------------------------------------------
    // #33 Special Instructions
    // When ths shaded Revenge capability is in play,
    // After Gov assault, the bot will place the Guerrilla:
    //  - First at assault space at Support with highest population
    //  - Then at assault space with FLN base with highest population
    //  - Then at random assault space with highest population
    entry(new Card(33, "Overkill", Dual, FlnMarked, AlwaysPlay,
      () => Shaded,
      (role: Role) => {
        // Let God sort 'em out: Neutralize removes up to 4 pieces
        // (still maximum 2 spaces)
        playCapability(CapOverkill)
      },
      (role: Role) => {
        // Revenge!: After Assault, FLN may place 1 guerrilla in any 1 of the
        // assault spaces from available.
        playCapability(CapRevenge)
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(34, "Elections", Dual, FlnMarked, false,
      () => Shaded,
      (role: Role) => {
        // Integrationist mandate: +1 Commitment or set 1 non-terrorized 
        // Neutral space to Support
      },
      (role: Role) => {
        // Voter Supression: Set 1 sector to Neutral
        val priorities = List(
          new Bot.HighestScorePriority[Space]("Highest population", _.population),
          new Bot.CriteriaFilter[Space]("Gov cannot train", sp => !sp.canTrain))
        
        val candidates = game.algerianSpaces filter (sp => sp.population > 0 && sp.isSupport)
        if (candidates.nonEmpty) {
          val sp = Bot.topPriority(candidates, priorities)
          setSupport(sp.name, Neutral)
        }
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(35, "Napalm", Dual, FlnMarked, AlwaysPlay,
      () => Shaded,
      (role: Role) => {
        // Effective: Remove 1 Guerrilla per assaulting cube in Mountain spaces.
        playCapability(CapNapalm)
      },
      (role: Role) => {
        // Scorch the Countryside: Each Assault space cost 3 resources.
        playCapability(CapScorch)
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(36, "Assassination", Dual, FlnMarked, AlwaysPlay,
      () => if (game.outOfPlay.hiddenGuerrillas > 0 || game.guerrillasAvailable > 0) Shaded else NoEvent,
      (role: Role) => {
        // Crippling leadership loss: Remove 1 Guerrilla from
        // any space to casualties, -1d6 FLN resources
        
      },
      (role: Role) => {
        // Martyr: Add 1 Guerrilla in any space from 
        // available or out of play, +1d6 FLN resources
        val priorities = List(
          new Bot.CriteriaFilter[Space]("Support space", _.isSupport),
          new Bot.CriteriaFilter[Space]("Unprotected base", sp => sp.flnBases > 0 && (sp.hiddenGuerrillas == 0 || sp.totalGuerrillas < 2)),
          new Bot.CriteriaFilter[Space]("Friendly pieces", _.totalFln > 0),
          new Bot.CriteriaFilter[Space]("In Algeria", !_.isCountry))
        val sp = Bot.topPriority(game.spaces, priorities)
        if (game.outOfPlay.hiddenGuerrillas > 0)
          placePiecesFromOutOfPlay(sp.name, Pieces(hiddenGuerrillas = 1))
        else
          placePieces(sp.name, Pieces(hiddenGuerrillas = 1))
        
        increaseResources(Fln, dieRoll)
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(37, "Integration", Dual, false, false,
      () => NoEvent,  // Bot will never use this event
      (role: Role) => {
        // Expansion: Free Train in up to 2 selectable spaces.
      },
      (role: Role) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(38, "Economic Crisis in France", Dual, false, false,
      () => Shaded,
      (role: Role) => {
        // Expatriate donations down: -1d6 FLN resources
        decreaseResources(Fln, dieRoll)
      },
      (role: Role) => {
        // Tight Military Budget: -1d6 Government resources, -1 Commitment
        decreaseResources(Gov, dieRoll)
        decreaseCommitment(1)
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(39, "Retreat into Djebel", Dual, false, false,
      () => NoEvent,  // Bot will never use this event
      (role: Role) => {
        // Harsh terrain: Select 2 Mountain spaces with no FLN Base.
        // Remove all guerrillas there to available.
      },
      (role: Role) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(40, "Strategic Movement", Dual, false, false,
      () => NoEvent,  // Bot will never use this event
      (role: Role) => {
        // Sea Lift: May redistribute up to 6 cubes among
        // any 3 coastal spaces for free.
      },
      (role: Role) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(41, "Egypt", Dual, FlnMarked, AlwaysPlay,
      () => Shaded,
      (role: Role) => {
        // Get out of Cairo: -3 FLN resources
        decreaseResources(Fln, 3)
      },
      (role: Role) => {
        // Arab Summit: +1d6 FLN resources
        increaseResources(Fln, dieRoll)
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(42, "Czech Arms Deal", Dual, FlnMarked, AlwaysPlay,
      () => Shaded,
      (role: Role) => {
        // Intercepted: Subtract FLN resources = twice the current Border Zone Status
        // (subtract 2 if Morocco and Tunisia no yet independent)
        decreaseResources(Fln, if (game.moroccoTunisiaIndependent) game.borderZoneTrack * 2 else 2)
      },
      (role: Role) => {
        // Arrived safely: +6 FLN resources
        increaseResources(Fln, 6)
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(43, "Refugees", Dual, FlnMarked, AlwaysPlay,
      () => if (game.moroccoTunisiaIndependent) Shaded else NoEvent,
      (role: Role) => {
        // Flee to the cities: Play a '+1 Pop' marker in any 2 cities.
        // Ask which two cities
        // addPlus1PopMarker(name)
      },
      (role: Role) => {
        // Flee the country: The stacking limit for Bases in Morocco and
        // Tunisia (if independent) is increased to 3.
        addBaseMarker(Morocco)
        addBaseMarker(Tunisia)
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(44, "Paranoia", Dual, false, false,
      () => NoEvent,  // Bot will never use this event
      (role: Role) => {
        // Rampant mistrust: Until Propaganda Round, FLN may not
        // march into a sector in a different Wilaya
        // (may still cross international borders)
        playMomentum(MoParanoia)
      },
      (role: Role) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(45, "Challe Plan", Dual, false, false,
      () => NoEvent,  // Bot will never use this event
      (role: Role) => {
        // Single HQ control: Until Propaganda round, Police cubes are
        // counted as Troops in all Assaults, not just City spaces.
        // Police do not move in Sweep.
        playMomentum(MoChallePlanGov)
      },
      (role: Role) => ()
    )),
    
    // ------------------------------------------------------------------------
    // Normally bot rules will not play this event.
    // I have set it up to play it if it can move guerrillas into
    // a sector at support.
    entry(new Card(46, "Moghazni", Dual, false, AlwaysPlay,
      () => if (game hasAlgerianSpace moghazniCriteria) Shaded else NoEvent,
      (role: Role) => {
        // Rural self-defense: Until Propaganda round, Government may Train
        // in any Sector that is at Support and is Government controlled;
        // however, may place only Algerian Police.
        playMomentum(MoMoghazni)
      },
      (role: Role) => {
        val priorities = List(
          new Bot.HighestScorePriority[Space]("Most alerian police", _.algerianPolice),
          new Bot.HighestScorePriority[Space]("Most Fln pieces", _.totalFln),
          new Bot.HighestScorePriority[Space]("Highest population", _.population))
        val sp = Bot.topPriority(game.algerianSpaces filter moghazniCriteria, priorities)
        val guerrillas = Bot.getGuerrillasToPlace(sp.algerianPolice, sp)
        removeToAvailableFrom(sp.name, Pieces(algerianPolice = guerrillas.total))
        Bot.placeGuerrillas(sp.name, guerrillas)
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(47, "Third Force", Dual, FlnMarked, AlwaysPlay,
      () => if (game hasAlgerianSpace thirdForceCriteria) Shaded else NoEvent,
      (role: Role) => {
        // Rally dissident insurgents: Replace up to 3 Guerrillas in 1 Sector
        // with an equal number of Algerian Police.
      },
      (role: Role) => {
        // Private army mistreats civilians: Set any 1 Sector with Algerian
        // Police in it to Opposition
        val sp = Bot.topPriority(game.algerianSpaces filter thirdForceCriteria, OppositionPriorities)
        setSupport(sp.name, Oppose)
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(48, "Ultras", Dual, FlnMarked, AlwaysPlay,
      () => if (game hasAlgerianSpace (_.algerianCubes > 0)) Shaded else NoEvent,
      (role: Role) => {
        // Freelancers: Remove up to 1 Guerrilla in each City space to available.
      },
      (role: Role) => {
        // Army supresses pied-noir hotheads: Remove 1-3 (1d6 halved round up)
        // algerian cubes to Available.
        val priorities = List(
          new Bot.CriteriaFilter[Space]("FLN base", _.flnBases > 0),
          new Bot.CriteriaFilter[Space]("Support space", _.isSupport))
        val die   = dieRoll
        val total = (die + 1) / 2
        log(s"Die roll is $die. $Fln may remove $total Algerian cubes")
        
        def removeCubes(removed: Int, pieceType: PieceType, candidates: List[Space]): Int = {
          if (removed < total && candidates.nonEmpty) {
            val sp = Bot.topPriority(candidates, priorities)
            val num = (total - removed) min sp.pieces.numOf(pieceType)
            removeToAvailableFrom(sp.name, Pieces().set(num, pieceType))
            removeCubes(removed + num, pieceType, candidates filterNot (_.name == sp.name))
          }
          else
            removed
        }
        
        val troopsRemoved = removeCubes(0, AlgerianTroops, game.algerianSpaces filter (_.algerianTroops > 0))
        removeCubes(troopsRemoved, AlgerianPolice, game.algerianSpaces filter (_.algerianPolice > 0))
      }
    )),
    
    // ------------------------------------------------------------------------
    // #49 Special Instructions
    // Distribute evenly, if already even choose Op(+SA)
    entry(new Card(49, "Factional Plot", Dual, FlnMarked, AlwaysPlay,
      () => {
        val morocco = game.getSpace(Morocco)
        val tunisia = game.getSpace(Tunisia)
        val maxAllowed = (morocco.totalGuerrillas + tunisia.totalGuerrillas + 1) / 2
        if (game.moroccoTunisiaIndependent && 
             (morocco.totalGuerrillas > maxAllowed || tunisia.totalGuerrillas > maxAllowed))
          Shaded
        else
          NoEvent
      },
      (role: Role) => {
        // Plot successful, mass confusion: Remove half of Guerrillas in 
        // Tunisia to Available (round down)
        val sp = game.getSpace(Tunisia)
        if (sp.totalGuerrillas == 0)
          log("There are no guerrillas in Tunisia")
        else {
          val num = sp.totalGuerrillas / 2
          val guerrillas = askPieces(sp.pieces, sp.totalGuerrillas / 2, HiddenGuerrillas::ActiveGuerrillas::Nil)
          removeToAvailableFrom(Tunisia, guerrillas)
        }
      },
      (role: Role) => {
        // Plot crushed, organizational realignment: May redistribute any number
        // of Guerrillas in Morocco and Tunisia between these two countries.
        val morocco = game.getSpace(Morocco)
        val tunisia = game.getSpace(Tunisia)
        val maxAllowed = (morocco.totalGuerrillas + tunisia.totalGuerrillas + 1) / 2
        if (morocco.totalGuerrillas > maxAllowed) {
          val num = morocco.totalGuerrillas - maxAllowed
          val numHidden = (morocco.hiddenGuerrillas - (morocco.hiddenGuerrillas + tunisia.hiddenGuerrillas + 1) / 2) max 0
          val guerrillas = Pieces(hiddenGuerrillas = numHidden, activeGuerrillas = num - numHidden)
          movePieces(guerrillas, Morocco, Tunisia)
        }
        else {
          val num = tunisia.totalGuerrillas - maxAllowed
          val numHidden = (tunisia.hiddenGuerrillas - (morocco.hiddenGuerrillas + tunisia.hiddenGuerrillas + 1) / 2) max 0
          val guerrillas = Pieces(hiddenGuerrillas = numHidden, activeGuerrillas = num - numHidden)
          movePieces(guerrillas, Tunisia, Morocco)
        }
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(50, "Bleuite", Dual, false, false,
      () => NoEvent,  // Bot never plays this event
      (role: Role) => {
        // Self-purge: Activate all Guerrillas in 1 spce in Algeria,
        // roll 1d6.  If result <= to total guerrillas in the space,
        // remove two guerrillas to casualties.
      },
      (role: Role) => ()
    )),
    
    // ------------------------------------------------------------------------
    // #51 Special Instructions
    // Place at Support. (shaded)
    // If unable, play unshaded part selecting highest Pop AND no FLN Base.
    // Otherwise chose Op(+SA)
    entry(new Card(51, "Stripey Hole", Dual, FlnMarked, AlwaysPlay,
      () => if (game hasAlgerianSpace stripeyHoleShaded)
              Shaded
            else if (game hasAlgerianSpace stripeyHoleUnshaded)
              Unshaded
            else
              NoEvent
      ,
      // Mass arbitrary imprisonment: Activate all Guerrillas in any 
      // Sector.  Set to Oppose.
      (role: Role) => {
        val priorities = List(
          new Bot.HighestScorePriority[Space]("Highest population", _.population),
          new Bot.CriteriaFilter[Space]("Support Space", _.isSupport))
          
        val name = if (role == Gov) {
          // To be done
          Medea
        }
        else //  role == Fln
          Bot.topPriority(game.algerianSpaces filter stripeyHoleUnshaded, priorities).name

        activateHiddenGuerrillas(name, game.getSpace(name).hiddenGuerrillas)
        setSupport(name, Oppose)
      },
      // Prison Break: Place 2 Guerrillas in any 1 Sector from Available or
      // Out of Play
      (role: Role) => {
        val priorities = List(
          new Bot.CriteriaFilter[Space]("None underground", _.hiddenGuerrillas == 0),
          new Bot.HighestScorePriority[Space]("Highest population", _.population))
        val sp = Bot.topPriority(game.algerianSpaces filter stripeyHoleShaded, priorities)
        val oop = game.outOfPlay.hiddenGuerrillas min 2
        placePiecesFromOutOfPlay(sp.name, Pieces(hiddenGuerrillas = oop))
        val toPlace = Bot.getGuerrillasToPlace(2 - oop, sp)
        Bot.placeGuerrillas(sp.name, toPlace)
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(52, "Cabinet Shuffle", Single, false, false,
      () => if (game.franceTrack < FranceTrackMax) Unshaded else NoEvent,
      (role: Role) => if (role == Gov) decreaseFranceTrack(1) else increaseFranceTrack(1)
      ,
      (role: Role) => () // Single event
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(53, "Population Control", Dual, FlnMarked, AlwaysPlay,
      () => if (game hasSpace (sp => sp.isCity && sp.isSupport)) Shaded else NoEvent,
      (role: Role) => {
        // ID cards and housing registers: Until Propaganda round, FLN
        // marching into city activates if more than 2 government cubes
        // are present.
        playMomentum(MoPopulationControl)
      },
      (role: Role) => {
        val priorities = List(
          new Bot.HighestScorePriority[Space]("Highest population", _.population))
        val sp = Bot.topPriority(game.spaces filter (sp => sp.isCity && sp.isSupport), priorities)
        setSupport(sp.name, Neutral)
      }
    )),
    
    // ------------------------------------------------------------------------
    // #54 Special Instructions
    // Move from random spaces to an empty 0 Pop Sector not adjacent to FLN Bases.
    // Otherwise choose Op(+SA)
    entry(new Card(54, "Operation 744", Single, FlnMarked, AlwaysPlay,
      () => if ((game hasAlgerianSpace (_.frenchTroops > 0)) && (game hasAlgerianSpace op744))
              Unshaded
            else
              NoEvent
      ,
      (role: Role) => {
        // Beat the bushes: Executor of Event moves up to 4 French troops
        // from any spaces to 1 Mountain sector.  Remove up to 2 FLN pieces
        // (may be Underground, but Guerrillas before bases) to Available
        // if any there.
        if (role == Gov) {
          // To be done
        }
        else { // role == Fln
          val priorities = List(
            new Bot.LowestScorePriority[Space]("Fewest guerrillas", _.totalGuerrillas))
          val dest = Bot.topPriority(game.algerianSpaces filter op744, priorities)
          val sources = game.algerianSpaces filter (_.frenchTroops > 0) sortBy (-_.frenchTroops)
          def moveTroops(remaining: Int, sources: List[Space]): Unit = (remaining, sources) match {
            case (0, _)   =>
            case (_, Nil) =>
            case (_, x :: xs) =>
              val num = remaining min x.frenchTroops
              movePieces(Pieces(frenchTroops = num), x.name, dest.name)
              moveTroops(remaining - num, xs)
          }
          
          val num = dest.totalGuerrillas min 2
          val active = num min dest.activeGuerrillas
          val guerrillas = Pieces(activeGuerrillas = active,
                                  hiddenGuerrillas = (num - active) min dest.hiddenGuerrillas)
          removeToAvailableFrom(dest.name, guerrillas)
        }
      },
      (role: Role) => () // Single event
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(55, "Development", Dual, FlnMarked, AlwaysPlay,
      () => if (game.resources(Fln) < EdgeTrackMax) Shaded else NoEvent,
      (role: Role) => {
        // Infrastructure: Place up to 2 government bases anywhere in
        // Algeria from Available or Out of Play.
      },
      (role: Role) => {
        // Siphoned: FLN +3 resources
        increaseResources(Fln, 3)
      }
    )),
    
    // ------------------------------------------------------------------------
    // #56 Special Instructions
    // Play in final Campaign only.
    entry(new Card(56, "Hardened Attitudes", Single, FlnMarked, AlwaysPlay,
      () => if (game.isFinalCampaign) Unshaded else NoEvent,
      (role: Role) => {
        // Plague on both your houses: Until Propaganda round:
        // - Government may not Train in a Sector without Support.
        // - FLN may not Extort in a sector without a base.
        playMomentum(MoHardendAttitudes)
      },
      (role: Role) => () // Single event
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(57, "Peace Talks", Single, FlnMarked, AlwaysPlay,
      () => NoEvent,
      (role: Role) => {
        // Play nice, now: Until Propaganda round: Government may not
        // Assault.  FLN may not Attack.
        playMomentum(MoPeaceTalks)
      },
      (role: Role) => () // Single event
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(58, "Army in Waiting", Dual, false, false,
      () => if ((game.getSpace(Morocco).canTakeBase || game.getSpace(Tunisia).canTakeBase) &&
               game.flnBasesAvailable > 0) Shaded else NoEvent,
      (role: Role) => {
        // Save strength against the day: Half (round down) of Guerrillas in Tunisia
        // (only) removed to Out of Play
        val sp = game.getSpace(Tunisia)
        if (sp.totalGuerrillas == 0)
          log("There are no guerrillas in Tunisia")
        else {
          val num = sp.totalGuerrillas / 2
          val guerrillas = askPieces(sp.pieces, sp.totalGuerrillas / 2, HiddenGuerrillas::ActiveGuerrillas::Nil)
          removeToOutOfPlay(Tunisia, guerrillas)
        }
      },
      (role: Role) => {
        val sp = shuffle(spaces(Morocco :: Tunisia :: Nil) filter (_.canTakeBase) sortBy (_.flnBases)).head
        placePieces(sp.name, Pieces(flnBases = 1))
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(59, "Bandung Conference", Dual, FlnMarked, AlwaysPlay,
      () => if (game.resources(Fln) < EdgeTrackMax) Shaded else NoEvent,
      (role: Role) => {
        // Waste of time: -1d6 FLN resources
        decreaseResources(Fln, dieRoll)
      },
      (role: Role) => {
        // Pledges of support: +1d6 FLN resources
        increaseResources(Fln, dieRoll)
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(60, "Soummam Conference", Dual, FlnMarked, AlwaysPlay,
      () => if (game.flnBasesAvailable > 0 && (game hasAlgerianSpace soumanConf)) Shaded else NoEvent,
      (role: Role) => {
        // Inter-Wilaya wrangling: Replace up to 2 FLN Bases in any spaces
        // in Algeria with 1 guerrilla each. No change in commitment.
      },
      (role: Role) => {
        // Productive meeting:  Place up to 2 FLN bases in any spaces in
        // Algeria for free
        val hiddenGuerrilla  = new Bot.CriteriaFilter[Space]("Underground", _.hiddenGuerrillas > 0)
        val atLeastTwo       = new Bot.CriteriaFilter[Space]("2+ guerrillas", _.totalGuerrillas > 1)
        val noAdjacentTroops = new Bot.CriteriaFilter[Space]("No adjacent troops", 
          sp => !((getAdjacent(sp.name) map game.getSpace) exists (_.totalTroops > 0)))
        val notResettled     = new Bot.CriteriaFilter[Space]("Not resettled", !_.isResettled)
        val mountainSpace    = new Bot.CriteriaFilter[Space]("Is mountain", _.isMountains)
        val lowestPop        = new Bot.LowestScorePriority[Space]("Lowest population", _.population)
        val lowestCubes      = new Bot.LowestScorePriority[Space]("Lowest cubes", _.totalCubes)

        val noCubePriorities = List(hiddenGuerrilla, noAdjacentTroops, lowestPop, 
                                    atLeastTwo, mountainSpace, notResettled)
        
        val otherPriorities = List(hiddenGuerrilla, lowestCubes, atLeastTwo, 
                                   noAdjacentTroops, mountainSpace, lowestPop, notResettled)
                                   
        def placeBase(remaining: Int, candidates: List[Space]): Unit = {
          if (remaining > 0 && candidates.nonEmpty) {
            val sp = {
              val noCubes = candidates filter (_.totalCubes == 0)
              if (noCubes.nonEmpty)
                Bot.topPriority(noCubes, noCubePriorities)
              else
                Bot.topPriority(candidates, otherPriorities)
            }
            placePieces(sp.name, Pieces(flnBases = 1))
            placeBase(remaining - 1, candidates filterNot (_.name == sp.name))
          }
        }
        placeBase(2 min game.flnBasesAvailable, game.algerianSpaces filter soumanConf)
      }
    )),
    
    // ------------------------------------------------------------------------
    // #61 Special Instructions
    // Play if current event marked for FLN and FLN is 2nd eligible
    entry(new Card(61, "Morocco and Tunisia Independent", Single, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => () // Single event
    )),
    
    // ------------------------------------------------------------------------
    // #62 Special Instructions
    // Bot will never play this event
    entry(new Card(62, "Suez Crisis", Single, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => () // Single event
    )),
    
    // ------------------------------------------------------------------------
    // #63 Special Instructions
    // Bot will never play this event
    entry(new Card(63, "OAS", Single, false, AlwaysPlay,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => () // Single event
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(64, "Mobilization", Single, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => () // Single event
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(65, "Recall De Gaulle", Single, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => () // Single event
    )),
    
    // ------------------------------------------------------------------------
    // #66 Special Instructions
    // If FLN wins die roll, reduce Commitment.
    entry(new Card(66, "Coup d'etat", Single, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => () // Single event
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(67, "Propaganda!", Single, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(68, "Propaganda!", Single, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(69, "Propaganda!", Single, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(70, "Propaganda!", Single, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(71, "Propaganda!", Single, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    ))
  )
}

