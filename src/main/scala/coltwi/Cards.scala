
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
  
  val leadershipSnatch = (sp: Space) => sp.population > 0 && sp.isFlnControlled && !sp.isOppose
  
  val fifthBureau = (sp: Space) => sp.population > 0 && !sp.isOppose
  
  val beniOuiOui = (sp: Space) => sp.terror == 0 && sp.isNeutral
  
  val flnNavySource = (sp: Space) =>
    sp.coastal &&
    !sp.isSupport &&
    Bot.ConsiderMarch.eligibleGuerrillas(sp, true, true).total > 0
    
  val flnNavyDest = (sp: Space) => sp.coastal && sp.isSupport
  
  val elections = (sp: Space) => sp.isSector && sp.population > 0 && sp.isSupport
  
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
    
  def spaceCandidates(criteria: Space => Boolean): List[String] = 
    spaceNames(game.spaces filter criteria).sorted
    
  def algerianCandidates(criteria: Space => Boolean): List[String] = 
    spaceNames(game.algerianSpaces filter criteria).sorted
  
  def wilayaCandidates(wilaya: String)(criteria: Space => Boolean): List[String] =
    spaceNames(game.wilayaSpaces(wilaya) filter criteria).sorted
  
  val deckMap: Map[Int, Card] = Map(
    // ------------------------------------------------------------------------
    entry(new Card(1, "Quadrillage", Dual, false, false,
      () => NoEvent,
      (role: Role) => {
        // Sector forces: Place up to all French Police in Available
        // in up to 3 spaces.
        def nextSpace(remaining: Int, candidates: List[String]): Unit = {
          if (remaining > 0 && game.frenchPoliceAvailable > 0) {
            val choices = List(
              "space" -> "Select a space to place French Police",
              "done"  -> "Finished")
            println(s"\nFrench police in the available box: ${game.frenchPoliceAvailable}")
            askMenu(choices).head match {
              case "space" =>
                val name = askCandidate("Which space: ", candidates)
                val num  = askInt("How many French Police", 0, game.frenchPoliceAvailable)
                placePieces(name, Pieces(frenchPolice = num))
                nextSpace(remaining - 1, candidates filterNot (_ == name))
              case _ =>
            }
          }
        }
        if (game.frenchPoliceAvailable > 0)
          nextSpace(3, spaceNames(game.algerianSpaces).sorted)
        else
          log("There are no French Police in the available box")
      },
      (role: Role) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(2, "Balky Conscripts", Dual, false, false,
      () => NoEvent,
      (role: Role) => {
        // Aux armes citoyens: Free Train in up to 2 selectable spaces.
        Human.Train.execute(Human.Params(free = true, maxSpaces = Some(2)))
      },
      (role: Role) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(3, "Leadership Snatch", Dual, FlnMarked, AlwaysPlay,
      () => if (game hasAlgerianSpace leadershipSnatch) Shaded else NoEvent,
      (role: Role) => {
        // Gotcha: Activate all Guerrillas in 1 Wilaya
        val wilaya = askWilaya("\nSelect Wilaya to activate guerrillas: ")
        println()
        for (sp <- game.wilayaSpaces(wilaya))
          activateHiddenGuerrillas(sp.name, sp.hiddenGuerrillas)
      },
      (role: Role) => {
         // Widespread rage: Set up to 2 FLN-Controlled spaces to Oppose.
        def setOppose(candidates: List[Space], remaining: Int): Unit = {
          if (remaining > 0 && candidates.nonEmpty) {
            val sp = Bot.topPriority(candidates, OppositionPriorities)
            setSupport(sp.name, Oppose)
            setOppose(candidates filterNot (_.name == sp.name), remaining - 1)
          }
        }
        setOppose(game.algerianSpaces filter leadershipSnatch, 2)
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(4, "Oil & Gas Discoveries", Single, false, false,
      () => NoEvent,  // Bot never plays this event
      (role: Role) => {
        // Executor of the Event my add up to +2 Commitment. French cubes
        // equal to twice the Commitment added are removed from the map or
        // Available to out of play (Gov player's choice which cubes go)
        val frenchCubes   = game.availablePieces.frenchCubes + game.totalOnMap(_.frenchCubes)
        val maxNum        = 2 min (frenchCubes / 2) min (EdgeTrackMax - game.commitment)
        val numCommitment = askInt("Add how much commitment", 0, maxNum)
        val numCubes      = numCommitment * 2
        if (numCommitment == 0) 
          log("No commitment added.")
        else {
          increaseCommitment(numCommitment)
          def removeCubes(remaining: Int): Unit = if (remaining > 0) {
            val frenchCubesAvail = game.availablePieces.frenchCubes
            val frenchCubesOnMap = game.totalOnMap(_.frenchCubes)
            println()
            println(s"You must remove $remaining French cubes to out of play")
            val choice = {
              if (frenchCubesAvail == 0)      "map"
              else if (frenchCubesOnMap == 0) "avail"
              else {
                val choices = List("avail" -> "Remove cubes from the available box",
                                   "map"   -> "Remove cubes from the map")
                println("\nChoose one:")
                askMenu(choices).head
              }
            }
            
            val numRemoved = if (choice == "avail") {
              val num = askInt("Remove how many total cubes from available", 0, game.availablePieces.frenchCubes min remaining)
              if (num > 0) {
                val cubes = askPieces(game.availablePieces, num, FRENCH_CUBES)
                movePiecesFromAvailableToOutOfPlay(cubes)
              }
              num
            }
            else {
              val name = askCandidate("Select space to remove french cubes: ", spaceCandidates(_.frenchCubes > 0))
              val sp = game.getSpace(name)
              val num = askInt(s"Remove how many total cubes from $name", 0, sp.frenchCubes min remaining)
              if (num > 0) {
                val cubes = askPieces(sp.pieces, num, FRENCH_CUBES)
                removeToOutOfPlay(name, cubes)
              }
              num
            }
            removeCubes(remaining - numRemoved)
          }
          
          removeCubes(numCubes)
        }
      },
      (role: Role) => () // Single event
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(5, "Peace of the Brave", Dual, false, false,
      () => Shaded,
      (role: Role) => {
        // Amnesty: Until Propaganda round, in each selected Sweep or Assault
        // space, may also pay an extra Resource ro remove 1 Guerrilla 
        // (may be underground, max 1 per space, removed to available)
        playMomentum(MoPeaceOfTheBrave) // TODO: need to modify sweep and assault
      },
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
      (role: Role) => {
        // Friction: Either remove up to 3 Guerrillas in any 1 Wilaya to Available,
        // or move the France Track up to 2 boxes towards 'A'
        val guerrillaInAlgeria = game hasAlgerianSpace (_.totalGuerrillas > 0)
        val choice = if (game.franceTrack > 0 && guerrillaInAlgeria) {
          val choices = List(
            "france" -> "Shift the France track 2 boxes towards 'A'",
            "wilaya" -> "Remove up to 3 Guerrillas in any 1 Wilaya")
          println("\nChoose one:")
          askMenu(choices).head
        }
        else if (game.franceTrack > 0) "france"
        else if (guerrillaInAlgeria)   "wilaya"
        else "no-effect"
          
        choice match {
          case "france" =>
            decreaseFranceTrack(2 min game.franceTrack)
          case "wilaya" =>
            val withGuerrillas = (game.algerianSpaces filter (_.totalGuerrillas > 0) map (_.wilaya)).toSet
            val wilaya = askWilaya(allowed = withGuerrillas)
            val numToRemove = 3 min (game.wilayaSpaces(wilaya) map (_.totalGuerrillas)).sum
            println(s"\nRemove a total of $numToRemove guerrillas from spaces in Wilaya $wilaya")
            
            def removeGuerrillas(remaining: Int): Unit = {
              val candidates = wilayaCandidates(wilaya)(_.totalGuerrillas > 0)
              if (remaining > 0  && candidates.nonEmpty) {
                val name = askCandidate(s"Select space in Wilaya $wilaya: ", candidates)
                val sp = game.getSpace(name)
                val num = askInt(s"Remove how many guerrillas from $name", 0, remaining min sp.totalGuerrillas)
                if (num > 0) {
                  val guerrillas = askPieces(sp.pieces, num, GUERRILLAS)
                  removeToAvailableFrom(name, guerrillas)
                }
                removeGuerrillas(remaining - num)
              }
            }
            
            removeGuerrillas(numToRemove)
          case _        =>
            log("The event has no effect")
        }
      },
      (role: Role) => {
        // Lube: Free Rally in any 1 selectable space with a base.
        Bot.turnState = Bot.TurnState(
          specialActivityAllowed = false,
          freeOperation          = true,
          maxSpaces              = Some(1),
          onlyIn                 = spaceCandidates(_.flnBases > 0).toSet)
        Bot.ConsiderRally.execute() // Rally will only be used if it adds a base and/or shifts the France Track
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(7, "5th Bureau", Dual, FlnMarked, AlwaysPlay,
      () => if (game hasAlgerianSpace fifthBureau) Shaded else NoEvent,
      (role: Role) => {
        // Psychological warfare: Choose 1 Wilaya and roll 1d6; 
        // if roll <= total guerrillas in wilaya, remove up to the number
        // rolled from that Wilaya to available.
        val withGuerrillas = (game.algerianSpaces filter (_.totalGuerrillas > 0) map (_.wilaya)).toSet
        if (withGuerrillas.isEmpty)
          log("The event has no effect")
        else {
          val wilaya = askWilaya(allowed = withGuerrillas)
          val numGs  = (game.wilayaSpaces(wilaya) map (_.totalGuerrillas)).sum
          val die = dieRoll
          log()
          log(s"Number of guerrillas in Wilaya $wilaya: $numGs")
          log(s"Die roll: $die")
          if (die > numGs)
            log("The event has no effect")
          else {
            println(s"\nRemove a total of $die guerrillas from spaces in Wilaya $wilaya")
            
            def removeGuerrillas(remaining: Int): Unit = {
              val candidates = wilayaCandidates(wilaya)(_.totalGuerrillas > 0)
              if (remaining > 0  && candidates.nonEmpty) {
                val name = askCandidate(s"Select space in Wilaya $wilaya: ", candidates)
                val sp = game.getSpace(name)
                val num = askInt(s"Remove how many guerrillas from $name", 0, remaining min sp.totalGuerrillas)
                if (num > 0) {
                  val guerrillas = askPieces(sp.pieces, num, GUERRILLAS)
                  removeToAvailableFrom(name, guerrillas)
                }
                removeGuerrillas(remaining - num)
              }
            }
        
            removeGuerrillas(die)
          }
        }
      },
      (role: Role) => {
        // Propaganda flop: Shift any 2 Sectors 1 level each towards Opposition. 
       def shiftOppose(candidates: List[Space], remaining: Int): Unit = {
         if (remaining > 0 && candidates.nonEmpty) {
           val sp = Bot.topPriority(candidates, OppositionPriorities)
           decreaseSupport(sp.name, 1)
           shiftOppose(candidates filterNot (_.name == sp.name), remaining - 1)
         }
       }
       shiftOppose(game.algerianSpaces filter fifthBureau, 2)
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(8, "Cross-border air strike", Dual, false, false,
      () => NoEvent,  // Bot never plays this event
      (role: Role) => {
        // Effective: Remove up to 3 Guerrillas (may be underground) from
        // either Morocco or Tunisia to available.
        if (game.moroccoTunisiaIndependent) {
          val morocco = game.getSpace(Morocco)
          val tunisia = game.getSpace(Tunisia)
          val country = if (morocco.totalGuerrillas == 0 && tunisia.totalGuerrillas == 0) "none"
          else if (morocco.totalGuerrillas == 0) Tunisia
          else if (tunisia.totalGuerrillas == 0) Morocco
          else {
            println("\nRemove guerrillas from which country:")
            askMenu(List(Morocco -> Morocco, Tunisia -> Tunisia)).head
          }
          
          if (country == "none")
            log("No guerrillas in Morocco or Tunisia. The event has no effect")
          else {
            val sp = game.getSpace(country)
            val guerrillas = askPieces(sp.pieces, 3 min sp.totalGuerrillas, GUERRILLAS)
            removeToAvailableFrom(country, guerrillas)
          }
        }
        else
          log("Morocco and Tunisia are not independent. The event has no effect")
      },
      (role: Role) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(9, "Beni-Oui-Oui", Single, FlnMarked, AlwaysPlay,
      () => if (game hasAlgerianSpace beniOuiOui) Unshaded else NoEvent,
      (role: Role) => {
        // Set any two non-terrorized Neutral spaces to Support or Oppose
        if (role == Gov) {
          def affectNeutral(candidates: List[String], remaining: Int): Unit = {
            if (remaining > 0 && candidates.nonEmpty) {
              println()
              val name = askCandidate("Select a non-terrorized neutral space: ", candidates)
              val choices  = List("support" -> s"Set $name to Support", "oppose" -> s"Set $name to Opposition")
              setSupport(name, if (askMenu(choices).head == "support") Support else Oppose)
              affectNeutral(candidates filterNot (_ == name), remaining - 1)
            }
          }
          val candidates = algerianCandidates(beniOuiOui)
          if (candidates.isEmpty)
            log("No non-terrorized neutral spaces. The event has no effect")
          else
            affectNeutral(candidates, 2)
        }
        else { // Role == Fln
         def setOppose(candidates: List[Space], remaining: Int): Unit = {
           if (remaining > 0 && candidates.nonEmpty) {
             val sp = Bot.topPriority(candidates, OppositionPriorities)
             setSupport(sp.name, Oppose)
             setOppose(candidates filterNot (_.name == sp.name), remaining - 1)
           }
         }
         setOppose(game.algerianSpaces filter beniOuiOui, 2)
        }
      },
      (role: Role) => () // Single event
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(10, "Moudjahidine", Dual, FlnMarked, AlwaysPlay,
      () => Shaded,
      (role: Role) => {
        // Braggadocio: Activate all Guerrillas in 1 Wilaya
        deck(3).executeUnshaded(role)
      },
      (role: Role) => {
        // Sign me up: Until Propaganda Round, treat each Rally in a FLN-Con-trolled
        // space with no Base as if it contained 1 Base.
        playMomentum(MoMoudjahidine)
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(11, "Bananes", Dual, FlnMarked, AlwaysPlay,
      () => if (game.commitment > 0) Shaded else NoEvent,
      (role: Role) => {
        // H-21 helicopters: Until Propaganda round, raise number
        // of troop lift spaces by 2
        playMomentum(MoBananes)
      },
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
      (role: Role) => {
        // S-55 helicopters: Until Propaganda round, raise number of 
        // troop lift spaces by 1
        playMomentum(MoVentilos)
      },
      (role: Role) => {
        // FLN Navy: May redistribute 1-3 guerrillas (1d6 halved, round up)
        // among any 3 coastal spaces for free.  Guerrillas do no activate.
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
        // Hearts and Minds: Train may Pacify in up to 2 selectable spaces.
        playCapability(CapGovSaS)
      },
      (role: Role) => {
        // Caution: Assault my target only 1 space per card.
        playCapability(CapFlnSaS)
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(14, "Protest in Paris", Single, FlnMarked, AlwaysPlay,
      () => if (game.franceTrack < FranceTrackMax) Shaded else NoEvent,
      (role: Role) => {
        // Executor of Event may move France Track marker up to 2 spaces
        // left or right
        if (role == Gov)
          decreaseFranceTrack(2)
        else
          increaseFranceTrack(2)
      },
      (role: Role) => () // Single event
    )),
    
    // ------------------------------------------------------------------------
    // By the standard Bot rules, only the shaded event would be
    // considered.  I have added considering the Unshaded event
    // in the unlikey case that government commitment is at zero.
    entry(new Card(15, "Jean-Paul Sarte", Dual, false, false,
      () => if (game.commitment > 0) Shaded 
            else if (game.resources(Fln) < EdgeTrackMax) Unshaded 
            else NoEvent
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
      () => NoEvent,  // Bot never plays this event
      (role: Role) => {
        // Force de Frappe releases conventional troops: 
        // Move 1d6 French cubes from Out of Play to Available.
        if (game.outOfPlay.frenchCubes == 0)
          log("No French cubes are out of play. The event has no effect")
        else {
          val die = dieRoll
          log(s"Die roll is: $die")
          val cubes = if (game.outOfPlay.frenchCubes <= die)
            game.outOfPlay.only(FRENCH_CUBES)
          else if (game.outOfPlay.frenchTroops == 0)
            Pieces(frenchPolice = die)
          else if (game.outOfPlay.frenchPolice == 0)
            Pieces(frenchTroops = die)
          else 
            askPieces(game.outOfPlay, die, FRENCH_CUBES, Some("Move cubes from Out of Play to Available"))
          
          movePiecesFromOutOfPlayToAvailable(cubes)
        }
      },
      // Continental war scare: Move 1d6 French cubes from available or
      // the map to Out of Play (Government player's choice)
      (role: Role) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(17, "Commandos", Dual, false, AlwaysPlay,
      () => Shaded,
      (role: Role) => {
        // Commandos de Chasse: Each Algerian cube participating in
        // a Garrison or Sweep in a Mountain sector activates 1 guerrilla.
        playCapability(CapGovCommandos)
      },
      (role: Role) => {
        // Zonal Commandos: Ambush does not Activate the Guerrilla.
        playCapability(CapFlnCommandos)
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(18, "Torture", Single, FlnMarked, AlwaysPlay,
      () => Unshaded,
      (role: Role) => {
        // -1 Commitment for each Neutralize executed. In each selected
        // Neutralize space, may remove 1 additional piece, which may be
        // underground.  (Guerrillas before bases rule still applies)
        // Removed Guerrilla goes to Available or Casualties depending 
        // sequence.
        playCapability(CapTorture)
      },
      (role: Role) => () // Single event
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(19, "General Strike", Dual, FlnMarked, AlwaysPlay,
      () => if (game.resources(Fln) < EdgeTrackMax) Shaded else NoEvent,
      (role: Role) => {
        // Heads broken: Set 1 selected City to Neutral. 
        // Add commitment = population of city
        val candidates = algerianCandidates(sp => sp.isCity && !sp.isNeutral)
        val name = if (candidates.isEmpty) ""
        else if (candidates.size == 1)
          candidates.head
        else
          askCandidate("Select a city: ", candidates)

        if (name == "")
          log("All cities are already Neutral.  The event has no effect")
        else {
          val sp = game.getSpace(name)
          setSupport(sp.name, Neutral)
          increaseCommitment(sp.population)
        }
      },
      (role: Role) => {
        // United Nations resolution condemning violence
        // raises FLN profile: +2 FLN Resources
        increaseResources(Fln, 2)
      }
    )),
    
    // ------------------------------------------------------------------------
    // #20 Special Instructions
    // Choose Op(+SA)
    entry(new Card(20, "Suave qui peut", Single, FlnMarked, false,
      () => NoEvent,  // Bot never plays this event
      (role: Role) => {
        // Defections and desertions: Executor of Event may removes up to 3
        // Guerrillas or Algerian Police to available, pay 1 resource each.
        if (game.resources(Gov) == 0)
          log(s"$Gov has no resources.  The event has no effect")
        else {
          val totalGuerrillas = game.totalOnMap(_.totalGuerrillas)
          val totalPolice     = game.totalOnMap(_.algerianPolice)
          val choice = if (totalGuerrillas + totalPolice == 0)
            "none"
          else if (totalGuerrillas == 0)
            "police"
          else if (totalPolice == 0)
            "guerrillas"
          else {
            println("\nChoose one:")
            askMenu(List("guerrillas" -> "Remove Guerrillas", "police" -> "Remove Algerian police")).head
          }

          def removePieces(remaining: Int, types: Seq[PieceType], desc: String): Unit = {
            val candidates = spaceCandidates(_.totalOf(types) > 0)
            if (remaining > 0 && candidates.nonEmpty) {
              val name   = askCandidate(s"\nSelect space to remove $desc: ", candidates)
              val sp     = game.getSpace(name)
              val most   = remaining min sp.totalOf(types)
              val num    = askInt(s"Remove how many $desc from $name", 0, most)
              val pieces = askPieces(sp.pieces, num, types)
              removeToAvailableFrom(name, pieces)
              removePieces(remaining - num, types, desc)
            }
          }
          
          choice match {
            case "guerrillas" =>
              val num = askInt("Remove how many total guerrillas", 1, 3 min game.resources(Gov))
              decreaseResources(Gov, num)
              removePieces(num, GUERRILLAS, "guerrillas")
            case "police" =>
              val num = askInt("Remove how many total Algerian police", 1, 3 min game.resources(Gov))
              decreaseResources(Gov, num)
              removePieces(num, Seq(AlgerianPolice), "Algerian police")
            case _ => log("There are no guerrillas or Algerian police on the map.  The event has no effect")
          }
        }
      },
      (role: Role) => () // Single event
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(21, "United Nations Resolution", Dual, false, false,
      () => if (game.commitment > 0) Shaded else NoEvent,
      (role: Role) => {
        // Mind your own business! +1 Commitment
        increaseCommitment(1)
      },
      (role: Role) => {
        // Binding: -1 Commitment
        decreaseCommitment(1)
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(22, "The Government of USA is Convinced...", Dual, FlnMarked, AlwaysPlay,
      () => if (game.commitment > 0) Shaded else NoEvent,
      (role: Role) => {
        // FLN are Communists: +2 Commitment
        increaseCommitment(2)
      },
      (role: Role) => {
        // Algeria is entitled to self-determination: -2 Commitment
        decreaseCommitment(2)
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(23, "Diplomatic Leanings", Dual, FlnMarked, AlwaysPlay,
      () => if (game.resources(Fln) < EdgeTrackMax) Shaded else NoEvent,
      (role: Role) => {
        // Pressure on protectorates: Remove up to 1 base in each
        // of Morocco and Tunisia. (If independent). No increase in
        // Commitment.
        if (!game.moroccoTunisiaIndependent)
          log("Morocco and Tunisia are not independent: The event has no effect")
        else if (game.getSpace(Morocco).flnBases == 0 && game.getSpace(Tunisia).flnBases == 0)
          log("No FLN bases in Morocco or Tunisia: The event has no effect")
        else {
          if (game.getSpace(Morocco).flnBases > 0)
            removeToAvailableFrom(Morocco, Pieces(flnBases = 1))
          if (game.getSpace(Tunisia).flnBases > 0)
            removeToAvailableFrom(Tunisia, Pieces(flnBases = 1))
        }
      },
      (role: Role) => {
        // Arab Bloc solidarity: +6 FLN resources
        increaseResources(Fln, 6)
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(24, "Economic Development", Dual, FlnMarked, AlwaysPlay,
      () => if (game.resources(Gov) > 0) Shaded else NoEvent,
      (role: Role) => {
        // Constantine Plan: Set up to 2 Government controlled spaces
        // to Support
        val canAffect = (sp: Space) => sp.isGovControlled && !sp.isSupport
        if (game hasAlgerianSpace canAffect) {
          def nextSpace(remaining: Int): Unit = {
            val candiates = algerianCandidates(canAffect)
            if (remaining > 0 && candiates.nonEmpty) {
              val name = askCandidate("Select space: ", candiates)
              setSupport(name, Support)
              nextSpace(remaining - 1)
            }
          }
          nextSpace(2)
        }
        else
          log("No Government controlled spaces not at Support. The event has no effect")
      },
      (role: Role) => {
        // Military funds diverted to social enterprises:
        // -6 government resources
        decreaseResources(Gov, 6)
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(25, "Purge", Single, false, false,
      () => Unshaded,
      (role: Role) => {
        // Disloyal commanders: 1-3 enemy pieces (1d6 halved, round up,
        // executor's choice, but not bases) moved from map to Available;
        // And -1d6 enemy resources
        val piecesDie = dieRoll
        val numPieces = (piecesDie + 1) / 2
        val resourcesDie = dieRoll
        if (role == Gov) {
          def removeGuerrillas(remaining: Int): Unit = {
            val candidates = spaceCandidates(_.totalGuerrillas > 0)
            if (remaining > 0 && candidates.nonEmpty) {
              val name   = askCandidate(s"\nSelect space to remove guerrillas: ", candidates)
              val sp     = game.getSpace(name)
              val most   = remaining min sp.totalGuerrillas
              val num    = askInt(s"Remove how many guerrillas from $name", 0, most)
              val pieces = askPieces(sp.pieces, num, GUERRILLAS)
              removeToAvailableFrom(name, pieces)
              removeGuerrillas(remaining - num)
            }
          }
          
          val guerrillasOnMap = game.totalOnMap(_.totalGuerrillas)
          if (guerrillasOnMap == 0)
            log("No guerrillas on the map to remove.")
          else {
            log(s"Die roll for guerrillas is $piecesDie.  $Gov may remove up to ${amountOf(numPieces, "guerrilla")}")
            removeGuerrillas(numPieces min guerrillasOnMap)
          }
          log(s"Die roll for resources is $resourcesDie")
          decreaseResources(Fln, resourcesDie)
        }
        else {
          var numRemoved = 0
          val basePriorities = List(
            new Bot.CriteriaFilter[Space]("Has FLN Base", _.flnBases > 0),
            new Bot.LowestScorePriority[Space]("Least guerrillas", _.totalGuerrillas))
          val otherPriorities = List(
            new Bot.CriteriaFilter[Space]("Guerrilla present", _.totalGuerrillas > 0),
            new Bot.CriteriaFilter[Space]("Gov control", _.isGovControlled),
            new Bot.HighestScorePriority[Space]("Most guerrillas", _.totalGuerrillas))
        
          log(s"Die roll for pieces is $piecesDie.  $Fln may remove up to $numPieces enemy pieces")
          def doRemove(candiates: List[Space], pieceType: PieceType): Unit = {
            if (numRemoved < numPieces && candiates.nonEmpty) {
              val sp = if (candiates exists (_.flnBases > 0))
                Bot.topPriority(candiates, basePriorities)
              else
                Bot.topPriority(candiates, otherPriorities)
              val num = (numPieces - numRemoved) min sp.pieces.numOf(pieceType)
              numRemoved += num
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
        }
      },
      (role: Role) => () // Single event
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(26, "Casbah", Dual, FlnMarked, AlwaysPlay,    
      () => if (Bot.getGuerrillasToPlace(4, game.getSpace(Algiers)).total > 0) Shaded else NoEvent,
      (role: Role) => {
        // Ratonade: Remove up to all FLN pieces in Algiers to Available.
        // +1 Commitment per base removed.  +1 FLN resource for each piece
        // removed.
        val sp = game.getSpace(Algiers)
        if (sp.totalFln == 0)
          log("There are no FLN pieces in Algiers.  The event has no effect")
        else {
          val flnPieces = sp.only(Seq(HiddenGuerrillas, ActiveGuerrillas, FlnBases))
          removeToAvailableFrom(Algiers, flnPieces)
          increaseCommitment(sp.flnBases)
          increaseResources(Fln, sp.totalFln)
        }
      },
      (role: Role) => {
        // Urban uprising: Place up to 4 Guerrillas in Algiers.  If this makes
        // Algiers FLN-controlled, may Agitage up to 1 level for free.
        // (Remove 1 terror marker or if there are no terror markers 
        //  shift 1 level toward Oppose)
        val toPlace = Bot.getGuerrillasToPlace(4, game.getSpace(Algiers))
        Bot.placeGuerrillas(Algiers, toPlace)
        
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
        // Dead Zones: FLN Guerrillas may not march again if in 
        // the same Wilaya.
        playCapability(CapDeadZones)
      },
      (role: Role) => {
        // Cross-Wilaya coordination: In Redeploy phase of Propaganda round,
        // Guerillas may move from any spaces to any spaces with friendly bases.
        // All spaces must be in Algeria.
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
      // Executor of Event may place up to 2 Terror markers, placed
      // after paying 1 resource per marker, in any spaces in Algeria
      // (even if terror already present)
      // Set these spaces to Neutral, -1 Commitment for each terror marker
      // placed (no matter who executed the event)
      (role: Role) => if (role == Gov) {
        if (game.resources(Gov) == 0)
          log(s"$Gov has no resources.  The event has no effect")
        else if (game.terrorMarkersAvailable == 0)
          log(s"There are no available terror markers.  The event has no effect")
        else {
          def nextMarker(remaining: Int): Unit = if (remaining > 0) {
            val name = askCandidate("\nSelect space for terror marker: ", spaceNames(game.algerianSpaces).sorted)
            decreaseResources(Gov, 1)
            addTerror(name, 1)
            setSupport(name, Neutral)
            decreaseCommitment(1)
            nextMarker(remaining - 1)
          }
          val num = if ((game.terrorMarkersAvailable min game.resources(Gov)) == 1) 1
          else askInt("Place how many terror markers", 1, 2)
          nextMarker(num)
        }
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
      (role: Role) => {
        // Bonjour M. Bidasse: Move any number of French Police from 
        // Out of Play to Available.  Subtract Commitment = 1/3 of
        // total moved (round down)
        if (game.outOfPlay.frenchPolice == 0) {
          log("There are no French police in the out of play box")
          log("The event has no effect")
        }
        else {
          val canAfford = game.commitment * 3 + 2
          val most = game.outOfPlay.frenchPolice min canAfford
          val num = askInt("\nMove how many French police from Out of Play to Available", 1, most)
          movePiecesFromOutOfPlayToAvailable(Pieces(frenchPolice = num))
          decreaseCommitment(num / 3)
        }
      },
      (role: Role) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(30, "Change in Tactics", Single, false, AlwaysPlay,
      () => if (game.capabilities exists (_.startsWith("Gov:"))) Unshaded else NoEvent,
      // Remove any 1 Capability marker that is in effect.
      // That capability no longer applies.
      (role: Role) => if (role == Gov) {
        val caps = (game.capabilities filter (_.startsWith("Dual:"))) :::
                   (game.capabilities filter (_.startsWith("FLN:")))  :::
                   (game.capabilities filter (_.startsWith("Gov:")))
        val index = caps.size match {
          case 0 => -1
          case 1 =>  0
          case _ =>
            val choices = caps.zipWithIndex map (_.swap)
            println("\nRemove which capability from play:")
            askMenu(choices).head
        }
        if (index == -1 )
          log("There are no capabilities in play.  The event has no effect")
        else
          removeCapabilityFromPlay(caps(index))
      }
      else {
        val cap = shuffle(game.capabilities filter (_.startsWith("Gov:"))).head
        removeCapabilityFromPlay(cap)
      },
      (role: Role) => () // Single event
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(31, "Intimidation", Dual, FlnMarked, AlwaysPlay,
      () => if (game.resources(Fln) < EdgeTrackMax) Shaded else NoEvent,
      (role: Role) => {
        // Empty Threat: Until Propaganda round, Terror places 
        // marker but does not set space to Neutral.
        playMomentum(MoIntimidation)
      },
      (role: Role) => {
        // Persuasive donation drive: Add FLN resources equal to the
        // number indicated by the marker in the France Track
        val entry = FranceTrack(game.franceTrack)
        increaseResources(Fln, entry.resource)
      }
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(32, "Teleb the Bomb-maker", Dual, false, AlwaysPlay,
      () => Shaded,
      (role: Role) => {
        // Amateurs: Terror in City requires Activation of
        // 2 underground guerrillas.
        playCapability(CapAmateurBomber)
      },
      (role: Role) => {
        // Effective: Terror in City costs 0 resources
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
    entry(new Card(34, "Elections", Dual, FlnMarked, AlwaysPlay,
      () => if (game hasAlgerianSpace elections) Shaded else NoEvent,
      (role: Role) => {
        // Integrationist mandate: +1 Commitment or set 1 non-terrorized 
        // Neutral space to Support
        val candidates = algerianCandidates(sp => sp.isNeutral && sp.terror == 0 && sp.population > 0)
        val choice = if (game.commitment == EdgeTrackMax && candidates.isEmpty) "none"
        else if (game.commitment == EdgeTrackMax) "space"
        else if (candidates.isEmpty) "commit"
        else {
          println("\nChoose one:")
          askMenu(List("commit" -> "Increase commitment by 1", 
                       "space"  -> "Set non-terrorized Neutral space to Support")).head
        }
        choice match {
          case "commit" => increaseCommitment(1)
          case "space"  => setSupport(askCandidate("Select space: ", candidates), Support)
          case _        => log("The event has no effect")
        }
      },
      (role: Role) => {
        // Voter Supression: Set 1 sector to Neutral
        val priorities = List(
          new Bot.HighestScorePriority[Space]("Highest population", _.population),
          new Bot.CriteriaFilter[Space]("Gov cannot train", sp => !sp.canTrain))
        
        val candidates = game.algerianSpaces filter elections
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
        if (game.totalOnMap(_.totalGuerrillas) == 0 && game.resources(Fln) == 0)
          log("The event has no effect")
        else {
          val candidates = spaceCandidates(_.totalGuerrillas > 0)
          if (candidates.nonEmpty) {
            val name      = askCandidate("Select space to remove guerrilla: ", candidates)
            val sp        = game.getSpace(name)
            val guerrilla = askPieces(sp.pieces, 1, GUERRILLAS)
            removeToCasualties(name, guerrilla)
          }
          decreaseResources(Fln, dieRoll)
        }
        
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
        Human.Train.execute(Human.Params(free = true, maxSpaces = Some(2)))
      },
      (role: Role) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(38, "Economic Crisis in France", Dual, false, false,
      () => if (game.resources(Gov) > 0 || game.commitment > 0) Shaded else NoEvent,
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
        def nextSpace(remaining: Int, candidates: List[String]): Unit = if (remaining > 0 && candidates.nonEmpty) {
          val name = askCandidate("\nSelect mountain space with guerrillas and no FLN base: ", candidates)
          removeToAvailableFrom(name, game.getSpace(name).only(GUERRILLAS))
          nextSpace(remaining - 1, candidates filterNot (_ == name))
        }

        val candidates = algerianCandidates(sp => sp.isMountains && sp.totalGuerrillas > 0 && sp.flnBases == 0)
        if (candidates.isEmpty)
          log("No mountain spaces qualify.  The event has no effect")
        else
          nextSpace(2, candidates)
      },
      (role: Role) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(40, "Strategic Movement", Dual, false, false,
      () => NoEvent,  // Bot will never use this event
      (role: Role) => {
        // Sea Lift: May redistribute up to 6 cubes among
        // any 3 coastal spaces for free.
        def movableCubes(sp: Space) = sp.pieces.only(CUBES) - Human.movingGroups(sp.name)
        def totalMoved = Human.movingGroups.toList.foldLeft(0) { case (sum, (_, p)) => sum + p.totalCubes }
        
        def selectSpaces(num: Int, total: Int, candidates: List[String]): List[String] = {
          if (num > total)
            Nil
          else {
            val name = askCandidate(s"Select ${ordinal(num)} coastal space: ", candidates)
            name :: selectSpaces(num + 1, total, candidates filterNot (_ == name))
          }
        }
        
        def nextChoice(selectedNames: List[String]): Unit = {
          val selectedSpaces = spaces(selectedNames)
          val sourceCandidates = selectedSpaces filter (movableCubes(_).total > 0)
          val choices = List(
            choice(totalMoved < 6 && sourceCandidates.nonEmpty, "cube",  s"Select a cubes to move"),
            choice(true,                                        "done",  s"Finished moving cubes"),
            choice(true,                                        "abort", s"Abort the entire $Gov turn")
          ).flatten
      
          val avail = sourceCandidates.sortBy(_.name) map { sp => 
            s"${sp.name} (${sp.pieces.only(CUBES) - Human.movingGroups(sp.name)})"
          }
          val moved = Human.movingGroups.toList.sortBy(_._1) map { case (n, p) => s"$n ($p)"}
          println("\n")
          println("Cubes available to move:")
          println(separator())
          wrap("  ", avail) foreach println
          
          println("\n")
          println("Cubes already to moved:")
          println(separator())
          wrap("  ", moved) foreach println
          
          println("\nChoose one:")
          askMenu(choices, allowAbort = false).head match {
            case "cube" =>
              val savedState = game
              try {
                val source = askCandidate(s"\nSelect source space: ", spaceNames(sourceCandidates).sorted)
                val dest   = askCandidate(s"Select destination space: ", (selectedNames filterNot (_ == source)))
                val sp     = game.getSpace(source)
                val most   = (6 - totalMoved) min movableCubes(sp).total
                val num    = askInt(s"Move how many cubes from $source to $dest", 0, most)
                val p      = askPieces(movableCubes(sp), num, CUBES)
                movePieces(p, source, dest)
                Human.movingGroups.add(dest, p)
              }
              catch {
                case AbortAction =>
                  println(s"\n>>>> Aborting cube movement <<<<")
                  println(separator())
                  displayGameStateDifferences(game, savedState)
                  game = savedState
              }
              nextChoice(selectedNames)
          
            case "abort" =>
              if (askYorN("Really abort? (y/n) ")) throw AbortAction
              nextChoice(selectedNames)
          
            case "done" =>
          }
        }
            
        val numSpaces = askInt("How many coastal spaces do you wish to select", 2, 3)
        val selectedNames = selectSpaces(1, numSpaces, algerianCandidates(_.coastal))
        nextChoice(selectedNames)
      },
      (role: Role) => ()
    )),
    
    // ------------------------------------------------------------------------
    entry(new Card(41, "Egypt", Dual, FlnMarked, AlwaysPlay,
      () => if (game.resources(Fln) < EdgeTrackMax) Shaded else NoEvent,
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
      () => if (game.resources(Fln) < EdgeTrackMax) Shaded else NoEvent,
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
        def selectCities(num: Int, total: Int, candidates: List[String]): List[String] = {
          if (num > total)
            Nil
          else {
            val name = askCandidate(s"Select ${ordinal(num)} city: ", candidates)
            name :: selectCities(num + 1, total, candidates filterNot (_ == name))
          }
        }
        
        selectCities(1, 2, spaceCandidates(_.isCity)) foreach addPlus1PopMarker
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
        // Force K: Replace up to all Algerian Police in any 1 sector with
        // an equal number of Guerrillas.
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
        val candidates = algerianCandidates(sp => sp.isSector && sp.totalGuerrillas > 0)
        if (candidates.isEmpty)
          log("The event has no effect")
        else {
          val name = askCandidate("Select sector with guerrillas: ", candidates)
          val guerrillas = askPieces(game.getSpace(name).pieces, 3, GUERRILLAS,
                                 Some(s"Select guerrillas to remove from $name"))
          removeToAvailableFrom(name, guerrillas)
          val police = askPiecesToPlace(name, List(AlgerianPolice), guerrillas.total)
          placePieces(name, police)
        }
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
          val guerrillas = askPieces(sp.pieces, sp.totalGuerrillas / 2, GUERRILLAS)
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
      () => Unshaded,
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
          val guerrillas = askPieces(sp.pieces, sp.totalGuerrillas / 2, GUERRILLAS)
          removeToOutOfPlay(Tunisia, guerrillas)
        }
      },
      (role: Role) => {
        // Strengthen government-in-exile:  Place 1 Base in Morocco or Tunisia
        // if stacking permits
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

