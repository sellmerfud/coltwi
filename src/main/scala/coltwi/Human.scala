
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
import java.io.IOException
import scala.util.Random.{shuffle, nextInt}
import scala.annotation.tailrec
import scala.util.Properties.{lineSeparator, isWin}
import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine
import ColonialTwilight._


object Human {
  // These variables are used throughout a single execution only.
  private var includeSpecialActivity  = false
  private var specialActivityComplete = false
  
  def canDoSpecialActivity = includeSpecialActivity && !specialActivityComplete
  
  case class Params(
    includeSpecialActivity: Boolean = false,
    maxSpaces: Option[Int]          = None,
    free: Boolean                   = false, // Events grant free commands
    onlyIn: Option[Set[String]]     = None,  // Limit command to the given spaces
    eventAction: Boolean            = false  // Event actions ignore some limitations.
  )
  
  sealed trait GovOp {
    val specialActivites: List[GovSpecial]
    def spaceFilter(sp: Space): Boolean
    def validSpaces(params: Params): Set[String] = {
      val names = spaceNames(game.spaces filter spaceFilter).toSet
      params.onlyIn map (only => names intersect only) getOrElse names
    }
    def performIn(spaceName: String, params: Params): Unit
    def afterSpaces(completedSpaces: Set[String], params: Params): Unit = ()
    
  }
  
  object Train extends GovOp {
    override def toString() = "Train"
    val specialActivites = List(Deploy, TroopLift)
    
    override def spaceFilter(sp: Space): Boolean = if (game.pivotalCardsPlayed(PivotalRecallDeGaulle))
        sp.isCity || sp.hasGovBase || (sp.isGovControlled && sp.pieces.totalTroops > 0 && sp.pieces.totalPolice > 0)
      else
        sp.isCity || sp.hasGovBase
    
    // Override the default so we can add the France Track and Border Zone Track
    override def validSpaces(params: Params): Set[String] = {
      val ft  = if (game.franceTrack > 0)
        Set(FranceTrackName)
      else
        Set.empty
      
      val bzt = if (game.pivotalCardsPlayed(PivotalMoroccoTunisiaIndepdent) && game.borderZoneTrack < BorderZoneTrackMax)
        Set(BorderZoneTrackName)
      else
        Set.empty
      
      ft ++ bzt ++ super.validSpaces(params)
    }
    
    override def performIn(spaceName: String, params: Params): Unit = {
      spaceName match {
        case FranceTrackName     => decreaseFranceTrack(1)
        case BorderZoneTrackName => increaseBorderZoneTrack(1)
        case _ =>
          val toPlace = askPiecesToPlace(spaceName, List(AlgerianTroops, AlgerianPolice), maxToPlace = 4)
          placePieces(spaceName, toPlace)
      }
    }
    
    // Ask the user in which space(s) to pacify
    override def afterSpaces(completedSpaces: Set[String], params: Params): Unit = {
      val mapSpaces = completedSpaces - FranceTrackName - BorderZoneTrackName
      val candidateSpaces = mapSpaces filter (!game.getSpace(_).isSupport)
      val maxSpaces = if (capabilityInPlay(CapGovSaS)) 2 else 1
    
      def nextPacify(completed: Int, candidates: Set[String]): Unit = {
        val prompt = if (completed == 0) "Do you wish to pacify in one of the training spaces? (y/n) "
                     else "Do you wish to pacify in a second training space? (y/n) "
        if (completed < maxSpaces && candidates.nonEmpty && game.resources(Gov) > 1 && askYorN(prompt)) {
          val name = askCandidate("Choose space to pacify: ", candidates.toList.sorted, allowAbort = false)
          val sp = game.getSpace(name)
          val maxInSpace = sp.terror + 1
          val maxPossible = maxInSpace min (game.resources(Gov) / 2)
          val choices = List.range(maxPossible, -1, -1) map {
            case 0                    => (0 -> "Do not pacify in this space")
            case 1 if sp.terror == 0  => (1 -> "Shift one level toward support")
            case n if n == maxInSpace => (n -> "Remove all terror markers and shift one level toward support")
            case n                    => (n -> s"Remove ${amountOf(n, "terror marker")}")
          }
          println("\nChoose one:")
          val num = askMenu(choices, allowAbort = false).head
          if (num == 0)
            nextPacify(completed, candidates)
          else {
            decreaseResources(Gov, num * 2)
            if (sp.terror > 0)
              removeTerror(name, if (num == maxInSpace) num-1 else num)
            if (num == maxInSpace)
              increaseSupport(name, 1)
            nextPacify(completed+1, candidates - name)
          }
        }
      }
      
      nextPacify(0, candidateSpaces)
    } 
  }
  
  object Garrison extends GovOp {
    override def toString() = "Garrison"
    val specialActivites = List(TroopLift, Neutralize)
    
    override def spaceFilter(sp: Space): Boolean = true
    override def validSpaces(params: Params): Set[String] = Set.empty
    override def performIn(spaceName: String, params: Params): Unit = ()
  }
  
  object Sweep extends GovOp {
    override def toString() = "Sweep"
    val specialActivites = List(TroopLift, Neutralize)
    
    override def spaceFilter(sp: Space): Boolean = true
    override def validSpaces(params: Params): Set[String] = Set.empty
    override def performIn(spaceName: String, params: Params): Unit = ()
  }
  
  object Assault extends GovOp {
    override def toString() = "Assault"
    val specialActivites = List(TroopLift)
    
    override def spaceFilter(sp: Space): Boolean = true
    override def validSpaces(params: Params): Set[String] = Set.empty
    override def performIn(spaceName: String, params: Params): Unit = ()
  }
  
  sealed trait GovSpecial
  object Deploy     extends GovSpecial { override def toString() = "Deploy"}
  object TroopLift  extends GovSpecial { override def toString() = "Troop Lift"}
  object Neutralize extends GovSpecial { override def toString() = "Neutralize"}
  
  
  def askOp(): GovOp = {
    val choices = List(
      Train    -> Train.toString,
      Garrison -> Garrison.toString,
      Sweep    -> Sweep.toString,
      Assault  -> Assault.toString)
    println("\nChoose Op:")
    askMenu(choices).head
  }
  
  def askSpecial(op: GovOp): GovSpecial = {
    val choices = op.specialActivites map (s => s -> s.toString)
    println("\nChoose special ability:")
    askMenu(choices).head
  }
  
  
  // Top level entry point to human actions
  def executeAction(action: Action): Action = action match {
    case Pass               => performPass(Gov); Pass
    case ExecOpPlusActivity => 
      val num = executeOp(Params(includeSpecialActivity = true))
      if (specialActivityComplete) ExecOpPlusActivity else if (num < 2) ExecLimitedOp else ExecOpOnly
    case ExecOpOnly         => if (executeOp() < 2) ExecLimitedOp else ExecOpOnly
    case ExecLimitedOp      => executeOp(Params(maxSpaces = Some(1))); ExecLimitedOp
    case Event              => executeEvent(); Event
  }
  
  // Return the number of spaces acted upon
  def executeOp(params: Params = Params()): Int =  {
    includeSpecialActivity  = params.includeSpecialActivity
    specialActivityComplete = false
    
    val op = askOp()
    var completedSpaces = Set.empty[String]

    def nextChoice(): Unit = {
      val candidateSpaces = params.maxSpaces match {
        case Some(m) if completedSpaces.size >= m => Set.empty
        case _  => op.validSpaces(params) -- completedSpaces
      }
      // TODO:  
      // The choices should be slightly different for Garrison since it may involve police
      // cubes from anywhere on the map.  The user first selects up to 6 police cubes.
      // Then select 1 or more destination spaces for those police cubes.
      // Finally, he selects 1 space to reveal underground guerrillas
      val choices = List(
        choice(candidateSpaces.nonEmpty,  "space",   s"Select a space and perform a $op operation there"),
        choice(canDoSpecialActivity,      "special",  s"Perform a special activity"),
          // Voluntary removal ONLY IF piece is not available
        // choice(true,                      "remove",   "Voluntarily remove pieces from the map"),
        choice(true,                      "done",     s"Finished selecting spaces"),
        choice(true,                      "abort",    s"Abort the entire $Gov turn")
      ).flatten
      
      println("\n")
      println(s"$Gov $op")
      println(separator(char = '='))
      // wrap("spaces available: ", candidateSpaces.toList.sorted) foreach println
      wrap("spaces completed: ", completedSpaces.toList.sorted(SpaceNameOrdering)) foreach println
      println(s"\nChoose one (${amtRes(Gov)} remaining):")
      askMenu(choices, allowAbort = false).head match {
        case "space" =>
          askCandidateAllowNone(s"\nChoose space to $op: ", candidateSpaces.toList.sorted(SpaceNameOrdering)) foreach { spaceName =>
            log()
            log(s"$Gov executes $op operation: $spaceName")
            if (performOpInSpace(spaceName, op, params))
              completedSpaces += spaceName
          }
          nextChoice()
          
        case "special" =>
          executeSpecialActivity(op, params)
          nextChoice()
          
        case "abort" =>
          if (askYorN("Really abort? (y/n) ")) throw AbortAction
          nextChoice()
          
        case "done" =>
      }
    }
    
    // Garrison is paid once for the entire operation.
    if (op == Garrison && !params.free) {
      log()
      decreaseResources(Gov, 2)
    }
    nextChoice()
    op.afterSpaces(completedSpaces, params)
    completedSpaces.size
  }
    
  // Returns true if the operation was not aborted.
  def performOpInSpace(spaceName: String, op: GovOp, params: Params): Boolean = {
    val savedState = game
    try {
      if (op != Garrison && !params.free) {
        log()
        decreaseResources(Gov, 2)
      }
      op.performIn(spaceName, params)
      true
    }
    catch {
      case AbortAction =>
        println(s"\n>>>> Aborting $op in $spaceName <<<<")
        println(separator())
        displayGameStateDifferences(game, savedState)
        game = savedState
        false
    }
  }
  
  def executeEvent(): Unit = {
    
  }
  
  def executeSpecialActivity(op: GovOp, params: Params): Unit = {
    // Ask which ability, then execute it.
    specialActivityComplete = true
  }
  
  
}