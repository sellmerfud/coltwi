
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

    
  object specialActivity {
    private var alllowSpecialActivity = false
    private var specialActivityTaken  = false
    
    def init(allow: Boolean): Unit = {
      alllowSpecialActivity = allow
      specialActivityTaken  = false
    }
    def allowed = alllowSpecialActivity && !specialActivityTaken
    def completed(): Unit = specialActivityTaken = true
    def taken = specialActivityTaken
  }
    
  case class Params(
    includeSpecialActivity: Boolean = false,
    maxSpaces: Option[Int]          = None,
    free: Boolean                   = false, // Events grant free commands
    onlyIn: Option[Set[String]]     = None,  // Limit command to the given spaces
    eventAction: Boolean            = false  // Event actions ignore some limitations.
  )
  
  sealed trait GovOp {
    def validSpaces(params: Params)(spaceFilter: (Space) => Boolean): Set[String] = {
      val names = spaceNames(game.spaces filter spaceFilter).toSet
      params.onlyIn map (only => names intersect only) getOrElse names
    }
    def execute(params: Params): Int   // Return the number of spaces acted upon
  }
  
  object Train extends GovOp {
    override def toString() = "Train"
    
    // Override the default so we can add the France Track and Border Zone Track
   override def validSpaces(params: Params)(spaceFilter: (Space) => Boolean): Set[String] = {
      val ft  = if (game.franceTrack > 0)
        Set(FranceTrackName)
      else
        Set.empty
      
      val bzt = if (game.pivotalCardsPlayed(PivotalMoroccoTunisiaIndepdent) && game.borderZoneTrack < BorderZoneTrackMax)
        Set(BorderZoneTrackName)
      else
        Set.empty
      
      ft ++ bzt ++ super.validSpaces(params)(spaceFilter)
    }
    
    override def execute(params: Params): Int = {
      val spaceFilter = (sp: Space) => {
        if (game.pivotalCardsPlayed(PivotalRecallDeGaulle))
          sp.isCity || sp.hasGovBase || (sp.isGovControlled && sp.pieces.totalTroops > 0 && sp.pieces.totalPolice > 0)
        else
          sp.isCity || sp.hasGovBase
      }
        
      var completedSpaces = Set.empty[String]
      specialActivity.init(params.includeSpecialActivity)
    
      def nextChoice(): Unit = {
        val candidateSpaces = params.maxSpaces match {
          case Some(m) if completedSpaces.size >= m => Set.empty
          case _  => validSpaces(params)(spaceFilter) -- completedSpaces
        }
        
        val choices = List(
          choice(candidateSpaces.nonEmpty,  "space",    s"Select a space to train"),
          choice(specialActivity.allowed,   "special",  s"Perform a special activity"),
          choice(true,                      "done",     s"Finished selecting spaces"),
          choice(true,                      "abort",    s"Abort the entire $Gov turn")
        ).flatten
      
        println("\n")
        println(s"$Gov Train operation")
        println(separator(char = '='))
        wrap("spaces completed: ", completedSpaces.toList.sorted(TrainingSpaceOrdering)) foreach println
        println(s"\nChoose one (${amtRes(Gov)} remaining):")
        askMenu(choices, allowAbort = false).head match {
          case "space" =>
            askCandidateAllowNone(s"\nChoose space to Train: ", candidateSpaces.toList.sorted(TrainingSpaceOrdering)) foreach { spaceName =>
              if (trainInSpace(spaceName, params))
                completedSpaces += spaceName
            }
            nextChoice()
          
          case "special" =>
            executeSpecialActivity(Deploy::TroopLift::Nil, params)
            nextChoice()
          
          case "abort" =>
            if (askYorN("Really abort? (y/n) ")) throw AbortAction
            nextChoice()
          
          case "done" =>
            pacifySpaces(completedSpaces, params)
        }
      }
    
      nextChoice()
      completedSpaces.size  // Return number of training spaces selected
    }
    
    def trainInSpace(spaceName: String, params: Params): Boolean = {
      val savedState = game
      try {
        log()
        log(s"$Gov executes Train operation: $spaceName")
        if (!params.free) {
          log()
          decreaseResources(Gov, 2)
        }
        
        spaceName match {
          case FranceTrackName     => decreaseFranceTrack(1)
          case BorderZoneTrackName => increaseBorderZoneTrack(1)
          case _ =>
            val toPlace = askPiecesToPlace(spaceName, List(AlgerianTroops, AlgerianPolice), maxToPlace = 4)
            placePieces(spaceName, toPlace)
        }
        true
      }
      catch {
        case AbortAction =>
          println(s"\n>>>> Aborting Train operation in $spaceName <<<<")
          println(separator())
          displayGameStateDifferences(game, savedState)
          game = savedState
          false
      }
    }
        
    // Ask the user in which space(s) to pacify
   def pacifySpaces(completedSpaces: Set[String], params: Params): Unit = {
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
    override def execute(params: Params): Int = {
      var movedPolice: Map[String, Pieces] = Map.empty.withDefaultValue(Pieces())
      def totalMoved = movedPolice.foldLeft(0) { case (sum, (_, p)) => sum + p.totalPolice }
      // Return the number of police cubes in the space that have not already been moved
      def availablePolice(sp: Space) = sp.pieces.totalPolice - movedPolice(sp.name).totalPolice
      val destFilter     = (sp: Space) => { sp.isResettled || sp.population > 0 }
      val sourceFilter   = (sp: Space) => { destFilter(sp) && availablePolice(sp) > 0 }
      val destCandidates = validSpaces(params)(destFilter)
      specialActivity.init(params.includeSpecialActivity)
      log()
      log(s"$Gov executes Garrison operation")
      if (!params.free) {
        log()
        decreaseResources(Gov, 2)
      }
      
      def nextChoice(): Unit = {
        val sourceCandidates = validSpaces(params)(sourceFilter)
        val choices = List(
          choice(totalMoved < 6 && sourceCandidates.nonEmpty, "police",   s"Select a police cube to move"),
          choice(specialActivity.allowed,                     "special",  s"Perform a special activity"),
          choice(true,                                        "done",     s"Finished selecting police cubes"),
          choice(true,                                        "abort",    s"Abort the entire $Gov turn")
        ).flatten
      
        println("\n")
        println(s"$Gov Garrison operation")
        println(separator(char = '='))
        val moved = movedPolice.toList.sortBy(_._1) map { case (n, p) => s"$n (${p.toString})"}
        wrap("police moved: ", moved) foreach println
        println(s"\nChoose one:")
        askMenu(choices, allowAbort = false).head match {
          case "police" =>
            val savedState = game
            try {
              askCandidateAllowNone(s"\nChoose space with police cube: ", sourceCandidates.toList.sorted) foreach { source =>
                val sp   = game.getSpace(source)
                val p    = askPieces(sp.pieces - movedPolice(source), 1, FrenchPolice::AlgerianPolice::Nil)
                val dest = if (params.maxSpaces == Some(1) && movedPolice.nonEmpty)
                  movedPolice.toList.head._1
                else
                  askCandidate(s"Choose destination space: ", destCandidates.toList.sorted)
                movePieces(p, source, dest)
                movedPolice += dest -> (movedPolice(dest) + p)
              }
            }
            catch {
              case AbortAction =>
                println(s"\n>>>> Aborting police cube movement <<<<")
                println(separator())
                displayGameStateDifferences(game, savedState)
                game = savedState
            }
            nextChoice()
          
          case "special" =>
            executeSpecialActivity(TroopLift::Neutralize::Nil, params)
            nextChoice()
          
          case "abort" =>
            if (askYorN("Really abort? (y/n) ")) throw AbortAction
            nextChoice()
          
          case "done" =>
            activateGuerrillas(params)
        }
      }
    
      nextChoice()
      movedPolice.size  // Return number of garrison destination spaces
    }
    
    // Activate guerrillas. Can be any space with hidden guerrillas where
    // there are enough police cubes.  
    def activateGuerrillas(params: Params): Unit = {
      val canActivate = (sp: Space) => {
        sp.pieces.hiddenGuerrillas > 0 &&
        ((sp.isMountains && sp.pieces.totalPolice > 1) || (!sp.isMountains && sp.pieces.totalPolice > 0))
      }
      val candidates = spaceNames(game.spaces filter canActivate)
      if (candidates.isEmpty)
        println(s"\nThere are no spaces where hidden guerrillas can be activated by police cubes")
      else {
        val name = askCandidate(s"Choose space to activate guerrillas: ", candidates.sorted, allowAbort = false)
        val sp = game.getSpace(name)
        val num = if (sp.isMountains) sp.pieces.totalPolice / 2 else sp.pieces.totalPolice
        activateHiddenGuerrillas(name, num min sp.pieces.hiddenGuerrillas)
      }
    }
  }
  
  object Sweep extends GovOp {
    override def toString() = "Sweep"
    val specialActivites = List(TroopLift, Neutralize)
    override def execute(params: Params): Int = 0
  }
  
  object Assault extends GovOp {
    override def toString() = "Assault"
    val specialActivites = List(TroopLift)
    override def execute(params: Params): Int = 0
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
  
  // Top level entry point to human actions
  def executeAction(action: Action): Action = action match {
    case Pass               => performPass(Gov); Pass
    case ExecOpPlusActivity => 
      val num = executeOp(Params(includeSpecialActivity = true))
      if (specialActivity.taken) ExecOpPlusActivity else if (num < 2) ExecLimitedOp else ExecOpOnly
    case ExecOpOnly         => if (executeOp() < 2) ExecLimitedOp else ExecOpOnly
    case ExecLimitedOp      => executeOp(Params(maxSpaces = Some(1))); ExecLimitedOp
    case Event              => executeEvent(); Event
  }
  
  // Ask user to select an operation and execute it.
  // Return the number of spaces acted upon
  def executeOp(params: Params = Params()): Int =  {
    val op = askOp()
    op.execute(params)
  }
    
  def executeEvent(): Unit = {
    
  }
  
  def executeSpecialActivity(allowedActivities: List[GovSpecial], params: Params): Unit = {
    val activity = allowedActivities match {
      case a::Nil => a
      case _ =>
        println("\nChoose special ability:")
        askMenu(allowedActivities map (s => s -> s.toString)).head
    }
    
    println(s"Special activity: $activity selected")
    println("Speical activities NOT YET IMPLEMENTED!!")
    // Ask which ability, then execute it.
    specialActivity.completed()
  }
  
  
}