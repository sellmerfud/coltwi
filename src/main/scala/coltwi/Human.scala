
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
  case class Params(
    includeSpecialActivity: Boolean = false,
    maxSpaces: Option[Int]          = None,
    free: Boolean                   = false, // Events grant free commands
    onlyIn: Option[Set[String]]     = None,  // Limit command to the given spaces
    eventAction: Boolean            = false  // Event actions ignore some limitations.
  )
  
  // Aid in keeping track of when a special activity can be taken
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
  
  // Use during a turn to keep track of pieces that have already moved
  // in each space.
  object movingGroups {
    var groups: Map[String, Pieces] = Map.empty.withDefaultValue(Pieces())
    
    def reset(): Unit = groups = Map.empty.withDefaultValue(Pieces())
    def apply(name: String): Pieces = groups(name)
    def add(name: String, pieces: Pieces): Unit = groups += name -> (groups(name) + pieces)
    
    def toList = groups.toList.sortBy(_._1)
    def size = groups.size
  }
    
  sealed trait GovOp {
    def validSpaces(params: Params)(spaceFilter: (Space) => Boolean): Set[String] = {
      val names = spaceNames(game.algerianSpaces filter spaceFilter).toSet
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
      var trainSpaces = Set.empty[String]
      val spaceFilter = (sp: Space) => {
        if (game.pivotalCardsPlayed(PivotalRecallDeGaulle))
          sp.isCity || sp.hasGovBase || (sp.isGovControlled && sp.pieces.totalTroops > 0 && sp.pieces.totalPolice > 0)
        else
          sp.isCity || sp.hasGovBase
      }
    
      def nextChoice(): Unit = {
        val candidateSpaces: Set[String] = if (game.resources(Gov) < 2 && !params.free)
          Set.empty
        else
          params.maxSpaces match {
            case Some(m) if trainSpaces.size >= m => Set.empty
            case _  => validSpaces(params)(spaceFilter) -- trainSpaces
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
        wrap("spaces trained: ", trainSpaces.toList.sorted(TrainingSpaceOrdering)) foreach println
        println(s"\nChoose one (${amtRes(Gov)} remaining):")
        askMenu(choices, allowAbort = false).head match {
          case "space" =>
            askCandidateAllowNone(s"\nChoose space to Train: ", candidateSpaces.toList.sorted(TrainingSpaceOrdering)) foreach { spaceName =>
              if (trainInSpace(spaceName, params))
                trainSpaces += spaceName
            }
            nextChoice()
          
          case "special" =>
            executeSpecialActivity(Deploy::TroopLift::Nil, params)
            nextChoice()
          
          case "abort" =>
            if (askYorN("Really abort? (y/n) ")) throw AbortAction
            nextChoice()
          
          case "done" =>
            pacifySpaces(trainSpaces, params)
        }
      }
    
      nextChoice()
      trainSpaces.size  // Return number of training spaces selected
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
   def pacifySpaces(trainSpaces: Set[String], params: Params): Unit = {
      val mapSpaces = trainSpaces - FranceTrackName - BorderZoneTrackName
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
      def totalMoved = movingGroups.toList.foldLeft(0) { case (sum, (_, p)) => sum + p.totalPolice }
      // Return the number of police cubes in the space that have not already been moved
      def movablePolice(sp: Space) = sp.pieces.totalPolice - movingGroups(sp.name).totalPolice
      val destFilter     = (sp: Space) => { sp.isResettled || sp.population > 0 }
      val sourceFilter   = (sp: Space) => { destFilter(sp) && movablePolice(sp) > 0 }
      val destCandidates = validSpaces(params)(destFilter)
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
        val moved = movingGroups.toList map { case (n, p) => s"$n (${p.toString})"}
        wrap("police moved: ", moved) foreach println
        println(s"\nChoose one:")
        askMenu(choices, allowAbort = false).head match {
          case "police" =>
            val savedState = game
            try {
              askCandidateAllowNone(s"\nChoose space with police cube: ", sourceCandidates.toList.sorted) foreach { source =>
                val sp   = game.getSpace(source)
                val p    = askPieces(sp.pieces - movingGroups(source), 1, FrenchPolice::AlgerianPolice::Nil)
                val dest = if (params.maxSpaces == Some(1) && movingGroups.size > 0)
                  movingGroups.toList.head._1
                else
                  askCandidate(s"Choose destination space: ", destCandidates.toList.sorted)
                movePieces(p, source, dest)
                movingGroups.add(dest, p)
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
      movingGroups.size  // Return number of garrison destination spaces
    }
    
    // Activate guerrillas. Can be any space with hidden guerrillas where
    // there are enough police cubes.  
    def activateGuerrillas(params: Params): Unit = {
      def guerrillasActivated(sp: Space) = if (sp.isMountains)
        (sp.pieces.totalPolice / 2) min sp.pieces.hiddenGuerrillas
      else
        sp.pieces.totalPolice min sp.pieces.hiddenGuerrillas
      val candidates = spaceNames(game.algerianSpaces filter (guerrillasActivated(_) > 0))
      if (candidates.isEmpty)
        println(s"\nThere are no spaces where hidden guerrillas can be activated by police cubes")
      else {
        val name = askCandidate(s"Choose space to activate guerrillas: ", candidates.sorted, allowAbort = false)
        val sp = game.getSpace(name)
        val num = if (sp.isMountains) sp.pieces.totalPolice / 2 else sp.pieces.totalPolice
        activateHiddenGuerrillas(name, guerrillasActivated(sp))
      }
    }
  }
  
  object Sweep extends GovOp {
    override def toString() = "Sweep"
    def movableTroops(sp: Space) = sp.pieces.totalTroops - movingGroups(sp.name).totalTroops
    // Return adjacent spaces that contains movable troops
    def sourceSpaces(sp: Space) = spaceNames(spaces(getAdjacent(sp.name)) filter (movableTroops(_) > 0))
    def guerrillasActivated(sp: Space) = if (sp.isMountains)
      (sp.pieces.totalCubes / 2) min sp.pieces.hiddenGuerrillas
    else
      sp.pieces.totalCubes min sp.pieces.hiddenGuerrillas
    
    override def execute(params: Params): Int = {
      var sweepSpaces = Set.empty[String]
      val sweepFilter = (sp: Space) => guerrillasActivated(sp) > 0 || sourceSpaces(sp).nonEmpty
      
      def nextChoice(): Unit = {
        val sweepCandidates: Set[String] = if (game.resources(Gov) < 2 && !params.free)
          Set.empty
        else 
          params.maxSpaces match {
            case Some(m) if sweepSpaces.size >= m => Set.empty
            case _  => validSpaces(params)(sweepFilter) -- sweepSpaces
          }
        
        val choices = List(
          choice(sweepCandidates.nonEmpty, "sweep",   s"Select a sweep space"),
          choice(specialActivity.allowed,  "special", s"Perform a special activity"),
          choice(true,                     "done",    s"Finished selecting sweep spaces"),
          choice(true,                     "abort",   s"Abort the entire $Gov turn")
        ).flatten
      
        println("\n")
        println(s"$Gov Sweep operation")
        println(separator(char = '='))
        wrap("spaces swept: ", sweepSpaces.toList.sorted) foreach println
        println(s"\nChoose one:")
        askMenu(choices, allowAbort = false).head match {
          case "sweep" =>
            askCandidateAllowNone(s"\nChoose space to Sweep: ", sweepCandidates.toList.sorted) foreach { spaceName =>
              if (sweepInSpace(spaceName, params))
                sweepSpaces += spaceName
            }
            nextChoice()
          
          case "special" =>
            executeSpecialActivity(TroopLift::Neutralize::Nil, params)
            nextChoice()
          
          case "abort" =>
            if (askYorN("Really abort? (y/n) ")) throw AbortAction
            nextChoice()
          
          case "done" =>
        }
      }
    
      nextChoice()
      for (name <- sweepSpaces.toList.sorted)
        activateHiddenGuerrillas(name, guerrillasActivated(game.getSpace(name)))
      sweepSpaces.size
    }
    
    def sweepInSpace(spaceName: String, params: Params): Boolean = {
      val savedState = game
      try {
        log()
        log(s"$Gov executes Sweep operation: $spaceName")
        if (!params.free) {
          log()
          decreaseResources(Gov, 2)
        }

        def nextSource(): Unit = {
          val sources = sourceSpaces(game.getSpace(spaceName))
          if (sources.nonEmpty) {
            val choices = (sources map (name => name -> s"Move troops from $name")) :+
                          "done"  -> "Finished moving troops from adjacent spaces"  :+
                          "abort" -> s"Abort the sweep operation in $spaceName"
            println(s"\nChoose one:")
            askMenu(choices, allowAbort = false).head match {
              case "done" =>
              case "abort" =>
                if (askYorN("Really abort? (y/n) ")) throw AbortAction
                nextSource()
              case source =>
                val sp = game.getSpace(source)
                val num = askInt(s"Move how many troops", 0, movableTroops(sp), allowAbort = false)
                if (num > 0) {
                  val p = askPieces(sp.pieces - movingGroups(source), num, FrenchTroops::AlgerianTroops::Nil)
                  movePieces(p, source, spaceName)
                  movingGroups.add(spaceName, p)
                }
                nextSource()
            }
          }
        }
        nextSource()
        true
      }
      catch {
        case AbortAction =>
          println(s"\n>>>> Aborting Sweep operation in $spaceName <<<<")
          println(separator())
          displayGameStateDifferences(game, savedState)
          game = savedState
          false
      }
    }
    
  }
  
  object Assault extends GovOp {
    override def toString() = "Assault"
    override def execute(params: Params): Int = {
      var assaultSpaces = Set.empty[String]
      def flnLosses(sp: Space): Pieces = {
        val totalCubes = if (sp.isCity || sp.isBorderSector) sp.pieces.totalCubes else sp.pieces.totalTroops
        val maxLosses  = if (sp.isMountains) totalCubes / 2 else totalCubes
        val numGuerrillas = sp.pieces.activeGuerrillas min maxLosses
        val numBases = if (sp.pieces.flnBases == 0 || sp.pieces.hiddenGuerrillas > 0 || sp.pieces.activeGuerrillas >= maxLosses)
          0
        else
          (maxLosses - numGuerrillas) min sp.pieces.flnBases
        Pieces(activeGuerrillas = numGuerrillas, flnBases = numBases)
      }
      
      def nextChoice(): Unit = {
        val assaultCandidates: Set[String] = if (game.resources(Gov) < 2 && !params.free)
          Set.empty
        else 
          params.maxSpaces match {
            case Some(m) if assaultSpaces.size >= m => Set.empty
            case _  => validSpaces(params)(flnLosses(_).total > 0) -- assaultSpaces
          }
        
        val choices = List(
          choice(assaultCandidates.nonEmpty, "assault", s"Select an assault space"),
          choice(specialActivity.allowed,    "special", s"Perform a special activity"),
          choice(true,                       "done",    s"Finished selecting asault spaces"),
          choice(true,                       "abort",   s"Abort the entire $Gov turn")
        ).flatten
      
        println("\n")
        println(s"$Gov Assault operation")
        println(separator(char = '='))
        wrap("spaces assaulted: ", assaultSpaces.toList.sorted) foreach println
        println(s"\nChoose one:")
        askMenu(choices, allowAbort = false).head match {
          case "assault" =>
            askCandidateAllowNone(s"\nChoose space to Assault: ", assaultCandidates.toList.sorted) foreach { spaceName =>
              log()
              log(s"$Gov executes Assault operation: $spaceName")
              if (!params.free) {
                log()
                decreaseResources(Gov, 2)
              }
              removeLosses(spaceName, flnLosses(game.getSpace(spaceName)))
              assaultSpaces += spaceName
            }
            nextChoice()
          
          case "special" =>
            executeSpecialActivity(TroopLift::Nil, params)
            nextChoice()
          
          case "abort" =>
            if (askYorN("Really abort? (y/n) ")) throw AbortAction
            nextChoice()
          
          case "done" =>
        }
      }
    
      nextChoice()
      assaultSpaces.size
    }
  }
  
  sealed trait GovSpecial {
    def execute(params: Params): Boolean
  }
  
  object Deploy extends GovSpecial {
    override def toString() = "Deploy"
    val moveFilter     = (sp: Space) => sp.isCity || sp.isGovControlled || sp.hasGovBase 
    val resettleFilter = (sp: Space) => !sp.isResettled && sp.basePop == 1 && (sp.isGovControlled || sp.hasGovBase)
    def availPieces = {
      val pieceTypes = if (momentumInPlay(MoBalkyConscripts)) List(FrenchTroops, GovBases) 
                       else List(FrenchTroops, FrenchPolice, GovBases)
      game.availablePieces.only(pieceTypes)
    }
    def getMoveCandidates     = spaceNames(game.algerianSpaces filter moveFilter).toSet
    def getResettleCandidates = spaceNames(game.algerianSpaces filter resettleFilter).toSet
    
    // Move pieces or resettle
    override def execute(params: Params): Boolean = {
      val canMove = {
        val candidates = getMoveCandidates
        candidates.nonEmpty && availPieces.total > 0 && {
          val mapPieces = spaces(candidates).foldLeft(0) { 
            case (sum, sp) => sum + sp.pieces.only(List(FrenchTroops, FrenchPolice, GovBases)).total
          }
          mapPieces > 0
        }
      }
      
      val choices = List(
        choice(canMove,                        "move",     "Deploy French pieces"),
        choice(getResettleCandidates.nonEmpty, "resettle", "Resettle an Algerian space"),
        choice(true,                           "cancel",   "Cancel Deploy special activity")
      ).flatten
      println("\nChoose one:")
      askMenu(choices).head match {
        case "move"     => moveFrenchPieces(params); true
        case "resettle" => resettleSpace(params); true
        case _          => false
      }
    }
    
    // Move     - Select up to 3 Algerian spaces
    //            Move up to 6 French troops, police, bases among 
    //            the selected spaces and the available box
    def moveFrenchPieces(params: Params): Unit = {
      def selectSpaces(selected: List[String], candidates: Set[String]): List[String] = {
        if (selected.size == 3 || candidates.isEmpty)
          selected
        else {
          val choices = List(
            choice(true,              "select", "Select an Algerian space for deployment"),
            choice(selected.nonEmpty, "done",   "Finish selecting spaces for deployment"),
            choice(true,              "abort",  "Abort the Deploy special activity")).flatten
          
          println()
          println(s"${amountOf(selected.size, "deploy space")} selected: ${andList(selected.toList.sorted)}")
          println("Choose one:")
          askMenu(choices).head match {
            case "select" => 
              val s = askCandidate("Select space: ", candidates.toList.sorted, allowAbort = false)
              selectSpaces(s :: selected, candidates - s)
            case "done"   => selected
            case "abort"  => if (askYorN("Really abort? (y/n) ")) throw AbortAction else selectSpaces(selected, candidates)
          }
        }
      }
      
      val FrenchPieces = List(FrenchTroops, FrenchPolice, GovBases)
      val AB = "Available box"
      val deploySpaces = AB :: selectSpaces(Nil, getMoveCandidates).reverse
      var deployed: Map[String, Pieces] = Map.empty.withDefaultValue(Pieces())
      def numDeployed = deployed.foldLeft(0) { case (sum, (_, p)) => sum + p.total }
      println()
      for {
        src <- deploySpaces
        if askYorN(s"Deploy pieces out of $src (y/n) ")
        dest <- deploySpaces filterNot (_ == src)
      } {
        val srcPieces = if (src == AB) availPieces else game.getSpace(src).pieces.only(FrenchPieces) - deployed(src)
        if (srcPieces.total > 0 && numDeployed < 6) {
          val num = askInt(s"Deploy how many pieces from $src to $dest", 0, srcPieces.total min (6 - numDeployed))
          if (num > 0) {
            val pieces = askPieces(srcPieces, num)
            (src, dest) match {
              case (AB, _) =>
                placePieces(dest, pieces)
                deployed += dest -> (deployed(dest) + pieces)
              case (_, AB) =>
                removeToAvailableFrom(src, pieces)
              case (_,  _) =>
                movePieces(pieces, src, dest)
                deployed += dest -> (deployed(dest) + pieces)
            }
          }
        }
      }
    }
    
    // Resettle - Choose one space and place a resettled marker there
    //            Remove any support/opposition
    def resettleSpace(params: Params): Unit = {
      addResettledMarker(askCandidate("Resettle which space? ", getResettleCandidates.toList.sorted))
    }
  }
  
  object TroopLift extends GovSpecial { 
    override def toString() = "Troop Lift"
    override def execute(params: Params): Boolean = {
      val numSpaces = 3 +
        (if (momentumInPlay(MoBananes))               2 else 0) +
        (if (momentumInPlay(MoVentilos))              1 else 0) +
        (if (momentumInPlay(MoCrossBorderAirStrike)) -1 else 0) +
        (if (momentumInPlay(MoStrategicMovement))    -1 else 0)
      true  
    }
  }
  
  object Neutralize extends GovSpecial { 
    override def toString() = "Neutralize"
    override def execute(params: Params): Boolean = {
      // if (capabilityInPlay(CapTorture))
      //   -1 commitment for executing Neutralize special activity
      //   In each space, remove one extra piece which may be underground (bases last)
      
      // if (capabilityInPlay(CapOverkill))
      //   Remove up to 4 pieces total (still only two spaces max)
      true
    }
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
    val choices = List(Train,Garrison,Sweep,Assault) map (o => o -> o.toString)
    println("\nChoose Operation:")
    val op = askMenu(choices).head
    
    specialActivity.init(params.includeSpecialActivity)
    movingGroups.reset()
    op.execute(params)
  }
    
  def executeSpecialActivity(activities: List[GovSpecial], params: Params): Unit = {
    println("\nChoose special ability:")
    val activity = askMenu(activities map (s => s -> s.toString)).head
    
    val savedState = game
    try {
      if (activity.execute(params))
        specialActivity.completed()
    }
    catch {
      case AbortAction =>
        println(s"\n>>>> Aborting $activity special activity <<<<")
        println(separator())
        displayGameStateDifferences(game, savedState)
        game = savedState
    }
  }
  
  def executeEvent(): Unit = {
    
  }
  
  
}