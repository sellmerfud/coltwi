
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
  val TROOPS = List(FrenchTroops, AlgerianTroops)
  
  case class Params(
    includeSpecialActivity: Boolean = false,
    maxSpaces: Option[Int]          = None,
    free: Boolean                   = false, // Events grant free commands
    onlyIn: Option[Set[String]]     = None   // Limit command to the given spaces
  )
  
  // Sorts alphabetically, but puts France track and Border zone track at the front.
 val TrainingSpaceOrdering = new Ordering[String] {
    def compare(x: String, y: String) = (x, y) match {
      case (FranceTrackName, _)     => -1
      case (_, FranceTrackName)     => 1
      case (BorderZoneTrackName, _) => -1
      case (_, BorderZoneTrackName) => 1
      case (_, _)                   => x compare y
    }
  }
  
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
    def remove(name: String, pieces: Pieces): Unit = groups += name -> (groups(name) - pieces)
    
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
    
    val trainFilter = (sp: Space) => sp.canTrain
    
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
    
      def nextChoice(): Unit = {
        val candidateSpaces: Set[String] = if (game.resources(Gov) < 2 && !params.free)
          Set.empty
        else
          params.maxSpaces match {
            case Some(m) if trainSpaces.size >= m => Set.empty
            case _  => validSpaces(params)(trainFilter) -- trainSpaces
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
    val destFilter     = (sp: Space) => { sp.isResettled || sp.population > 0 }
    val sourceFilter   = (sp: Space) => { destFilter(sp) && movablePolice(sp) > 0 }
    def movablePolice(sp: Space) = sp.totalPolice - movingGroups(sp.name).totalPolice
    
    override def execute(params: Params): Int = {
      def totalMoved = movingGroups.toList.foldLeft(0) { case (sum, (_, p)) => sum + p.totalPolice }
      // Return the number of police cubes in the space that have not already been moved
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
        if (capabilityInPlay(CapGovCommandos))
          (sp.algerianPolice + (sp.frenchPolice / 2)) min sp.hiddenGuerrillas
        else
          (sp.totalPolice / 2) min sp.hiddenGuerrillas
      else
        sp.totalPolice min sp.hiddenGuerrillas
      val candidates = spaceNames(game.algerianSpaces filter (guerrillasActivated(_) > 0))
      if (candidates.isEmpty)
        println(s"\nThere are no spaces where hidden guerrillas can be activated by police cubes")
      else {
        val name = askCandidate(s"Choose space to activate guerrillas: ", candidates.sorted, allowAbort = false)
        val sp = game.getSpace(name)
        val num = if (sp.isMountains) sp.totalPolice / 2 else sp.totalPolice
        activateHiddenGuerrillas(name, guerrillasActivated(sp))
      }
    }
  }
  
  object Sweep extends GovOp {
    override def toString() = "Sweep"
    def movableTroops(sp: Space) = sp.totalTroops - movingGroups(sp.name).totalTroops
    // Return adjacent spaces that contains movable troops
    def sourceSpaces(sp: Space) = spaceNames(spaces(getAdjacent(sp.name)) filter (movableTroops(_) > 0))
    def guerrillasActivated(sp: Space) = if (sp.isMountains)
      if (capabilityInPlay(CapGovCommandos))
        (sp.algerianCubes + (sp.frenchCubes / 2)) min sp.hiddenGuerrillas
      else
        (sp.totalCubes / 2) min sp.hiddenGuerrillas
    else
      sp.totalCubes min sp.hiddenGuerrillas
    val sweepFilter = (sp: Space) => guerrillasActivated(sp) > 0 || sourceSpaces(sp).nonEmpty
    
    override def execute(params: Params): Int = {
      var sweepSpaces = Set.empty[String]
      
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
                          "done"  -> s"Finished moving troops to $spaceName from adjacent spaces"  :+
                          "abort" -> s"Abort the sweep operation in $spaceName"
            println(s"\nChoose one:")
            askMenu(choices, allowAbort = false).head match {
              case "done" =>
              case "abort" =>
                if (askYorN("Really abort? (y/n) ")) throw AbortAction
                nextSource()
              case source =>
                val sp = game.getSpace(source)
                val movable = (sp - movingGroups(source)).only(TROOPS)
                println()
                println(s"The following troops are eligible to move from $source:")
                wrap("  ", movable.stringItems) foreach println 
                val num = askInt(s"Move how many troops", 0, movableTroops(sp), allowAbort = false)
                if (num > 0) {
                  val p = askPieces(sp - movingGroups(source), num, TROOPS)
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
    
    def flnLosses(sp: Space): Pieces = {
      val totalCubes = if (sp.isCity || sp.isBorderSector) sp.totalCubes else sp.totalTroops
      val maxLosses  = if (sp.isMountains) totalCubes / 2 else totalCubes
      val numGuerrillas = sp.activeGuerrillas min maxLosses
      val numBases = if (sp.flnBases == 0 || sp.hiddenGuerrillas > 0 || sp.activeGuerrillas >= maxLosses)
        0
      else
        (maxLosses - numGuerrillas) min sp.flnBases
      Pieces(activeGuerrillas = numGuerrillas, flnBases = numBases)
    }
    val assaultFilter = (sp: Space) => flnLosses(sp).total > 0
    
    override def execute(params: Params): Int = {
      var assaultSpaces = Set.empty[String]
      
      def nextChoice(): Unit = {
        val assaultCandidates: Set[String] = if (capabilityInPlay(CapFlnSaS) && assaultSpaces.nonEmpty)
          Set.empty
        else if (game.resources(Gov) < 2 && !params.free)
          Set.empty
        else 
          params.maxSpaces match {
            case Some(m) if assaultSpaces.size >= m => Set.empty
            case _  => validSpaces(params)(assaultFilter) -- assaultSpaces
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
      
      if (assaultSpaces.nonEmpty && capabilityInPlay(CapRevenge) && game.guerrillasAvailable > 0) {
        //  - First at assault space at Support with highest population
        //  - Then at assault space with FLN base with highest population
        //  - Then at random assault space with highest population
        val candidates = (assaultSpaces map game.getSpace).toList match {
          case xs if xs exists (_.isSupport)    => xs filter (_.isSupport)
          case xs if xs exists (_.flnBases > 0) => xs filter (_.flnBases > 0)
          case xs                               => xs filter (_.isSupport)
        }
        val priorities = List(
          new Bot.HighestScorePriority[Space]("Highest population", _.population))
        
        val sp = Bot.topPriority(candidates, priorities)
        log(s"Fln places a guerrilla in one assault spaces due to capability: $CapRevenge")
        placePieces(sp.name, Pieces(hiddenGuerrillas = 1))
      }
      
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
        val mapPieces  = spaces(candidates).foldLeft(0) { 
          case (sum, sp) => sum + sp.totalOf(List(FrenchTroops, FrenchPolice, GovBases))
        }
        candidates.nonEmpty && (availPieces.total > 0 || mapPieces > 0)
      }
      
      if (canMove == false && getResettleCandidates.isEmpty) {
        println("\nDeploy special activity is not possible at this time.")
        pause()
        false
      }
      else {
        log()
        log(s"$Gov executes a Deploy special ability")
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
      val FrenchCubes  = List(FrenchTroops, FrenchPolice)
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
        val validTypes = if (game.getSpace(dest).totalBases == 2) FrenchCubes else FrenchPieces 
        val srcPieces = if (src == AB) availPieces.only(validTypes) 
                        else           game.getSpace(src).only(validTypes) - deployed(src)
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
    // Select 3 spaces
    // Move any FRENCH troops among the three spaces.
    override def execute(params: Params): Boolean = {
      val maxSpaces = 3 +
        (if (momentumInPlay(MoBananes))               2 else 0) +
        (if (momentumInPlay(MoVentilos))              1 else 0) +
        (if (momentumInPlay(MoCrossBorderAirStrike)) -1 else 0) +
        (if (momentumInPlay(MoStrategicMovement))    -1 else 0)
        
      if (maxSpaces < 2) {
        println("\nTroop Lift is not possible at this time due to the events:")
        wrap("  ", List(MoCrossBorderAirStrike, MoStrategicMovement)) foreach println
        pause()
        false
      }
      else {
        def selectSpaces(selected: List[String]): List[String] = {
          val candidates = spaceNames(game.algerianSpaces)
          if (selected.size == maxSpaces)
            selected
          else {
            val choices = List(
              choice(true,              "select", "Select a Troop Lift space"),
              choice(selected.size > 1, "done",   "Finished selecting Troop Lift spaces"),
              choice(true,              "abort",  "Abort the Troop Lift special activity")).flatten
            
            println()
            println(s"Troop Lift spaces selected (${selected.size} of ${maxSpaces}):")
            wrap("  ", selected) foreach println
            println("Choose one:")
            askMenu(choices).head match {
              case "select" => 
                val name = askCandidate("Select space: ", candidates.toList.sorted)
                selectSpaces(name :: selected)
              case "done"   => selected
              case "abort"  => if (askYorN("Really abort? (y/n) ")) throw AbortAction else selectSpaces(selected)
            }
          }
        }
        
        def liftTroops(src: String, dst: String): Unit = {
          val (srcSpace, dstSpace) = (game.getSpace(src), game.getSpace(dst))
          if (srcSpace.totalTroops > 0) {
            askInt(s"Lift how many troops from $src to $dst", 0, srcSpace.totalTroops) match {
              case 0   =>
              case num => 
                val (pieces, swept) = askTroops(srcSpace, movingGroups(src), num)
                movePieces(pieces, src, dst)
                movingGroups.remove(src, swept)
                movingGroups.add(dst, swept)
            }
          }
        }
        
        log()
        log(s"$Gov executes a Troop Lift special ability")
        log(s"May select up to ${maxSpaces} spaces")
        
        val selectedSpaces = selectSpaces(Nil).reverse
        val withTroops = selectedSpaces filter (name => game.getSpace(name).totalTroops > 0)
        
        if (withTroops.isEmpty) {
          println()
          println("\nNone of the selected spaces contains contains troops!")
          pause()
          false
        }
        else {
          for (sourceName <- withTroops; destName <- selectedSpaces filterNot (_ == sourceName))
            liftTroops(sourceName, destName)
          true  
        }
      }
    }
    
    // This is a specialized version of the askPieces() functions that distinguishes 
    // between troops that have already moved (for sweep) and those that haven't.
    // Returns (total pieces selected, pieces that are in moving group)
    def askTroops(pieces: Pieces, movedPieces: Pieces, num: Int): (Pieces, Pieces) = {
      val troops   = pieces.only(TROOPS)
      val moved    = movedPieces.only(TROOPS)
      val notMoved = troops - moved
      
      if (num == troops.total)
        (troops, moved) // Take the lot
      else if (moved.total == 0)
        (askPieces(troops, num, TROOPS), Pieces())
      else if (moved.total == troops.total) {
        val p = askPieces(troops, num, TROOPS)
        (p, p)
      }
      else {
        println()
        println(s"Not moved in sweep: ${notMoved}")
        println(s"Moved in sweep    : ${moved}")
        val numNoMove = askInt("Lift how many that have NOT swept", num - moved.total min 0, num min notMoved.total)
        val nonMovers = askPieces(notMoved, numNoMove, TROOPS, heading = Some("Troops that have NOT swept"))
        val movers    = askPieces(moved, num - numNoMove, TROOPS, heading = Some("Troops that have swept"))
        (nonMovers + movers, movers)
      }
    }
  }
  
  object Neutralize extends GovSpecial { 
    override def toString() = "Neutralize"
    val neutralizeFilter = (sp: Space) => sp.totalTroops > 0 && sp.totalPolice > 0
    def getCandidates = spaceNames(game.algerianSpaces filter neutralizeFilter).toSet
    
    override def execute(params: Params): Boolean = {
      if (getCandidates.isEmpty) {
        println("\nNeutralize is not possible at this time.")
        pause()
        false
      }
      else {
        def selectSpaces(selected: List[String]): List[String] = {
          val candidates = getCandidates filterNot selected.contains
          if (selected.size == 2 || candidates.isEmpty)
            selected
          else {
            val choices = List(
              choice(true,              "select", "Select a Neutralize space"),
              choice(selected.nonEmpty, "done",   "Finished selecting Neutralize spaces"),
              choice(true,              "abort",  "Abort the Neutralize special activity")).flatten
            
            println()
            println("Neutralize spaces selected:")
            wrap("  ", selected) foreach println
            println("Choose one:")
            askMenu(choices).head match {
              case "select" => 
                val name = askCandidate("Select space: ", candidates.toList.sorted)
                selectSpaces(name :: selected)
              case "done"   => selected
              case "abort"  => if (askYorN("Really abort? (y/n) ")) throw AbortAction else selectSpaces(selected)
            }
          }
        }
        
        log()
        log(s"$Gov executes a Neutralize special ability")
        if (capabilityInPlay(CapOverkill))
          log("The Overkill capability is in play: Up to four total pieces may be removed")
        else
          log("Up to two total pieces may be removed")

        if (capabilityInPlay(CapTorture)) {
          log("The Torture capability is in play")
          decreaseCommitment(1)
          log("One extra piece will be removed from each space (may be underground)")
        }
        
        val totalRemoval = if (capabilityInPlay(CapOverkill)) 4 else 2 // Excluding overkill
          
        // The second return may contain a single piece to satisfy the Torture capability
        // The first return contains any active guerrillas/bases that can be removed.
        def targetPieces(sp: Space, maxNum: Int): (Pieces, Pieces) = {
          val torturePiece = if (capabilityInPlay(CapTorture)) {
            if (sp.totalGuerrillas == 0 && sp.flnBases > 0) Pieces(flnBases = 1)
            else if (sp.hiddenGuerrillas > 0) Pieces(hiddenGuerrillas = 1) // Hidden first for torture
            else if (sp.activeGuerrillas > 0) Pieces(activeGuerrillas = 1)
            else Pieces()
          }
          else Pieces()
          
          val regularPieces = if (maxNum == 0)
            Pieces()
          else {
            val p = (sp.pieces - torturePiece)
            if (p.hiddenGuerrillas > 0) // Hidden guerrillas shield bases
              Pieces(activeGuerrillas = maxNum min p.activeGuerrillas)
            else {
              val active = maxNum min p.activeGuerrillas
              val bases  = (maxNum - active) min p.flnBases
              Pieces(activeGuerrillas = active, flnBases = bases)
            }
          }
          (regularPieces, torturePiece)
        }
        
        def nextSpace(remainingSpaces: List[String], numRemoved: Int): List[(String, Pieces)] = 
        if (remainingSpaces.isEmpty)
          Nil
        else {
          val name = remainingSpaces.head
          val sp = game.getSpace(name)
          val (regular, torture) = targetPieces(sp, totalRemoval - numRemoved)
          val removed = if (remainingSpaces.size == 1 || regular.total == 0)
            regular
          else {
            regular -> s"Remove $regular"
            
            def buildChoices(collection: List[Pieces]): List[(Pieces, String)] = {
              if (collection == Nil)
                (Pieces() -> "Remove no pieces") :: Nil
              else {
                val opt = Pieces.combined(collection)
                (opt -> s"Remove $opt") :: buildChoices(collection.dropRight(1))
              }
            }
            
            val choices = buildChoices(regular.explode(ActiveGuerrillas::FlnBases::Nil)).reverse
              
            println()
            if (torture.total > 0)
              println(s"$torture will be removed from $name due to the Torture capability")
            println(s"Choose pieces to remove from $name:")
            askMenu(choices).head 
          }
          (name -> (removed + torture)) :: nextSpace(remainingSpaces.tail, numRemoved + removed.total)
        }
        
        val losses = nextSpace(selectSpaces(Nil).reverse, 0)
        val spaceNames = losses map (_._1)
        log(s"\nGovernment neutralizes in ${andList(spaceNames)}")
        removeLosses(losses)
        log()
        for (name <- spaceNames; sp = game.getSpace(name)) {
          if (!sp.isOppose)
            decreaseSupport(sp.name, 1)
          else if (sp.isOppose && !sp.hasTerror)
            addTerror(sp.name, 1)
        }
        true
      }
    }
  }
    
  // Ask user to select an operation and execute it.
  // Return the number of spaces acted upon
  def executeOp(params: Params = Params()): Int =  {
    specialActivity.init(params.includeSpecialActivity)
    movingGroups.reset()
    
    val choices = List(Train,Garrison,Sweep,Assault) map (o => o -> o.toString)
    println("\nChoose Operation:")
    val op = askMenu(choices).head
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
  
  // A human player has opted to take an action on the current card.
  def act(): Unit = {
    // Save the game state to handle the user aborting the action.
    val savedState = game
    try {
      val action = if (deck.isGovPivotalCard(game.currentCard.get))
        Event
      else {
        val isFree = Set[Action](Event, Pass)
        val actions = if (game.resources(Gov) < 2) 
          game.sequence.availableActions filter isFree.apply
        else
          game.sequence.availableActions
          
        println("\nChoose one:")
        askMenu(actions map (a => a -> a.toString)).head
      }
      
      log(s"\nGovernment chooses: $action")
      val executedAction = action match {
        case Pass               => performPass(Gov); Pass
        case ExecOpPlusActivity => 
          val num = executeOp(Params(includeSpecialActivity = true))
          if (specialActivity.taken) ExecOpPlusActivity else if (num < 2) ExecLimitedOp else ExecOpOnly
        case ExecOpOnly         => if (executeOp() < 2) ExecLimitedOp else ExecOpOnly
        case ExecLimitedOp      => executeOp(Params(maxSpaces = Some(1))); ExecLimitedOp
        case Event              => executeEvent(); Event
      }
      
      // For the first eligible role, we adjust the action base on what they
      // actually did.  This affects eligibiliy for the following turn.
      val finalAction = if (game.sequence.numActed == 0) executedAction else action
      
      log(s"\nPlace the ${Gov} eligibility cylinder in the ${finalAction} box")
      game = game.copy(sequence = game.sequence.nextAction(finalAction))
    }
    catch {
      case AbortAction =>
        println("\n>>>> Aborting the current action <<<<")
        println(separator())
        displayGameStateDifferences(game, savedState)
        game = savedState
    }
  }
  
}