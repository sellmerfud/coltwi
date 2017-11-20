
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


object Bot {

  def botDebug(msg: => String) = if (game.params.botDebug) println(msg)

  // Does space have a guerrilla that can be flipped without exposing any bases?
  def hasSafeHiddenGuerrilla(sp: Space) = 
    ((sp.flnBases == 0 || sp.isCountry) && sp.hiddenGuerrillas > 0) ||
    (sp.flnBases > 0 && sp.hiddenGuerrillas > 1)

  def tryOperations[T](code: => T): (GameState, T) = {
    val savedGameState = game
    val savedEchoState = echoLogging
    echoLogging = false
    try {
      val result = code
      (game, result)
    }
    finally {
      game        = savedGameState
      echoLogging = savedEchoState
    }
  }
  
  def costToAgitate(sp: Space, maxShifts: Int = 1): Int = {
    val shifts = sp.support match {
      case Oppose  => 0
      case Neutral => 1
      case Support => 2
    }
    sp.terror + (shifts min maxShifts)
  }
  
  // Print the log entries in the updated game state that do not 
  // exist in the original gamestate.
  def showLogEntries(orig: GameState, updated: GameState): Unit = {
    updated.history.drop(orig.history.size) foreach println
  }
  

  // Space filters are used when selecting the top priority from of spaces.
  trait SpaceFilter {
    val desc: String
    def filter(spaces: List[Space]): List[Space]
    override def toString() = desc
  }
  
  type PriorityList = List[SpaceFilter]
  
  // Find the valid candidate spaces using the given list of space filters.
  //
  // Each filter is first used against the given spaces and if the filter does not find any matching
  // candidates, the next filter is given a chance.
  // As soon as a filter finds at least one matching country, then the procees stops and the
  // results from that filter are returned.
  // If none of the filters finds at least one matching country we return Nil.
  @tailrec final def selectCandidates(spaces: List[Space], filters: PriorityList): List[Space] = {
    botDebug(s"selectCandidates: [${(spaces map (_.name)) mkString ", "}]")
    (spaces, filters) match {
      case (Nil, _) =>
        botDebug("selectCandidates: no spaces to consider")
        Nil    // No spaces to consider
      case (_, Nil) => 
        botDebug("selectCandidates: no spaces found")
        Nil    // No filter found any candidates
      case (xs, f::fs) =>
        (f filter xs) match {
          case Nil =>            // Filter did not match anything, try the next filter
            botDebug(s"selectCandidates ($f): failed")
            selectCandidates(xs, fs)
          case results =>        // We got some results…
            botDebug(s"selectCandidates ($f): [${(results map (_.name) mkString ", ")}]")
            results
        }
    }
  }
  
  // Process the list of spaces by each space filter in the prorities list.
  // In this function each filter is processed in order until we have used all filters
  // in the list to narrow the choices to a single space.  If we go through all of
  // the filters and we stil have more than one viable country, then we pick one at
  // random.
  // Note: This function should not be called with an empty list of spaces!
  def topPriority(spaces: List[Space], priorities: PriorityList): Space = {
    assert(spaces.nonEmpty, "topPriority: called with empty list of spaces!")
    botDebug(s"topPriority: [${(spaces map (_.name)) mkString ", "}]")
    @tailrec def nextPriority(spaces: List[Space], priorities: PriorityList): Space = {
      (spaces, priorities) match {
        case (Nil, _)    => throw new IllegalArgumentException("nextPriority: empty list")
        case (sp::Nil, _) => 
          botDebug(s"topPriority: Picked a winner [${sp.name}]")
          sp
        case (best, Nil)   =>
          val sp = shuffle(best).head        // Take one at random
          botDebug(s"topPriority: Picked random country [${sp.name}]")
          sp
        case (list, f::fs) =>
          (f filter list) match {
            case Nil =>
              botDebug(s"topPriority ($f) no matches")
              nextPriority(list, fs) // Filter entire list by next priority
            case best  =>
              botDebug(s"topPriority ($f) matched [${(best map (_.name) mkString ", ")}]")
              nextPriority(best, fs) // Filter matched list by next priority
          }
      }
    }
    nextPriority(spaces, priorities)
  }
  
  // A boolean criteria filter
  // Filters the given spaces and returns the results.
  case class CriteriaFilter(val desc: String, criteria: (Space) => Boolean) extends SpaceFilter {
    def filter(spaces: List[Space]) = (spaces filter criteria)
  }
  
  // Highest integer score filter used with Priority Tables.
  // Applies the given score function to each country in the input list and
  // takes the highest value.
  // Then returns the list of spaces whose score matches that highest value.
  case class HighestScorePriority(val desc: String, score: (Space) => Int) extends SpaceFilter {
    def filter(spaces: List[Space]): List[Space] = {
      val high = (spaces map score).max
      botDebug(s"Highest ($desc): score = $high")
      spaces filter (c => score(c) == high)
    }
  }

  // Lowest integer score filter used with Priority Tables.
  // Applies the given score function to each country in the input list and
  // takes the lowest value.
  // Then returns the list of spaces whose score matches that lowest value.
  case class LowestScorePriority(val desc: String, score: (Space) => Int) extends SpaceFilter {
    def filter(spaces: List[Space]): List[Space] = {
      val low = (spaces map score).min
      botDebug(s"Lowest ($desc): score = $low")
      spaces filter (c => score(c) == low)
    }
  }
  
  // Is the Bot currently acting second and guarenteed to act first on the next card?
  def botWillActTwice = game.sequence.secondEligible == Fln && 
      (game.sequence.firstAction == Some(ExecOpPlusActivity) || 
       game.sequence.firstAction == Some(ExecOpOnly))

  def botCanDo(action: Action): Boolean = game.sequence.availableActions contains action
  def canDoMultipleSpaces = botCanDo(ExecOpPlusActivity) || botCanDo(ExecOpOnly)
  
  var specialActivityTaken = false 
  def canDoSpecialActivity = botCanDo(ExecOpPlusActivity) && !specialActivityTaken
  
  // Return the effective action based on what was performed
  def effectiveAction(numSpaces: Int): Action = {
    if (specialActivityTaken)
      ExecOpPlusActivity
    else if (numSpaces == 1 && botCanDo(ExecLimitedOp))
      ExecLimitedOp
    else
      ExecOpOnly
  }
  
  def terrorCandidates = game.algerianSpaces filter { sp =>
    sp.population > 0 &&
    hasSafeHiddenGuerrilla(sp) && 
    (sp.isSupport || (game.isFinalCampaign && sp.isNeutral && sp.terror == 0 && sp.canTrain))
  }  
  
  case class SubvertCmd(replace: Boolean, name: String, pieces: Pieces)
  
  // - Remove last 2 cubes in one space (police top priority)
  // - Remove last cube in two spaces (police top priority)
  // - Replace police with guerrilla in one space
  // - Replace last troop with guerrilla in one space

  def subvertCommands: List[SubvertCmd] = {
    val ALGERIAN = List(AlgerianPolice, AlgerianTroops)
    val hasG = (sp: Space) => sp.hiddenGuerrillas > 0
    val last2Cubes = game.algerianSpaces filter (sp => hasG(sp) && sp.algerianCubes == 2 && sp.frenchCubes == 0)
    val lastCube   = game.algerianSpaces filter (sp => hasG(sp) && sp.algerianCubes == 1 && sp.frenchCubes == 0)
    val withPolice = game.algerianSpaces filter (sp => hasG(sp) && sp.algerianPolice > 0)
    val has2Police = CriteriaFilter("2 Police cubes", _.algerianPolice == 1)
    val hasPolice  = CriteriaFilter("Police cube", _.algerianPolice > 0)
    val generic    = game.algerianSpaces filter (sp => hasG(sp) && sp.algerianCubes > 0)
    def bestPiece(pieces: Pieces): Pieces = 
      if (pieces.algerianPolice > 0) Pieces(algerianPolice = 1)
      else Pieces(algerianTroops = 1)
    if (last2Cubes.nonEmpty) {
      val target = topPriority(last2Cubes, List(has2Police, hasPolice))
      List(SubvertCmd(false, target.name, target.only(ALGERIAN)))
    }
    else if (lastCube.size == 1) {
      val target = lastCube.head
      (generic filter (sp => sp.name != target.name)) match {
        case Nil if game.guerrillasAvailable > 0 => List(SubvertCmd(true,  target.name, bestPiece(target.pieces)))
        case Nil                                 => List(SubvertCmd(false, target.name, bestPiece(target.pieces)))
        case rest                                => 
          val t2 = topPriority(rest, List(hasPolice))
          List(SubvertCmd(false, target.name, bestPiece(target.pieces)), 
               SubvertCmd(false, t2.name, bestPiece(t2.pieces)))
      }
    }
    else if (lastCube.nonEmpty) {
      def getBest(candidates: List[Space]): List[SubvertCmd] = if (candidates.size == 2)
        Nil
      else {
        val target = topPriority(candidates, List(hasPolice))
        SubvertCmd(false, target.name, bestPiece(target.pieces)) :: getBest(candidates filterNot (_.name == target.name))
      }
      getBest(lastCube)
    }
    else if (withPolice.nonEmpty && game.guerrillasAvailable > 0) {
      val target = shuffle(withPolice).head
      List(SubvertCmd(true, target.name, bestPiece(target.pieces)))
    }
    else if (generic.size == 1) {
      val target = generic.head
      List(SubvertCmd(true, target.name, bestPiece(target.pieces)))
    }
    else
      generic take 2 map (sp => SubvertCmd(false, sp.name, bestPiece(sp.pieces)))
  }
  
  // Note that no guerrillas are activated when subverting
  def trySubvert(): Unit = if (canDoSpecialActivity) {
    subvertCommands match {
      case Nil =>
      case cmds =>
        specialActivityTaken = true
        log()
        log(s"$Fln executes a Subvert special ability")
        for (SubvertCmd(replace, name, pieces) <- cmds) {
          removeToAvailableFrom(name, pieces)
          if (replace)
            placePieces(name, Pieces(hiddenGuerrillas = 1))
        }
    }
  }
  
  def tryExtort(): Unit = if (canDoSpecialActivity && game.resources(Fln) < 5) {
    val primary = (game.algerianSpaces filter { sp => 
      sp.population > 0  &&
      sp.isFlnControlled && (
        (sp.flnBases > 0 && sp.totalCubes > 0 && sp.hiddenGuerrillas > 2) ||
        ((sp.flnBases == 0 || sp.totalCubes == 0) && sp.hiddenGuerrillas > 1)
      ) 
    }) ::: (game.countrySpaces filter hasSafeHiddenGuerrilla)
    
    val primaryNames = spaceNames(primary).toSet
    val secondary  = game.algerianSpaces filter 
      (sp => !primaryNames(sp.name) && sp.isFlnControlled && hasSafeHiddenGuerrilla(sp))
    
    if (primary.nonEmpty || (game.resources(Fln) == 0 && secondary.nonEmpty)) {
      specialActivityTaken = true
      log()
      log(s"$Fln executes an Extort special ability")
      for (sp <- primary) {
        activateHiddenGuerrillas(sp.name, 1)
        increaseResources(Fln, 1)
      }
      if (game.resources(Fln) == 0)
        for (sp <- secondary) {
          activateHiddenGuerrillas(sp.name, 1)
          increaseResources(Fln, 1)
        }
    }
  }
  
  trait ActionFlowchartNode {
    val desc: String
    override def toString() = desc
    // If an ActionFlowchartNode  is returned, then we continue down the flowchart
    // If a GameState is returned, then an action has been peformed.
    def execute: Either[ActionFlowchartNode, Action]
  }

  // This is the starting point FLN Bot flowchart
  object LimOpAndZeroResources extends ActionFlowchartNode {
    val desc = "LimOp and Resources == 0?"
    def execute: Either[ActionFlowchartNode, Action] = {
      if (game.resources(Fln) == 0 && !canDoMultipleSpaces) {
        log(s"\nFLN chooses: ${Pass}")
        performPass(Fln)
        Right(Pass)
      }
      else
        Left(EachAlgerianBaseHasUnderground)
    }
  }
  
  object EachAlgerianBaseHasUnderground extends ActionFlowchartNode {
    val desc = "Each Algerian Base at +1 Pop has 2+ underground guerrillas...?"
    def execute: Either[ActionFlowchartNode, Action] = {
      val basesCovered = (game.algerianSpaces filter (_.flnBases > 0)) forall { sp => 
        (sp.population == 0 && sp.hiddenGuerrillas > 0) ||
        (sp.population > 0  && sp.hiddenGuerrillas > 1)
      }
      if (basesCovered)
        Left(ConsiderTerrorOrEvent)
      else
        Left(WillActTwice)
    }
  }

  object WillActTwice extends ActionFlowchartNode {
    val desc = "Gov already active and will be 2nd eligible next card?"
    def execute: Either[ActionFlowchartNode, Action] = 
      if (botWillActTwice) Left(ConsiderTerrorOrEvent) else Left(ConsiderRally)
  }

  object ConsiderTerrorOrEvent extends ActionFlowchartNode {
    val desc = "Consider Terror Operation"
    // If event is playable then save game state and execute it with logging suppressed
    // If terror is possible the save game state and execute it with logging suppressed
    // If event was playable and terror not possible or if victory margin better then 
    //     CONSIDER the event.
    // If event not playable and terror not possible, the next node: ConsiderAttack
    // If event not playable or worse than terror then do Terror.
    def execute: Either[ActionFlowchartNode, Action] = {
      val originalGameState = game
      val card = deck(game.currentCard.get)
      
      def executeEvent(which: EventSelection): Unit = {
        if (which == Unshaded) {
          log(s"\nFLN chooses: Unshaded event")
          card.executeUnshaded(Fln)
        }
        else {
          log(s"\nFLN chooses: Shaded event")
          card.executeShaded(Fln)
        }
      }
      
      def willPlayEvent(eventState: GameState): Boolean = {
        card.markedForFLN ||  // Always play if marked
        card.isCapability ||  // Alwasy play if capability
        (dieRoll < 5 && 
           (eventState.govScore < game.govScore ||
            eventState.franceTrack > game.franceTrack ||
            eventState.totalOnMap(_.flnBases) > game.totalOnMap(_.flnBases) ||
            eventState.resources(Fln) > game.resources(Fln))
        )
      }
      
      val eventResult = (if (botCanDo(Event)) card.botEventSelection() else NoEvent) match {
        case NoEvent  => None
        case selected => Some(tryOperations { executeEvent(selected) })
      }
      val terrorResult = if (terrorCandidates.nonEmpty) Some(tryOperations { doTerror() }) else None
      
      (eventResult, terrorResult) match {
        case (Some((event, _)), Some((terror, _))) if event.govScore < terror.govScore && willPlayEvent(event) =>
          showLogEntries(game, event)
          game = event
          Right(Event)
        case (Some((event, _)), None) if willPlayEvent(event) =>
          showLogEntries(game, event)
          game = event
          Right(Event)
        case (_, Some((terror, action))) if terror.govScore < game.govScore || 
                                            terror.totalOnMap(_.terror) > game.totalOnMap(_.terror) =>
          showLogEntries(game, terror)
          game = terror
          Right(action)
        case _ =>
          Left(ConsiderAttack)
      }
    }
  }

  object ConsiderAttack extends ActionFlowchartNode {
    val desc = "Consider Attack Operation"
    
    // Sort priority
    //     Can remove base
    //     Can remove French Troop
    //     Can remove French Police
    //     Max pieces removed
    def spacePriority(ambush: Boolean) = new Ordering[Space] {
      val govBaseExposed =
        if (ambush) (sp: Space) => sp.govBases > 0 && sp.totalCubes == 0
        else        (sp: Space) => sp.govBases > 0 && sp.totalCubes < 2
      val frenchTroopExposed =
        if (ambush) (sp: Space) => sp.frenchTroops > 0 && sp.totalPolice == 0
        else        (sp: Space) => sp.frenchTroops > 0 && sp.totalPolice < 2
      def compare(x: Space, y: Space) = {
        (govBaseExposed(x), govBaseExposed(y)) match {
          case (true, false) => -1
          case (false, true) =>  1
          case _ =>
            (frenchTroopExposed(x), frenchTroopExposed(y)) match {
              case (true, false) => -1
              case (false, true) =>  1
              case _ =>
                (x.frenchPolice > 0, y.frenchPolice > 0) match {
                  case (true, false) => -1
                  case (false, true) =>  1
                  case _ =>
                    if (ambush) ((y.totalCubes + y.govBases) min 1) - ((x.totalCubes + x.govBases) min 1)
                    else        ((y.totalCubes + y.govBases) min 2) - ((x.totalCubes + x.govBases) min 2)
                }
            }
        }
      }
    }
    
    def attackInSpace(name: String, ambush: Boolean): Unit = {
      var sp = game.getSpace(name)
      if (ambush) {
        log()
        log(s"$Fln executes Attack operation with ambush: $name")
        decreaseResources(Fln, 1)
        activateHiddenGuerrillas(name, 1)
        removeLosses(name, attackLosses(sp.pieces, ambush))
      }
      else {
        log()
        log(s"$Fln executes Attack operation: $name")
        decreaseResources(Fln, 1)
        activateHiddenGuerrillas(name, sp.hiddenGuerrillas)
        sp = game.getSpace(name)
        log(s"${amountOf(sp.totalGuerrillas, "guerrilla")} present")
        val die = dieRoll
        val result = if (die <= sp.totalGuerrillas) s"$die (succeeds)" else s"$die (fails)"
        log(s"Die roll result is: $result")
        if (die <= sp.totalGuerrillas) {
          removeLosses(name, attackLosses(sp.pieces, ambush))
          if (die == 1) {
            log("Capture goods (die roll == 1)")
            if (game.guerrillasAvailable == 0)
              log("No guerrillas in the available box")
            else
              placePieces(name, Pieces(hiddenGuerrillas = 1))
          }
        }
      }
    }
    
    def attackLosses(pieces: Pieces, ambush: Boolean): Pieces = {
      val maxLosses = if (ambush) 1 else 2
      var losses    = Pieces()
      def remaining = maxLosses - losses.total
      
      for (pieceType <- List(FrenchPolice, AlgerianPolice, FrenchTroops, AlgerianTroops, GovBases))
        losses = losses.add(pieces.numOf(pieceType) min remaining, pieceType)
        
      val attrition = (if (ambush) 0 else losses.govBases + losses.frenchCubes) min pieces.activeGuerrillas
      losses + Pieces(activeGuerrillas = attrition)
    }
    
    // Must be able to kill at least two government pieces total.
    def execute: Either[ActionFlowchartNode, Action] = {
      val hasGov          = (sp: Space) => sp.totalCubes > 0 || sp.govBases > 0
      val noAmbush        = (sp: Space) => sp.flnBases == 0 && sp.totalGuerrillas >= 6 && hasGov(sp)
      val withAmbush      = (sp: Space) => hasSafeHiddenGuerrilla(sp) && hasGov(sp)
      val with4Guerrillas = (sp: Space) => sp.flnBases == 0 && sp.totalGuerrillas >= 4 && hasGov(sp)
      
      // Do we have at least one space where we can guaranteee success without ambush
      val noAmbushCandidates = game.algerianSpaces filter noAmbush
      val noAmbushKillTwoCandidates = noAmbushCandidates filter (sp => sp.totalCubes + sp.govBases > 1)
      val noAmbushNames      = spaceNames(noAmbushCandidates).toSet
      val ambushCandidates   = game.algerianSpaces filter (sp => !noAmbushNames(sp.name) && withAmbush(sp))
      
      // Bot does not extort to pay for attacks
      // With only one resource there must be a space with 6+ guerrillas (and no base)
      // and at least two government pieces.
      val canKillAtLeastTwo = game.resources(Fln) match {
        case 0                         => false
        case 1                         => noAmbushKillTwoCandidates.nonEmpty
        case _ if !canDoMultipleSpaces => noAmbushKillTwoCandidates.nonEmpty // LimOp
        case _ if canDoSpecialActivity => noAmbushCandidates.size + ambushCandidates.size > 1
        case _                         => noAmbushCandidates.size > 1
      }
      
      if (canKillAtLeastTwo) {
        log()
        log(s"$Fln chooses: Attack")
        val numSpaces = if (game.resources(Fln) == 1 || !canDoMultipleSpaces) {
          val target = noAmbushKillTwoCandidates.sorted(spacePriority(false)).head
          attackInSpace(target.name, false)
          1
        }
        else {
          def doAttacks(candidates: List[Space], ambush: Boolean): List[String] = {
            candidates match {
              case Nil                           => Nil
              case _ if game.resources(Fln) == 0 => Nil
              case t::ts =>
                attackInSpace(t.name, ambush)
                t.name :: doAttacks(ts, ambush)
            }
          }
          
          // First do guarenteed attacks without ambush
          val preAmbush = doAttacks(noAmbushCandidates.sorted(spacePriority(false)), false)
          // Next if we can to an special activity then add up to two ambush spaces (resources permitting)
          val withAmbush = if (canDoSpecialActivity)
            doAttacks(ambushCandidates.sorted(spacePriority(true)) take 2, true)
          else
            Nil
          // Finally, if we a resource remaining and there is a candidate space with at least
          // four guerrillas the attack there as well.
          val completed  = preAmbush.toSet ++ withAmbush.toSet
          val candidates = game.algerianSpaces filter (sp => !completed(sp.name) && with4Guerrillas(sp))
          val postAmbush = doAttacks(candidates.sorted(spacePriority(false)) take 1, false)
          (preAmbush.size + withAmbush.size + postAmbush.size)
        }
        
        tryExtort()  // If did not already ambush and can do special activity
        Right(effectiveAction(numSpaces))
      }
      else
        Left(ConsiderRally)
    }
  }

  object ConsiderRally extends ActionFlowchartNode {
    val desc = "Consider Rally Operation"
    sealed trait RallyType
    case object PlaceBase       extends RallyType
    case object PlaceGuerrillas extends RallyType  // Place guerrillas or flip them underground
    
    val BasePriorities: PriorityList = List(
      CriteriaFilter("Has 2+ active & 1+ hidden", sp => sp.activeGuerrillas > 1 && sp.hiddenGuerrillas > 0),
      CriteriaFilter("Has 1+ active & 1+ hidden", sp => sp.activeGuerrillas > 0 && sp.hiddenGuerrillas > 0),
      CriteriaFilter("Has 1+ active",             sp => sp.activeGuerrillas > 0))
    val UnprotectedBasePriorities: PriorityList = List(
      CriteriaFilter("In Algeria",         sp => !sp.isCountry),
      CriteriaFilter("With cubes",         sp => sp.totalCubes > 0),
      CriteriaFilter("1+ population",      sp => sp.population > 0),
      LowestScorePriority("Fewest hidden", sp => sp.hiddenGuerrillas))
    val SupportSectorPriorities: PriorityList = List(
      HighestScorePriority("Highest population", sp => sp.population))
    val GuerrillasNoBasePriorities: PriorityList = List(
      CriteriaFilter("In Algeria",            sp => !sp.isCountry),
      HighestScorePriority("Most guerrillas", sp => sp.totalGuerrillas),
      CriteriaFilter("No Gov cubes",          sp => sp.totalCubes == 0))

    // name will be either "available" or the name of a map space
    case class ToPlace(num: Int, name: String)
    
    // Get guerrillas from the available box first, then from map spaces.
    // Return ToPlace() entries with up to the requested number of guerrillas
    def getGuerrillasToPlace(num: Int, target: Space): (ToPlace, List[ToPlace]) = {
      if (num == 0)
        (ToPlace(0, "available"), Nil)
      else if (game.guerrillasAvailable >= num)
        (ToPlace(num, "available"), Nil)
      else if (game.guerrillasAvailable > 0)
        (ToPlace(game.guerrillasAvailable, "available"), eligibleGuerrillasOnMap(num - game.guerrillasAvailable, target))
      else
        (ToPlace(0, "available"), eligibleGuerrillasOnMap(num, target))
    }
    
    def eligibleGuerrillasOnMap(totalNeeded: Int, target: Space): List[ToPlace] = {
      def numEligible(sp: Space) = if (sp.flnBases > 0 || sp.isSupport)
        sp.activeGuerrillas min (sp.totalGuerrillas - 2)
      else
        sp.activeGuerrillas
            
      def getNext(remaining: Int, entries: List[ToPlace]): List[ToPlace] = {
        entries match {
          case Nil => Nil
          case _ if remaining == 0 => Nil
          case x :: xs =>
            val num = x.num min remaining
            x.copy(num = num) :: getNext(remaining - num, xs)
        }
      }
      
      val all = (if (target.isCountry) game.spaces else game.algerianSpaces) filterNot (_.name == target.name)
      val eligible = (all flatMap { sp =>
        val n = numEligible(sp)
        if (n > 0) Some(ToPlace(n, sp.name)) else None
      }).sortBy { case ToPlace(n, _) => -n }
      
      getNext(totalNeeded, eligible)
    }
      

    def execute: Either[ActionFlowchartNode, Action] = {
      val supportCity = (sp: Space) => sp.isCity && sp.isSupport
      val placeBaseNoCubes = (sp: Space) => !supportCity(sp)   &&
                                            sp.canTakeBase     &&
                                            (sp.isCountry || sp.flnBases == 0) &&
                                            sp.totalCubes == 0 &&
                                            sp.totalGuerrillas >= 3
      val minGWithCubes = if (canDoMultipleSpaces) 4 else 3
      val placeBaseWithCubes = (sp: Space) => !supportCity(sp)  &&
                                              sp.canTakeBase    &&
                                              sp.totalCubes > 0 &&
                                              sp.flnBases == 0  &&
                                              sp.totalGuerrillas >= minGWithCubes
      val unprotectedBase = (sp: Space) => !supportCity(sp) &&
                                           sp.flnBases > 0  &&
                                           ((!sp.isCountry && sp.population > 0 && sp.hiddenGuerrillas < 2) ||
                                            ((sp.isCountry || sp.population == 0) && sp.hiddenGuerrillas == 0))
      val sectorsAtSupport = (sp: Space) => sp.isSector && sp.isSupport && sp.hiddenGuerrillas == 0
      val agitate2population = (sp: Space) => !supportCity(sp)  && 
                                              sp.population > 1 && 
                                              !sp.isOppose      &&
                                              (sp.govBases > 0 || sp.isFlnControlled)
      val agitateForShift = (sp: Space) => !supportCity(sp)  && 
                                           sp.population > 0 && 
                                           !sp.isOppose      &&
                                           (sp.govBases > 0 || sp.isFlnControlled)
      val guerrillasNoBase = (sp: Space) => !supportCity(sp)  && sp.flnBases == 0 && sp.totalGuerrillas > 0
      val baseNoCubeCandidates   = spaceNames(game.spaces filter placeBaseNoCubes)
      val baseWithCubeCandidates = spaceNames(game.spaces filter placeBaseWithCubes)
      val canPlaceBase        = game.flnBasesAvailable > 0 && (baseNoCubeCandidates.nonEmpty || baseWithCubeCandidates.nonEmpty)
      val guerrillasWithBases = game.totalOnMap(sp => if (sp.flnBases > 0) sp.totalGuerrillas else 0)
      
      var rallySpaces = Set.empty[String]
      var shiftedFranceTrack = false
      var agitateSpace: Option[String] = None
      var reservedResources = 0
      val maxTotalRallies = if (!canDoMultipleSpaces)         1
                            else if (game.resources(Fln) < 9) 1000 // No limit
                            else                              game.resources(Fln) * 2 / 3
      
      def numRallies  = rallySpaces.size + (if (shiftedFranceTrack) 1 else 0)
      def canContinue = numRallies < maxTotalRallies 
      def hasRallied(name: String) = rallySpaces contains name
      def totalAgitateCost(sp: Space) = costToAgitate(sp) + (if (hasRallied(sp.name)) 0 else 1)
      def chooseAgitateTarget(candidates: List[Space]): Option[Space] = candidates find { sp =>
        if (hasRallied(sp.name) || canContinue) {
          val (withExort, sufficientResources) = tryOperations {
            if (game.resources(Fln) < totalAgitateCost(sp))
              tryExtort()
            game.resources(Fln) >= totalAgitateCost(sp)
          }
          if (sufficientResources) {
            showLogEntries(game, withExort)
            game = withExort
          }
          sufficientResources
        }
        else
          false
      }
        
      
      if (canPlaceBase || (game.totalOnMap(_.flnBases) * 2) > (guerrillasWithBases + dieRoll/2)) {
        log()
        log(s"$Fln chooses: Rally")
        
        def doRallies(candidates: List[String], rallyType: RallyType, priorities: PriorityList, force: Boolean = false, numRemaining: Int = 1000): Unit = {
          def haveAResource(): Boolean = {
            if (game.resources(Fln) == reservedResources)
              tryExtort()
            game.resources(Fln) > reservedResources
          }
          
          if (candidates.nonEmpty && canContinue) {
            // Note that we fetch the spaces from the current game state each time
            val sp = topPriority(spaces(candidates), priorities)
            rallyType match {
              case PlaceBase if game.flnBasesAvailable == 0 =>
                if (force && haveAResource()) {
                  log()
                  log(s"$Fln executes Rally operation to allow agitation: ${sp.name}")
                  decreaseResources(Fln, 1)
                  rallySpaces += sp.name
                }
              case PlaceBase =>
                if (haveAResource()) {
                  log()
                  log(s"$Fln executes Rally operation: ${sp.name}")
                  decreaseResources(Fln, 1)
                  val numActive = 2 min sp.activeGuerrillas
                  val numHidden = 2 - numActive
                  removeToAvailableFrom(sp.name, Pieces(activeGuerrillas = numActive, hiddenGuerrillas = numHidden))
                  placePieces(sp.name, Pieces(flnBases = 1))
                  rallySpaces += sp.name
                }

              case PlaceGuerrillas =>
                // When placing guerrillas in a space with a base the final number of 
                // guerrillas should not exceed the population of the space + 1.
                // If it does then we will flip the active guerrillas to hidden
                val numToPlace = if (sp.flnBases == 0)
                  1
                else
                  (sp.flnBases + sp.population) min ((sp.population + 1) - sp.totalGuerrillas) max 0

                getGuerrillasToPlace(numToPlace, sp) match {
                  case (ToPlace(0, _), Nil) if numToPlace == 0 =>  // Flip any active
                    if (sp.activeGuerrillas > 0 && haveAResource()) {
                      log()
                      log(s"$Fln executes Rally operation: ${sp.name}")
                      decreaseResources(Fln, 1)
                      hideActiveGuerrillas(sp.name, sp.activeGuerrillas)
                      rallySpaces += sp.name
                    }
                  case (ToPlace(0, _), Nil) if force =>
                    if (haveAResource()) {
                      log()
                      log(s"$Fln executes Rally operation to allow agitation: ${sp.name}")
                      decreaseResources(Fln, 1)
                      rallySpaces += sp.name
                    }
                  
                  case (ToPlace(avail, _), fromMap) =>  // Place guerrillas from sources
                    if (haveAResource()) {
                      log()
                      log(s"$Fln executes Rally operation: ${sp.name}")
                      decreaseResources(Fln, 1)
                      fromMap foreach { case ToPlace(n, name) => removeToAvailableFrom(name, Pieces(activeGuerrillas = n)) }
                      placePieces(sp.name, Pieces(hiddenGuerrillas = numToPlace min game.guerrillasAvailable))
                      rallySpaces += sp.name
                    }
                }
            }
            doRallies(candidates filterNot (_ == sp.name), rallyType, priorities, force, numRemaining - 1)
          }
        }
        
        doRallies(baseNoCubeCandidates, PlaceBase, BasePriorities)
        doRallies(baseWithCubeCandidates, PlaceBase, BasePriorities)
        val unprotectedBaseCandidates = spaceNames(game.spaces filter (sp => (!hasRallied(sp.name) && unprotectedBase(sp))))
        doRallies(unprotectedBaseCandidates, PlaceGuerrillas, UnprotectedBasePriorities)
        // France track?
        if (game.franceTrack < FranceTrackMax && canContinue) {
          if (game.resources(Fln) == 0)
            tryExtort()
          if (game.resources(Fln) > 0) {
            log()
            log(s"$Fln executes Rally operation: France Track")
            decreaseResources(Fln, 1)
            increaseFranceTrack(1)
            shiftedFranceTrack = true
          }
        }
        val supportSectorCandidates = spaceNames(game.spaces filter (sp => (!hasRallied(sp.name) && sectorsAtSupport(sp))))
        doRallies(supportSectorCandidates, PlaceGuerrillas, SupportSectorPriorities)
        
        // If we have a 2+ population space were we can agitate for effect
        // then attempt to reserve resources to do so and rally in the space if not done already.
        val agitate2popCandidates = game.algerianSpaces filter agitate2population
        if (agitate2popCandidates.nonEmpty) {
          val best        = agitate2popCandidates.sortBy(sp => -sp.population).sortBy(totalAgitateCost(_)).head
          val cheapest    = agitate2popCandidates.sortBy(totalAgitateCost(_)).head
          chooseAgitateTarget(best :: cheapest :: Nil) foreach { sp =>
            agitateSpace      = Some(sp.name)
            reservedResources = costToAgitate(sp)
            if (!hasRallied(sp.name))
              doRallies(sp.name :: Nil, PlaceGuerrillas, Nil, force = true)
          }
        }

        val guerrillasNoBaseCandidates = spaceNames(game.spaces filter (sp => (!hasRallied(sp.name) && guerrillasNoBase(sp))))
        doRallies(guerrillasNoBaseCandidates, PlaceGuerrillas, GuerrillasNoBasePriorities, numRemaining = 2)  // 2 max

        // If we have not yet agitated, then pick a space to agitate
        // where we can shift to neutral or oppose rallying there if necessary
        
        val agitateForShiftCandidates = game.algerianSpaces filter agitateForShift
        if (agitateSpace.isEmpty && agitateForShiftCandidates.nonEmpty) {
          val best        = agitateForShiftCandidates.sortBy(sp => -sp.population).sortBy(totalAgitateCost(_)).head
          val cheapest    = agitateForShiftCandidates.sortBy(totalAgitateCost(_)).head
          chooseAgitateTarget(best :: cheapest :: Nil) foreach { sp =>
            agitateSpace = Some(sp.name)
            if (!hasRallied(sp.name))
              doRallies(sp.name :: Nil, PlaceGuerrillas, Nil, force = true)
          }
        }
        
        // Agitate in the selected space we should have sufficient resources to
        // remove all terror and shift one level (this is assured by chooseAgitateTarget())
        agitateSpace foreach { name =>
          val sp = game.getSpace(name)
          log()
          log(s"$Fln agitates in $name")
          decreaseResources(Fln, costToAgitate(sp))
          removeTerror(name, sp.terror)
          decreaseSupport(name, 1)
        }
      }
      
      if (numRallies > 0) {
        trySubvert()
        tryExtort()
        Right(effectiveAction(numRallies))
      }
      else
        Left(ConsiderMarch)
    }
  }
  
  object ConsiderMarch extends ActionFlowchartNode {
    val desc = "Consider March Operation"
    def execute: Either[ActionFlowchartNode, Action] = Right(Pass)
  }


  
  def act(): Unit = {
    specialActivityTaken = false
    
    @tailrec def evaluateNode(node: ActionFlowchartNode): Action = {
      botDebug(s"Bot Flowchart: $node")
      node.execute match {
        case Left(nextNode) => evaluateNode(nextNode)
        case Right(action)  => action
      }
    }
    
    val action = evaluateNode(LimOpAndZeroResources)
    log(s"\nPlace the ${Fln} eligibility cylinder in the ${action} box")
    game = game.copy(sequence = game.sequence.nextAction(action))
  }
  
  val TerrorPriorities = List(
    CriteriaFilter("Remove support", sp => sp.isSupport),
    CriteriaFilter("Add terror to neutral in final campaign", sp => sp.isNeutral && game.terrorMarkersAvailable > 0),
    HighestScorePriority("Highest population", sp => sp.population))
  
  def doTerror(): Action = {
    def nextTerror(candidates: List[Space], num: Int): Int = {
      if (candidates.isEmpty || (num == 1 && !canDoMultipleSpaces))
        num
      else {
        if (game.resources(Fln) == 0)
          tryExtort()
      
        // If the bot is still broke then we are done
        if (game.resources(Fln) == 0)
           num
        else {
          val target = topPriority(candidates, TerrorPriorities)
          log()
          log(s"$Fln executes Terror operation: ${target.name}")
          decreaseResources(Fln, 1)
          activateHiddenGuerrillas(target.name, 1)
          if (target.terror == 0 && game.terrorMarkersAvailable > 0)
            addTerror(target.name, 1)
          setSupport(target.name, Neutral)
        
          nextTerror(candidates filterNot (_.name == target.name), num + 1)
        }
      }
    }
    
    log()
    log(s"$Fln chooses: Terror")
    val numSpaces = nextTerror(terrorCandidates, 0)
    trySubvert()
    tryExtort()
    effectiveAction(numSpaces)
  }
}
  