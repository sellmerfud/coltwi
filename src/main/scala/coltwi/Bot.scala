
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
import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine
import ColonialTwilight._


object Bot {

  def botDebug(msg: => String) = if (game.params.botDebug) println(msg)
    
  def botInspect[T](label: String, value: T): T = {
    botDebug(s"DEBUG ==> $label == $value")
    value
  }
  
  // class used to keep track of gobal state variables that
  // can change during the execution of a single turn.
  case class TurnState(
    rallyConsidered: Boolean          = false,
    marchConsidered: Boolean          = false,
    specialActivityAllowed: Boolean   = false,
    specialActivityTaken: Boolean     = false,
    freeOperation: Boolean            = false,
    maxSpaces: Option[Int]            = None,      // None == unlimited
    onlyIn: Set[String]               = Set.empty, // Operation limited to these spaces only 
    movingGroups: Map[String, Pieces] = Map.empty.withDefaultValue(Pieces())
  ) {
    
    val maxAllowed           = maxSpaces getOrElse 1000
    val multipleSpaces       = maxAllowed > 1
    val limOpOnly            = maxAllowed == 1
    val canDoSpecialActivity = specialActivityAllowed && !specialActivityTaken
    
    def addMovingGroup(name: String, pieces: Pieces): TurnState =
      copy(movingGroups = movingGroups + (name -> (movingGroups(name) + pieces)))
    def removeMovingGroup(name: String, pieces: Pieces): TurnState =
      copy(movingGroups = movingGroups + (name -> (movingGroups(name) - pieces)))
  }
  
  var turnState = TurnState()
  
  // Used to filter spaces by the onlyIn field of turnState
  val onlyIn = (sp: Space) => turnState.onlyIn.isEmpty || turnState.onlyIn(sp.name)

  // Does space have a guerrilla that can be flipped without exposing any bases?
  def hasSafeHiddenGuerrillas(sp: Space, num: Int) =
    ((sp.flnBases == 0 || sp.isCountry) && sp.hiddenGuerrillas >= num) ||
    (sp.flnBases > 0 && sp.hiddenGuerrillas > num)

  def tryOperations[T](code: => T): (GameState, TurnState, T) = {
    val savedGameState = game
    val savedTurnState = turnState
    val savedEchoState = echoLogging
    echoLogging = false
    try {
      val result = code
      (game, turnState, result)
    }
    finally {
      game        = savedGameState
      turnState   = savedTurnState
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
  
  // In order for the Bot to play an event that is not marked for FLN play,
  // the effects of the event must do at least one of the following:
  // - Reduce the government's victory margin
  // - Reduce the government's resources
  // - Shift the France track toward 'F'
  // - Place an FLN base
  // - Increase FLN resources
  def eventIsEffective(beforeState: GameState, afterState: GameState): Boolean = (
    beforeState.govScore               > afterState.govScore               ||
    beforeState.resources(Gov)         > afterState.resources(Gov)         ||
    beforeState.franceTrack            < afterState.franceTrack            ||
    beforeState.totalOnMap(_.flnBases) < afterState.totalOnMap(_.flnBases) ||
    beforeState.resources(Fln)         < afterState.resources(Fln)
  )

  // name will be either "available" or the name of a map space
  case class FromMap(num: Int, name: String)
  case class GuerrillasToPlace(numAvailable: Int, fromMap: List[FromMap]) {
    def total = numAvailable + (fromMap map (_.num)).sum
  }
  
  // Get guerrillas from the available box first, then from map spaces.
  def getGuerrillasToPlace(num: Int, target: Space): GuerrillasToPlace = {
    if (num == 0)
      GuerrillasToPlace(0, Nil)
    else if (game.guerrillasAvailable >= num)
      GuerrillasToPlace(num, Nil)
    else
      GuerrillasToPlace(game.guerrillasAvailable, eligibleGuerrillasOnMap(num - game.guerrillasAvailable, target))
  }
  
  def eligibleGuerrillasOnMap(totalNeeded: Int, target: Space): List[FromMap] = {
    def numEligible(sp: Space) = if (sp.flnBases > 0 || sp.isSupport)
      sp.activeGuerrillas min (sp.totalGuerrillas - 2)
    else
      sp.activeGuerrillas
          
    def getNext(remaining: Int, entries: List[FromMap]): List[FromMap] = {
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
      if (n > 0) Some(FromMap(n, sp.name)) else None
    }).sortBy { case FromMap(n, _) => -n }
    
    getNext(totalNeeded, eligible)
  }
  
  // Place guerrillas from available.  First removing any from
  // the map if necessary.
  def placeGuerrillas(spaceName: String, guerrillas: GuerrillasToPlace): Unit = {
    placePieces(spaceName, Pieces(hiddenGuerrillas = guerrillas.numAvailable))
    guerrillas.fromMap foreach { case FromMap(n, name) => 
      removeToAvailable(name, Pieces(activeGuerrillas = n))
      placePieces(spaceName, Pieces(hiddenGuerrillas = n))
    }
  }
    

  trait Filter[T] {
    val desc: String
    def filter(spaces: List[T]): List[T]
    override def toString() = desc
  }
  
  // Space filters are used when selecting the top priority from of spaces.
  type SpacePriorities = List[Filter[Space]]
  
  // Find the valid candidate spaces using the given list of space filters.
  //
  // Each filter is first used against the given spaces and if the filter does not find any matching
  // candidates, the next filter is given a chance.
  // As soon as a filter finds at least one matching country, then the procees stops and the
  // results from that filter are returned.
  // If none of the filters finds at least one matching country we return Nil.
  // @tailrec final def selectCandidates(spaces: List[Space], filters: SpacePriorities): List[Space] = {
  //   botDebug(s"selectCandidates: [${(spaces map (_.name)) mkString ", "}]")
  //   (spaces, filters) match {
  //     case (Nil, _) =>
  //       botDebug("selectCandidates: no spaces to consider")
  //       Nil    // No spaces to consider
  //     case (_, Nil) =>
  //       botDebug("selectCandidates: no spaces found")
  //       Nil    // No filter found any candidates
  //     case (xs, f::fs) =>
  //       (f filter xs) match {
  //         case Nil =>            // Filter did not match anything, try the next filter
  //           botDebug(s"selectCandidates ($f): failed")
  //           selectCandidates(xs, fs)
  //         case results =>        // We got some results…
  //           botDebug(s"selectCandidates ($f): [${(results map (_.name) mkString ", ")}]")
  //           results
  //       }
  //   }
  // }
  
  // Process the list of spaces by each space filter in the prorities list.
  // In this function each filter is processed in order until we have used all filters
  // in the list to narrow the choices to a single space.  If we go through all of
  // the filters and we stil have more than one viable country, then we pick one at
  // random.
  // Note: This function should not be called with an empty list of spaces!
  def topPriority[T](entries: List[T], priorities: List[Filter[T]]): T = {
    assert(entries.nonEmpty, "topPriority: called with empty list!")
    botDebug(s"topPriority: [${(entries map (_.toString)) mkString ", "}]")
    @tailrec def nextPriority(entries: List[T], priorities: List[Filter[T]]): T = {
      (entries, priorities) match {
        case (Nil, _)    => throw new IllegalArgumentException("nextPriority: empty list")
        case (sp :: Nil, _) => 
          botDebug(s"topPriority: Picked a winner [${sp.toString}]")
          sp
        case (best, Nil)   =>
          val sp = shuffle(best).head        // Take one at random
          botDebug(s"topPriority: Picked random winner [${sp.toString}]")
          sp
        case (list, f :: fs) =>
          (f filter list) match {
            case Nil =>
              botDebug(s"topPriority ($f) no matches")
              nextPriority(list, fs) // Filter entire list by next priority
            case best  =>
              botDebug(s"topPriority ($f) matched [${(best map (_.toString) mkString ", ")}]")
              nextPriority(best, fs) // Filter matched list by next priority
          }
      }
    }
    nextPriority(entries, priorities)
  }
  
  // A boolean criteria filter
  // Filters the given entries and returns the results.
  class BooleanPriority[T](val desc: String, criteria: (T) => Boolean) extends Filter[T] {
    def filter(entries: List[T]) = (entries filter criteria)
  }
  
  // Highest integer score filter used with Priority Tables.
  // Applies the given score function to each entry in the input list and
  // takes the highest value.
  // Then returns the list of entries whose score matches that highest value.
  class HighestPriority[T](val desc: String, score: (T) => Int) extends Filter[T] {
    def filter(entries: List[T]): List[T] = {
      val high = (entries map score).max
      botDebug(s"Highest ($desc): score = $high")
      entries filter (score(_) == high)
    }
  }

  // Lowest integer score filter used with Priority Tables.
  // Applies the given score function to each entry in the input list and
  // takes the lowest value.
  // Then returns the list of entries whose score matches that lowest value.
  class LowestPriority[T](val desc: String, score: (T) => Int) extends Filter[T] {
    def filter(entries: List[T]): List[T] = {
      val low = (entries map score).min
      botDebug(s"Lowest ($desc): score = $low")
      entries filter (score(_) == low)
    }
  }

  // Is the Bot currently acting second and guarenteed to act first on the next card?
  def botWillActTwice = game.sequence.secondEligible == Fln && 
      (game.sequence.firstAction == Some(ExecOpPlusActivity) || 
       game.sequence.firstAction == Some(ExecOpOnly))

  def botCanDo(action: Action): Boolean = game.sequence.availableActions contains action
  
  // Return the effective action based on what was performed
  def effectiveAction(numSpaces: Int): Action = {
    if (turnState.specialActivityTaken)
      ExecOpPlusActivity
    else if (numSpaces == 1 && botCanDo(ExecLimitedOp))
      ExecLimitedOp
    else
      ExecOpOnly
  }
  
  def hiddenNeeded(sp: Space) = if (capabilityInPlay(CapAmateurBomber) && sp.isCity) 2 else 1
    
  def terrorCandidates(offLimits: Set[String]) = game.algerianSpaces filter { sp =>
    !offLimits(sp.name) &&
    sp.population > 0   &&
    hasSafeHiddenGuerrillas(sp, hiddenNeeded(sp)) && 
    ((sp.isSupport && !momentumInPlay(MoIntimidation)) || 
     (game.isFinalCampaign && sp.isNeutral && sp.terror == 0 && sp.canTrain))
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
    val has2Police = new BooleanPriority[Space]("2 Police cubes", _.algerianPolice == 1)
    val hasPolice  = new BooleanPriority[Space]("Police cube",    _.algerianPolice > 0)
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
      (generic filter (_.name != target.name)) match {
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
  def trySubvert(): Unit = if (turnState.canDoSpecialActivity) {
    subvertCommands match {
      case Nil =>
      case cmds =>
        turnState = turnState.copy(specialActivityTaken = true)
        log()
        log(s"$Fln executes a Subvert special ability")
        for (SubvertCmd(replace, name, pieces) <- cmds) {
          removeToAvailable(name, pieces)
          if (replace)
            placePieces(name, Pieces(hiddenGuerrillas = 1))
        }
    }
  }
  
  def tryExtort(protectedGuerrillas: List[(String, Int)] = Nil): Unit = if (turnState.canDoSpecialActivity && game.resources(Fln) < 5) {
    val protectedGs = protectedGuerrillas.toMap.withDefaultValue(0)
    def hasSafeHiddenGuerrilla(sp: Space) = 
      ((sp.flnBases == 0 || sp.isCountry) && sp.hiddenGuerrillas > protectedGs(sp.name)) ||
      (sp.flnBases > 0 && sp.hiddenGuerrillas > (1 + protectedGs(sp.name)))
    
    val primary = (game.algerianSpaces filter { sp =>
      sp.population > 0  &&
      (!momentumInPlay(MoHardendAttitudes) || !sp.isSector ||sp.flnBases > 0) &&
      sp.isFlnControlled && (
        (sp.flnBases > 0 && sp.totalCubes > 0 && sp.hiddenGuerrillas > (2 + protectedGs(sp.name))) ||
        ((sp.flnBases == 0 || sp.totalCubes == 0) && sp.hiddenGuerrillas > (1 + protectedGs(sp.name)))
      ) 
    }) ::: (game.countrySpaces filter hasSafeHiddenGuerrilla)
    
    val primaryNames = spaceNames(primary).toSet
    val secondary  = game.algerianSpaces filter (sp => 
      !primaryNames(sp.name)     &&
      sp.isFlnControlled         && 
      hasSafeHiddenGuerrilla(sp) && 
      (!momentumInPlay(MoHardendAttitudes) || !sp.isSector || sp.flnBases > 0))
    
    if (primary.nonEmpty || (game.resources(Fln) == 0 && secondary.nonEmpty)) {
      turnState = turnState.copy(specialActivityTaken = true)
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
    def execute(): Either[ActionFlowchartNode, Action]
  }

  // This is the starting point FLN Bot flowchart
  object BotPasses extends ActionFlowchartNode {
    val desc = "FLN Bot chooses to pass"
    def execute(): Either[ActionFlowchartNode, Action] = {
      log()
      log(separator())
      log(s"FLN chooses: ${Pass}")
      performPass(Fln)
      Right(Pass)
    }
  }

  // This is the starting point FLN Bot flowchart
  object LimOpAndZeroResources extends ActionFlowchartNode {
    val desc = "LimOp and Resources == 0?"
    def execute(): Either[ActionFlowchartNode, Action] = {
      if (game.resources(Fln) == 0 && turnState.limOpOnly && !botCanDo(Event))
        Left(BotPasses)
      else
        Left(EachAlgerianBaseHasUnderground)
    }
  }
  
  object EachAlgerianBaseHasUnderground extends ActionFlowchartNode {
    val desc = "Each Algerian Base at +1 Pop has 2+ underground guerrillas...?"
    def execute(): Either[ActionFlowchartNode, Action] = {
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
    def execute(): Either[ActionFlowchartNode, Action] = 
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
    def execute(): Either[ActionFlowchartNode, Action] = {
      val originalGameState = game
      val card = deck(game.currentCard.get)
      
      def executeEvent(which: EventSelection): Unit = {
        if (which == Unshaded) {
          if (card.dual == false)
            log(s"\nFLN executes the event")
          else
            log(s"\nFLN executes the Unshaded event")
          card.executeUnshaded(Fln)
        }
        else {
          log(s"\nFLN executes the Shaded event")
          card.executeShaded(Fln)
        }
      }
      
      def willPlayEvent(eventState: GameState): Boolean = {
        card.flnAlwaysPlay ||
        (eventIsEffective(game, eventState) && (card.markedForFLN || dieRoll < 5))
      }
      
      val terrorResult = if (terrorCandidates(Set.empty).isEmpty)
        None
      else
        Some(tryOperations { doTerror() })
      
      val eventResult = if (botCanDo(Event) == false)
        None
      else
        card.botEventSelection() match {
          case NoEvent  => None
          case selected => Some(tryOperations { executeEvent(selected) })
        }
      
      (eventResult, terrorResult) match {
        case (Some((eventState, ts, _)), Some((terrorState, _, _))) if eventState.govScore <= terrorState.govScore && willPlayEvent(eventState) =>
          showLogEntries(game, eventState)
          turnState = ts
          game = eventState
          Right(Event)
        case (Some((eventState, ts, _)), None) if willPlayEvent(eventState) =>
          showLogEntries(game, eventState)
          turnState = ts
          game = eventState
          Right(Event)
        case (_, Some((terrorState, ts, action))) if terrorState.govScore < game.govScore || 
                                                terrorState.totalOnMap(_.terror) > game.totalOnMap(_.terror) =>
          showLogEntries(game, terrorState)
          turnState = ts
          game = terrorState
          Right(action)
        case _ =>
          Left(ConsiderAttack)
      }
    }
  }

  val TerrorPriorities = List(
    new BooleanPriority[Space]("Remove support",                          _.isSupport),
    new BooleanPriority[Space]("Add terror to neutral in final campaign", _.isNeutral && game.terrorMarkersAvailable > 0),
    new HighestPriority[Space]("Highest population",                      _.population))
  
  def doTerror(): Action = {
    var terrorized = Set.empty[String]
    def nextTerror(): Unit = {
      val candidates = terrorCandidates(terrorized)
      if (candidates.nonEmpty && terrorized.size < turnState.maxAllowed) {
        val target = topPriority(candidates, TerrorPriorities)
        val isFree = turnState.freeOperation || (target.isCity && capabilityInPlay(CapEffectiveBomber))
        
        if (!isFree && game.resources(Fln) == 0) {
          val protectedGs = List((target.name -> hiddenNeeded(target)))
          tryExtort(protectedGs)
        }
      
        // If the bot is still broke then we are done
        if (isFree || game.resources(Fln) > 0) {
          terrorized += target.name
          log()
          log(s"$Fln executes Terror operation: ${target.name}")
          if (isFree)
            log(s"Terror is free in city because '${CapEffectiveBomber}' is in play")
          else
            decreaseResources(Fln, 1)
          activateHiddenGuerrillas(target.name, hiddenNeeded(target))
          if (target.terror == 0 && game.terrorMarkersAvailable > 0)
            addTerror(target.name, 1)
          if (!momentumInPlay(MoIntimidation))
            setSupport(target.name, Neutral)
        
          nextTerror()
        }
      }
    }

    log()
    log(separator())
    log(s"$Fln chooses: Terror")
    val numSpaces = nextTerror()
    trySubvert()
    tryExtort()
    effectiveAction(terrorized.size)
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
        if (!turnState.freeOperation)
          decreaseResources(Fln, 1)
        if (!capabilityInPlay(CapFlnCommandos))
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
    def execute(): Either[ActionFlowchartNode, Action] = {
      val hasGov          = (sp: Space) => sp.totalCubes > 0 || sp.govBases > 0
      val noAmbush        = (sp: Space) => sp.flnBases == 0 && sp.totalGuerrillas >= 6 && hasGov(sp)
      val withAmbush      = (sp: Space) => hasSafeHiddenGuerrillas(sp, 1) && hasGov(sp)
      val with4Guerrillas = (sp: Space) => sp.flnBases == 0 && sp.totalGuerrillas >= 4 && hasGov(sp)
      
      // Do we have at least one space where we can guaranteee success without ambush
      val noAmbushCandidates = game.algerianSpaces filter noAmbush
      val noAmbushKillTwoCandidates = noAmbushCandidates filter (sp => sp.totalCubes + sp.govBases > 1)
      val noAmbushNames      = spaceNames(noAmbushCandidates).toSet
      val ambushCandidates   = game.algerianSpaces filter (sp => !noAmbushNames(sp.name) && withAmbush(sp))
      
      // Bot does not extort to pay for attacks
      // With only one resource there must be a space with 6+ guerrillas (and no base)
      // and at least two government pieces.
      val effectiveSpaces = if (turnState.freeOperation) 
        turnState.maxAllowed min 3
      else
        turnState.maxAllowed min game.resources(Fln) min 3
      val canKillAtLeastTwo = effectiveSpaces match {
        case 0                                   => false
        case 1                                   => noAmbushKillTwoCandidates.nonEmpty
        case _ if turnState.limOpOnly            => noAmbushKillTwoCandidates.nonEmpty // LimOp
        case _ if turnState.canDoSpecialActivity => noAmbushCandidates.size + ambushCandidates.size > 1
        case _                                   => noAmbushCandidates.size > 1
      }
      
      val oneResourceOnly = !turnState.freeOperation && game.resources(Fln) == 1
      if (!momentumInPlay(MoPeaceTalks) && canKillAtLeastTwo) {
        log()
        log(separator())
        log(s"$Fln chooses: Attack")
        val numSpaces = if (turnState.limOpOnly || oneResourceOnly) {
          val target = noAmbushKillTwoCandidates.sorted(spacePriority(false)).head
          attackInSpace(target.name, false)
          1
        }
        else {
          def doAttacks(candidates: List[Space], ambush: Boolean): List[String] = {
            candidates match {
              case Nil => Nil
              case _ if game.resources(Fln) == 0 && !turnState.freeOperation => Nil
              case t::ts =>
                attackInSpace(t.name, ambush)
                t.name :: doAttacks(ts, ambush)
            }
          }
          
          // First do guarenteed attacks without ambush
          val preAmbush = doAttacks(noAmbushCandidates.sorted(spacePriority(false)), false)
          // Next if we can to an special activity then add up to two ambush spaces (resources permitting)
          val withAmbush = if (turnState.canDoSpecialActivity)
            doAttacks(ambushCandidates.sorted(spacePriority(true)) take 2, true)
          else
            Nil
          turnState = turnState.copy(specialActivityTaken = withAmbush.nonEmpty)
          
          // Finally, if we have a resource remaining and there is a candidate space with at least
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
    
    val BasePriorities: SpacePriorities = List(
      new BooleanPriority[Space]("Has 2+ active & 1+ hidden", sp => sp.activeGuerrillas > 1 && sp.hiddenGuerrillas > 0),
      new BooleanPriority[Space]("Has 1+ active & 1+ hidden", sp => sp.activeGuerrillas > 0 && sp.hiddenGuerrillas > 0),
      new BooleanPriority[Space]("Has 1+ active",             _.activeGuerrillas > 0))
    val UnprotectedBasePriorities: SpacePriorities = List(
      new BooleanPriority[Space]("In Algeria",      !_.isCountry),
      new BooleanPriority[Space]("With cubes",       _.totalCubes > 0),
      new BooleanPriority[Space]("1+ population",    _.population > 0),
      new LowestPriority[Space]("Fewest hidden",     _.hiddenGuerrillas))
    val SupportSectorPriorities: SpacePriorities = List(
      new HighestPriority[Space]("Highest population", _.population))
    val HighestPopPriorities: SpacePriorities = List(
      new HighestPriority[Space]("Highest population", _.population),
      new BooleanPriority[Space]("Neutral/gain control", sp => sp.isNeutral && sp.totalGov == sp.totalFln),
      new BooleanPriority[Space]("Neutral/remove Gov control", sp => sp.isNeutral && (sp.totalGov - sp.totalFln) == 1),
      new BooleanPriority[Space]("Wilaya with City", sp => Set("II", "IV", "V").contains(sp.wilaya)),
      new LowestPriority[Space]("Least terror", _.terror))
    val GuerrillasNoBasePriorities: SpacePriorities = List(
      new BooleanPriority[Space]("In Algeria",     !_.isCountry),
      new HighestPriority[Space]("Most guerrillas", _.totalGuerrillas),
      new BooleanPriority[Space]("No Gov cubes",    _.totalCubes == 0))
      
    def execute(): Either[ActionFlowchartNode, Action] = if (turnState.rallyConsidered)
      Left(BotPasses)
    else {
      val supportCity = (sp: Space) => sp.isCity && sp.isSupport
      // If a lim-Op (turnState.limOpOnly) then the no cubes requirement is skipped because the
      // Bot will go first on the next turn.
      val placeBaseNoCubes = (sp: Space) => !supportCity(sp)   &&
                                            sp.canTakeBase     &&
                                            (sp.isCountry || sp.flnBases == 0) &&
                                            (sp.totalCubes == 0 || turnState.limOpOnly) &&
                                            sp.totalGuerrillas >= 3
      val placeBase = (sp: Space) => !supportCity(sp)  &&
                                     sp.canTakeBase    &&
                                     sp.flnBases == 0  &&
                                     sp.totalGuerrillas > 3
      val unprotectedBase = (sp: Space) => !supportCity(sp) &&
                                           sp.flnBases > 0  &&
                                           ((!sp.isCountry && sp.population > 0 && sp.hiddenGuerrillas < 2) ||
                                            ((sp.isCountry || sp.population == 0) && sp.hiddenGuerrillas == 0))
      val sectorsAtSupport = (sp: Space) => sp.isSector && sp.isSupport && sp.hiddenGuerrillas == 0
      val agitate2population = (sp: Space, hasRallied: Boolean) => {
        val addedGs = if (hasRallied) 0
        else if (sp.flnBases > 0) (sp.flnBases + sp.population) min (sp.population + 1) min game.guerrillasAvailable
        else 1 min game.guerrillasAvailable
        val willControl = sp.isFlnControlled || sp.totalGov < (sp.totalFln + addedGs)
          
        !supportCity(sp)  && 
        !sp.isCountry     &&
        sp.population > 1 && 
        !sp.isOppose      &&
        (sp.flnBases > 0 || willControl)
      }
      val agitateForShift = (sp: Space, hasRallied: Boolean) => {
        val addedGs = if (hasRallied) 0
        else if (sp.flnBases > 0) (sp.flnBases + sp.population) min (sp.population + 1) min game.guerrillasAvailable
        else 1 min game.guerrillasAvailable
        val willControl = sp.isFlnControlled || sp.totalGov < (sp.totalFln + addedGs)
        
        !supportCity(sp)  && 
        !sp.isCountry     &&
        sp.population > 0 && 
        !sp.isOppose      &&
        (sp.flnBases > 0 || willControl)
      }
      
      val highestPopCriteria = (sp: Space) => !supportCity(sp) && (sp.flnBases == 0 || sp.totalGuerrillas < sp.population + 1)
      val guerrillasNoBase = (sp: Space) => !supportCity(sp)  && sp.flnBases == 0 && sp.totalGuerrillas > 0
      
      val baseNoCubeCandidates = spaceNames(game.spaces filter onlyIn filter placeBaseNoCubes)
      val baseCandidates       = spaceNames(game.spaces filter onlyIn filter placeBase) filterNot baseNoCubeCandidates.contains
      val canPlaceBase         = game.flnBasesAvailable > 0 && (baseNoCubeCandidates.nonEmpty || baseCandidates.nonEmpty)
      val guerrillasWithBases  = game.totalOnMap(sp => if (sp.flnBases > 0) sp.totalGuerrillas else 0)
      
      var rallySpaces = Set.empty[String]
      var shiftedFranceTrack = false
      var agitateSpace: Option[String] = None
      var reservedResources = 0
      val maxTotalRallies = (turnState.maxSpaces, game.resources(Fln)) match {
        case (Some(n), _) => n
        case (None, res) if res < 9 => 1000 // No limit
        case (None, res) => res * 2 / 3
      }
      
      def numRallies  = rallySpaces.size + (if (shiftedFranceTrack) 1 else 0)
      def canContinue = numRallies < maxTotalRallies 
      def hasRallied(name: String) = rallySpaces contains name
      def totalAgitateCost(sp: Space) = costToAgitate(sp) + (if (hasRallied(sp.name)) 0 else 1)
      def chooseAgitateTarget(candidates: List[Space]): Option[Space] = candidates find { sp =>
        if (hasRallied(sp.name) || canContinue) {
          val (withExort, ts, sufficientResources) = tryOperations {
            if (game.resources(Fln) < totalAgitateCost(sp))
              tryExtort()
            game.resources(Fln) >= totalAgitateCost(sp)
          }
          if (sufficientResources) {
            showLogEntries(game, withExort)
            turnState = ts
            game = withExort
          }
          sufficientResources
        }
        else
          false
      }
      
      def haveAResource(): Boolean = {
        if (turnState.freeOperation)
          true
        else {
          if (game.resources(Fln) == reservedResources)
            tryExtort()
          game.resources(Fln) > reservedResources
        }
      }
      
      def logRallyChoice(): Unit = {
        if (numRallies == 0) {
          log()
          log(separator())
          log(s"$Fln chooses: Rally")
        }
      }
        
      if (canPlaceBase || ((game.totalOnMap(_.flnBases) * 2) > (guerrillasWithBases + dieRoll/2))) {
        def doRallies(candidates: List[String], rallyType: RallyType, priorities: SpacePriorities, 
                        force: Boolean = false, numRemaining: Int = 1000): Unit = {
          
          if (candidates.nonEmpty && numRemaining > 0 && canContinue) {
            // Note that we fetch the spaces from the current game state each time
            val sp = topPriority(spaces(candidates), priorities)
            rallyType match {
              case PlaceBase if game.flnBasesAvailable == 0 =>
                if (force && haveAResource()) {
                  logRallyChoice()
                  log()
                  log(s"$Fln executes Rally operation to allow agitation: ${sp.name}")
                  if (!turnState.freeOperation)
                    decreaseResources(Fln, 1)
                  rallySpaces += sp.name
                }
              case PlaceBase =>
                if (haveAResource()) {
                  logRallyChoice()
                  log()
                  log(s"$Fln executes Rally operation: ${sp.name}")
                  if (!turnState.freeOperation)
                    decreaseResources(Fln, 1)
                  val numActive = 2 min sp.activeGuerrillas
                  val numHidden = 2 - numActive
                  removeToAvailable(sp.name, Pieces(activeGuerrillas = numActive, hiddenGuerrillas = numHidden), logControl = false)
                  placePieces(sp.name, Pieces(flnBases = 1), logControl = false)
                  logControlChange(sp, game.getSpace(sp.name))
                  rallySpaces += sp.name
                }

              case PlaceGuerrillas =>
                // When placing guerrillas in a space with a base the final number of 
                // guerrillas should not exceed the population of the space + 1.
                // If it does then we will flip the active guerrillas to hidden
                
                val numToPlace = if (sp.flnBases > 0 )
                  (sp.flnBases + sp.population) min ((sp.population + 1) - sp.totalGuerrillas) max 0
                else if (momentumInPlay(MoMoudjahidine))
                  (1 + sp.population) min ((sp.population + 1) - sp.totalGuerrillas) max 0
                else
                  1

                getGuerrillasToPlace(numToPlace, sp) match {
                  case GuerrillasToPlace(0, Nil) if force =>
                    if (haveAResource()) {
                      logRallyChoice()
                      log()
                      log(s"$Fln executes Rally operation to allow agitation: ${sp.name}")
                      if (!turnState.freeOperation)
                        decreaseResources(Fln, 1)
                      rallySpaces += sp.name
                    }
                    
                  case GuerrillasToPlace(0, Nil) =>  // Flip any active
                    if (numToPlace == 0 && sp.activeGuerrillas > 0 && haveAResource()) {
                      logRallyChoice()
                      log()
                      log(s"$Fln executes Rally operation: ${sp.name}")
                      if (!turnState.freeOperation)
                        decreaseResources(Fln, 1)
                      hideActiveGuerrillas(sp.name, sp.activeGuerrillas)
                      rallySpaces += sp.name
                    }
                                    
                  case guerrillas =>  // Place guerrillas from sources
                    if (haveAResource()) {
                      logRallyChoice()
                      log()
                      log(s"$Fln executes Rally operation: ${sp.name}")
                      if (!turnState.freeOperation)
                        decreaseResources(Fln, 1)
                      placeGuerrillas(sp.name, guerrillas)
                      rallySpaces += sp.name
                    }
                }
              case _ => 
            }
            doRallies(candidates filterNot (_ == sp.name), rallyType, priorities, force, numRemaining - 1)
          }
        }
        
        doRallies(baseNoCubeCandidates, PlaceBase, BasePriorities)
        doRallies(baseCandidates, PlaceBase, BasePriorities)
        val unprotectedBaseCandidates = spaceNames(game.spaces filter onlyIn filter (sp => (!hasRallied(sp.name) && unprotectedBase(sp))))
        doRallies(unprotectedBaseCandidates, PlaceGuerrillas, UnprotectedBasePriorities)
        // France track?
        if ((turnState.onlyIn.isEmpty || turnState.onlyIn(FranceTrackName)) &&
            game.franceTrack < FranceTrackMax && canContinue && haveAResource()) {
          logRallyChoice()
          log()
          log(s"$Fln executes Rally operation: France Track")
          if (!turnState.freeOperation)
            decreaseResources(Fln, 1)
          increaseFranceTrack(1)
          shiftedFranceTrack = true
        }
        val supportSectorCandidates = spaceNames(game.spaces filter onlyIn filter (sp => (!hasRallied(sp.name) && sectorsAtSupport(sp))))
        doRallies(supportSectorCandidates, PlaceGuerrillas, SupportSectorPriorities)
        
        // If we have a 2+ population space were we can agitate for effect
        // then attempt to reserve resources to do so and rally in the space if not done already.
        val agitate2popSpaces = game.algerianSpaces filter onlyIn filter (sp => agitate2population(sp, hasRallied(sp.name)))
        val agitate2popCandidates = if (canContinue) agitate2popSpaces else agitate2popSpaces filter (sp => hasRallied(sp.name))
        if (agitate2popCandidates.nonEmpty) {
          val best     = agitate2popCandidates.sortBy(sp => -sp.population).sortBy(totalAgitateCost(_)).head
          val cheapest = agitate2popCandidates.sortBy(totalAgitateCost(_)).head
            
          chooseAgitateTarget(best :: cheapest :: Nil) foreach { sp =>
            if (!hasRallied(sp.name))
              doRallies(sp.name :: Nil, PlaceGuerrillas, Nil, force = true)
            if (hasRallied(sp.name)) {
              agitateSpace      = Some(sp.name)
              reservedResources = costToAgitate(sp)
            }
          }
        }
        
        val highPopSpaceCandidates = spaceNames(game.algerianSpaces filter onlyIn filter (sp => !hasRallied(sp.name) && highestPopCriteria(sp)))
        doRallies(highPopSpaceCandidates, PlaceGuerrillas, HighestPopPriorities, numRemaining = 2)  // 2 max

        val guerrillasNoBaseCandidates = spaceNames(game.spaces filter onlyIn filter (sp => !hasRallied(sp.name) && guerrillasNoBase(sp)))
        doRallies(guerrillasNoBaseCandidates, PlaceGuerrillas, GuerrillasNoBasePriorities, numRemaining = 2)  // 2 max

        // If we have not yet agitated, then pick a space to agitate
        // where we can shift to neutral or oppose rallying there if necessary
        
        val agitateForShiftSpaces = game.algerianSpaces filter onlyIn filter (sp => agitateForShift(sp, hasRallied(sp.name)))
        val agitateForShiftCandidates = if (canContinue) agitateForShiftSpaces else agitateForShiftSpaces filter (sp => hasRallied(sp.name))
        if (agitateSpace.isEmpty && agitateForShiftCandidates.nonEmpty) {
          val best     = agitateForShiftCandidates.sortBy(sp => -sp.population).sortBy(totalAgitateCost(_)).head
          val cheapest = agitateForShiftCandidates.sortBy(totalAgitateCost(_)).head
          chooseAgitateTarget(best :: cheapest :: Nil) foreach { sp =>
            agitateSpace = Some(sp.name)
            if (!hasRallied(sp.name))
              doRallies(sp.name :: Nil, PlaceGuerrillas, Nil, force = true)
            if (hasRallied(sp.name))
              agitateSpace = Some(sp.name)
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
      else {
        turnState = turnState.copy(rallyConsidered = true)
        Left(ConsiderMarch)
      }
    }
  }
  
  // March objectives:
  // 1.  March one underground guerrilla to each base without one
  //     Priority: lowest cost
  // 2.  March one guerrilla to each space at Support if none there
  //     (if capabilityInPlay(CapAmateurBomber) then march to have two guerrillas to ONE city at support)
  //     OK if arriving guerrillas are activated UNLESS in final campaign
  //     Priority: will stay underground, lowest cost
  // 3. In ONE populated sector under Government control and not at oppose march enough
  //    guerrillas to set it to Uncontrolled.
  //    Priority: Mountain, Highest Population, lowest cost
  // 4. March exactly 3 guerrillas (possible from multiple sources) to a non-resettled
  //    Population 0 space that can take a base (and no FLN base there).
  //    Priority: Fewest cubes, Mountain, lowest cost, at least on guerrilla stays underground
  //
  // Notes: 
  //   - if capabilityInPlay(CapDeadZones) #=> No multiple march in same Wilaya
  //   - Never march last underground guerrilla from a base
  //   - Never march last two guerrillas from base (active or underground)
  //   - Never march last guerrilla from a space at support
  //   - Never march enough guerrillas from a populated space to give Government control
  //   - Always march underground before active unless the arriving guerrilla(s) are certain
  //     to become active.  In that case march active first.
  //   - if momentum: MoParanoia, guerrillas cannot cross Wilaya boundaries (International borders OK)
  //   - if momentum: MoPopulationControl, guerrillas marching into city activates guerrillas if 2+ cubes present (regardless of support)
  object ConsiderMarch extends ActionFlowchartNode {
    val desc = "Consider March Operation"

    // A minimum of two spaces (source, dest)
    case class MarchPath(spaces: List[String]) {
      val length = spaces.size
      val isAdjacent = length == 2
      def source = spaces.head
      def dest   = spaces.last
      
      // The maximum number of guerrillas the can march along this path
      // without worring about activation
      def maxGuerrillas: Int = eligibleGuerrillas(game.getSpace(source), false, false).total
      
      // Return the maximum number of hidden guerrillas that could
      // march along this path without activating.
      def maxGuerrillasNoActivate: Int = {
        def checkNumber(hidden: Int): Int = {
          hidden match {
            case n if n > 0 && activates(n) => checkNumber(n - 1)
            case n => n
          }
        }
        checkNumber(eligibleGuerrillas(game.getSpace(source), true, true).hiddenGuerrillas)
      }
      
      def activatedBySpace(numGuerrillas: Int): Option[String] =
        spaces.sliding(2) find { xs => wouldActivate(numGuerrillas, xs.head, xs.last) } map (_.last)
        
      def activates(numGuerrillas: Int): Boolean = activatedBySpace(numGuerrillas).nonEmpty
        
      def sourceHasBase = game.getSpace(source).hasFlnBase

      def cost(paidFor: Set[String]) = (spaces.tail filterNot paidFor).size
      
      // Assumes that num <= the number of guerrillas that are eligible!
      def marchers(num: Int, hiddenOnly: Boolean, hiddenFirst: Boolean): Pieces = {
        val movers = eligibleGuerrillas(game.getSpace(source), hiddenOnly, hiddenFirst)
        if (hiddenOnly) {
          val h = num min movers.hiddenGuerrillas
          Pieces(hiddenGuerrillas = h)
        }
        else if (hiddenFirst) {
          val h = num min movers.hiddenGuerrillas
          val a = (num - h) min movers.activeGuerrillas
          Pieces(hiddenGuerrillas = h, activeGuerrillas = a)
        }
        else {
          val a = num min movers.activeGuerrillas
          val h = (num - a) min movers.hiddenGuerrillas
          Pieces(hiddenGuerrillas = h, activeGuerrillas = a)
        }
      }
    }
    
    // Order by cheapest path. If two paths have the same cost
    // then those with more movable guerrillas come first.
    def cheapestPathOrder(paidFor: Set[String]) = new Ordering[MarchPath] {
      def compare(x: MarchPath, y: MarchPath) = (x.cost(paidFor), y.cost(paidFor)) match {
        case (cx, cy) if cx == cy => y.maxGuerrillas compare x.maxGuerrillas // Reversed: largest comes first
        case (cx, cy) => cx compare cy
      }
    }
    
    // Order by path with the most guerrillas that will arrive hidden. If two paths have the 
    // same numer, then take the one that is cheaper.
    def nonActivatePathOrder(paidFor: Set[String]) = new Ordering[MarchPath] {
      def compare(x: MarchPath, y: MarchPath) = (x.maxGuerrillasNoActivate, y.maxGuerrillasNoActivate) match {
        case (cx, cy) if cx == cy => x.cost(paidFor) compare y.cost(paidFor)
        case (cx, cy) => cy compare cx // Reversed: largest comes first
      }
    }
    
    
    // A source space that has at least one guerrillas that can leave and
    // has one or more valid possible paths to reach the destination
    case class March(sourceName: String, paths: List[MarchPath]) {
      def canMarchHidden = paths exists (_.maxGuerrillasNoActivate > 0)
      def cheapest(paidFor: Set[String]) = {
        val bestPath = paths.sorted(cheapestPathOrder(paidFor)).head
        March(sourceName, List(bestPath))
      }
      def bestNonActivatePath(paidFor: Set[String]) = {
        val bestPath = paths.sorted(nonActivatePathOrder(paidFor)).head
        March(sourceName, List(bestPath))
      }
    } 
      
    // A group of marches that all reach the same destination
    case class MarchDest(destName: String, marches: List[March])
    
    case class ResolvedMarch(path: MarchPath, guerrillas: Pieces)
    case class ResolvedDest(destName: String, marches: List[ResolvedMarch]) {
      val guerrillas = marches.foldLeft(Pieces()) { (all, m) => all + m.guerrillas }
      val anyHidden  = guerrillas.hiddenGuerrillas > 0
      def cost(alreadyPaid: Set[String]) = {
        def getCost(remaining: List[ResolvedMarch], paidFor: Set[String]): Int = remaining match {
          case Nil => 0
          case x :: xs =>
            val marchSpaces = x.path.spaces.tail.toSet
            x.path.cost(paidFor) + getCost(xs, paidFor ++ marchSpaces)
        }
        getCost(marches, alreadyPaid)
      }
      def marchSpaces = (marches flatMap (_.path.spaces.tail)).toSet
    }
    
    case class ExecutedMarch(
      dest:         Space,
      cost:         Int,
      anyHidden:    Boolean,
      marchSpaces:  Set[String],
      gameState:    GameState,
      turnState:    TurnState) {
        override def toString() = dest.toString
      }

    type MarchPriorities = List[Filter[ExecutedMarch]]
    
    val LowestCost        = new LowestPriority[ExecutedMarch]("Lowest cost",                    _.cost)
    val ArrivesUndergroud = new BooleanPriority[ExecutedMarch]("Guerrillas arrive underground", _.anyHidden)
    val MountainDest      = new BooleanPriority[ExecutedMarch]("Mountain Destination",          _.dest.isMountains)
    val HighestPopulation = new HighestPriority[ExecutedMarch]("Highest population",            _.dest.population)
    val FewestGovCubes    = new LowestPriority[ExecutedMarch]("Fewest Gov cubes",               _.dest.totalCubes)
    
    sealed trait MarchType {
      def desc: String
      def maxTargets: Int = 1000      // Default is no limit
      def hiddenOnly: Boolean         // Only hidden guerrillas wanted in destination
      def preferHidden: Boolean       // Prefer hidden if possible and cost is equal another path with non-hidden
      def numNeeded(sp: Space): Int   // Number of guerillas need in destination
      def candidates: List[MarchDest] // Return all destinations that match this march type
      def priorities: MarchPriorities // List of priorities to choose among destinations
    }
    
    // FLN bases without an underground guerilla
    case object ProtectExposedBase extends MarchType {
      override def desc = "March a hidden guerrilla to an exposed base"
      
      override def hiddenOnly = true
      override def preferHidden = true
      override def numNeeded(sp: Space): Int = 1
      override def candidates = {
        val hasExposedBase = (sp: Space) => sp.flnBases > 0 && sp.hiddenGuerrillas == 0
        game.algerianSpaces filter hasExposedBase flatMap (sp => findMarches(sp.name, true))
      }
      override def priorities: MarchPriorities = List(LowestCost)
    }
    

    // If the Amateur Bomber event is in effect march to have two guerrillas in one city at support
    // Hidden only if this is the final campaign
    case object TwoGuerrillasAtSupportCity extends MarchType {
      override def desc = if (game.isFinalCampaign)
        "March to have two hidden guerrillas at a support city"
      else
        "March to have two guerrillas at a support city"
      
      override def maxTargets: Int = 1
      override def hiddenOnly = game.isFinalCampaign
      override def preferHidden = true
      override def numNeeded(sp: Space): Int = if (hiddenOnly)
        (2 - sp.pieces.hiddenGuerrillas) max 0
      else
        (2 - sp.pieces.totalGuerrillas) max 0
      
      override def candidates = if (capabilityInPlay(CapAmateurBomber)) {
        val supportCity = (sp: Space) => sp.isCity && sp.isSupport && 
                                         ((hiddenOnly && sp.hiddenGuerrillas < 2) || 
                                          (!hiddenOnly && sp.totalGuerrillas < 2))
        game.algerianSpaces filter supportCity flatMap (sp => findMarches(sp.name, hiddenOnly))
      }
      else
        Nil
      override def priorities: MarchPriorities = List(ArrivesUndergroud, LowestCost)
    }
    
    // March to have a guerrilla in each support space that does not have one.
    // Hidden only if this is the final campaign
    case object MarchGuerrillaToSupportSpace extends MarchType {
      override def desc = if (game.isFinalCampaign)
        "March a hidden guerrilla to a support space"
      else
        "March a guerrilla to a support space"
      
      override def hiddenOnly = game.isFinalCampaign
      override def preferHidden = true
      
      override def numNeeded(sp: Space): Int = if (hiddenOnly)
        (1 - sp.pieces.hiddenGuerrillas) max 0
      else
        (1 - sp.pieces.totalGuerrillas) max 0
      
      override def candidates = {
        val supportDest = (sp: Space) => sp.isSupport &&
                                         ((hiddenOnly && sp.hiddenGuerrillas == 0) || 
                                          (!hiddenOnly && sp.totalGuerrillas == 0))
        game.algerianSpaces filter supportDest flatMap (sp => findMarches(sp.name, hiddenOnly))
      }
      override def priorities: MarchPriorities = List(ArrivesUndergroud, LowestCost)
    }
    
    // In one government controlled sector that is not at Oppose
    // march enough guerrillas to make the space uncontrolled
    case object ToRemoveGovControl extends MarchType {
      override def desc = "March guerillas to remove government control"
      override def maxTargets: Int = 1
      override def hiddenOnly = false
      override def preferHidden = false
      override def numNeeded(sp: Space): Int = (sp.totalGov - sp.totalFln) max 0
      override def candidates = {
        val canRemoveGovControl = (sp: Space) => sp.isSector && sp.isGovControlled && sp.population > 0 && !sp.isOppose
        game.algerianSpaces filter canRemoveGovControl flatMap (sp => findMarches(sp.name, hiddenOnly))
      }
      override def priorities: MarchPriorities = List(MountainDest, HighestPopulation, LowestCost)
    }
    
    // In one non-resettled space with zero population and room for a base
    // March exactly 3 guerrillas
    case object MarchThreeToZeroPop extends MarchType {
      override def desc = "March exactly three guerillas to a zero population sector"
      override def maxTargets: Int = 1
      override def numNeeded(sp: Space): Int = 3  // Exactly 3
      override def hiddenOnly = false
      override def preferHidden = false
      override def candidates = {
        val popZeroSpace = (sp: Space) => sp.isSector && !sp.isResettled && sp.population == 0 &&
                                          sp.flnBases == 0 && sp.canTakeBase
        game.algerianSpaces filter popZeroSpace flatMap (sp => findMarches(sp.name, hiddenOnly))
      }
      override def priorities: MarchPriorities = List(FewestGovCubes, MountainDest, LowestCost, ArrivesUndergroud)
    }
    
    // Marches will be tried in this order
    val AllMarchTypes = List(
      ProtectExposedBase,
      TwoGuerrillasAtSupportCity,
      MarchGuerrillaToSupportSpace,
      ToRemoveGovControl,
      MarchThreeToZeroPop)
    
    def execute(): Either[ActionFlowchartNode, Action] = if (turnState.marchConsidered)
      Left(BotPasses)
    else {
      def ALL = 1000  // Unlimited march targets
      var marchDestinations = Set.empty[String]
      val maxTotalDestinations = (turnState.maxSpaces, game.resources(Fln)) match {
        case (Some(n), _) => n
        case (None, res) if res < 9 => 1000 // No limit
        case (None, res) => res * 2 / 3
      }
            
      // Try to execute each
      def doMarches(marchType: MarchType, numTargets: Int, candidates: List[MarchDest]): Unit = {
        def executeResolvedDest(resolved: ResolvedDest): Option[ExecutedMarch] = {
          val (newGame, newState, (success, cost, anyHidden)) = tryOperations {
            if ((marchDestinations ++ resolved.marchSpaces).size <= maxTotalDestinations) {
              val cost = resolved.cost(marchDestinations)
              if (marchDestinations.isEmpty) {
                log()
                log(separator())
                log(s"$Fln chooses: March")
              }
            
              if (game.resources(Fln) < cost && !turnState.freeOperation) {
                // Protect hidden guerrillas from exort if necessary
                val protectedGuerrillas = if (marchType.hiddenOnly)
                  resolved.marches map (m => (m.path.source, m.guerrillas.hiddenGuerrillas))
                else
                  Nil
                tryExtort(protectedGuerrillas)
              }
            
              if (game.resources(Fln) >= cost || turnState.freeOperation) {
                val newSpaces = (resolved.marchSpaces -- marchDestinations).toList.sorted
                if (newSpaces.nonEmpty) {
                  log()
                  if (newSpaces.size > 1)
                    wrap(s"$Fln selects March spaces: ", newSpaces) foreach (log(_))
                  else
                    log(s"$Fln selects March space: ${newSpaces.toList.head}")
                }
            
                if (!turnState.freeOperation)
                  decreaseResources(Fln, cost)
              
                var hiddenArrived = false
                // Recalcuate the marcher on each path because some of them may have
                // been activated by a previous extort
                for {
                  march <- resolved.marches 
                  path   = march.path
                  num    = march.guerrillas.total
                } {
                  val activates  = path.activates(num)
                  val guerrillas = path.marchers(num, marchType.hiddenOnly, !(path.sourceHasBase || activates))
                  log()
                  if (path.isAdjacent)
                    log(s"$Fln marches adjacent from ${path.source} to ${path.dest}")
                  else {
                    log(s"$Fln marches from ${path.source} to ${path.dest} via: ${andList(path.spaces.tail.init)}")
                    if (guerrillas.hiddenGuerrillas > 0 && activates) {
                      val activateSpace = path.activatedBySpace(num).get
                      log(s"Entering $activateSpace activates the hidden guerrillas")
                    }
                  }
                  hiddenArrived = hiddenArrived | (guerrillas.hiddenGuerrillas > 0 && !activates)
                  val beforePieces = game.getSpace(path.dest).pieces
                  movePieces(guerrillas, path.source, path.dest, activates)
                  turnState = turnState.addMovingGroup(path.dest, game.getSpace(path.dest).pieces - beforePieces)
                }
                (true, cost, hiddenArrived)
              } 
              else
                (false, 0, false) // Failed due to lack of resources
            }
            else
              (false, 0, false) // Failed, too many destinations
          }
          
          if (success)
            Some(ExecutedMarch(game.getSpace(resolved.destName), cost, anyHidden, resolved.marchSpaces, newGame, newState))
          else
            None
        }
    
        // Find the best group of one or more marches from different source spaces
        // that fulfill the given march type.  Return None if the march cannot be fulfilled.
        def resolveDest(marchDest: MarchDest, totalNeeded: Int, hiddenOnly: Boolean, preferHidden: Boolean, paidFor: Set[String]): Option[ResolvedDest] = {
          // Iterate through the marches until we get the number of needed guerrillas
          def nextMarch(sortedMarches: List[March], marchesSoFar: List[ResolvedMarch], numMarched: Int, alreadyPaid: Set[String]): List[ResolvedMarch] = {
            if      (numMarched == totalNeeded) marchesSoFar
            else if (sortedMarches.isEmpty) Nil
            else {
              val numNeeded    = totalNeeded - numMarched
              val march        = sortedMarches.head
              val path         = march.paths.head
              val numThisMarch = (if (hiddenOnly) path.maxGuerrillasNoActivate else path.maxGuerrillas) min numNeeded
              val guerrillas   = path.marchers(numThisMarch, hiddenOnly, !(path.sourceHasBase || path.activates(numThisMarch)))
              val resolved     = ResolvedMarch(path, guerrillas)
              nextMarch(sortedMarches.tail, marchesSoFar :+ resolved, numMarched + numThisMarch, alreadyPaid ++ path.spaces.tail.toSet)
            }
          }

          def build(optimized: List[March]): Option[ResolvedDest] = {
            nextMarch(optimized, Nil, 0, paidFor) match {
              case Nil             => None
              case resolvedMarches => Some(ResolvedDest(marchDest.destName, resolvedMarches))
            }
          }
          
          val marches = marchDest.marches
          // Get the best path in each of our marches
          if (hiddenOnly) {
            val optimized = (marches filter (_.canMarchHidden) map (_.bestNonActivatePath(paidFor))).sortBy(_.paths.head)(cheapestPathOrder(paidFor))
            build(optimized)
          }
          else if (preferHidden) {
            // If we find a path that delivers at least one hidden guerrilla, then use it.
            // Otherwise accept one that delivers only active guerrillas.
            val optimizedHidden = (marches map (_.bestNonActivatePath(paidFor))).sortBy(_.paths.head)(nonActivatePathOrder(paidFor))
            val optimized = (marches map (_.cheapest(paidFor))).sortBy(_.paths.head)(cheapestPathOrder(paidFor))
            build(optimizedHidden) orElse build(optimized)
          }
          else {
            // We consider paths that will allow a hidden guerrilla to arrive
            // But if if is cheaper to have any activated guerillas arrive, then that will win
            val optimizedHidden = (marches map (_.bestNonActivatePath(paidFor))).sortBy(_.paths.head)(nonActivatePathOrder(paidFor))
            val optimized = (marches map (_.cheapest(paidFor))).sortBy(_.paths.head)(cheapestPathOrder(paidFor))
            (build(optimizedHidden), build(optimized)) match {
              case (Some(withHidden), Some(regular)) if withHidden.cost(paidFor) <= regular.cost(paidFor) => Some(withHidden)
              case (withHidden, regular) => regular orElse withHidden
            }
          }
        }
    
        // Try to execute each canidiate to see if twe have sufficent resources to carry it out.
        // Filter out those that cannot be affored, then find the top priority among those
        // that remain.
        
        val topExecDest = if (numTargets == marchType.maxTargets)
          None
        else if (!marchType.hiddenOnly && marchType.preferHidden) {
          // Try first with hidden guerrillas as a priority, then if that fails
          // with the cheapest route as a priority
          val hiddenFirst = for {
            marchDest  <- candidates
            totalNeeded = marchType.numNeeded(game.getSpace(marchDest.destName))
            resolved   <- resolveDest(marchDest, totalNeeded, false, true, marchDestinations);
            executed   <- executeResolvedDest(resolved)
          } yield executed
          
          if (hiddenFirst.nonEmpty)
            Some(topPriority(hiddenFirst, marchType.priorities))
          else {
            val activeFirst = for {
              marchDest  <- candidates
              totalNeeded = marchType.numNeeded(game.getSpace(marchDest.destName))
              resolved   <- resolveDest(marchDest, totalNeeded, false, false, marchDestinations);
              executed   <- executeResolvedDest(resolved)
            } yield executed
            if (activeFirst.nonEmpty) Some(topPriority(activeFirst, marchType.priorities)) else None
          }
        }
        else {
          val other = for {
            marchDest  <- candidates
            totalNeeded = marchType.numNeeded(game.getSpace(marchDest.destName))
            resolved   <- resolveDest(marchDest, totalNeeded, marchType.hiddenOnly, marchType.preferHidden, marchDestinations)
            executed   <- executeResolvedDest(resolved)
          } yield executed
          
          if (other.nonEmpty) Some(topPriority(other, marchType.priorities)) else None
        }
        
        if (topExecDest.nonEmpty) {
          val exec = topExecDest.get
          showLogEntries(game, exec.gameState)
          turnState         = exec.turnState
          game              = exec.gameState
          marchDestinations = marchDestinations ++ exec.marchSpaces
          
          doMarches(marchType, numTargets + 1, candidates filterNot (_.destName == exec.dest.name))
        }
      }
      
      for (marchType <- AllMarchTypes)
        doMarches(marchType, 0, marchType.candidates)
      
      if (marchDestinations.size > 0) {
        trySubvert()
        tryExtort()
        Right(effectiveAction(marchDestinations.size))
      }
      else {
        turnState = turnState.copy(marchConsidered = true)
        Left(ConsiderRally)
      }
    }
    
    def wouldActivate(numGuerrillas: Int, fromName: String, toName: String): Boolean = {
      val (from, to) = (game.getSpace(fromName), game.getSpace(toName))
      // If Population Control in effect, moving into a city with 3+ cubes activates
      // regarless of the size of the moving party.
      if (momentumInPlay(MoPopulationControl) && to.isCity && to.totalCubes > 2)
        true
      // If crossing an international border then the Border Zone track
      // is added to the number of cubes in the destination
      else if (from.isCountry != to.isCountry)
        numGuerrillas + to.totalCubes + game.borderZoneTrack > 3
      else if (to.isSupport)
        numGuerrillas + to.totalCubes > 3
      else
        false
    }
    
    // Return the guerrillas that are eligible to march out of the space.
    def eligibleGuerrillas(sp: Space, hiddenOnly: Boolean, hiddenFirst: Boolean): Pieces = {
      val allowed = if (hiddenOnly) List(HiddenGuerrillas) else List(ActiveGuerrillas, HiddenGuerrillas)
      var staying = turnState.movingGroups(sp.name)
      var moving  = sp.pieces.only(allowed) - staying
      
      def leaveBehind(num: Int): Unit = {
        val leftBehind = if (hiddenOnly || hiddenFirst) {
          val a = num min moving.activeGuerrillas
          val h = (num - a) min moving.hiddenGuerrillas
          Pieces(hiddenGuerrillas = h, activeGuerrillas = a)
        }
        else {
          val h = num min moving.hiddenGuerrillas
          val a = (num - h) min moving.activeGuerrillas
          Pieces(hiddenGuerrillas = h, activeGuerrillas = a)
        }
        staying = staying + leftBehind
        moving  = moving  - leftBehind
      }
      
      // Leave 1 hidden guerrilla at base if we have one
      if (sp.flnBases > 0 && !sp.isCountry && staying.hiddenGuerrillas == 0 && moving.hiddenGuerrillas > 0)
        leaveBehind(1)
      
      // Leave at least two guerrillas at base
      if (sp.flnBases > 0 && !sp.isCountry && staying.totalGuerrillas < 2)
        leaveBehind(2 - staying.totalGuerrillas)
      
      // Do not march last guerrila from support space
      if (sp.isSupport && staying.totalGuerrillas == 0)
        leaveBehind(1)
      
      // Do not trigger Gov Control
      if (!sp.isCountry && !sp.isGovControlled && sp.totalGov > staying.total)
        leaveBehind(sp.totalGov - staying.total)
      
      moving
    }
    
    // Returns a list of March objects that can reach the destination.
    // If hiddenOnly is true, only returns marchers that would arrive hidden at the destination.
    def findMarches(destName: String, hiddenOnly: Boolean): Option[MarchDest] = {
      val dest = game.getSpace(destName)
      
      def hasGuerrillas(sp: Space) = if (hiddenOnly) sp.hiddenGuerrillas > 0 else sp.totalGuerrillas > 0
      def canCross = (sp: Space) => !momentumInPlay(MoParanoia) || 
                                    (sp.isCountry || dest.isCountry || sp.wilaya == dest.wilaya)
                                    
      // Get a list of adjacent spaces with guerrillas that can leave the space
      // Ensure that they are not crossing a Wilaya boundary if Paranoia event is in effect.
      // Finally if we are only interested in hidden guerrillas, then filter out any
      // that would activate the guerrillas
      val adjacent = for {
        sp <-  spaces(getAdjacent(destName))
        num =  eligibleGuerrillas(sp, hiddenOnly, true).total
        if     num > 0 && canCross(sp)
        path = MarchPath(List(sp.name, destName)) // Adjacents have only 1 path
        if    !(hiddenOnly && path.activates(num))
      } yield March(sp.name, List(path))
      
      // Get a list of other spaces in the Wilaya with guerillas that can leave.
      // There may be more than one possible path of travel.  Remove any that would
      // activate guerrillas if we are only interested in hidden guerrillas.
      // Later we will pick the least costly among the multiple paths.
      val inWilaya = if (turnState.limOpOnly || capabilityInPlay(CapDeadZones))
        Nil  // Cannot make multiple marches to reach destination
      else
        for {
          sp <-   game.wilayaSpaces(dest.wilaya)
          if      sp.name != destName && !areAdjacent(sp.name, destName)
          num =   eligibleGuerrillas(sp, hiddenOnly, true).total
          if      num > 0
          paths = allPathsBetween(sp, dest) filter (path => !(hiddenOnly && path.activates(num)))
          if      paths.nonEmpty
        } yield March(sp.name, paths)

      if (adjacent.isEmpty && inWilaya.isEmpty)
        None
      else
        Some(MarchDest(destName, adjacent ::: inWilaya))
    }
    
    // Return all paths from source to dest using spaces within the same Wilaya
    def allPathsBetween(src: Space, dest: Space): List[MarchPath] = {
      assert(src.wilaya == dest.wilaya, s"allPathsBetween(${src.name}, ${dest.name}): different wilayas!")
      val inWilaya = spaceNames(game.wilayaSpaces(dest.wilaya)).toSet
      
      def pathsFrom(name: String, visited: Set[String], upstream: MarchPath): List[MarchPath] = {
        val newPath = MarchPath(upstream.spaces :+ name)
        val adjacents: List[String] = (getAdjacent(name) & inWilaya &~ visited).toList
        if (name == dest.name)      List(newPath)  // Found the destination
        else if (adjacents.isEmpty) Nil            // This is a dead end path
        else                        adjacents flatMap (a => pathsFrom(a, visited + name, newPath))
      }
      pathsFrom(src.name, Set.empty, MarchPath(Nil))
    }
  }


  
  def act(): Unit = {
    turnState = TurnState(
      specialActivityAllowed = botCanDo(ExecOpPlusActivity),
      maxSpaces              = if (botCanDo(ExecOpPlusActivity) || botCanDo(ExecOpOnly)) None else Some(1))
    
    @tailrec def evaluateNode(node: ActionFlowchartNode): Action = {
      botDebug(s"Bot Flowchart: $node")
      node.execute() match {
        case Left(nextNode) => evaluateNode(nextNode)
        case Right(action)  => action
      }
    }
    
    val action = evaluateNode(LimOpAndZeroResources)
    if (game.sequence.numActed == 0)
      log(s"\nPlace the ${Fln} eligibility cylinder in the ${action} box")
    game = game.copy(sequence = game.sequence.nextAction(action))
  }
  
   // Spsend up to 2/3 of resources
   // If Resources are insufficient for Agitation everywhere it is possible, Agitate first for the
   // - greatest shift
   // - then where Support
   // - then where fewest Terror markers
   // - then randomly. 
   // Remove Terror only if a shift would be achieved.
   
  def propAgitatePhase(): Unit = {
    var agitatedOnce = false
    def nextAgitate(resRemaining: Int, agitated: Set[String]): Unit = {
      // Must have enough resource to remove all terror and affect at least 1 shift
      val canAgitate = (sp: Space) => sp.population > 0  &&
                                     !sp.isOppose        &&
                                     sp.totalFln > 0     && 
                                     !sp.isGovControlled &&
                                     resRemaining > sp.terror
      val maxShift = (sp: Space) => (if (sp.isSupport) 2 else 1) min (resRemaining - sp.terror)
      val priorities = List(
        new HighestPriority[Space]("Greatest shift", maxShift),
        new BooleanPriority[Space]("Suppport space", _.isSupport),
        new LowestPriority[Space]("Fewest terror",   _.terror))
      
      // Re-evaluate candidates each time because FLN resources will have changed
      val candidates = (game.algerianSpaces filter (sp => !agitated(sp.name) && canAgitate(sp)))
      if (candidates.nonEmpty) {
        agitatedOnce = true
        val sp = topPriority(candidates, priorities)
        log(s"\nFLN agitates: ${sp.name}")
        log(separator())
        val cost = sp.terror + maxShift(sp)
        decreaseResources(Fln, cost)
        removeTerror(sp.name, sp.terror)
        decreaseSupport(sp.name, maxShift(sp))
        nextAgitate(resRemaining - cost, agitated + sp.name)
      }
    }
    log("\nFLN agitation")
    nextAgitate(game.resources(Fln) * 2 / 3, Set.empty)
    if (!agitatedOnce)
      log("FLN does not agitate any spaces")
  }
  
  // The FLN player may now move any Guerrillas from any space or spaces within a given Wilaya,
  // to any other space or spaces within the same Wilaya with a friendly Base.
  //
  // If the "Cross Wilaya Coordination" capability is in effect, then guerrillas can
  // redeploy to any space in Algeria that has a base.
  //
  // The Bot will redeploy to attempt to get at least Pop+1 guerrillas at each base,
  // no more than 3 guerrillas at each base.  If this is no possible, then spread 
  // the guerrillas as evenly as possible among the base spaces.
  // IMPORTANT: Never redeploy the last guerrilla out of a City or a space at Support!
  def propRedeployPhase(): Unit = {
    
    val numNeeded = (sp: Space) =>
      if (sp.flnBases > 0) (sp.population + 1) - sp.totalGuerrillas max 0
      else 0
    val canMove = (sp: Space) =>
      if (sp.flnBases > 0)                (sp.totalGuerrillas - (sp.population + 1)) max 0
      else if (sp.isCity || sp.isSupport) (sp.totalGuerrillas - 1) max 0
      else                                sp.totalGuerrillas
    
    case class Source(sp: Space, canMove: Int)
    case class Dest(sp: Space, numNeeded: Int)
    
    def getSources(spaces: List[Space]) = (spaces map (sp => Source(sp, canMove(sp)))
                                                  filter (_.canMove > 0))
    def getDests(spaces: List[Space]) = (spaces map (sp => Dest(sp, numNeeded(sp)))
                                                filter (_.numNeeded > 0))

    // First to protect bases with no guerrillas
    // Then were the most are needed
    val destPriorities = List(
      new BooleanPriority[Dest]("None in space", d => d.numNeeded == d.sp.population + 1),
      new HighestPriority[Dest]("Most needed",   _.numNeeded),
      new HighestPriority[Dest]("Most cubes",    _.sp.totalCubes),
      new BooleanPriority[Dest]("Support space", _.sp.isSupport))
      
    val srcPriorities = List(
      new BooleanPriority[Source]("Not city/support",       d => !(d.sp.isCity || d.sp.isSupport)),
      new BooleanPriority[Source]("No trigger Gov Support", d => d.sp.population > 0 && d.sp.totalGov != d.sp.totalFln),
      new LowestPriority[Source]("Least movers",            _.canMove),
      new LowestPriority[Source]("Least cubes",             _.sp.totalCubes))
      
    var redeployed = false
    val EmptyRelos: Map[String, Map[String, Int]] = Map.empty.withDefaultValue(Map.empty.withDefaultValue(0))
    def redeployGuerrillas(sources: List[Source], dests: List[Dest], 
                          redeployments: Map[String, Map[String, Int]] = EmptyRelos): Unit = {

      if (sources.isEmpty || dests.isEmpty) {
        // Process all of the accumulated redeployments
        for {
          (destName, movers) <- redeployments.toList.sortBy(_._1)
          (srcName, num)     <- movers.toList.sortBy(_._1)
        } {
          val sp = game.getSpace(srcName)
          val a = num min sp.activeGuerrillas
          val h = (num - a) min sp.hiddenGuerrillas
          redeployPieces(Pieces(hiddenGuerrillas = h, activeGuerrillas = a), srcName, destName)
          redeployed = true
        }
      }
      else {
        val dest = topPriority(dests, destPriorities)
        val src  = topPriority(sources, srcPriorities)
        val destName = dest.sp.name
        val srcName  = src.sp.name
        val newDests = (dests map (d => if (d.sp.name == destName) d.copy(numNeeded = d.numNeeded - 1) else d)
                             filter (_.numNeeded > 0))
        val newSrcs  = (sources map (s => if (s.sp.name == srcName) s.copy(canMove = s.canMove - 1) else s)
                             filter (_.canMove > 0))
        val destMap          = redeployments(destName)
        val newDestMap       = destMap + (srcName -> (destMap(srcName) + 1))
        val newRedeployments = redeployments + (destName -> newDestMap)
        redeployGuerrillas(newSrcs, newDests, newRedeployments)
      }
    }
    
    log("\nFLN guerrilla redeploymnet")
    log(separator())
    
    if (capabilityInPlay(CapXWilayaCoord)) {
      log(s"Guerrillas can redeploy across Wilaya borders: $CapXWilayaCoord")
      redeployGuerrillas(getSources(game.algerianSpaces), getDests(game.algerianSpaces))
    }
    else
      for (wilaya <- ALL_WILAYAS)
        redeployGuerrillas(getSources(game.wilayaSpaces(wilaya)), getDests(game.wilayaSpaces(wilaya)))
    if (!redeployed)
      log("FLN does not redeploy any guerrillas")
  }
}
  