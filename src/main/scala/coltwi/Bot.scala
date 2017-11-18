
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
    (sp.pieces.flnBases == 0 && sp.pieces.hiddenGuerrillas > 0) ||
    (sp.pieces.flnBases > 0  && sp.pieces.hiddenGuerrillas > 1)

  def executeQuietly[T](code: => T): (GameState, T) = {
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
  
  // Find the valid candidate spaces using the given list of space filters.
  //
  // Each filter is first used against the given spaces and if the filter does not find any matching
  // candidates, the next filter is given a chance.
  // As soon as a filter finds at least one matching country, then the procees stops and the
  // results from that filter are returned.
  // If none of the filters finds at least one matching country we return Nil.
  @tailrec final def selectCandidates(spaces: List[Space], filters: List[SpaceFilter]): List[Space] = {
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
  def topPriority(spaces: List[Space], priorities: List[SpaceFilter]): Space = {
    assert(spaces.nonEmpty, "topPriority: called with empty list of spaces!")
    botDebug(s"topPriority: [${(spaces map (_.name)) mkString ", "}]")
    @tailrec def nextPriority(spaces: List[Space], priorities: List[SpaceFilter]): Space = {
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
    hasSafeHiddenGuerrilla(sp) && 
      (game.hasSpace(sp => sp.isSupport) ||
      (game.isFinalCampaign && game.hasSpace(sp => sp.isNeutral && sp.terror == 0 && sp.canTrain)))
  }  
  
  case class SubvertCmd(replace: Boolean, name: String, pieces: Pieces)
  
  // - Remove last 2 cubes in one space (police top priority)
  // - Remove last cube in two spaces (police top priority)
  // - Replace police with guerrilla in one space
  // - Replace last troop with guerrilla in one space

  def subvertCommands: List[SubvertCmd] = {
    val ALGERIAN = List(AlgerianPolice, AlgerianTroops)
    val hasG = (sp: Space) => sp.pieces.hiddenGuerrillas > 0
    val last2Cubes = game.algerianSpaces filter (sp => hasG(sp) && sp.pieces.algerianCubes == 2 && sp.pieces.frenchCubes == 0)
    val lastCube   = game.algerianSpaces filter (sp => hasG(sp) && sp.pieces.algerianCubes == 1 && sp.pieces.frenchCubes == 0)
    val withPolice = game.algerianSpaces filter (sp => hasG(sp) && sp.pieces.algerianPolice > 0)
    val has2Police = CriteriaFilter("2 Police cubes", _.pieces.algerianPolice == 1)
    val hasPolice  = CriteriaFilter("Police cube", _.pieces.algerianPolice > 0)
    val generic    = game.algerianSpaces filter (sp => hasG(sp) && sp.pieces.algerianCubes > 0)
    def bestPiece(pieces: Pieces): Pieces = 
      if (pieces.algerianPolice > 0) Pieces(algerianPolice = 1)
      else Pieces(algerianTroops = 1)
    if (last2Cubes.nonEmpty) {
      val target = topPriority(last2Cubes, List(has2Police, hasPolice))
      List(SubvertCmd(false, target.name, target.pieces.only(ALGERIAN)))
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
      sp.isFlnControlled && (
        (sp.pieces.flnBases > 0 && sp.pieces.totalCubes > 0 && sp.pieces.hiddenGuerrillas > 2) ||
        ((sp.pieces.flnBases == 0 || sp.pieces.totalCubes == 0) && sp.pieces.hiddenGuerrillas > 1)
      ) 
    }) ::: (game.countrySpaces filter hasSafeHiddenGuerrilla)
    
    val primaryNames = (primary map (_.name)).toSet
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
      val basesCovered = (game.algerianSpaces filter (_.pieces.flnBases > 0)) forall { sp => 
        (sp.population == 0 && sp.pieces.hiddenGuerrillas > 0) ||
        (sp.population > 0  && sp.pieces.hiddenGuerrillas > 1)
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
            eventState.totalOnMap(_.pieces.flnBases) > game.totalOnMap(_.pieces.flnBases) ||
            eventState.resources(Fln) > game.resources(Fln))
        )
      }
      
      val eventResult = (if (botCanDo(Event)) card.botEventSelection() else NoEvent) match {
        case NoEvent  => None
        case selected => Some(executeQuietly { executeEvent(selected) })
      }
      val terrorResult = if (terrorCandidates.nonEmpty) Some(executeQuietly { doTerror() }) else None
      
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

  object ConsiderRally extends ActionFlowchartNode {
    val desc = "Consider Rally Operation"
    def execute: Either[ActionFlowchartNode, Action] = Right(Pass)
  }

  object ConsiderAttack extends ActionFlowchartNode {
    val desc = "Consider Attack Operation"
    def execute: Either[ActionFlowchartNode, Action] = Right(Pass)
  }


  
  def act(): Unit = {
    specialActivityTaken = false
    
    @tailrec def evaluateNode(node: ActionFlowchartNode): Action = {
      botDebug(s"Bot Flowchart: $node")
      node.execute match {
        case Left(nextNode) => evaluateNode(nextNode)
        case Right(action)  =>  action
      }
    }
    
    val action = evaluateNode(LimOpAndZeroResources)
    log(s"\nPlace the ${Fln} eligibility cylinder in the ${action} box")
    game = game.copy(sequence = game.sequence.nextAction(action))
  }
  
  val TerrorPriorities = List(
    new CriteriaFilter("Remove support", sp => sp.isSupport),
    new CriteriaFilter("Add terror to neutral in final campaign", sp => sp.isNeutral && game.terrorMarkersAvailable > 0),
    new HighestScorePriority("Highest population", sp => sp.population))
  
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
  