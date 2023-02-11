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
import FUtil.Pathname
import ColonialTwilight.{ GameState, GameParameters, SequenceOfPlay, Space, Pieces, Role, Action,
                          SpaceType, Terrain, SupportValue, Resources }

object SavedGame {
  def save(filepath: Pathname, gameState: GameState): Unit = {
    try {
      filepath.dirname.mkpath() // Make sure that the game directory exists
      filepath.writeFile(toGameJson(gameState))
    }
    catch {
      case e: IOException =>
        val suffix = if (e.getMessage == null) "" else s": ${e.getMessage}"
        println(s"IO Error writing saved game ($filepath)$suffix")
      case e: Throwable =>
        val suffix = if (e.getMessage == null) "" else s": ${e.getMessage}"
        println(s"Error writing saved game ($filepath)$suffix")
    }
  }
  
  // The path should be the full path to the file to load.
  // Will set the game global variable
  def load(filepath: Pathname): GameState = {
    try fromGameJson(filepath.readFile())
    catch {
      case e: IOException =>
        val suffix = if (e.getMessage == null) "" else s": ${e.getMessage}"
        println(s"IO Error reading saved game ($filepath)$suffix")
        sys.exit(1)
      case e: Throwable =>
        val suffix = if (e.getMessage == null) "" else s": ${e.getMessage}"
        println(s"Error reading saved game ($filepath)$suffix")
        sys.exit(1)
    }
  }
    
  private def asString(x: Any): String = x.toString
  private def asBoolean(x: Any): Boolean = x match {
    case b: Boolean => b
    case _          => throw new Exception(s"Not a valid Boolean value: $x")
  }  
  private def asInt(x: Any): Int = x match {
    case i: Int => i
    case _      => throw new Exception(s"Not a valid Boolean value: $x")
  }  
  private def asMap(x: Any): Map[String, Any] = x match {
    case m: Map[_, _] => m.asInstanceOf[Map[String, Any]]
    case _      => throw new Exception(s"Not a valid Map value!")
  }  
  private def asList(x: Any): List[Any] = x match {
    case l: List[_] => l.asInstanceOf[List[Any]]
    case _      => throw new Exception(s"Not a valid List value!")
  }  
  
  private def gameParametersToMap(params: GameParameters): Map[String, Any] =
    Map(
      "scenarioName"     -> params.scenarioName,
      "finalPropSupport" -> params.finalPropSupport,
      "botDebug"         -> params.botDebug
    )
  
  private def gameParametersFromMap(data: Map[String, Any]): GameParameters = {
    GameParameters(
      asString(data("scenarioName")),
      asBoolean(data("finalPropSupport")),
      asBoolean(data("botDebug"))
    )
  }
  
  private def sequenceOfPlayToMap(seq: SequenceOfPlay): Map[String, Any] =
    Map(
      "firstEligible"  -> seq.firstEligible,
      "secondEligible" -> seq.secondEligible,
      "firstAction"    -> (seq.firstAction getOrElse null),
      "secondAction"   -> (seq.secondAction getOrElse null)
    )
  
  private def sequenceOfPlayFromMap(data: Map[String, Any]): SequenceOfPlay = {
    SequenceOfPlay(
      Role(asString(data("firstEligible"))),
      Role(asString(data("secondEligible"))),
      if (data("firstAction") == null) None else Some(Action(asString(data("firstAction")))),
      if (data("secondAction") == null) None else Some(Action(asString(data("secondAction"))))
    )
  }
  
  private def piecesToMap(pieces: Pieces): Map[String, Any] =
    Map(
      "frenchTroops"     -> pieces.frenchTroops,
      "frenchPolice"     -> pieces.frenchPolice,
      "algerianTroops"   -> pieces.algerianTroops,
      "algerianPolice"   -> pieces.algerianPolice,
      "hiddenGuerrillas" -> pieces.hiddenGuerrillas,
      "activeGuerrillas" -> pieces.activeGuerrillas,
      "govBases"         -> pieces.govBases,
      "flnBases"         -> pieces.flnBases
    )
  
  private def piecesFromMap(data: Map[String, Any]): Pieces = {
    Pieces(
      asInt(data("frenchTroops")),
      asInt(data("frenchPolice")),
      asInt(data("algerianTroops")),
      asInt(data("algerianPolice")),
      asInt(data("hiddenGuerrillas")),
      asInt(data("activeGuerrillas")),
      asInt(data("govBases")),
      asInt(data("flnBases"))
    )
  }
  
  private def spaceToMap(sp: Space): Map[String, Any] =
    Map(
      "name"      -> sp.name,
      "spaceType" -> sp.spaceType,
      "zone"      -> sp.zone,
      "terrain"   -> sp.terrain,
      "basePop"   -> sp.basePop,
      "coastal"   -> sp.coastal,
      "support"   -> sp.support,
      "pieces"    -> piecesToMap(sp.pieces),
      "markers"   -> sp.markers
    )
  
  private def spaceFromMap(data: Map[String, Any]): Space = {
    Space(
      asString(data("name")),
      SpaceType(asString(data("spaceType"))),
      asString(data("zone")),
      Terrain(asString(data("terrain"))),
      asInt(data("basePop")),
      asBoolean(data("coastal")),
      SupportValue(asString(data("support"))),
      piecesFromMap(asMap(data("pieces"))),
      asList(data("markers")) map (_.toString)
    )
  }
  
  
  private def toGameJson(gameState: GameState): String = {
    val top = Map(
      "params"                  -> gameParametersToMap(gameState.params),
      "turn"                    -> gameState.turn,
      "numberOfPropCards"       -> gameState.numberOfPropCards,
      "spaces"                  -> (gameState.spaces map spaceToMap),
      "franceTrack"             -> gameState.franceTrack,
      "borderZoneTrack"         -> gameState.borderZoneTrack,
      "commitment"              -> gameState.commitment,
      "gov_resources"           -> gameState.resources.gov,
      "fln_resources"           -> gameState.resources.fln,
      "outOfPlay"               -> piecesToMap(gameState.outOfPlay),
      "casualties"              -> piecesToMap(gameState.casualties),
      "sequence"                -> sequenceOfPlayToMap(gameState.sequence),
      "capabilities"            -> gameState.capabilities,
      "momentum"                -> gameState.momentum,
      "currentCard"             -> (gameState.currentCard getOrElse null),
      "previousCard"            -> (gameState.previousCard getOrElse null),
      "propCardsPlayed"         -> gameState.propCardsPlayed,
      "pivotalCardsPlayed"      -> gameState.pivotalCardsPlayed,
      "coupdEtatPlayedOnce"     -> gameState.coupdEtatPlayedOnce,
      "recallDeGaulleCancelled" -> gameState.recallDeGaulleCancelled,
      "history"                 -> gameState.history
    )
    Json.build(top)
  }
  
  private def fromGameJson(jsonValue: String): GameState = {
    //  These momentum cards had typos in the name.
    //  Fix them when loading in case we are loading from a save
    //  file that was saved with the incorrect name
    val fixMomentumTypos = (name: String) => name match {
      case "Dual: Hardend Attitudes" => "Dual: Hardened Attitudes"
      case n => n
    }
      
    
    val top = asMap(Json.parse(jsonValue))
    GameState(
      gameParametersFromMap(asMap(top("params"))),
      asInt(top("turn")),
      asInt(top("numberOfPropCards")),
      asList(top("spaces")) map (s => spaceFromMap(asMap(s))),
      asInt(top("franceTrack")),
      asInt(top("borderZoneTrack")),
      asInt(top("commitment")),
      Resources(asInt(top("gov_resources")), asInt(top("fln_resources"))),
      piecesFromMap(asMap(top("outOfPlay"))),
      piecesFromMap(asMap(top("casualties"))),
      sequenceOfPlayFromMap(asMap(top("sequence"))),
      asList(top("capabilities")) map (_.toString),
      asList(top("momentum")) map (_.toString) map fixMomentumTypos,
      if (top("currentCard") == null) None else Some(asInt(top("currentCard"))),
      if (top("previousCard") == null) None else Some(asInt(top("previousCard"))),
      asInt(top("propCardsPlayed")),
      (asList(top("pivotalCardsPlayed")) map asInt).toSet,
      asBoolean(top("coupdEtatPlayedOnce")),
      asBoolean(top("recallDeGaulleCancelled")),
      (asList(top("history")) map (_.toString)).toVector
    )
  }
}

