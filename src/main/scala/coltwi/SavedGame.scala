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
                          SpaceType, Terrain, SupportValue, Resources, SOFTWARE_VERSION }

object SavedGame {
  val CurrentFileVersion = 1
  
  def save(filepath: Pathname, gameState: GameState): Unit = {
    try {
      filepath.dirname.mkpath() // Make sure that the game directory exists
      filepath.writeFile(toJson(gameState))
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
  
  private def toJson(gameState: GameState): String = {
    val top = Map(
      "file-version"     -> CurrentFileVersion,
      "software-version" -> SOFTWARE_VERSION,
      "game-state"       -> gameStateToMap(gameState)
    )
    Json.build(top)
  }
  
  // The path should be the full path to the file to load.
  // Will set the game global variable
  def load(filepath: Pathname): GameState = {
    try fromJson(filepath.readFile())
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
  
  private def fromJson(jsonValue: String): GameState = {
    val top = asMap(Json.parse(jsonValue))
    //  The file-version started with version 1
    //  If this is missing, then it is an older file
    //  with no file-version, ie verion zero
    if (top.contains("file-version")) {
      if (!top.contains("game-state"))
        throw new IllegalArgumentException(s"Invalid save file - No game-state")
      
      asInt(top("file-version")) match {
        case 1 => gameFromVersion1(asMap(top("game-state")))
        case v => throw new IllegalArgumentException(s"Invalid save file version: $v")
      }
    }
    else {
      //  In version zero the game state was at the top level of the json
      gameFromVersionZero(top)
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
  
  
  private def gameStateToMap(gameState: GameState): Map[String, Any] = {
    Map(
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
  }
  
  //  Version 1 game state is in the same foremat as version zero
  //  (It was just down one level from the top meta dat)
  private def gameFromVersion1(data: Map[String, Any]): GameState = gameFromVersionZero(data)
  
  private def gameFromVersionZero(data: Map[String, Any]): GameState = {
    //  These momentum cards had typos in the name.
    //  Fix them when loading in case we are loading from a save
    //  file that was saved with the incorrect name
    val fixMomentumTypos = (name: String) => name match {
      case "Dual: Hardend Attitudes" => "Dual: Hardened Attitudes"
      case n => n
    }
    
    GameState(
      gameParametersFromMap(asMap(data("params"))),
      asInt(data("turn")),
      asInt(data("numberOfPropCards")),
      asList(data("spaces")) map (s => spaceFromMap(asMap(s))),
      asInt(data("franceTrack")),
      asInt(data("borderZoneTrack")),
      asInt(data("commitment")),
      Resources(asInt(data("gov_resources")), asInt(data("fln_resources"))),
      piecesFromMap(asMap(data("outOfPlay"))),
      piecesFromMap(asMap(data("casualties"))),
      sequenceOfPlayFromMap(asMap(data("sequence"))),
      asList(data("capabilities")) map (_.toString),
      asList(data("momentum")) map (_.toString) map fixMomentumTypos,
      if (data("currentCard") == null) None else Some(asInt(data("currentCard"))),
      if (data("previousCard") == null) None else Some(asInt(data("previousCard"))),
      asInt(data("propCardsPlayed")),
      (asList(data("pivotalCardsPlayed")) map asInt).toSet,
      asBoolean(data("coupdEtatPlayedOnce")),
      asBoolean(data("recallDeGaulleCancelled")),
      (asList(data("history")) map (_.toString)).toVector
    )
  
 }  
}

