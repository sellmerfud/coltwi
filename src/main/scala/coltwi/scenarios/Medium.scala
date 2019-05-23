
//   _____      _             _       _   _______       _ _ _       _     _
//  / ____|    | |           (_)     | | |__   __|     (_) (_)     | |   | |
// | |     ___ | | ___  _ __  _  __ _| |    | |_      ___| |_  __ _| |__ | |_
// | |    / _ \| |/ _ \| '_ \| |/ _` | |    | \ \ /\ / / | | |/ _` | '_ \| __|
// | |___| (_) | | (_) | | | | | (_| | |    | |\ V  V /| | | | (_| | | | | |_
//  \_____\___/|_|\___/|_| |_|_|\__,_|_|    |_| \_/\_/ |_|_|_|\__, |_| |_|\__|
//                                                             __/ |
//                                                            |___/
// An scala implementation of the solo AI for the game 
// Falling Sky The Gallic Revolt Against Caesar, 
// designed by Andrew Ruhnke and Volko Ruhnke
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

package coltwi.scenarios

import coltwi.ColonialTwilight._

class Medium extends Scenario {
  val name               = "Medium: 1957-1962: Midgame Development"
  val numberOfPropCards  = 4
  val resources          = Resources(gov = 24, fln = 15)
  val commitment         = 20
  val franceTrack        = franceTrackFromLetter('C')
  val borderZoneTrack    = 2
  val outOfPlay          = Pieces(hiddenGuerrillas = 2)
  val pivotalCardsPlayed = Set(PivotalMobilization, PivotalMoroccoTunisiaIndepdent)

  val spaces = List(
    // Wilaya I
    DefaultBarika.copy(
      support = Oppose, 
      pieces  = Pieces(hiddenGuerrillas = 2, flnBases = 1)),
    DefaultBatna.copy(
      pieces = Pieces(hiddenGuerrillas = 1)),
    DefaultBiskra.copy(
      pieces  = Pieces(frenchPolice = 1)),
    DefaultOumElBouaghi.copy(
      pieces = Pieces(hiddenGuerrillas = 1)),
    DefaultTebessa.copy(
      pieces  = Pieces(frenchPolice = 1)),
    DefaultNegrine.copy(
      pieces  = Pieces(frenchPolice = 1)),
      
    // Wilaya II
    DefaultConstantine.copy(
      support = Support,
      pieces  = Pieces(frenchPolice = 2)),
    DefaultSetif.copy(
      pieces  = Pieces(algerianPolice = 1, hiddenGuerrillas = 1)),
    DefaultPhilippeville.copy(
      support = Oppose,
      pieces  = Pieces(frenchPolice = 1, algerianPolice = 1, hiddenGuerrillas = 2, flnBases = 1)),
    DefaultSoukAhras.copy(
      pieces  = Pieces(frenchTroops = 4, algerianTroops = 1, frenchPolice = 2, govBases = 1,
                       hiddenGuerrillas = 1)),
                       
    // Wilaya III
    DefaultTiziOuzou.copy(
      support = Oppose,
      pieces  = Pieces(frenchPolice = 1, hiddenGuerrillas = 2, flnBases = 1)),
    DefaultBordjBouArreridj,
    DefaultBougie.copy(
      support = Oppose,
      pieces  = Pieces(frenchPolice = 1, algerianPolice = 1, hiddenGuerrillas = 2, flnBases = 1)),
      
    // Wilaya IV
    DefaultAlgiers.copy(
      support = Support,
      pieces  = Pieces(frenchTroops = 5, algerianTroops = 1, frenchPolice = 2, algerianPolice = 1)),
    DefaultMedea.copy(
      pieces  = Pieces(algerianPolice = 1, govBases = 1, hiddenGuerrillas = 1)),
    DefaultOrleansVille.copy(
      support = Oppose,
      pieces  = Pieces(frenchPolice = 1, algerianPolice = 1, hiddenGuerrillas = 1, flnBases = 1)),
      
    // Wilaya V
    DefaultOran.copy(
      support = Support,
      pieces  = Pieces(algerianTroops = 1, frenchPolice = 2)),
    DefaultMecheria.copy(
      pieces  = Pieces(frenchPolice = 1, algerianPolice = 1)),
    DefaultTlemcen.copy(
      pieces  = Pieces(frenchPolice = 2),
      markers = List(ResettledMarker)),
    DefaultSidiBelAbbes.copy(
      pieces  = Pieces(frenchPolice = 1, govBases = 1)),
    DefaultMostaganem,
    DefaultSaida.copy(
      pieces  = Pieces(hiddenGuerrillas = 1, flnBases = 1)),
    DefaultMascara,
    DefaultTiaret,
    DefaultAinSefra.copy(
      pieces  = Pieces(frenchPolice = 1)),
    DefaultLaghouat,
    
    // Wilaya VI
    DefaultSidiAissa.copy(
      pieces  = Pieces(hiddenGuerrillas = 1, flnBases = 1)),
    DefaultAinOussera,

    // Countries
    DefaultMorocco.copy(
      pieces  = Pieces(hiddenGuerrillas = 2, flnBases = 1)),
    DefaultTunisia.copy(
      pieces  = Pieces(hiddenGuerrillas = 4, flnBases = 2))
  )
}

