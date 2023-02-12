## Overview
This is a console application that implements the FLN Bot for the board game
*Colonial Twilight*, designed by Brian Train and published by GMT Games.
The FLN Bot was designed by VPJ Arponen.


This program supports the Short, Medium and Full scenarios.
You must have a copy of the board game in order to use this program.

## Downloading the package

You can download the zip file with the latest release (version 2.8) [here][1]

If you are upgrading to a newer version and the **major** version numbers of the two versions
are the same, then any saved games in progress will be compatible.

Simply copy the *games* folder from the older *coltwi-x.x* directory to the 
new *coltwi-x.x* directory.


[1]: https://www.dropbox.com/s/br7vmbbke42adnc/coltwi-2.8.zip?dl=0


## Running the program

This is a Scala program, so you will need to have the Java JVM installed and make sure that
the `java` command is on your `PATH`

There are two scripts provided that will start the program.

* `coltwi` -- As bash script used on Mac and Linux
* `coltwi.cmd` -- A Windows command file for use on Windoze

## Using the program

When you first run the program, it will prompt you to enter:

1. The scenario that you wish to play
2. A name for your game (so your progress can be saved)

Your game is saved after each turn or Propaganda round is completed.

The next time you run the program, it will ask if you want to resume a saved game.

## Entering commands
The program does not maintain the deck of cards.  You must setup and shuffle the deck and then
tell the program which card has been drawn.  To do this you simply enter `card 3`.  
This indicates that the third card *Leadership Snatch* has been drawn. You can shorten all
commands to their shortest prefix so entering `c 3` would have the same effect.

Use the `help` command to see all of the the available commands.  You can get further help for 
a specific command by typing its name after `help`. For example for help on the `show` 
command type `help show`.

The `show` command allows you to inspect the current state of the board, game status, etc.

The `history` command allow you to review the actions of the current card or to review the
entire game log.

The `rollback` command will let you restart the game from the beginning of any turn.

You can also abort the current action that you are playing at most prompts by entering `abort`.


All commands can be shorted to the prefix of the command name that is unique.  The `h` 
command is actually shorthand for `history`.

In fact this use of abbreviated prefixes works at every prompt in the game.  So if you are
choosing the country where you want to train troops and police, you can enter `cons` to indicate 
`Constantine` or `med` for `Medea`.  To select the *France Track* enter `fra` which is short for `france track`.
If the prefix you type is not unqiue, the program will display the valid choices.  
To get a list of all choices enter a question mark `?`.


## License

    Copyright (c) 2017 Curt Sellmer
    
    Permission is hereby granted, free of charge, to any person obtaining
    a copy of this software and associated documentation files (the
    "Software"), to deal in the Software without restriction, including
    without limitation the rights to use, copy, modify, merge, publish,
    distribute, sublicense, and/or sell copies of the Software, and to
    permit persons to whom the Software is furnished to do so, subject to
    the following conditions:
    
    The above copyright notice and this permission notice shall be
    included in all copies or substantial portions of the Software.
    
    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
    NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
    LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
    OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
    WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
