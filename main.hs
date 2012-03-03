-----------------------------------------------------------------
--main module for the Haskell Adventure project
--Copyright Zach Greenvoss 2012
--  Contains the entry point for the game
------------------------------------------------------------------
module Main (main) where

import HaskellAdventure.Input
import HaskellAdventure.Data

-----------------------------------------------------------------
--main function
--  Simply calls the HaskellAdventure.Output.executeRoom function
--      with the start GameState.  This will recursively evaluate
--      successive GameStates as the player moves through the
--      game world.
-----------------------------------------------------------------
main =
 do
    executeState start
    putStrLn ("Thanks for playing!")
