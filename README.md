# project-cis5520

This project implements the Candy Crush game in Haskell. The project includes a 
complete gameplay loop, state management, and interactive user input, allowing 
players to perform actions such as swapping candies, activating special effects, 
and progressing through game levels.

This project also includes a robust custom parser, designed to interpret 
structured configuration files that define the game's constants, candies, and 
effects. It allows for flexibility and customization by enabling users to define 
the game grid dimensions, candy properties, scoring rules, and special effects 
in a structured and human-readable format.

## Module organization

The project code is in three separate places:

  - The source code is in the `src` directory.
    - [GeneralStateParser.hs](src/GeneralStateParser.hs):
    This module defines a general-purpose parser library built on the 
    StateParser abstraction, which enables parsing with state tracking. It 
    supports custom error handling, including recoverable (FailError) and 
    fatal errors (FatalError), and provides utilities for constructing flexible 
    and reusable parsers.
    - [CandyCrushParser.hs](src/CandyCrushParser.hs):
    This module defines a Candy Crush game-specific parser that uses the 
    general-purpose parser library GeneralStateParser. It parses and validates 
    structured data for initializing the game's constants, candy effects, 
    candies, and player actions.
    - [Constants.hs](src/Constants.hs):
    This module defines a set of string constants used as tags for identifying 
    various elements within the Candy Crush game configuration. 
    - [Phd.hs](src/Phd.hs):
    This module provides a plain Haskell definition framework for the Candy 
    Crush game, including data types, game states, and actions. It serves as 
    the core data layer for the game.
    - [GameState.hs](src/GameState.hs):
    This module manages the state and grid of the Candy Crush game. It defines 
    functions for initializing, modifying, and querying the game state, 
    enabling core gameplay operations.
    - [GameUtils.hs](src/GameUtils.hs):
    This module provides utility functions and helper methods to support the 
    Candy Crush game's core functionalities, including managing candies, 
    effects, the game board, and persisting game data.
    - [GameController.hs](src/GameController.hs):
    This module handles the control flow, user interactions, and game mechanics 
    for the Candy Crush game. It integrates various components to manage the 
    gameplay loop, execute player actions, and maintain game state.
  
  - The entry point for the executable is in [Main.hs](app/Main.hs). 
  
  - All of test cases are in [the test directory](test/Spec.hs).

`Configration files`:

User can define their own game configurations under [config/](config/) folder
following the definition rules specified in example configuration files under
the config folder.

## Building, running, and testing

This project compiles with `stack build`. 
You can run the main executable with `stack run`.
You can run the tests with `stack test`. 

Finally, you can start a REPL with `stack ghci`.

