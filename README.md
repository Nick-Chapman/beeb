# beeb

Playing around with examples to run on the BBC Micro.
Eventual goal: write a graphics game.

## game1
- Have A be unaffected by fine-Y adjust, and use FY for y-index when plotting.
- Use self-mod code for sprite data access avoiding x-index for FX.
- Have more than one object (test with tab-key to switch which is controlled by keys).
- Support multi sprite columns, using rightCoarse(or the like) for adjust.
- Support sprite columns of any depth
- show raster position during sprite plot
- use self-mod-code to update screen-pointer.
- better: gen code for screen writes
- 3rd object
- sprites with varing data in sprite column strips
- avoid code-repetition between multiple objects
- different sprite data for diff objects
- allow objects with a 3rd strip
- diffent objects can have diff nummber of strips
- small meteor sprite data
- objects with independent movement; different speeds
- Code (initial) compute of A(hi/lo) from x/y-coarse.
- medium meteor sprite data
- dynamic object creation
- random position and direction
- bullets
- collision detection... first exploration

## game2
- cheap dual-space erase
- cheap non-coliision plotting (intended for bullets)
- mask/eor collion plotting (intended for rocks/saucer/player)
- simple game logic: rocks and bullets
- simple scene change: level start; detect level end; wait and restart
- post plot collision detection (intended for bullets)

## todo
- reinstate sprites form game1: bullets, rocks
- four game object classes classes: bullets, rocks, (LATER:saucer), player
- game logic and different hit-detection for player
- reinstate keyboard control for player
- player shoots bullets
- large meteor sprite
- spaceship sprite, 4 orientations, mirror plotting for 4-->16 orientations
- saucer: spite, game logic
- spaceship with thrust & rotation control
- slow object movement; interesting angles
- plot/erase only half the objects each frame
- improve vsync with timer
