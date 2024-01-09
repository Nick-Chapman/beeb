# beeb

Playing around with examples to run on the BBC Micro.
Eventual goal: write a graphics game.

## Play using JsBeeb
- [game3](https://bbc.godbolt.org/?&disc1=https://github.com/Nick-Chapman/beeb/tree/main/release/game3.ssd&autoboot)
- [jan](https://bbc.godbolt.org/?&disc1=https://github.com/Nick-Chapman/beeb/tree/main/release/jan.ssd&autoboot)

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

## game3
- reinstate sprites from game1: bullets, rocks
- collision detection for rocks and bullets
- improve vsync with timer
- slow rock movement; independent V/H counters per rock
- game logic: kill rocks under screen clear, then restart
- game logic: 2x medium: each medium splits into 2x small
- reinstate keyboard control for player
- player shoots bullets from invisible space-ship (source of bullets!)
- (invisible) ship rotation control: become bullet direction
- basic thrust control (move the source of bullets)
- ship: see something!

## jan (January 2024 -- New year; New fun!)
- thrust and momentum for ship
- trig for ship rotation
- sprite(s) for ship: 5 sprites in 32 orientations (mirror: 4,8,8,8,4)

## NEXT
- ship: collision detection
- find some easy wins for speedup
- large meteor sprite
- prepare/plot objects every other frame to allow double number of objects

## one day
- saucer: sprite, collision, game logic
