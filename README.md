# beeb

Playing around with examples to run on the BBC Micro.
Goal: Re-Implementation of Acorsoft Meteors.
[Original Game](https://bbc.godbolt.org/?&disc1=https://nick-chapman.github.io/beeb/Meteors.ssd&autoboot)

## WIP demos in JsBeeb
- [2023: Game3 (final state of 2023 work)](https://bbc.godbolt.org/?&disc1=https://nick-chapman.github.io/beeb/game3.ssd&autoboot)
- [2024: New year; new fun: Ship with thrust](https://bbc.godbolt.org/?&disc1=https://nick-chapman.github.io/beeb/jan.ssd&autoboot)
- [WIP Sound: drone,fire,explode](https://bbc.godbolt.org/?&disc1=https://nick-chapman.github.io/beeb/explode.ssd&autoboot)
- [objects: ship, rocks, bullets, firing](https://bbc.godbolt.org/?&disc1=https://nick-chapman.github.io/beeb/objects.ssd&autoboot)

## Resources

- http://www.6502.org/tutorials/6502opcodes.html
- https://www.masswerk.at/6502/6502_instruction_set.html
- http://6502.org/users/obelisk/
- https://www.nesdev.org/wiki/6502_cycle_times
- https://www.pagetable.com/c64ref/6502/?tab=4

- https://beebwiki.mdfs.net/Keyboard
- https://tobylobster.github.io/mos/mos/S-s4.html
- https://www.chibiakumas.com/6502/bbc.php
- https://www.smspower.org/Development/SN76489

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

## own-emit
- printing chars to screen without using osasci

## explode
- figure out simultaneous control of sound chip and keyboard polling
- sounds for drone, fire, and rock explosion

## render
- Rework plot and render code to use multiple buffers.
- still too slow!
- finally realize I must give up the goal of perfect rendering.
- and allow screen writing at any time (not just during vertical blanking)

## objects
- Objects, with render loop and synced update
- per-object info tables for update/render function, outline, position,...
- unplot/plot controlled by per-object active/rendered-bits
- debug: show frame lag
- large rock outline. woohoo!
- (rethink physical->logic colour mapping, so red & cyan are on separate bit planes)
- unplot/plot with collision detection (active/rendered/hit bits)
- record per-object kind; track per-kind counts; inc/dec on spawn/kill
- hit rock: child rocks activated
- full rock destruction logic: large -> 2medium -> 4small
- hit during unplot, dont replot to avoid incorrect secondary collission.
- bullet death on timer (state: frameCounter when spawned)
- support speed on all objects, and update position
- child rock inherits position/speed from parent
- bullet inherits speed/position from ship
- reinstate ship controls: thrust/decaying speed
- child rock inherits position(+ fixed delta) from parent
- global state for direction, caps/control
- change ship outline when direction changes
- render now works w.r.t a direction
- save rendered direction/outline for unplot
