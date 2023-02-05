# beeb

Playing around with examples to run on the BBC Micro.
Eventual goal: write a graphics game.

## game1

DONE
- Have A be unaffected by fine-Y adjust, and use FY for y-index when plotting.
- Use self-mod code for sprite data access avoiding x-index for FX.
- Have more than one object (test with tab-key to switch which is controlled by keys).
- Support multi sprite columns, using rightCoarse(or the like) for adjust.
- Support sprite columns of any depth
- show raster position during sprite plot
- use self-mod-code to update screen-pointer.
- better: gen code for screen writes
- 3rd object

TODO:
- avoid code-repetition between multiple objects
- 4th object
- objects which move themselves
- sprites with varing data in sprite column strips
- small meteors
- different sprite data for diff objects
- medium & large meteors
- spaceship
- bullets
- collision detection
- improve vsync with timer
- Disconnect keys from direct position control; go via intermediate speed.
- Code (initial) compute of A(hi/lo) from x/y-coarse.
