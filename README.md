# beeb

Playing around with examples to run on the BBC Micro.
Eventual goal: write a graphics game.

## game1

DONE
- Have A be unaffected by fine-Y adjust, and use FY for y-index when plotting.
- Use self-mod code for sprite data access avoiding x-index for FX.
- Have more than one object (test with tab-key to switch which is controlled by keys).
- Support multi sprite columns, using rightCoarse(or the like) for adjust.

TODO:
- Disconnect keys from direct position control; go via intermediate speed.
- Code (initial) compute of A(hi/lo) from x/y-coarse.
- Support sprite columns of any depth, with self-mod to update screen-pointer.
