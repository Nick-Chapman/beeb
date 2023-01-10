
top: _build run-first

run-%: _build/%.ssd
	b-em $<

_build/%.ssd: src/%.asm
	beebasm -i $< -do $@ -boot Code

_build: ; @mkdir -p $@
