
top: _build run-bf

ssd: _build/bf.ssd

run-%: _build/%.ssd
	b-em $<

_build/%.ssd: src/%.asm
	beebasm -i $< -do $@ -boot Code || rm $@

_build: ; @mkdir -p $@
