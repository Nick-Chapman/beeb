
top: build-all

#run: run-bfInHaskell
run: run-jan

game2view: src/game2.asm
	beebasm -v -i $< -do /dev/null -boot Code

hunits = $(patsubst haskell/%.hs, %, $(wildcard haskell/*.hs))
aunits = $(patsubst src/%.asm, %, $(wildcard src/*.asm))
ssds = $(patsubst %, _build/%.ssd, $(aunits) $(hunits))

build-all: _build $(ssds)

run-%: _build/%.ssd
	b-em $<

_build/%.ssd: src/%.asm Makefile
	@ echo Building $<
	@ beebasm -i $< -do $@ -boot Code || rm $@

_build/%.bytes: haskell/%.hs stack.yaml package.yaml Makefile
	stack run $*.exe > $@

_build/%.ssd: _build/%.bytes wrap.asm Makefile
	@ echo 'Wrapping $< as disc-image: $@'
	@ beebasm -S BinFile=$< -i wrap.asm -do $@ -boot Code || rm $@

_build: ; @mkdir -p $@

docs-index:
	./make-docs-index.sh > docs/README.md
