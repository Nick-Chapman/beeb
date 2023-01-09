
BEEBASM = ../stardot/beebasm/beebasm
B-EM = ../stardot/b-em/b-em

run: demo.ssd
	$(B-EM) $^

demo.ssd: demo.6502
	$(BEEBASM) -i $^ -do $@ -boot Code -v
