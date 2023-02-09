ula = &fe21
osasci = &ffe3
oswrch = &ffee
osbyte = &fff4

ORG &70

.theA SKIP 2
.mesPtr SKIP 2
.eraseRunner SKIP 2
.eraseCodeStart SKIP 2
.eraseSpaceEnd SKIP 2
.ptrE SKIP 2
.ptrP SKIP 2

ORG &1900

.start:
    jmp main

;----------------------------------------------------------------------
; main loop

.examplePos SKIP 1 ; for first example drive

.main: {
    lda #0 : sta examplePos
    jsr setupMachine
    jsr initErase
    jsr initPlot
.loop:

    lda #2 : sta ula ; magenta
    jsr prepare
    inc examplePos
    lda #7 : sta ula ; black

    ;lda #3 : sta ula ; blue
    jsr syncDelay
    lda #7 : sta ula ; black

    lda #4 : sta ula ; yellow
    jsr runErase
    jsr runPlot
    lda #7 : sta ula ; black

    jmp loop
    rts }

.prepare:
    lda examplePos : sta theA : lda #&40 : sta theA+1
    lda #&ac : jsr genPlot : jsr genErase ; cyan/red/yellow/black
    lda examplePos : sta theA : lda #&60 : sta theA+1
    lda #&0e : jsr genPlot : jsr genErase ; cyan/red/yellow/black
    rts

;----------------------------------------------------------------------
; dual-space erase

.initErase:
    jmp switchEraseA

numberEraseBlocks = 50
.eraseSpaceA:
FOR i, 1, numberEraseBlocks
    sta &BEEF ; SCREEN-ADDRESS(1,2)
NEXT
.eraseSpaceA_end:
    rts

.eraseSpaceB:
FOR i, 1, numberEraseBlocks
    sta &BEEF ; SCREEN-ADDRESS(1,2)
NEXT
.eraseSpaceB_end:
    rts

.genErase:
    jsr checkSpaceErase ; dev-time-debug
    ;; 1,2 are offsets into "sta &BEEF", 3 is size
    lda theA   : ldy #1 : sta (ptrE),y
    lda theA+1 : ldy #2 : sta (ptrE),y
    lda ptrE : clc : adc #3 : sta ptrE
    bcs hi : rts : .hi : inc ptrE+1
    rts

.runErase: {
    ;; Finalize the generated code with an "rts" at "ptrE"...
    ;; First copy "ptr" into 3 places in the code. (marked BEEF)
    lda ptrE   : sta A1+1 : sta A2+1 : sta A3+1
    lda ptrE+1 : sta A1+2 : sta A2+2 : sta A3+2
    ;; Save the byte we are about to overwrite.
    .A1 : lda &BEEF
    sta SavedByte+1
    ;; Overwrite byte with an "rts" opcode.
    lda Rts
    .A2 : sta &BEEF
    ;; Dispatch to the generated code.
    lda #0
    jsr dispatchEraseRunner
    ;; Reinstate the saved byte.
    .SavedByte : lda #0
    .A3 : sta &BEEF
    ;; Return
    .Rts : rts }

.dispatchEraseRunner:
    jmp (eraseRunner)

.eraseRunnerA:
    ;; Run the finalized code (in A)
    jsr eraseSpaceA
.switchEraseA:
    ;; Next gen in A; next run in B
    lda #LO(eraseSpaceA) : sta ptrE
    lda #HI(eraseSpaceA) : sta ptrE+1
    lda #LO(eraseSpaceA_end) : sta eraseSpaceEnd
    lda #HI(eraseSpaceA_end) : sta eraseSpaceEnd+1
    lda #LO(eraseRunnerB) : sta eraseRunner
    lda #HI(eraseRunnerB) : sta eraseRunner+1
    rts

.eraseRunnerB:
    ;; Run the finalized code (in B)
    jsr eraseSpaceB
.switchEraseB:
    ;; Next gen in B; next run in A
    lda #LO(eraseSpaceB) : sta ptrE
    lda #HI(eraseSpaceB) : sta ptrE+1
    lda #LO(eraseSpaceB_end) : sta eraseSpaceEnd
    lda #HI(eraseSpaceB_end) : sta eraseSpaceEnd+1
    lda #LO(eraseRunnerA) : sta eraseRunner
    lda #HI(eraseRunnerA) : sta eraseRunner+1
    rts

.checkSpaceErase: {
    lda ptrE+1 : cmp eraseSpaceEnd+1 : bcc ok : bne fail
    lda ptrE   : cmp eraseSpaceEnd   : bcs fail
.ok:
    rts
.fail:
    lda #LO(msg) : sta mesPtr
    lda #HI(msg) : sta mesPtr+1
    jmp writeMessageAndSpin
.msg EQUS "Erase Overflow", 13, 0 }

;----------------------------------------------------------------------
; plot

.initPlot:
    lda #LO(plotSpace) : sta ptrP
    lda #HI(plotSpace) : sta ptrP+1
    rts

numberPlotBlocks = 50
.plotSpace:
FOR i, 1, numberPlotBlocks
    lda #0
    sta &BEEF ; SCREEN-ADDRESS(3,4)
    ;; size=5
NEXT
.plotSpace_end:
    rts

.genPlot: ; data byte to plot in A
    sta DB+1
    jsr checkSpacePlot ; dev-time-debug
    .DB : lda #0 : ldy #1 : sta (ptrP),y
    lda theA     : ldy #3 : sta (ptrP),y
    lda theA+1   : ldy #4 : sta (ptrP),y
    lda ptrP : clc : adc #5 : sta ptrP
    { bcs hi : rts : .hi } : inc ptrP+1
    rts

.runPlot:
    ;; Finalize the generated code with an "rts" at "ptrP"...
    ;; First copy "ptr" into 3 places in the code. (marked BEEF)
    lda ptrP   : sta A1+1 : sta A2+1 : sta A3+1
    lda ptrP+1 : sta A1+2 : sta A2+2 : sta A3+2
    ;; Can reset the generation-pointer now
    jsr initPlot
    ;; Save the byte we are about to overwrite.
    .A1 : lda &BEEF
    sta SavedByte+1
    ;; Overwrite byte with an "rts" opcode.
    lda Rts
    .A2 : sta &BEEF
    ;; Dispatch to the generated code.
    jsr plotSpace
    ;; Reinstate the saved byte.
    .SavedByte : lda #0
    .A3 : sta &BEEF
    ;; Return
    .Rts : rts

.checkSpacePlot: {
    lda ptrP+1 : cmp plotSpace_end+1 : bcc ok : bne fail
    lda ptrP   : cmp plotSpace_end   : bcs fail
.ok:
    rts
.fail:
    lda #LO(msg) : sta mesPtr
    lda #HI(msg) : sta mesPtr+1
    jmp writeMessageAndSpin
.msg EQUS "Plot Overflow", 13, 0 }

;----------------------------------------------------------------------
; misc

.syncDelay:
    lda #19 : jsr osbyte
    rts

.writeMessageAndSpin: {
    ldy #0
.loop
    lda (mesPtr),y
    beq spin
    jsr osasci
    iny
    bne loop
.spin:
    jmp spin }

;----------------------------------------------------------------------
; setupMachine

.setupMachine:
    jsr mode1
    jsr cursorOff
    jsr replaceWhiteWithCyan
    rts

.mode1:
    lda #22 : jsr oswrch
    lda #1 : jsr oswrch
    rts

.cursorOff:
    lda #23 : jsr oswrch
    lda #1 : jsr oswrch
    lda #0 : jsr oswrch
    lda #0 : jsr oswrch
    lda #0 : jsr oswrch
    lda #0 : jsr oswrch
    lda #0 : jsr oswrch
    lda #0 : jsr oswrch
    lda #0 : jsr oswrch
    lda #0 : jsr oswrch
    rts

.replaceWhiteWithCyan:
    lda #19 : jsr oswrch
    lda #7 : jsr oswrch
    lda #6 : jsr oswrch
    lda #0 : jsr oswrch
    lda #0 : jsr oswrch
    lda #0 : jsr oswrch
    rts

.end:
SAVE "Code", start, end
