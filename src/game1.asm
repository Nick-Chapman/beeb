
osasci = &ffe3
oswrch = &ffee
osbyte = &fff4

keyCodeU = -58
keyCodeD = -42
keyCodeL = -26
keyCodeR = -122
keyCodeEscape = -113

screenStart = &3000
screenEnd = &8000

ORG &70

.keyU SKIP 1
.keyD SKIP 1
.keyL SKIP 1
.keyR SKIP 1
.keyEscape SKIP 1

.lastKeyU SKIP 1
.lastKeyD SKIP 1
.lastKeyL SKIP 1
.lastKeyR SKIP 1

.currCX SKIP 1
.currCY SKIP 1 ; coarse-Y
.currA SKIP 2 ; current screen address (which we fill)
.lastA SKIP 2 ; last screen address (which we erase)

.write SKIP 2 ; pointer for writing to screen

ORG &1900
GUARD &1C00
GUARD screenStart

.start:
    jmp main

.main: {
    jsr setupMachine
    jsr initVars
    jsr drawGrid
    jsr drawCurr
.loop:
    jsr saveLastKeys
    jsr readKeys
    lda keyEscape : bne quit
    jsr updateGameStateWithRepeat
    jsr prepareForDraw
    jsr syncDelay
    jsr drawScreen
    jmp loop
.quit:
    rts
    }

.saveLastKeys:
    lda keyU : sta lastKeyU
    lda keyD : sta lastKeyD
    lda keyL : sta lastKeyL
    lda keyR : sta lastKeyR
    rts

.readKeys:
    jsr checkU
    jsr checkD
    jsr checkL
    jsr checkR
    jsr checkEscape
    rts

.checkU: {
    lda #0 : sta keyU
    lda #&81
    ldx #(keyCodeU AND &ff)
    ldy #(keyCodeU AND &ff00) DIV 256
    jsr osbyte
    cpx #&ff : bne no
    lda #1 : sta keyU : .no:
    rts }

.checkD: {
    lda #0 : sta keyD
    lda #&81
    ldx #(keyCodeD AND &ff)
    ldy #(keyCodeD AND &ff00) DIV 256
    jsr osbyte
    cpx #&ff : bne no
    lda #1 : sta keyD : .no:
    rts }

.checkL: {
    lda #0 : sta keyL
    lda #&81
    ldx #(keyCodeL AND &ff)
    ldy #(keyCodeL AND &ff00) DIV 256
    jsr osbyte
    cpx #&ff : bne no
    lda #1 : sta keyL : .no:
    rts }

.checkR: {
    lda #0 : sta keyR
    lda #&81
    ldx #(keyCodeR AND &ff)
    ldy #(keyCodeR AND &ff00) DIV 256
    jsr osbyte
    cpx #&ff : bne no
    lda #1 : sta keyR : .no:
    rts }

.checkEscape: {
    lda #0 : sta keyEscape
    lda #&81
    ldx #(keyCodeEscape AND &ff)
    ldy #(keyCodeEscape AND &ff00) DIV 256
    jsr osbyte
    cpx #&ff : bne no
    lda #1 : sta keyEscape : .no:
    rts }

.updateGameState:
    jsr saveLastScreenAddr
    { lda keyU : beq no : lda lastKeyU : bne no : jsr onU : .no }
    { lda keyD : beq no : lda lastKeyD : bne no : jsr onD : .no }
    { lda keyL : beq no : lda lastKeyL : bne no : jsr onL : .no }
    { lda keyR : beq no : lda lastKeyR : bne no : jsr onR : .no }
    rts

.updateGameStateWithRepeat:
    jsr saveLastScreenAddr
    { lda keyU : beq no : jsr onU : .no }
    { lda keyD : beq no : jsr onD : .no }
    { lda keyL : beq no : jsr onL : .no }
    { lda keyR : beq no : jsr onR : .no }
    rts

.initVars:
    lda #0 : sta currCX : sta currCY
    lda #HI(screenStart) : sta currA+1
    lda #LO(screenStart) : sta currA
    rts

.saveLastScreenAddr:
    lda currA : sta lastA
    lda currA+1 : sta lastA+1
    rts

.onU:
    lda currCY : bne nowrap
    lda currA+1 : clc : adc #HI(screenEnd-screenStart) : sta currA+1 ; wrap
.nowrap:
    jsr upLine
    lda currCY : sec : sbc #1 : and #31 : sta currCY
    rts

.onD: {
    lda currCY : clc : adc #1 : and #31 : sta currCY : bne nowrap
    lda currA+1 : sec : sbc #HI(screenEnd-screenStart) : sta currA+1 ; wrap
.nowrap:
    jsr downLine
    rts }

.onL: {
    lda currCX : bne nowrap
    lda #80 : sta currCX
    jsr downLine
.nowrap:
    dec currCX
    lda currA : sec : sbc #8 : sta currA
    lda currA+1     : sbc #0 : sta currA+1
    rts }

.onR: {
    inc currCX : lda currCX : cmp #80 : bne nowrap
    lda #0 : sta currCX
    jsr upLine
.nowrap:
    lda currA : clc : adc #8 : sta currA
    lda currA+1     : adc #0 : sta currA+1
    rts }

.upLine:
    lda currA : sec : sbc #&80 : sta currA
    lda currA+1     : sbc #2   : sta currA+1
    rts

.downLine:
    lda currA : clc : adc #&80 : sta currA
    lda currA+1     : adc #2   : sta currA+1
    rts


.prepareForDraw:
    rts

.drawScreen:
    jsr drawLast ; erasing
    jsr drawCurr
    rts

.drawLast:
    lda lastA : sta write
    lda lastA+1 : sta write+1
    jsr eorWrite
    rts

.drawCurr:
    lda currA : sta write
    lda currA+1 : sta write+1
    jsr eorWrite
    rts

.eorWrite:
    ldy #0
    lda (write),y
    eor #&88 ; single dot
    sta (write),y
    rts

.drawGrid: {
    lda #LO(screenStart) : sta write
    lda #HI(screenStart) : sta write+1
    ldy #0
.next:
    lda #&08
    sta (write),y
    clc
    lda write : adc #8 : sta write
    lda write+1 : adc #0 : sta write+1
    cmp #&80
    bmi next
    rts }

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

.syncDelay:
    ;lda #1 : jsr pause
    jsr vsyncReal
    rts

.vsyncReal
    lda #19 : jsr osbyte
    rts

.pause: {
    tax
.loopX:
    ldy #255
.loopY:
    dey
    bne loopY
    dex
    bne loopX
    rts }

.end:
SAVE "Code", start, end
