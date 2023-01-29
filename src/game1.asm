
osasci = &ffe3
oswrch = &ffee
osbyte = &fff4

keyCodeX = -67
keyCodeZ = -98

screenStart = &3000
screenEnd = &8000

ORG &70

.curr SKIP 2 ; current screen address (which we fill)
.last SKIP 2 ; last screen address (which we erase)
.write SKIP 2

.pressedX SKIP 1
.pressedZ SKIP 1

ORG &1900
GUARD &1B00
GUARD screenStart

.start:
    jmp main

.main: {
    jsr setupMachine
    jsr initVars
    jsr drawGrid
    jsr drawCurr
.loop:
    jsr readKeys
    jsr updateGameState
    jsr prepareForDraw
    jsr syncDelay
    jsr drawScreen
    jmp loop
    }

.readKeys:
    jsr checkX
    jsr checkZ
    rts

.checkX: {
    lda #0 : sta pressedX
    lda #&81
    ldx #(keyCodeX AND &ff)
    ldy #(keyCodeX AND &ff00) div 256
    jsr osbyte
    cpx #&ff : bne no
    lda #1 : sta pressedX : .no:
    rts }

.checkZ: {
    lda #0 : sta pressedZ
    lda #&81
    ldx #(keyCodeZ AND &ff)
    ldy #(keyCodeZ AND &ff00) div 256
    jsr osbyte
    cpx #&ff : bne no
    lda #1 : sta pressedZ : .no:
    rts }

.initVars:
    lda #HI(screenStart) : sta curr+1
    lda #LO(screenStart) : sta curr
    rts

.updateGameState: {
    jsr saveLast
    lda pressedX : beq noX : jsr incrementCurr : .noX
    lda pressedZ : beq noZ : jsr decrementCurr : .noZ
    rts }

.saveLast:
    lda curr : sta last
    lda curr+1 : sta last+1
    rts

.incrementCurr: {
    inc curr : bne done
    inc curr+1 : lda curr+1 : cmp #HI(screenEnd) : bne done
    lda #HI(screenStart) : sta curr+1
.done
    rts }

.decrementCurr: {
    lda curr : bne low
    lda curr+1 : cmp #HI(screenStart) : bne high
    lda #HI(screenEnd) : sta curr+1
.high:
    dec curr+1
.low:
    dec curr
    rts }

.prepareForDraw:
    rts

.drawScreen:
    jsr drawLast ; erasing
    jsr drawCurr
    rts

.drawLast:
    lda last : sta write
    lda last+1 : sta write+1
    jsr eorWrite
    rts

.drawCurr:
    lda curr : sta write
    lda curr+1 : sta write+1
    jsr eorWrite
    rts

.eorWrite:
    ldy #0
    lda (write),y
    eor #255
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
