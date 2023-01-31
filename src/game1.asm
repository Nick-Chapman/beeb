
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

.write SKIP 2 ; pointer for indirect-indexed addresing

;;; object in focus... 6 bytes
.theObjectStart
.theCX SKIP 1 ; coarse-X : 0..79
.theCY SKIP 1 ; coarse-Y : 0..31
.theFX SKIP 1 ; fine-X   : 0..3
.theFY SKIP 1 ; fine-Y   : 0..7
.theA SKIP 2  ; screen address of the 8x8 char
.theObjectEnd

objectSize = theObjectEnd - theObjectStart

.curr1 SKIP objectSize
.last1 SKIP objectSize

;;; TODO: Have 2nd object: curr/last

ORG &1900
GUARD &1D00
GUARD screenStart

.start:
    jmp main



.focusObject: {
    ldy #0
.loop:
    lda (write),y
    sta theObjectStart,y
    iny
    cpy #objectSize
    bne loop
    rts }

.saveObject: {
    ldy #0
.loop:
    lda theObjectStart,y
    sta (write),y
    iny
    cpy #objectSize
    bne loop
    rts }

.focusCurr1:
    lda #LO(curr1) : sta write
    lda #HI(curr1) : sta write+1
    jmp focusObject

.focusLast1:
    lda #LO(last1) : sta write
    lda #HI(last1) : sta write+1
    jmp focusObject

.saveCurr1:
    lda #LO(curr1) : sta write
    lda #HI(curr1) : sta write+1
    jmp saveObject

.saveLast1:
    lda #LO(last1) : sta write
    lda #HI(last1) : sta write+1
    jmp saveObject



.main: {
    jsr setupMachine
    jsr initCurr1
    jsr drawGrid
    jsr focusCurr1 : jsr drawFocused
.loop:
    jsr saveLastKeys
    jsr readKeys
    lda keyEscape : bne quit
    jsr saveLastScreenAddr
    jsr updateFocussedWithRepeat
    jsr saveCurr1
    jsr prepareForDraw
    jsr syncDelay
    jsr drawScreen
    jmp loop
.quit:
    rts
    }


.initCurr1:
    lda #0 : sta theCX : sta theCY : sta theFY : sta theFX
    lda #HI(screenStart) : sta theA+1
    lda #LO(screenStart) : sta theA
    jsr saveCurr1
    rts

.saveLastScreenAddr:
    jsr focusCurr1 : jsr saveLast1
    rts


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

.updateFocussed:
    { lda keyU : beq no : lda lastKeyU : bne no : jsr onU : .no }
    { lda keyD : beq no : lda lastKeyD : bne no : jsr onD : .no }
    { lda keyL : beq no : lda lastKeyL : bne no : jsr onL : .no }
    { lda keyR : beq no : lda lastKeyR : bne no : jsr onR : .no }
    rts

.updateFocussedWithRepeat:
    { lda keyU : beq no : jsr onU : .no }
    { lda keyD : beq no : jsr onD : .no }
    { lda keyL : beq no : jsr onL : .no }
    { lda keyR : beq no : jsr onR : .no }
    rts

.onU: {
    lda theFY : bne no
    lda #8 : sta theFY
    jsr upCoarse
.no:
    dec theFY
    rts }

.onD: {
    inc theFY
    lda theFY : cmp #8 : bne no
    lda #0 : sta theFY
    jsr downCoarse
.no:
    rts }

.onL: {
    lda theFX : bne no
    jsr leftCoarse
    lda #4 : sta theFX
.no:
    dec theFX
    rts }

.onR: {
    inc theFX
    lda theFX : cmp #4 : bne no
    lda #0 : sta theFX
    jsr rightCoarse
.no:
    rts }


.upCoarse:
    jsr unwrapScreen
    dec theCY
    jsr upA
    rts
.downCoarse:
    inc theCY
    jsr downA
    jsr wrapScreen
    rts
.leftCoarse:
    jsr unwrapLine
    dec theCX
    jsr leftA
    rts
.rightCoarse:
    inc theCX
    jsr rightA
    jsr wrapLine
    rts


.unwrapScreen: {
    lda theCY
    bne no
    lda #32 : sta theCY
    lda theA+1 : clc : adc #HI(screenEnd-screenStart) : sta theA+1
.no:
    rts }

.wrapScreen: {
    lda theCY
    cmp #32 : bne no
    lda #0 : sta theCY
    lda theA+1 : sec : sbc #HI(screenEnd-screenStart) : sta theA+1
.no:
    rts }

.unwrapLine : {
    lda theCX
    bne no
    lda #80 : sta theCX
    jsr downA
.no:
    rts }

.wrapLine : {
    lda theCX
    cmp #80 : bne no
    lda #0 : sta theCX
    jsr upA
.no:
    rts }


.leftA:
    lda theA : sec : sbc #8 : sta theA
    lda theA+1     : sbc #0 : sta theA+1
    rts

.rightA:
    lda theA : clc : adc #8 : sta theA
    lda theA+1     : adc #0 : sta theA+1
    rts

.upA:
    lda theA : sec : sbc #&80 : sta theA
    lda theA+1     : sbc #2   : sta theA+1
    rts

.downA:
    lda theA : clc : adc #&80 : sta theA
    lda theA+1     : adc #2   : sta theA+1
    rts


.prepareForDraw:
    rts

.drawScreen:
    jsr focusLast1 : jsr drawFocused ; erasing
    jsr focusCurr1 : jsr drawFocused
    rts

.drawFocused:
    lda theA : sta write
    lda theA+1 : sta write+1
    ldx theFX
    ldy theFY
    jsr eorWrite
    rts

.eorWrite:
    txa : asl a : tax
    lda sprite,x : sta pokeSprite+1
    lda sprite+1,x : sta pokeSprite+2
    lda (write),y
    .pokeSprite : eor &BEEF
    sta (write),y
    rts

.sprite: EQUW sprite0, sprite1, sprite2, sprite3
.sprite0: EQUB &88
.sprite1: EQUB &44
.sprite2: EQUB &22
.sprite3: EQUB &11


.dummySprite:
    EQUB &dd

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
