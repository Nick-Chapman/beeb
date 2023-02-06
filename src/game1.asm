
ula = &fe21

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

stack = &100

GUARD stack
GUARD screenStart

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

.gridPtr SKIP 2
.ptr SKIP 2
.theStrip SKIP 2
.theObj SKIP 2
.funcPtr SKIP 2
.mesPtr SKIP 2

;;; object in focus...
.theObjectStart
.theBehaviour SKIP 2
.theSpriteData SKIP 2
.theCX SKIP 1 ; coarse-X : 0..79
.theCY SKIP 1 ; coarse-Y : 0..31
.theFX SKIP 1 ; fine-X   : 0..3
.theFY SKIP 1 ; fine-Y   : 0..7
.theA SKIP 2  ; screen address of the 8x8 char
.theObjectEnd

objectSize = theObjectEnd - theObjectStart

;;; TODO: objects are in zero-page, but perhas dont need to be
.curr1 SKIP objectSize
.curr2 SKIP objectSize
.curr3 SKIP objectSize

;.allObjectsStart: EQUW curr1, curr2, curr3
;.allObjectsEnd

ORG &1900 ; could start at 1100 for loads of extra space!

.start:
    jmp main

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

;;;----------------------------------------------------------------------
;;; object focus/save...

.focusObject: {
    ldy #0
.loop:
    lda (theObj),y
    sta theObjectStart,y
    iny
    cpy #objectSize
    bne loop
    rts }

.saveObject: {
    ldy #0
.loop:
    lda theObjectStart,y
    sta (theObj),y
    iny
    cpy #objectSize
    bne loop
    rts }


;;;----------------------------------------------------------------------
;;; screen blitting...

maxPreparedObjects = 360

;;; template for 8-byte generated code segments to perform screen-eor-write
;;; op-codes for lda/eor/sta are fixed; generation fills the other 5 bytes
.blitCodeStart:
FOR n, 1, maxPreparedObjects
    lda &ffff
    eor #&ff
    sta &ffff
NEXT
.blitCodeEnd:
    rts

.resetDataPrepPtr:
    lda #LO(blitCodeStart) : sta ptr
    lda #HI(blitCodeStart) : sta ptr+1
    rts

.checkEnoughBlitSpace: {
    lda ptr+1 : cmp #HI(blitCodeEnd) : bcc ok : bne fail
    lda ptr   : cmp #LO(blitCodeEnd) : bcs fail
.ok:
    rts
.fail:
    lda #LO(msg) : sta mesPtr
    lda #HI(msg) : sta mesPtr+1
    jmp writeMessageAndSpin
.msg EQUS "Blit overflow.", 13, 0 }

.genCodeForScreenEor:
    ldy #4 : sta (ptr),y
    lda theA   : ldy #1 : sta (ptr),y : ldy #6 : sta (ptr),y
    lda theA+1 : ldy #2 : sta (ptr),y : ldy #7 : sta (ptr),y
    lda ptr : clc : adc #8 : sta ptr
    lda ptr+1     : adc #0 : sta ptr+1
    rts

;;; run the generated code
.blitScreen:
    lda rtsTemplate : ldy #0 : sta (ptr),y ; finalize generated-code with an rts
    jsr blitCodeStart ; execute generated code
    .ldaTemplate : lda ldaTemplate : ldy #0 : sta (ptr),y ; restore with lda
    .rtsTemplate : rts


;;;----------------------------------------------------------------------
;;; main loop

.main: {
    jsr setupMachine
    ;jsr drawGrid
    jsr initKeyVars
    jsr initCurr1
    jsr initCurr2
    jsr initCurr3

    jsr initialDraw
    jsr syncDelay
    jsr blitScreen

    sei
.loop:
    jsr saveLastKeys
    jsr readKeys
    lda keyEscape : bne escaped

    ;lda #2 : sta ula ; magenta
    jsr redraw
    ;lda #7 : sta ula ; black

    lda #3 : sta ula ; blue
    jsr syncDelay ; TODO : explore half speed
    ;jsr syncDelay ; TODO : explore half speed
    lda #7 : sta ula ; black

    lda #4 : sta ula ; yellow
    jsr blitScreen
    lda #7 : sta ula ; black

    jmp loop
.escaped:
    lda #LO(msg) : sta mesPtr
    lda #HI(msg) : sta mesPtr+1
    jmp writeMessageAndSpin
.msg EQUS "Escape pressed.", 13, 0
    }

;;;----------------------------------------------------------------------
;;; object movement calculation

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

.rightCoarse2:
    jsr rightCoarse
    jsr rightCoarse
    rts
.atPoint:
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


;;;----------------------------------------------------------------------
;;; object creation...

.initCurr1:
    lda #LO(move1) : sta theBehaviour
    lda #HI(move1) : sta theBehaviour+1
    lda #LO(spriteDataM) : sta theSpriteData
    lda #HI(spriteDataM) : sta theSpriteData+1
    lda #0 : sta theCX
    lda #0 : sta theFX
    lda #0 : sta theCY
    lda #0 : sta theFY
    lda #HI(screenStart) : sta theA+1
    lda #LO(screenStart) : sta theA
    lda #LO(curr1) : sta theObj
    lda #HI(curr1) : sta theObj+1
    jmp saveObject
    rts

.initCurr2: ; 19,17
    lda #LO(move2) : sta theBehaviour
    lda #HI(move2) : sta theBehaviour+1
    lda #LO(spriteDataM) : sta theSpriteData
    lda #HI(spriteDataM) : sta theSpriteData+1
    lda #4 : sta theCX
    lda #3 : sta theFX
    lda #2 : sta theCY
    lda #1 : sta theFY
    lda #HI(screenStart)+5 : sta theA+1
    lda #LO(screenStart)+32 : sta theA
    lda #LO(curr2) : sta theObj
    lda #HI(curr2) : sta theObj+1
    jmp saveObject
    rts

.initCurr3:
    lda #LO(move3) : sta theBehaviour
    lda #HI(move3) : sta theBehaviour+1
    lda #LO(spriteDataM) : sta theSpriteData
    lda #HI(spriteDataM) : sta theSpriteData+1
    lda #0 : sta theCX
    lda #0 : sta theFX
    lda #4 : sta theCY
    lda #0 : sta theFY
    lda #HI(screenStart)+10 : sta theA+1
    lda #LO(screenStart) : sta theA
    lda #LO(curr3) : sta theObj
    lda #HI(curr3) : sta theObj+1
    jmp saveObject
    rts


.move1:
    jsr onR : jsr onD
    rts

.move2:
    jsr onR : jsr onR : jsr onD
    rts

.move3:
    jsr onL : jsr onD : jsr onD
    ;jsr updateFocussedWithRepeat
    rts


;;;----------------------------------------------------------------------
;;; initial draw & redraw

.initialDraw:
    jsr resetDataPrepPtr
    lda #LO(curr1) : sta theObj
    lda #HI(curr1) : sta theObj+1
    jsr id1
    lda #LO(curr2) : sta theObj
    lda #HI(curr2) : sta theObj+1
    jsr id1
    lda #LO(curr3) : sta theObj
    lda #HI(curr3) : sta theObj+1
    jsr id1
    rts

.id1:
    jsr focusObject
    jsr plotObjectStrips
    rts


.redraw:
    jsr resetDataPrepPtr
    lda #LO(curr1) : sta theObj
    lda #HI(curr1) : sta theObj+1
    jsr ed1
    lda #LO(curr2) : sta theObj
    lda #HI(curr2) : sta theObj+1
    jsr ed1
    lda #LO(curr3) : sta theObj
    lda #HI(curr3) : sta theObj+1
    jsr ed1
    rts

.ed1:
    jsr focusObject
    jsr plotObjectStrips
    jsr focusObject
    jsr dispatchBehavior
    jsr saveObject
    jsr plotObjectStrips
    rts
.dispatchBehavior:
    jmp (theBehaviour)


;;;----------------------------------------------------------------------
;;; sprite plotting

.plotObjectStrips: {
    ldy #0
    lda (theSpriteData),y : sta pokeNumStrips+1
    iny
.loop:
    lda (theSpriteData),y : sta theStrip : iny
    lda (theSpriteData),y : sta theStrip+1 : iny
    .pokeNumStrips : cpy #&f
    beq done
    tya : pha
    jsr plotTheStrip
    jsr focusObject
    pla : tay
    jmp loop
.done:
    jsr plotTheStrip
    rts }


.plotTheStrip:
    jsr offsetPositionForTheStrip
    ldy #2 : lda (theStrip),y : sta pokeStripHeight+1
    lda theFX : asl a : tay : iny : iny
    iny : lda (theStrip),y : sta pokeSprite+1
    iny : lda (theStrip),y : sta pokeSprite+2
    jsr eorWrite
    rts

.offsetPositionForTheStrip:
    ldy #0 : lda (theStrip),y : sta funcPtr
    ldy #1 : lda (theStrip),y : sta funcPtr+1
    jmp (funcPtr)


.eorWrite:
    ldx #0
    lda theA : clc : adc theFY : sta theA
.plotLoop:
    jsr checkEnoughBlitSpace
    .pokeSprite : lda &BEEF,x
    jsr genCodeForScreenEor
    inc theA
    inc theFY
    lda theFY : cmp #8 : beq down : .afterDown
    inx
    .pokeStripHeight : cpx #&ff
    bne plotLoop
    rts
.down:
    lda theA : sec : sbc #8 : sta theA
    jsr downCoarse
    lda #0 : sta theFY
    jmp afterDown


;;;----------------------------------------------------------------------
;;; initial screen setup

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

.drawGrid: {
    lda #LO(screenStart) : sta gridPtr
    lda #HI(screenStart) : sta gridPtr+1
    ldy #0
.next:
    lda #&08
    sta (gridPtr),y
    clc
    lda gridPtr : adc #8 : sta gridPtr
    lda gridPtr+1 : adc #0 : sta gridPtr+1
    cmp #&80
    bmi next
    rts }


;;;----------------------------------------------------------------------
;;; sync with vblank

.syncDelay:
    lda #19 : jsr osbyte
    rts


;;;----------------------------------------------------------------------
;;; reading keyboard...

.initKeyVars:
    lda #0 : sta keyEscape
    lda #0 : sta keyU
    lda #1 : sta keyD
    lda #0 : sta keyL
    lda #1 : sta keyR
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


;;;----------------------------------------------------------------------
;;; sprite data...

.spriteData1: {
    EQUB 5 ; 2*num-strips+1
    EQUW stripA, stripB
.stripA: EQUW atPoint : EQUB 9 : EQUW stripA0, stripA1, stripA2, stripA3
.stripA0: EQUB &ff,&ff,&ff,&dd,&88,&dd,&ff,&ff,&ff
.stripA1: EQUB &77,&77,&77,&66,&44,&66,&77,&77,&77
.stripA2: EQUB &33,&33,&33,&33,&22,&33,&33,&33,&33
.stripA3: EQUB &11,&11,&11,&11,&11,&11,&11,&11,&11
.stripB: EQUW rightCoarse : EQUB 9 : EQUW stripB0, stripB1, stripB2, stripB3
.stripB0: EQUB &88,&88,&88,&88,&88,&88,&88,&88,&88
.stripB1: EQUB &cc,&cc,&cc,&cc,&44,&cc,&cc,&cc,&cc
.stripB2: EQUB &ee,&ee,&ee,&66,&22,&66,&ee,&ee,&ee
.stripB3: EQUB &ff,&ff,&ff,&bb,&11,&bb,&ff,&ff,&ff
}

.spriteData2: { EQUB 7 : EQUW stripA, stripB, stripC
.stripA: EQUW atPoint : EQUB 13 : EQUW stripA0, stripA1, stripA2, stripA3
.stripA0: EQUB &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff
.stripA1: EQUB &77,&77,&77,&77,&77,&77,&77,&77,&77,&77,&77,&77,&77
.stripA2: EQUB &33,&33,&33,&33,&33,&33,&33,&33,&33,&33,&33,&33,&33
.stripA3: EQUB &11,&11,&11,&11,&11,&11,&11,&11,&11,&11,&11,&11,&11
.stripB: EQUW rightCoarse : EQUB 13 : EQUW stripB0, stripB1, stripB2, stripB3
.stripB0: EQUB &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff
.stripB1: EQUB &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff
.stripB2: EQUB &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff
.stripB3: EQUB &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff
.stripC: EQUW rightCoarse2 : EQUB 13 : EQUW stripC0, stripC1, stripC2, stripC3
.stripC0: EQUB &88,&88,&88,&88,&88,&88,&88,&88,&88,&88,&88,&88,&88
.stripC1: EQUB &cc,&cc,&cc,&cc,&cc,&cc,&cc,&cc,&cc,&cc,&cc,&cc,&cc
.stripC2: EQUB &ee,&ee,&ee,&ee,&ee,&ee,&ee,&ee,&ee,&ee,&ee,&ee,&ee
.stripC3: EQUB &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff
}

.spriteData3: { EQUB 5 : EQUW stripA, stripB
.stripA: EQUW atPoint : EQUB 7 : EQUW stripA0, stripA1, stripA2, stripA3
.stripA0: EQUB &ff,&ff,&dd,&88,&dd,&ff,&ff
.stripA1: EQUB &77,&77,&66,&44,&66,&77,&77
.stripA2: EQUB &33,&33,&33,&22,&33,&33,&33
.stripA3: EQUB &11,&11,&11,&11,&11,&11,&11
.stripB: EQUW rightCoarse : EQUB 7 : EQUW stripB0, stripB1, stripB2, stripB3
.stripB0: EQUB &88,&88,&88,&88,&88,&88,&88
.stripB1: EQUB &cc,&cc,&cc,&44,&cc,&cc,&cc
.stripB2: EQUB &ee,&ee,&66,&22,&66,&ee,&ee
.stripB3: EQUB &ff,&ff,&bb,&11,&bb,&ff,&ff
}

.spriteDataM: { EQUB 7 : EQUW stripA, stripB, stripC
.stripA: EQUW atPoint : EQUB 8 : EQUW stripA0, stripA1, stripA2, stripA3
.stripA0: EQUB &33,&44,&88,&88,&44,&22,&44,&33
.stripA1: EQUB &11,&22,&44,&44,&22,&11,&22,&11
.stripA2: EQUB &00,&11,&22,&22,&11,&00,&11,&00
.stripA3: EQUB &00,&00,&11,&11,&00,&00,&00,&00
.stripB: EQUW rightCoarse : EQUB 8 : EQUW stripB0, stripB1, stripB2, stripB3
.stripB0: EQUB &66,&99,&11,&00,&00,&11,&99,&66
.stripB1: EQUB &bb,&44,&00,&00,&00,&00,&44,&bb
.stripB2: EQUB &dd,&22,&00,&00,&00,&88,&22,&dd
.stripB3: EQUB &66,&99,&00,&00,&88,&44,&99,&66
.stripC: EQUW rightCoarse2 : EQUB 8 : EQUW stripC0, stripC1, stripC2, stripC3
.stripC0: EQUB &00,&00,&00,&88,&88,&00,&00,&00
.stripC1: EQUB &00,&88,&88,&44,&44,&88,&88,&00
.stripC2: EQUB &88,&44,&44,&22,&22,&44,&44,&88
.stripC3: EQUB &cc,&22,&22,&11,&11,&22,&22,&cc
}


.end:
SAVE "Code", start, end
