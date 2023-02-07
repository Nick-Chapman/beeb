
ula = &fe21

osasci = &ffe3
oswrch = &ffee
osbyte = &fff4

keyCodeU = -58
keyCodeD = -42
keyCodeL = -26
keyCodeR = -122
keyCodeTab = -97
keyCodeEscape = -113

screenStart = &3000
screenEnd = &8000

stack = &100

GUARD stack
GUARD screenStart

ORG &70

.frames SKIP 1 ; used for poor man's randomizing

.keyU SKIP 1
.keyD SKIP 1
.keyL SKIP 1
.keyR SKIP 1
.keyTab SKIP 1
.keyEscape SKIP 1

.lastKeyU SKIP 1
.lastKeyD SKIP 1
.lastKeyL SKIP 1
.lastKeyR SKIP 1
.lastKeyTab SKIP 1

.gridPtr SKIP 2
.ptr SKIP 2
.theStrip SKIP 2
.theObj SKIP 2
.funcPtr SKIP 2
.mesPtr SKIP 2

;;; the object in focus
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
;;; object focus/save

.curr1 SKIP objectSize
.curr2 SKIP objectSize
.curr3 SKIP objectSize
.curr4 SKIP objectSize

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
;;; screen blitting

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
    lda rtsTemplate : ldy #0 : sta (ptr),y ; finalize with rts
    jsr blitCodeStart ; dispatch to generated code
    .ldaTemplate : lda ldaTemplate : ldy #0 : sta (ptr),y ; restore lda
    .rtsTemplate : rts


;;;----------------------------------------------------------------------
;;; main loop

.main: {
    jsr setupMachine
    ;jsr drawGrid
    jsr initKeyVars
    lda #0 : sta frames
    jsr initCurr1
    jsr initCurr2
    jsr initCurr3
    jsr initCurr4

    jsr initialDraw
    jsr syncDelay
    jsr blitScreen

    sei
.loop:
    inc frames
    jsr saveLastKeys
    jsr readKeys
    lda keyEscape : bne escaped

    ;lda #2 : sta ula ; magenta
    jsr redraw
    ;lda #7 : sta ula ; black

    ;lda #3 : sta ula ; blue
    jsr syncDelay ; TODO : explore half speed
    jsr syncDelay ; TODO : explore half speed
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

.calculateAfromXY:
    ;; (coarse)X                    0..79
    ;; (coarse)Y                    0..31
    ;; hbOnRow = X/16               0..4
    ;; hi(A) = (5*Y+hbOnRow)/2+&30
    lda theCX : lsr a : lsr a : lsr a : lsr a
    sta hbOnRow+1
    lda theCY : asl a : asl a : clc : adc theCY
    .hbOnRow : adc #0
    lsr a
    clc : adc #HI(screenStart)
    sta theA+1
    ;; oddRow = Y % 2               0,1
    ;; Xoffset = oddRow * 16        0,16
    ;; Xmod = X ^ Xoffset           0..79
    ;; lo(A) = Xmod * 8
    lda theCY : and #1              ; oddRow
    asl a : asl a : asl a : asl a   ; Xoffset
    eor theCX                       ; Xmod
    asl a : asl a : asl a           ; Xmod*8
    sta theA
    rts

;;;----------------------------------------------------------------------
;;; object movement calculation

.nothing:
    rts

.right8:
    jsr right4
    jsr right4
    rts
.right12:
    jsr right4
    jsr right4
    jsr right4
    rts
.right16:
    jsr right4
    jsr right4
    jsr right4
    jsr right4
    rts

.up8:
    jsr unwrapScreen
    dec theCY
    jsr upA
    rts
.down8:
    inc theCY
    jsr downA
    jsr wrapScreen
    rts
.left4:
    jsr unwrapLine
    dec theCX
    jsr leftA
    rts
.right4:
    inc theCX
    jsr rightA
    jsr wrapLine
    rts

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


.up1: {
    lda theFY : bne no
    lda #8 : sta theFY
    jsr up8
.no:
    dec theFY
    rts }

.down1: {
    inc theFY
    lda theFY : cmp #8 : bne no
    lda #0 : sta theFY
    jsr down8
.no:
    rts }

.left1: {
    lda theFX : bne no
    jsr left4
    lda #4 : sta theFX
.no:
    dec theFX
    rts }

.right1: {
    inc theFX
    lda theFX : cmp #4 : bne no
    lda #0 : sta theFX
    jsr right4
.no:
    rts }

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

;;;----------------------------------------------------------------------
;;; object creation...

.initCurr1:
    lda #LO(move1) : sta theBehaviour
    lda #HI(move1) : sta theBehaviour+1
    lda #LO(mediumMeteor) : sta theSpriteData
    lda #HI(mediumMeteor) : sta theSpriteData+1
    lda #0 : sta theCX
    lda #0 : sta theFX
    lda #0 : sta theCY
    lda #0 : sta theFY
    jsr calculateAfromXY
    lda #LO(curr1) : sta theObj
    lda #HI(curr1) : sta theObj+1
    jmp saveObject

.initCurr2: ; 19,17
    lda #LO(move2) : sta theBehaviour
    lda #HI(move2) : sta theBehaviour+1
    lda #LO(shield5x9) : sta theSpriteData
    lda #HI(shield5x9) : sta theSpriteData+1
    lda #4 : sta theCX
    lda #3 : sta theFX
    lda #2 : sta theCY
    lda #1 : sta theFY
    jsr calculateAfromXY
    lda #LO(curr2) : sta theObj
    lda #HI(curr2) : sta theObj+1
    jmp saveObject

.initCurr3:
    lda #LO(move3) : sta theBehaviour
    lda #HI(move3) : sta theBehaviour+1
    lda #LO(smallMeteor) : sta theSpriteData
    lda #HI(smallMeteor) : sta theSpriteData+1
    lda #0 : sta theCX
    lda #0 : sta theFX
    lda #4 : sta theCY
    lda #0 : sta theFY
    jsr calculateAfromXY
    lda #LO(curr3) : sta theObj
    lda #HI(curr3) : sta theObj+1
    jmp saveObject

.initCurr4:
    lda #LO(move4) : sta theBehaviour
    lda #HI(move4) : sta theBehaviour+1
    lda #LO(block9x13) : sta theSpriteData
    lda #HI(block9x13) : sta theSpriteData+1
    lda #39 : sta theCX
    lda #0 : sta theFX
    lda #15 : sta theCY
    lda #0 : sta theFY
    jsr calculateAfromXY
    lda #LO(curr4) : sta theObj
    lda #HI(curr4) : sta theObj+1
    jmp saveObject

.move1:
    jsr right1 : jsr up1 : jsr up1
    rts
.move2:
    jsr right1 : jsr right1 : jsr down1
    rts
.move3:
    jsr left1 : jsr down1 : jsr down1
    rts
.move4:
    { lda keyTab : beq no : lda lastKeyTab : bne no : jmp randomizePos : .no }
    ;{ lda keyTab : beq no : jmp randomizePos : .no }
    jsr right1 : jsr down1
    ;jsr onArrowWithRepeat
    rts

.randomizePos:
    jsr getRandom : and #63 : sta theCX ; really should be %80
    jsr getRandom : and #3 : sta theFX
    jsr getRandom : and #31 : sta theCY
    jsr getRandom : and #7 : sta theFY
    jsr calculateAfromXY
    rts

.getRandom:
    inc frames
    ldx frames
    lda randomBytes,x
    rts

;;;----------------------------------------------------------------------
;;; draw & redraw

.allObjectsStart: EQUW curr1, curr2, curr3, curr4 ; LIST OBJECTS HERE
;.allObjectsStart: EQUW curr4
.allObjectsEnd
numberObjects = (allObjectsEnd - allObjectsStart) DIV 2

.initialDraw: {
    jsr resetDataPrepPtr
    ldx #0
.loop:
    lda allObjectsStart,x : sta theObj
    lda allObjectsStart+1,x : sta theObj+1
    txa : pha
    jsr focusObject
    jsr plotObjectStrips
    pla : tax
    inx : inx
    cpx #(2 * numberObjects) : bne loop
    rts }

.redraw: {
    jsr resetDataPrepPtr
    ldx #0
.loop:
    lda allObjectsStart,x : sta theObj
    lda allObjectsStart+1,x : sta theObj+1
    txa : pha
    jsr ed1
    pla : tax
    inx : inx
    cpx #(2 * numberObjects) : bne loop
    rts }

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

.plotTheStrip: {
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
    beq nogen : jsr genCodeForScreenEor : .nogen
    inc theA
    inc theFY
    lda theFY : cmp #8 : beq down : .afterDown
    inx
    .pokeStripHeight : cpx #&ff
    bne plotLoop
    rts
.down:
    lda theA : sec : sbc #8 : sta theA
    jsr down8
    lda #0 : sta theFY
    jmp afterDown }

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
;;; keyboard

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
    lda keyTab : sta lastKeyTab
    rts

.readKeys:
    jsr checkU
    jsr checkD
    jsr checkL
    jsr checkR
    jsr checkTab
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

.checkTab: {
    lda #0 : sta keyTab
    lda #&81
    ldx #(keyCodeTab AND &ff)
    ldy #(keyCodeTab AND &ff00) DIV 256
    jsr osbyte
    cpx #&ff : bne no
    lda #1 : sta keyTab : .no:
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

.onArrow:
    { lda keyU : beq no : lda lastKeyU : bne no : jsr up1 : .no }
    { lda keyD : beq no : lda lastKeyD : bne no : jsr down1 : .no }
    { lda keyL : beq no : lda lastKeyL : bne no : jsr left1 : .no }
    { lda keyR : beq no : lda lastKeyR : bne no : jsr right1 : .no }
    rts

.onArrowWithRepeat:
    { lda keyU : beq no : jsr up1 : .no }
    { lda keyD : beq no : jsr down1 : .no }
    { lda keyL : beq no : jsr left1 : .no }
    { lda keyR : beq no : jsr right1 : .no }
    rts

;;;----------------------------------------------------------------------
;;; sprite data

.shield5x7: { EQUB 5 : EQUW A, B
.A: EQUW nothing : EQUB 7 : EQUW A0, A1, A2, A3
.A0: EQUB &ff,&ff,&dd,&88,&dd,&ff,&ff
.A1: EQUB &77,&77,&66,&44,&66,&77,&77
.A2: EQUB &33,&33,&33,&22,&33,&33,&33
.A3: EQUB &11,&11,&11,&11,&11,&11,&11
.B: EQUW right4 : EQUB 7 : EQUW B0, B1, B2, B3
.B0: EQUB &88,&88,&88,&88,&88,&88,&88
.B1: EQUB &cc,&cc,&cc,&44,&cc,&cc,&cc
.B2: EQUB &ee,&ee,&66,&22,&66,&ee,&ee
.B3: EQUB &ff,&ff,&bb,&11,&bb,&ff,&ff
}

.shield5x9: { EQUB 5 : EQUW A, B
.A: EQUW nothing : EQUB 9 : EQUW A0, A1, A2, A3
.A0: EQUB &ff,&ff,&ff,&dd,&88,&dd,&ff,&ff,&ff
.A1: EQUB &77,&77,&77,&66,&44,&66,&77,&77,&77
.A2: EQUB &33,&33,&33,&33,&22,&33,&33,&33,&33
.A3: EQUB &11,&11,&11,&11,&11,&11,&11,&11,&11
.B: EQUW right4 : EQUB 9 : EQUW B0, B1, B2, B3
.B0: EQUB &88,&88,&88,&88,&88,&88,&88,&88,&88
.B1: EQUB &cc,&cc,&cc,&cc,&44,&cc,&cc,&cc,&cc
.B2: EQUB &ee,&ee,&ee,&66,&22,&66,&ee,&ee,&ee
.B3: EQUB &ff,&ff,&ff,&bb,&11,&bb,&ff,&ff,&ff
}

.block9x13: { EQUB 7 : EQUW A, B, C
.A: EQUW nothing : EQUB 13 : EQUW A0, A1, A2, A3
.A0: EQUB &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff
.A1: EQUB &77,&77,&77,&77,&77,&77,&77,&77,&77,&77,&77,&77,&77
.A2: EQUB &33,&33,&33,&33,&33,&33,&33,&33,&33,&33,&33,&33,&33
.A3: EQUB &11,&11,&11,&11,&11,&11,&11,&11,&11,&11,&11,&11,&11
.B: EQUW right4 : EQUB 13 : EQUW B0, B1, B2, B3
.B0: EQUB &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff
.B1: EQUB &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff
.B2: EQUB &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff
.B3: EQUB &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff
.C: EQUW right8 : EQUB 13 : EQUW C0, C1, C2, C3
.C0: EQUB &88,&88,&88,&88,&88,&88,&88,&88,&88,&88,&88,&88,&88
.C1: EQUB &cc,&cc,&cc,&cc,&cc,&cc,&cc,&cc,&cc,&cc,&cc,&cc,&cc
.C2: EQUB &ee,&ee,&ee,&ee,&ee,&ee,&ee,&ee,&ee,&ee,&ee,&ee,&ee
.C3: EQUB &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff
}

.smallMeteor: { EQUB 7 : EQUW A, B, C
.A: EQUW nothing : EQUB 8 : EQUW A0, A1, A2, A3
.A0: EQUB &33,&44,&88,&88,&44,&22,&44,&33
.A1: EQUB &11,&22,&44,&44,&22,&11,&22,&11
.A2: EQUB &00,&11,&22,&22,&11,&00,&11,&00
.A3: EQUB &00,&00,&11,&11,&00,&00,&00,&00
.B: EQUW right4 : EQUB 8 : EQUW B0, B1, B2, B3
.B0: EQUB &66,&99,&11,&00,&00,&11,&99,&66
.B1: EQUB &bb,&44,&00,&00,&00,&00,&44,&bb
.B2: EQUB &dd,&22,&00,&00,&00,&88,&22,&dd
.B3: EQUB &66,&99,&00,&00,&88,&44,&99,&66
.C: EQUW right8 : EQUB 8 : EQUW C0, C1, C2, C3
.C0: EQUB &00,&00,&00,&88,&88,&00,&00,&00
.C1: EQUB &00,&88,&88,&44,&44,&88,&88,&00
.C2: EQUB &88,&44,&44,&22,&22,&44,&44,&88
.C3: EQUB &cc,&22,&22,&11,&11,&22,&22,&cc
}

.mediumMeteor: { EQUB 11 : EQUW A, B, C, D, E
.A: EQUW nothing : EQUB 16 : EQUW A0, A1, A2, A3
.A0: EQUB &00,&00,&11,&22,&44,&88,&88,&88,&44,&22,&11,&22,&44,&22,&11,&00
.A1: EQUB &00,&00,&00,&11,&22,&44,&44,&44,&22,&11,&00,&11,&22,&11,&00,&00
.A2: EQUB &00,&00,&00,&00,&11,&22,&22,&22,&11,&00,&00,&00,&11,&00,&00,&00
.A3: EQUB &00,&00,&00,&00,&00,&11,&11,&11,&00,&00,&00,&00,&00,&00,&00,&00
.B: EQUW right4 : EQUB 16 : EQUW B0, B1, B2, B3
.B0: EQUB &11,&22,&cc,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&ff
.B1: EQUB &00,&11,&ee,&00,&00,&00,&00,&00,&00,&00,&88,&00,&00,&00,&88,&77
.B2: EQUB &00,&00,&77,&88,&00,&00,&00,&00,&00,&88,&44,&88,&00,&88,&44,&33
.B3: EQUB &00,&00,&33,&44,&88,&00,&00,&00,&88,&44,&22,&44,&88,&44,&22,&11
.C: EQUW right8 : EQUB 16 : EQUW C0, C1, C2, C3
.C0: EQUB &ff,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&44,&aa,&11
.C1: EQUB &ff,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&22,&55,&88
.C2: EQUB &77,&88,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&11,&22,&cc
.C3: EQUB &33,&44,&88,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&11,&ee
.D: EQUW right12 : EQUB 16 : EQUW D0, D1, D2, D3
.D0: EQUB &00,&88,&66,&11,&00,&11,&22,&44,&22,&11,&00,&00,&00,&11,&22,&cc
.D1: EQUB &88,&44,&33,&00,&00,&00,&11,&22,&11,&00,&00,&00,&00,&00,&11,&ee
.D2: EQUB &cc,&22,&11,&00,&00,&00,&00,&11,&00,&00,&00,&00,&00,&00,&88,&77
.D3: EQUB &ee,&11,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&88,&44,&33
.E: EQUW right16 : EQUB 16 : EQUW E0, E1, E2, E3
.E0: EQUB &00,&00,&00,&00,&88,&00,&00,&00,&00,&00,&88,&88,&88,&00,&00,&00
.E1: EQUB &00,&00,&00,&88,&44,&88,&00,&00,&00,&88,&44,&44,&44,&88,&00,&00
.E2: EQUB &00,&00,&88,&44,&22,&44,&88,&00,&88,&44,&22,&22,&22,&44,&88,&00
.E3: EQUB &00,&00,&cc,&22,&11,&22,&44,&88,&44,&22,&11,&11,&11,&22,&44,&88
}

.randomBytes:
EQUB &22,&52,&6a,&51,&a7,&35,&26,&bc,&ce,&54,&e8,&56,&60,&af,&45,&04
EQUB &ce,&65,&54,&70,&df,&d4,&36,&b1,&7c,&0f,&0d,&dd,&1f,&66,&bd,&98
EQUB &7e,&a0,&8e,&36,&27,&5a,&9b,&31,&7e,&70,&48,&65,&6f,&39,&45,&60
EQUB &db,&4f,&fb,&ba,&e4,&7a,&a7,&a7,&96,&f0,&b0,&e6,&a8,&e9,&99,&bb
EQUB &10,&6f,&28,&02,&dc,&79,&bc,&b3,&18,&18,&81,&cc,&bb,&b3,&e0,&ff
EQUB &8b,&4f,&11,&e0,&f2,&1b,&ff,&7a,&ee,&37,&c5,&ca,&9d,&57,&ba,&c4
EQUB &cd,&65,&b5,&43,&f7,&5c,&82,&10,&d2,&8c,&5e,&b0,&c5,&aa,&c6,&1a
EQUB &bd,&a4,&3a,&f7,&37,&0f,&5c,&5f,&63,&61,&93,&0a,&05,&54,&21,&7a
EQUB &b2,&c3,&fe,&3f,&74,&a6,&5c,&3e,&ca,&1b,&5c,&26,&57,&ef,&01,&32
EQUB &f9,&ff,&82,&b4,&ee,&df,&7c,&d6,&2f,&f2,&e5,&20,&84,&3b,&a6,&d0
EQUB &ac,&2a,&88,&c3,&9b,&01,&81,&9f,&a5,&3a,&c4,&fa,&fc,&6d,&d4,&46
EQUB &e2,&f6,&7d,&39,&63,&0a,&97,&6d,&b9,&9a,&97,&71,&f8,&ea,&ff,&7f
EQUB &85,&bc,&88,&06,&3b,&30,&c5,&3f,&33,&3a,&67,&d7,&a7,&f7,&f7,&83
EQUB &8f,&f6,&ae,&f1,&1f,&07,&3b,&a6,&0b,&3b,&b3,&9b,&f9,&f4,&67,&fe
EQUB &1a,&c0,&ce,&41,&cc,&26,&13,&b8,&64,&c0,&77,&42,&00,&9f,&63,&e2
EQUB &70,&3b,&a5,&0d,&f2,&13,&e8,&72,&9b,&e0,&ad,&7e,&aa,&8e,&d0,&f5
.randomBytesEnd:
ASSERT ((randomBytesEnd-randomBytes) = 256)

.end:
SAVE "Code", start, end
