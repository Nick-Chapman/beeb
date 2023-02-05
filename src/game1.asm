
ula = &fe21

osasci = &ffe3
oswrch = &ffee
osbyte = &fff4

keyCodeU = -58
keyCodeD = -42
keyCodeL = -26
keyCodeR = -122
keyCodeEscape = -113
keyCodeTab = -97

screenStart = &3000
screenEnd = &8000

ORG &70

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
.numDataObjects SKIP 1
.theStrip SKIP 2
.theObj SKIP 2

;;; object in focus...
.theObjectStart
.theSpriteData SKIP 2
.theCX SKIP 1 ; coarse-X : 0..79
.theCY SKIP 1 ; coarse-Y : 0..31
.theFX SKIP 1 ; fine-X   : 0..3
.theFY SKIP 1 ; fine-Y   : 0..7
.theA SKIP 2  ; screen address of the 8x8 char
.theObjectEnd

objectSize = theObjectEnd - theObjectStart

.selectedObj SKIP 1 ; using tab key

numObjects = 2

;;; objects dont need to be in zero-page
.curr1 SKIP objectSize
.last1 SKIP objectSize
.curr2 SKIP objectSize
.last2 SKIP objectSize
.curr3 SKIP objectSize
.last3 SKIP objectSize

dataPrep = &2000 ; 16 pages (4k) here before screen starts

ORG &1900
GUARD &1fc0
GUARD screenStart

.start:
    jmp main

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


.focusCurr1:
    lda #LO(curr1) : sta theObj
    lda #HI(curr1) : sta theObj+1
    jmp focusObject

.focusLast1:
    lda #LO(last1) : sta theObj
    lda #HI(last1) : sta theObj+1
    jmp focusObject

.saveCurr1:
    lda #LO(curr1) : sta theObj
    lda #HI(curr1) : sta theObj+1
    jmp saveObject

.saveLast1:
    lda #LO(last1) : sta theObj
    lda #HI(last1) : sta theObj+1
    jmp saveObject

.focusCurr2:
    lda #LO(curr2) : sta theObj
    lda #HI(curr2) : sta theObj+1
    jmp focusObject

.focusLast2:
    lda #LO(last2) : sta theObj
    lda #HI(last2) : sta theObj+1
    jmp focusObject

.saveCurr2:
    lda #LO(curr2) : sta theObj
    lda #HI(curr2) : sta theObj+1
    jmp saveObject

.saveLast2:
    lda #LO(last2) : sta theObj
    lda #HI(last2) : sta theObj+1
    jmp saveObject


.focusCurr3:
    lda #LO(curr3) : sta theObj
    lda #HI(curr3) : sta theObj+1
    jmp focusObject

.focusLast3:
    lda #LO(last3) : sta theObj
    lda #HI(last3) : sta theObj+1
    jmp focusObject

.saveCurr3:
    lda #LO(curr3) : sta theObj
    lda #HI(curr3) : sta theObj+1
    jmp saveObject

.saveLast3:
    lda #LO(last3) : sta theObj
    lda #HI(last3) : sta theObj+1
    jmp saveObject


.main: {
    jsr setupMachine
    jsr initVars
    jsr initCurr1
    jsr initCurr2
    jsr initCurr3
    jsr drawGrid

    jsr resetDataPrepPtr
    jsr prepDraw
    jsr blitScreen
    sei
.loop:
    jsr saveLastKeys
    jsr readKeys
    lda keyEscape : bne quit
    { lda keyTab : beq no : lda lastKeyTab : bne no : jsr onTab : .no }
    jsr saveLastScreenAddr
    jsr updateSelected

    jsr resetDataPrepPtr
    jsr prepErase
    jsr prepDraw

    lda #3 : sta ula ; blue
    jsr syncDelay
    lda #4 : sta ula ; yellow
    jsr blitScreen
    lda #7 : sta ula ; black
    jmp loop
.quit:
    rts
    }


.initVars:
    lda #0 : sta selectedObj
    lda #0 : sta keyTab
    lda #0 : sta keyEscape
    lda #0 : sta keyU
    lda #1 : sta keyD
    lda #0 : sta keyL
    lda #1 : sta keyR
    rts


.initCurr1:
    lda #LO(spriteData1) : sta theSpriteData
    lda #HI(spriteData1) : sta theSpriteData+1
    lda #0 : sta theCX
    lda #0 : sta theFX
    lda #0 : sta theCY
    lda #0 : sta theFY
    lda #HI(screenStart) : sta theA+1
    lda #LO(screenStart) : sta theA

    jsr saveCurr1
    rts

.initCurr2: ; 19,17
    lda #LO(spriteData2) : sta theSpriteData
    lda #HI(spriteData2) : sta theSpriteData+1
    lda #4 : sta theCX
    lda #3 : sta theFX
    lda #2 : sta theCY
    lda #1 : sta theFY
    lda #HI(screenStart)+5 : sta theA+1
    lda #LO(screenStart)+32 : sta theA
    jsr saveCurr2
    rts

.initCurr3:
    lda #LO(spriteData3) : sta theSpriteData
    lda #HI(spriteData3) : sta theSpriteData+1
    lda #0 : sta theCX
    lda #0 : sta theFX
    lda #4 : sta theCY
    lda #0 : sta theFY
    lda #HI(screenStart)+10 : sta theA+1
    lda #LO(screenStart) : sta theA
    jsr saveCurr3
    rts


.saveLastScreenAddr:
    jsr focusCurr1 : jsr saveLast1
    jsr focusCurr2 : jsr saveLast2
    jsr focusCurr3 : jsr saveLast3
    rts


.updateSelected:
    jsr focusSelected
    jsr updateFocussedWithRepeat
    jsr saveSelected
    rts

.focusSelected: {
    lda selectedObj : bne two
    jmp focusCurr2  : .two
    jmp focusCurr3 }

.saveSelected: {
    lda selectedObj : bne two
    jmp saveCurr2  : .two
    jmp saveCurr3 }


.saveLastKeys:
    lda keyTab : sta lastKeyTab
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

.onTab: {
    inc selectedObj
    lda selectedObj
    cmp #numObjects
    bne done
    lda #0 : sta selectedObj
.done:
    rts }

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


.prepErase:
    jsr drawStripsLast1
    jsr drawStripsLast2
    jsr drawStripsLast3
    rts

.prepDraw:
    jsr drawStripsCurr1
    jsr drawStripsCurr2
    jsr drawStripsCurr3
    rts


.drawStripsLast1:
    lda #LO(last1) : sta theObj
    lda #HI(last1) : sta theObj+1
    jsr drawStrips
    rts
.drawStripsLast2:
    lda #LO(last2) : sta theObj
    lda #HI(last2) : sta theObj+1
    jsr drawStrips
    rts
.drawStripsLast3:
    lda #LO(last3) : sta theObj
    lda #HI(last3) : sta theObj+1
    jsr drawStrips
    rts
.drawStripsCurr1:
    lda #LO(curr1) : sta theObj
    lda #HI(curr1) : sta theObj+1
    jsr drawStrips
    rts
.drawStripsCurr2:
    lda #LO(curr2) : sta theObj
    lda #HI(curr2) : sta theObj+1
    jsr drawStrips
    rts
.drawStripsCurr3:
    lda #LO(curr3) : sta theObj
    lda #HI(curr3) : sta theObj+1
    jsr drawStrips
    rts


.drawStrips: {
    jsr focusObject
    ldy #0
.loop:
    lda (theSpriteData),y : sta theStrip : iny
    lda (theSpriteData),y : sta theStrip+1 : iny
    cpy #4 ; twice num-strips (2 bytes per strip)
    beq done
    tya : pha
    jsr drawTheStrip
    jsr focusObject
    jsr rightCoarse ; this wont generalize to 3 strips! (3rd strips need to step right twice)
    pla : tay
    jmp loop
.done:
    jsr drawTheStrip
    rts }


.drawTheStrip:
    ldy #0 : lda (theStrip),y : sta pokeStripHeight+1
    lda theFX : asl a : tay
    iny : lda (theStrip),y : sta pokeSprite+1
    iny : lda (theStrip),y : sta pokeSprite+2
    jsr eorWrite
    rts


.eorWrite:
    ldx #0
    lda theA : clc : adc theFY : sta theA
.plotLoop:
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


.spriteData1: { EQUW stripA, stripB
.stripA: EQUB 9 : EQUW stripA0, stripA1, stripA2, stripA3
.stripA0: EQUB &ff,&ff,&ff,&dd,&88,&dd,&ff,&ff,&ff
.stripA1: EQUB &77,&77,&77,&66,&44,&66,&77,&77,&77
.stripA2: EQUB &33,&33,&33,&33,&22,&33,&33,&33,&33
.stripA3: EQUB &11,&11,&11,&11,&11,&11,&11,&11,&11
.stripB: EQUB 9 : EQUW stripB0, stripB1, stripB2, stripB3
.stripB0: EQUB &88,&88,&88,&88,&88,&88,&88,&88,&88
.stripB1: EQUB &cc,&cc,&cc,&cc,&44,&cc,&cc,&cc,&cc
.stripB2: EQUB &ee,&ee,&ee,&66,&22,&66,&ee,&ee,&ee
.stripB3: EQUB &ff,&ff,&ff,&bb,&11,&bb,&ff,&ff,&ff
}

.spriteData2: { EQUW stripA, stripB
.stripA: EQUB 13 : EQUW stripA0, stripA1, stripA2, stripA3
.stripA0: EQUB &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff
.stripA1: EQUB &77,&77,&77,&77,&77,&77,&77,&77,&77,&77,&77,&77,&77
.stripA2: EQUB &33,&33,&33,&33,&33,&33,&33,&33,&33,&33,&33,&33,&33
.stripA3: EQUB &11,&11,&11,&11,&11,&11,&11,&11,&11,&11,&11,&11,&11
.stripB: EQUB 13 : EQUW stripB0, stripB1, stripB2, stripB3
.stripB0: EQUB &88,&88,&88,&88,&88,&88,&88,&88,&88,&88,&88,&88,&88
.stripB1: EQUB &cc,&cc,&cc,&cc,&cc,&cc,&cc,&cc,&cc,&cc,&cc,&cc,&cc
.stripB2: EQUB &ee,&ee,&ee,&ee,&ee,&ee,&ee,&ee,&ee,&ee,&ee,&ee,&ee
.stripB3: EQUB &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff
}

.spriteData3: { EQUW stripA, stripB
.stripA: EQUB 7 : EQUW stripA0, stripA1, stripA2, stripA3
.stripA0: EQUB &ff,&ff,&dd,&88,&dd,&ff,&ff
.stripA1: EQUB &77,&77,&66,&44,&66,&77,&77
.stripA2: EQUB &33,&33,&33,&22,&33,&33,&33
.stripA3: EQUB &11,&11,&11,&11,&11,&11,&11
.stripB: EQUB 7 : EQUW stripB0, stripB1, stripB2, stripB3
.stripB0: EQUB &88,&88,&88,&88,&88,&88,&88
.stripB1: EQUB &cc,&cc,&cc,&44,&cc,&cc,&cc
.stripB2: EQUB &ee,&ee,&66,&22,&66,&ee,&ee
.stripB3: EQUB &ff,&ff,&bb,&11,&bb,&ff,&ff
}


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

maxPreparedObjects = 256

;;; prepare code generation for a new frame
.resetDataPrepPtr:
    lda #0 : sta numDataObjects
    lda #LO(dataPrep) : sta ptr
    lda #HI(dataPrep) : sta ptr+1
    rts

;;; code gen for screen-write: value to eor in A; screen-address in theA/+1
.genCodeForScreenEor:
    ldy #4 : sta (ptr),y
    lda theA   : ldy #1 : sta (ptr),y : ldy #6 : sta (ptr),y
    lda theA+1 : ldy #2 : sta (ptr),y : ldy #7 : sta (ptr),y
    inc numDataObjects
    lda ptr : clc : adc #8 : sta ptr
    lda ptr+1     : adc #0 : sta ptr+1
    rts

;;; run the generated code
.blitScreen:
    lda rtsTemplate : ldy #0 : sta (ptr),y ; finalize generated-code with an rts
    jsr dataPrep ; execute generated code
    .ldaTemplate : lda ldaTemplate : ldy #0 : sta (ptr),y ; restore with lda
    .rtsTemplate : rts

;;; templates for 256x 8-byte generated code segments.
;;; op-codes for lda/eor/sta are fixed; generation fills the other 5 bytes
ORG dataPrep
FOR n, 1, maxPreparedObjects
    lda &ffff
    eor #&ff
    sta &ffff
NEXT

.end:
SAVE "Code", start, end
