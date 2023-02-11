ula = &fe21
osasci = &ffe3
oswrch = &ffee
osbyte = &fff4

screenStart = &3000
screenEnd = &8000
stack = &100

maxObjects = 8

GUARD &90 ; keep an eye on zeropage space usage
GUARD stack
ORG &70

.hitFlags SKIP maxObjects

.theA SKIP 2
.msgPtr SKIP 2

.overwritePtr SKIP 2
.xorplotPtr SKIP 2
.hitplotPtr SKIP 2

.erasePtr SKIP 2
.eraseRunPtr SKIP 2
.eraseSwitcher SKIP 2

GUARD &2800 ; keep an eye on overall code size
GUARD screenStart
ORG &1900

macro copy16i I,V
    lda #LO(I) : sta V
    lda #HI(I) : sta V+1
endmacro

macro copy16v A,B
    lda A : sta B
    lda A+1 : sta B+1
endmacro

macro STOP N
    lda #N : jsr stop
endmacro

.start:
    jmp main

;----------------------------------------------------------------------
; main loop

.main: {
    sei
    jsr setupMachine
    jsr eraseInit
    jsr initHitFlags
.loop:
    ;lda #3 : sta ula ; blue (prepare time)
    jsr prepareScene
    ;jsr prepareScene ; idempoent
    jsr updateScene
    lda #7 : sta ula ; black

    jsr syncDelay

    lda #2 : sta ula ; magenta (shows we missed vblank)
    jsr blitScene
    jsr blitScene ; idempoent ; magenta will show 50% vlank used
    lda #7 : sta ula ; black

    jsr eraseFlip
    jmp loop
    rts }

.blitScene:
    jsr eraseRun
    jsr overwriteRun
    jsr xorplotRun
    jsr hitplotRun
    rts

.examplePos SKIP 1 ; for first example drive

.updateScene:
    jsr checkHitFlags
    inc examplePos
    rts

.prepareScene:
    jsr overwriteReInit
    jsr xorplotReInit
    jsr hitplotReInit
    ;; two fixed in place bars
    copy16i &4033, theA : lda #&01 : jsr overwriteGen : jsr eraseGen ; red dot
    copy16i &4053, theA : lda #&11 : jsr overwriteGen : jsr eraseGen ; cyn dot
    copy16i &4073, theA : lda #&02 : jsr overwriteGen : jsr eraseGen ; red dot
    ;; moving cyan block
    ldx #32
    jsr eraseGen
.SPLAT
{ .loop:
    txa
    clc : adc examplePos
    sta theA : lda #&40 : sta theA+1
    lda #&ee : jsr hitplotGen : jsr eraseGen
    dex
    bne loop }
    rts

;----------------------------------------------------------------------
; hit-flags

.initHitFlags: {
    lda #0
    ldx #0
.loop:
    sta hitFlags,x
    inx
    cpx #maxObjects
    bne loop
    rts }

.checkHitFlags: {
    ldx #0
.loop:
    lda hitFlags,x : bne isHit
    inx
    cpx #maxObjects
    bne loop
    rts
.isHit:
    txa
    jsr printHexA
    copy16i msg, msgPtr
    jmp printMessageAndSpin
.msg EQUS "Object Hit.", 13, 0
    }

;----------------------------------------------------------------------
; misc: stop, sync, print stuff

.stop: { ; byte in Acc
    jsr printHexA
    copy16i msg, msgPtr
    jmp printMessageAndSpin
    .msg EQUS "Stop", 13, 0 }

.syncDelay:
    lda #19 : jsr osbyte
    rts

.printMessageAndSpin: {
    ldy #0
.loop
    lda (msgPtr),y
    beq spin
    jsr osasci
    iny
    bne loop
.spin:
    jmp spin }

.printHexA: {
    pha : lda #'[' : jsr osasci : pla
    pha
    and #&f0 : lsr a : lsr a : lsr a : lsr a : tax
    lda digits,x
    jsr osasci
    pla
    and #&f : tax
    lda digits,x
    jsr osasci
    pha : lda #']' : jsr osasci : pla
    rts
.digits EQUS "0123456789abcdef" }

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

;----------------------------------------------------------------------
; dual-space erase

.eraseRun:
    lda #&0 ;50 ; yellow for dev
    jmp (eraseRunPtr)

eraseNumberBlocks = 50
macro eraseTemplate
    sta SPLAT ; SCREEN-ADDRESS(1,2)
endmacro

.eraseSpaceA:
eraseTemplate ; one copy for size
eraseBlockSize = *-eraseSpaceA
FOR i, 1, eraseNumberBlocks-1 : eraseTemplate : NEXT
.eraseSpaceA_End:
    rts
.eraseSpaceB:
FOR i, 1, eraseNumberBlocks : eraseTemplate : NEXT
.eraseSpaceB_End:
    rts

.eraseGen:
    ;; decrement ptr one block
    lda erasePtr : sec : sbc #eraseBlockSize : sta erasePtr
    { bcs noHiDec : dec erasePtr+1 : .noHiDec }
    ;; check we are within the space
    jsr eraseSpaceCheck
    ;; fill in the generated code
    lda theA     : ldy #1 : sta (erasePtr),y
    lda theA+1   : ldy #2 : sta (erasePtr),y
    rts

.eraseSpaceCheck: {
    lda erasePtr+1 : cmp eraseSpace+1 : bcc fail : bne ok
    lda erasePtr   : cmp eraseSpace   : bcc fail
.ok:
    rts
.fail:
    copy16i msg, msgPtr
    jmp printMessageAndSpin
.msg EQUS "Erase Overflow", 13, 0 }

.eraseFlip:
    jmp (eraseSwitcher)
    rts ; todo: rm

.eraseSpace SKIP 2

.eraseInit:
    copy16i eraseSpaceB_End, erasePtr
.eraseSwitcherA:
    copy16v erasePtr, eraseRunPtr
    copy16i eraseSpaceA, eraseSpace
    copy16i eraseSpaceA_End, erasePtr
    copy16i eraseSwitcherB, eraseSwitcher
    rts

.eraseSwitcherB:
    copy16v erasePtr, eraseRunPtr
    copy16i eraseSpaceB, eraseSpace
    copy16i eraseSpaceB_End, erasePtr
    copy16i eraseSwitcherA, eraseSwitcher
    rts

;----------------------------------------------------------------------
; overwrite

.overwriteRun:
    jmp (overwritePtr)

overwriteNumberBlocks = 50
macro overwriteTemplate
    lda #&ff ; DATA-BYTE(1)
    sta SPLAT ; SCREEN-ADDRESS(3,4)
endmacro

.overwriteSpace:
overwriteTemplate ; one copy for size
overwriteBlockSize = *-overwriteSpace
FOR i, 1, overwriteNumberBlocks-1 : overwriteTemplate : NEXT
.overwriteSpaceEnd:
    rts

.overwriteGen: ; byte to overwrite in ACC
    {sta DB+1 ; save byte to overwrite until after the ptr decrement
    ;; decrement ptr one block
    lda overwritePtr : sec : sbc #overwriteBlockSize : sta overwritePtr
    { bcs noHiDec : dec overwritePtr+1 : .noHiDec }
    ;; check we are within the space
    jsr overwriteSpaceCheck
    ;; fill in the generated code
    .DB}: lda #0 : ldy #1 : sta (overwritePtr),y
    lda theA     : ldy #3 : sta (overwritePtr),y
    lda theA+1   : ldy #4 : sta (overwritePtr),y
    rts

.overwriteSpaceCheck: {
    lda overwritePtr+1 : cmp #HI(overwriteSpace) : bcc fail : bne ok
    lda overwritePtr   : cmp #LO(overwriteSpace) : bcc fail
.ok:
    rts
.fail:
    copy16i msg, msgPtr
    jmp printMessageAndSpin
.msg EQUS "Overwrite Overflow", 13, 0 }

.overwriteReInit:
    copy16i overwriteSpaceEnd, overwritePtr
    rts

;----------------------------------------------------------------------
; xorplot

.xorplotRun:
    jmp (xorplotPtr)

xorplotNumberBlocks = 50
macro xorplotTemplate
    lda SPLAT ; SCREEN-ADDRESS(1,2)
    eor #&ff  ; DATA-BYTE(4)
    sta SPLAT ; SCREEN-ADDRESS(6,7)
endmacro

.xorplotSpace:
xorplotTemplate ; one copy for size
xorplotBlockSize = *-xorplotSpace
FOR i, 1, xorplotNumberBlocks-1 : xorplotTemplate : NEXT
.xorplotSpaceEnd:
    rts

.xorplotGen: ; byte to xorplot in ACC
    {sta DB+1 ; save byte to xorplot until after the ptr decrement
    ;; decrement ptr one block
    lda xorplotPtr : sec : sbc #xorplotBlockSize : sta xorplotPtr
    { bcs noHiDec : dec xorplotPtr+1 : .noHiDec }
    ;; check we are within the space
    jsr xorplotSpaceCheck
    ;; fill in the generated code
    .DB}: lda #0 : ldy #4 : sta (xorplotPtr),y
    lda theA     : ldy #1 : sta (xorplotPtr),y : ldy #6 : sta (xorplotPtr),y
    lda theA+1   : ldy #2 : sta (xorplotPtr),y : ldy #7 : sta (xorplotPtr),y
    rts

.xorplotSpaceCheck: {
    lda xorplotPtr+1 : cmp #HI(xorplotSpace) : bcc fail : bne ok
    lda xorplotPtr   : cmp #LO(xorplotSpace) : bcc fail
.ok:
    rts
.fail:
    copy16i msg, msgPtr
    jmp printMessageAndSpin
.msg EQUS "Xorplot Overflow", 13, 0 }

.xorplotReInit:
    copy16i xorplotSpaceEnd, xorplotPtr
    rts

;----------------------------------------------------------------------
; hitplot -- detect collision (with red) and set ZP var

.hitplotRun:
    jmp (hitplotPtr)

hitplotNumberBlocks = 50
macro hitplotTemplate
    lda SPLAT;ScreenAddr(1,2)  |  0  |  3 |  4us
    tay;                       |  3  |  1 |
    and #&ff ;DataByte(5)      |  4  |  2 |  2us
    tax;                       |  6  |  1 |  2us
    lda hitTableR,x;           |  7  |  3 |  4us
    beq noHit;                 | 10  |  2 |  3us (noHit-taken), 2us(hit-not)
    sta hitFlags;HitFlag(13)   | 12  |  2 |  3us (hit)
    .noHit
    tya;                       | 14  |  1 |
    eor #&ff ;DataByte(16)     | 15  |  2 |  2us
    sta SPLAT;ScreenAddr(18,19)| 17  |  3 |  4us
endmacro;                      | 20

.hitplotSpace:
hitplotTemplate ; one copy for size
hitplotBlockSize = *-hitplotSpace
ASSERT hitplotBlockSize = 20
FOR i, 1, hitplotNumberBlocks-1 : hitplotTemplate : NEXT
.hitplotSpaceEnd:
    rts

.hitplotGen: ; byte to hitplot in ACC
    {sta DB+1 ; save byte to hitplot until after the ptr decrement
    ;; decrement ptr one block
    lda hitplotPtr : sec : sbc #hitplotBlockSize : sta hitplotPtr
    { bcs noHiDec : dec hitplotPtr+1 : .noHiDec }
    ;; check we are within the space
    jsr hitplotSpaceCheck
    ;; fill in the generated code
    .DB}: lda #0 : ldy #5 : sta (hitplotPtr),y : ldy #16 : sta (hitplotPtr),y
    lda theA     : ldy #1 : sta (hitplotPtr),y : ldy #18 : sta (hitplotPtr),y
    lda theA+1   : ldy #2 : sta (hitplotPtr),y : ldy #19 : sta (hitplotPtr),y
    rts

.hitplotSpaceCheck: {
    lda hitplotPtr+1 : cmp #HI(hitplotSpace) : bcc fail : bne ok
    lda hitplotPtr   : cmp #LO(hitplotSpace) : bcc fail
.ok:
    rts
.fail:
    copy16i msg, msgPtr
    jmp printMessageAndSpin
.msg EQUS "Hitplot Overflow", 13, 0 }

.hitplotReInit:
    copy16i hitplotSpaceEnd, hitplotPtr
    rts

ALIGN &100
.nohitTable:
FOR i, 1, 256 : EQUB 0 : NEXT
.nohitTableEnd:
ASSERT ((nohitTableEnd-nohitTable) = 256)

x = 1
ALIGN &100
.hitTableY: ; if one of the 4 pixels is yellow
    EQUB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    EQUB x,0,x,0,x,0,x,0,x,0,x,0,x,0,x,0
    EQUB x,x,0,0,x,x,0,0,x,x,0,0,x,x,0,0
    EQUB x,x,x,0,x,x,x,0,x,x,x,0,x,x,x,0
    EQUB x,x,x,x,0,0,0,0,x,x,x,x,0,0,0,0
    EQUB x,x,x,x,x,0,x,0,x,x,x,x,x,0,x,0
    EQUB x,x,x,x,x,x,0,0,x,x,x,x,x,x,0,0
    EQUB x,x,x,x,x,x,x,0,x,x,x,x,x,x,x,0
    EQUB x,x,x,x,x,x,x,x,0,0,0,0,0,0,0,0
    EQUB x,x,x,x,x,x,x,x,x,0,x,0,x,0,x,0
    EQUB x,x,x,x,x,x,x,x,x,x,0,0,x,x,0,0
    EQUB x,x,x,x,x,x,x,x,x,x,x,0,x,x,x,0
    EQUB x,x,x,x,x,x,x,x,x,x,x,x,0,0,0,0
    EQUB x,x,x,x,x,x,x,x,x,x,x,x,x,0,x,0
    EQUB x,x,x,x,x,x,x,x,x,x,x,x,x,x,0,0
    EQUB x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,0
.hitTableYEnd:
ASSERT ((hitTableYEnd-hitTableY) = 256)

ALIGN &100
.hitTableR: ; if one of the 4 pixels is red
    EQUB 0,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x
    EQUB 0,0,x,x,x,x,x,x,x,x,x,x,x,x,x,x
    EQUB 0,x,0,x,x,x,x,x,x,x,x,x,x,x,x,x
    EQUB 0,0,0,0,x,x,x,x,x,x,x,x,x,x,x,x
    EQUB 0,x,x,x,0,x,x,x,x,x,x,x,x,x,x,x
    EQUB 0,0,x,x,0,0,x,x,x,x,x,x,x,x,x,x
    EQUB 0,x,0,x,0,x,0,x,x,x,x,x,x,x,x,x
    EQUB 0,0,0,0,0,0,0,0,x,x,x,x,x,x,x,x
    EQUB 0,x,x,x,x,x,x,x,0,x,x,x,x,x,x,x
    EQUB 0,0,x,x,x,x,x,x,0,0,x,x,x,x,x,x
    EQUB 0,x,0,x,x,x,x,x,0,x,0,x,x,x,x,x
    EQUB 0,0,0,0,x,x,x,x,0,0,0,0,x,x,x,x
    EQUB 0,x,x,x,0,x,x,x,0,x,x,x,0,x,x,x
    EQUB 0,0,x,x,0,0,x,x,0,0,x,x,0,0,x,x
    EQUB 0,x,0,x,0,x,0,x,0,x,0,x,0,x,0,x
    EQUB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.hitTableREnd:
ASSERT ((hitTableREnd-hitTableR) = 256)

;----------------------------------------------------------------------
.end:
SAVE "Code", start, end
