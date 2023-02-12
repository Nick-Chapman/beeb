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

.badHit SKIP 1

.theA SKIP 2
.msgPtr SKIP 2

.overwritePtr SKIP 2
.hitplotPtr SKIP 2

.erasePtr SKIP 2
.eraseRunPtr SKIP 2
.eraseSwitcher SKIP 2

.hitFlags SKIP maxObjects

GUARD &2e00 ; keep an eye on overall code size
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
    lda #0 : sta badHit
.loop:
    { lda badHit : beq noBadHit : STOP &44 : .noBadHit }

    ;lda #3 : sta ula ; blue (prepare time)
    jsr prepScene
    ;jsr prepScene ; idempoent
    lda #7 : sta ula ; black

    jsr syncDelay

    lda #2 : sta ula ; magenta (shows we missed vblank)
    jsr blitScene
    jsr blitScene ; idempoent ; magenta will show 50% vlank used
    lda #7 : sta ula ; black

    jsr eraseFlip
    inc frames
    jmp loop
    rts }

.blitScene:
    jsr eraseRun
    jsr overwriteRun
    jsr hitplotRun
    rts

;----------------------------------------------------------------------
; update/prepare -- NEW with game logic

.prepScene:
    jsr prepInit
    jsr prepBullets
    jsr prepRocks
    rts

.prepInit:
    jsr overwriteReInit
    jsr hitplotReInit
    rts

;----------------------------------------------------------------------
; bullets

numBullets = 1
.bulletAlive : SKIP numBullets ; dead
.bulletTimer : SKIP numBullets ; come alive instantly; reset when die
.bulletPos : SKIP numBullets ; set when spawn
.bulletNum: SKIP 1

.prepBullets: {
    lda #(numBullets-1) : sta bulletNum
.loop:
    jsr updateBullet
    jsr prepBullet
    dec bulletNum
    bpl loop
    rts }

.updateBullet: {
    ldx bulletNum
    dec bulletTimer,x
    lda bulletTimer,x
    bmi change
    rts
.change:
    lda bulletAlive,x
    beq spawn
.die:
    lda #0 : sta bulletAlive,x ; die
    jsr getRandom : and #31 : sta bulletTimer,x ; respawn shortly
    rts
.spawn:
    lda #1 : sta bulletAlive,x ; spawn
    lda #100 : sta bulletTimer,x ; alive for 2s
    jsr getRandom : sta bulletPos,x ; randomize position
    rts
}

.prepBullet: {
    ldx bulletNum
    lda bulletAlive,x : beq noplot
    lda bulletPos,x : sta theA : lda #&40 : sta theA+1 ; where
    lda #&01 : jsr overwriteGen : jsr eraseGen ; red dot (which will hit)
    .noplot : rts }

;----------------------------------------------------------------------
; rocks

numRocks = 5
.rockAlive:
FOR i, 1, numRocks : EQUB 1 : NEXT
ASSERT *-rockAlive = numRocks
.rockPos :
FOR i, 0, numRocks-1 : EQUB (i*50) : NEXT
ASSERT *-rockPos = numRocks
.rockNum: SKIP 1

.prepRocks: {
    lda #(numRocks-1) : sta rockNum
.loop:
    jsr updateRock
    jsr prepRock
    dec rockNum
    bpl loop
    rts }

.updateRock:
    ldx rockNum
    inc rockPos,x
    lda hitFlags,x : beq noHit
    lda #0 : sta rockAlive,x ; explode
.noHit:
    rts

sizeRock = 10 ; #bars
.rockBarNum SKIP 1

.prepRock: {
    ldx rockNum
    lda rockAlive,x : beq noplot
    lda #(sizeRock-1) : sta rockBarNum
    txa : clc : adc #hitFlags : sta hitme ; me
.loop:
    lda rockBarNum
    clc : adc rockPos,x : sta theA : lda #&40 : sta theA+1 ; where
    lda #&33 : jsr hitplotGen : jsr eraseGen ; cyan bar
    dec rockBarNum
    bpl loop
.noplot:
    rts
}

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
    jsr setupColours
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

.setupColours:
    ;jsr replaceRedWithMagenta
    jsr replaceYellowWithBlue ; easier to see on cyan
    jsr replaceWhiteWithCyan
    rts

;; .replaceRedWithMagenta:
;;     lda #19 : jsr oswrch
;;     lda #1 : jsr oswrch ; logical red
;;     lda #5 : jsr oswrch ; physical magenta
;;     lda #0 : jsr oswrch
;;     lda #0 : jsr oswrch
;;     lda #0 : jsr oswrch
;;     rts

.replaceYellowWithBlue:
    lda #19 : jsr oswrch
    lda #2 : jsr oswrch ; logical yellow
    lda #4 : jsr oswrch ; physical blue
    lda #0 : jsr oswrch
    lda #0 : jsr oswrch
    lda #0 : jsr oswrch
    rts

.replaceWhiteWithCyan:
    lda #19 : jsr oswrch
    lda #3 : jsr oswrch ; logical white
    lda #6 : jsr oswrch ; physical cyan
    lda #0 : jsr oswrch
    lda #0 : jsr oswrch
    lda #0 : jsr oswrch
    rts


;----------------------------------------------------------------------
; dual-space erase

.eraseRun:
    lda #&0 ;50 ; yellow for dev
    jmp (eraseRunPtr)

eraseNumberBlocks = 100
macro eraseTemplate
    sta &BEEF ; SCREEN-ADDRESS(1,2)
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

SPLAT = screenEnd-1 ; dev; bug catch

.overwriteRun:
    jmp (overwritePtr)

overwriteNumberBlocks = 100
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
; hitplot -- detect collision (with red) and set ZP var

.hitplotRun:
    jmp (hitplotPtr)

hitplotNumberBlocks = 100
macro hitplotTemplate
    lda SPLAT;ScreenAddr(1,2)  |  0  |  3 |  4us ; TODO update time calc
    tay;                       |  3  |  1 |
    and #&ff ;DataByte(5)      |  4  |  2 |  2us
    tax;                       |  6  |  1 |  2us
    lda hitTableR,x;           |  7  |  3 |  4us
    beq noHit;                 | 10  |  2 |  3us (noHit-taken), 2us(hit-not)
    sta badHit;  HitFlag(13)   | 12  |  2 |  3us (hit)
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

.hitme : EQUS badHit ; caller must initialize
.hitplotGen: ; byte to plot in ACC; screen-addr in theA; hitFlag in "hitme"
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
    lda hitme : ldy #13 : sta (hitplotPtr),y
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

;----------------------------------------------------------------------
; hit tables

x = 1
;ALIGN &100
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
; random

.frames SKIP 1
.getRandom:
    inc frames
    ldy frames
    lda randomBytes,y
    rts

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

;----------------------------------------------------------------------
.end:
SAVE "Code", start, end
