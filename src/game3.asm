
;;; Raster debug for prep time; dont move rocks!
Debug = FALSE
DontMove = FALSE

keyCodeU = -58
keyCodeD = -42
keyCodeL = -26
keyCodeR = -122

ula = &fe21
osasci = &ffe3
oswrch = &ffee
osbyte = &fff4

screenStart = &3000
screenEnd = &8000
stack = &100

maxObjects = 16

GUARD &B0 ; keep an eye on zeropage space usage
GUARD stack
ORG &70

.vsync SKIP 1

.keyU SKIP 1
.keyD SKIP 1
.keyL SKIP 1
.keyR SKIP 1

.lastKeyU SKIP 1
.lastKeyD SKIP 1
.lastKeyL SKIP 1
.lastKeyR SKIP 1

.stripPtr SKIP 2
.stripPtrPtr SKIP 2

.theCX SKIP 1 ; coarse-X : 0..79
.theCY SKIP 1 ; coarse-Y : 0..31
.theFX SKIP 1 ; fine-X   : 0..3
.theFY SKIP 1 ; fine-Y   : 0..7
.theA SKIP 2  ; screen address of the 8x8 char

.badHit SKIP 1

.scenePtr SKIP 2
.postBlitPtr SKIP 2

.msgPtr SKIP 2

.overwritePtr SKIP 2
.hitplotPtr SKIP 2

.erasePtr SKIP 2
.eraseRunPtr SKIP 2
.eraseSwitcher SKIP 2

.iterAction SKIP 2
.reposPtr SKIP 2

.hitFlags SKIP maxObjects

GUARD screenStart
ORG &1100

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

;;;----------------------------------------------------------------------
;;; main loop

timerlength = 0;100 ; smaller->0
.irq
    LDA &FE4D:AND #2:BNE irqvsync
.irqtimer
    LDA #&40:STA &FE4D:INC vsync
    LDA &FC
    RTI
.irqvsync
    STA &FE4D
    LDA #LO(timerlength):STA &FE44
    LDA #HI(timerlength):STA &FE45
    LDA &FC
    RTI

.mySync: {
    { .loop : lda vsync : beq loop }
    cmp #2 : bcs failSync ; sometimes triggers surprisingly
    lda #0 : sta vsync
    rts
.failSync:
    copy16i msg, msgPtr
    jmp printMessageAndSpin
.msg EQUS "Too slow to sync", 13, 0 }

.initFromDemo:
    sei
    ;LDX #&FF:TXS                ; reset stack (CAN DO IF INLINE)
    STX &FE44:STX &FE45
    LDA #&7F:STA &FE4E          ; disable all interrupts
    STA &FE43                   ; set keyboard data direction
    LDA #&C2:STA &FE4E          ; enable VSync and timer interrupt
    LDA #&0F:STA &FE42          ; set addressable latch for writing
    LDA #3:STA &FE40            ; keyboard write enable
    LDA #0:STA &FE4B            ; timer 1 one shot mode
    LDA #LO(irq):STA &204
    LDA #HI(irq):STA &205       ; set interrupt handler

    rts

.main: {

    jsr initFromDemo

    lda #0: sta vsync

    jsr setupMachine
    jsr eraseInit
    jsr initHitFlags
    jsr initKeyVars

    lda #0 : sta badHit
    copy16i startScene, scenePtr
    copy16i nothingPostBlit, postBlitPtr

    cli

    jsr mySync

.loop:

    { lda badHit : beq noBadHit : STOP &11 : .noBadHit }

    jsr saveLastKeys
    jsr readKeys

    IF Debug : lda #3 : sta ula : ENDIF ; blue (prepare scene)
    jsr prepare
    ;jsr prepare ; IDEMPOENT -- nope, does double update
    lda #7 : sta ula ; black

    jsr mySync

    lda #2 : sta ula ; magenta (shows if we are too slow to blit)
    jsr blitScene
    ;jsr blitScene ; IDEMPOENT - still not quite! seems ok with 3!
    ;jsr blitScene
    lda #7 : sta ula ; black

    jsr postBlit
    jsr eraseFlip
    inc frames
    jmp loop
    rts }

.prepare:
    ;; init
    jsr overwriteReInit
    jsr hitplotReInit
    ;; scene
    jmp (scenePtr)

.blitScene:
    jsr eraseRun
    jsr overwriteRun
    jsr hitplotRun
    rts

.postBlit:
    jmp (postBlitPtr)

.nothingPostBlit:
    rts

;;;----------------------------------------------------------------------
;;; scenes: startScene, mainLevelScene, levelClearScene, waitAndRestartScene

.startScene:
    jsr spawnRocks
    jsr delayedSpawnBullets
    copy16i mainLevelScene, scenePtr
    rts

.mainLevelScene:
    jsr handleBullets
    jsr handleRocks
    copy16i postBlitBulletCheck, postBlitPtr
    rts

.eliminateRock: {
    dec numberRocksLeft
    bne continue
    copy16i levelClearScene, scenePtr
.continue:
    rts }

.levelClearScene:
    jsr killAllBullets
    lda #50 : sta waitCount ; 1sec
    copy16i waitAndRestartScene, scenePtr
    rts

.waitAndRestartScene: {
    dec waitCount
    bne continue
    copy16i startScene, scenePtr
.continue:
    rts }

.waitCount: SKIP 1

;;;----------------------------------------------------------------------
;;; rocks...

numRocks = 4
.rockAlive: SKIP numRocks ; 2-medium, 1-small, 0-dead
.rockFX: SKIP numRocks
.rockFY: SKIP numRocks
.rockCX: SKIP numRocks
.rockCY: SKIP numRocks
.speedH: SKIP numRocks ; bigger val is slower
.countH: SKIP numRocks ; number of ticks till H move
.reverseH: SKIP numRocks
.speedV: SKIP numRocks
.countV: SKIP numRocks
.reverseV: SKIP numRocks
.rockNum: SKIP 1

.numberRocksLeft SKIP 1

.spawnRocks: {
    ldx #0 : lda #0 : sta rockAlive,x
    ldx #1 : lda #2 : sta rockAlive,x : inc numberRocksLeft
    ldx #2 : lda #0 : sta rockAlive,x
    ldx #3 : lda #2 : sta rockAlive,x : inc numberRocksLeft
    ldx #(numRocks-1)
.loop:
    stx rockNum

    jsr randomPos
    lda theFX : sta rockFX,x
    lda theFY : sta rockFY,x
    lda theCX : sta rockCX,x
    lda theCY : sta rockCY,x

    lda #0 : sta reverseH,x
    jsr getRandom
    lsr a : rol reverseH,x
    and #3 : clc : adc #1 : sta speedH,x : sta countH,x

    lda #0 : sta reverseV,x
    jsr getRandom
    lsr a : rol reverseV,x
    and #3 : clc : adc #1 : sta speedV,x : sta countV,x

    dex
    bpl loop
    rts }

.handleRocks: {
    lda #(numRocks-1) : sta rockNum
.loop:
    jsr updateRock
    jsr drawRock
    dec rockNum
    bpl loop
    rts }

.updateRock: {
    ldx rockNum
    lda rockAlive,x : bne notDead
    rts
.notDead:
    ;; grab position
    lda rockFX,x : sta theFX
    lda rockFY,x : sta theFY
    lda rockCX,x : sta theCX
    lda rockCY,x : sta theCY
    ;; did we get hit?
    lda hitFlags,x : beq moveRock ; nope, so just move
    lda #0 : sta hitFlags,x
    ;; yes, we got hit
    dec rockAlive,x : beq killRock ; we are small, so die
    ;; we are medium, so split
    inc numberRocksLeft
    ;; wake up small twin
    dex : lda #1 : sta rockAlive,x
    ;; copy position into woken rock
    lda theFX : sta rockFX,x
    lda theFY : sta rockFY,x
    lda theCX : sta rockCX,x
    lda theCY : sta rockCY,x
    inx
    ;; match direction
    lda reverseV,x : dex : sta reverseV,x : inx
    lda reverseH,x : dex : sta reverseH,x : inx
    ;; move the (possibly shrunken) rock
.moveRock:
    IF Not(DontMove) : jsr moveRockH : jsr moveRockV : ENDIF
    lda theFX : sta rockFX,x
    lda theFY : sta rockFY,x
    lda theCX : sta rockCX,x
    lda theCY : sta rockCY,x
    rts
.killRock:
    jmp eliminateRock
}

.moveRockH: {
    lda countH,x
    bne no
    { lda reverseH,x : bne back : jsr right1 : jmp end : .back : jsr left1 : .end }
    lda speedH,x
    sta countH,x
    rts
.no:
    dec countH,x
    rts }

.moveRockV: {
    lda countV,x
    bne no
    { lda reverseV,x : bne back : jsr down1 : jmp end : .back : jsr up1 : .end }
    lda speedV,x
    sta countV,x
    rts
.no:
    dec countV,x
    rts }

.drawRock: {
    ldx rockNum
    lda rockAlive,x : bne plot
    rts
.plot
    copy16i smallMeteor, stripPtrPtr
    { lda rockAlive,x : cmp #2 : bne no : copy16i mediumMeteor, stripPtrPtr : .no }
    txa : clc : adc #hitFlags : sta hitme ; me
    lda rockCX,x : sta theCX
    lda rockCY,x : sta theCY
    lda rockFY,x : sta theFY
    jsr calculateAfromXY
    lda rockFX,x : asl a
    tay : lda (stripPtrPtr),y : sta stripPtr
    iny : lda (stripPtrPtr),y : sta stripPtr+1
    ldy #0 : lda (stripPtr),y : sta stripCount
    lda #1 : sta dataNum
.loopStrip:
    ldy dataNum : lda (stripPtr),y : sta stripItemCount
    inc dataNum
.loopItem:
    ldy dataNum : lda (stripPtr),y
    { beq nogen : jsr hitplotGen : jsr eraseGen : .nogen }
    inc dataNum
    jsr down1
    dec stripItemCount : bne loopItem
    dec stripCount : beq done
    ldy dataNum
    lda (stripPtr),y : sta reposPtr : iny
    lda (stripPtr),y : sta reposPtr+1 : iny
    sty dataNum
    jsr dispatchReposition
    jmp loopStrip
.done:
    rts }

.dispatchReposition:
    jmp (reposPtr)

;;; consider zero page
.dataNum SKIP 1
.stripCount SKIP 1
.stripItemCount SKIP 1

.r4u8:  jsr right4 : jsr up8 : rts
.r4u16: jsr right4 : jsr up8 : jsr up8 : rts

;;;----------------------------------------------------------------------
;;; sprite data
.smallMeteor: { EQUW x0,x1,x2,x3
.x0 : EQUB 3
EQUB 8, &33,&44,&88,&88,&44,&22,&44,&33 : EQUW r4u8
EQUB 8, &66,&99,&11,&00,&00,&11,&99,&66 : EQUW r4u8
EQUB 8, &00,&00,&00,&88,&88,&00,&00,&00
.x1 : EQUB 3
EQUB 8, &11,&22,&44,&44,&22,&11,&22,&11 : EQUW r4u8
EQUB 8, &bb,&44,&00,&00,&00,&00,&44,&bb : EQUW r4u8
EQUB 8, &00,&88,&88,&44,&44,&88,&88,&00
.x2 : EQUB 3
EQUB 8, &00,&11,&22,&22,&11,&00,&11,&00 : EQUW r4u8
EQUB 8, &dd,&22,&00,&00,&00,&88,&22,&dd : EQUW r4u8
EQUB 8, &88,&44,&44,&22,&22,&44,&44,&88
.x3 : EQUB 3
EQUB 8, &00,&00,&11,&11,&00,&00,&00,&00 : EQUW r4u8
EQUB 8, &66,&99,&00,&00,&88,&44,&99,&66 : EQUW r4u8
EQUB 8, &cc,&22,&22,&11,&11,&22,&22,&cc
}

.mediumMeteor: { EQUW x0,x1,x2,x3
.x0 : EQUB 5
EQUB 16, &00,&00,&11,&22,&44,&88,&88,&88,&44,&22,&11,&22,&44,&22,&11,&00 : EQUW r4u16
EQUB 16, &11,&22,&cc,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&ff : EQUW r4u16
EQUB 16, &ff,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&44,&aa,&11 : EQUW r4u16
EQUB 16, &00,&88,&66,&11,&00,&11,&22,&44,&22,&11,&00,&00,&00,&11,&22,&cc : EQUW r4u16
EQUB 16, &00,&00,&00,&00,&88,&00,&00,&00,&00,&00,&88,&88,&88,&00,&00,&00
.x1 : EQUB 5
EQUB 16, &00,&00,&00,&11,&22,&44,&44,&44,&22,&11,&00,&11,&22,&11,&00,&00 : EQUW r4u16
EQUB 16, &00,&11,&ee,&00,&00,&00,&00,&00,&00,&00,&88,&00,&00,&00,&88,&77 : EQUW r4u16
EQUB 16, &ff,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&22,&55,&88 : EQUW r4u16
EQUB 16, &88,&44,&33,&00,&00,&00,&11,&22,&11,&00,&00,&00,&00,&00,&11,&ee : EQUW r4u16
EQUB 16, &00,&00,&00,&88,&44,&88,&00,&00,&00,&88,&44,&44,&44,&88,&00,&00
.x2 : EQUB 5
EQUB 16, &00,&00,&00,&00,&11,&22,&22,&22,&11,&00,&00,&00,&11,&00,&00,&00 : EQUW r4u16
EQUB 16, &00,&00,&77,&88,&00,&00,&00,&00,&00,&88,&44,&88,&00,&88,&44,&33 : EQUW r4u16
EQUB 16, &77,&88,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&11,&22,&cc : EQUW r4u16
EQUB 16, &cc,&22,&11,&00,&00,&00,&00,&11,&00,&00,&00,&00,&00,&00,&88,&77 : EQUW r4u16
EQUB 16, &00,&00,&88,&44,&22,&44,&88,&00,&88,&44,&22,&22,&22,&44,&88,&00
.x3 : EQUB 5
EQUB 16, &00,&00,&00,&00,&00,&11,&11,&11,&00,&00,&00,&00,&00,&00,&00,&00 : EQUW r4u16
EQUB 16, &00,&00,&33,&44,&88,&00,&00,&00,&88,&44,&22,&44,&88,&44,&22,&11 : EQUW r4u16
EQUB 16, &33,&44,&88,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&11,&ee : EQUW r4u16
EQUB 16, &ee,&11,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&88,&44,&33 : EQUW r4u16
EQUB 16, &00,&00,&cc,&22,&11,&22,&44,&88,&44,&22,&11,&11,&11,&22,&44,&88
}


;;;----------------------------------------------------------------------
;;; bullets

numBullets = 4
.bulletAlive : SKIP numBullets
.bulletTimer : SKIP numBullets ; until spawn
.bulletCX : SKIP numBullets
.bulletCY : SKIP numBullets
.bulletFX : SKIP numBullets
.bulletFY : SKIP numBullets
.bulletNum: SKIP 1

.delayedSpawnBullets: {
    ldx #(numBullets-1)
.loop:
    lda #5 : sta bulletTimer,x ; spawn in .1s
    jsr randomPos
    lda theFX : sta bulletFX,x
    lda theFY : sta bulletFY,x
    lda theCX : sta bulletCX,x
    lda theCY : sta bulletCY,x
    dex
    bpl loop
    rts }

.randomPos: ; into the{FX,FY,CX,CY}
    lda #0 : sta theFX
    jsr getRandom
    ; shift 2 bits into FX
    lsr a : rol theFX
    lsr a : rol theFX
    sta theCX ; leaving 6 bits in CX (0..63) ; TODO: translate to (0..31 | 48..79)

    lda #0 : sta theFY
    jsr getRandom
    ; shift 3 bits into FY
    lsr a : rol theFY
    lsr a : rol theFY
    lsr a : rol theFY
    sta theCY ; leaving 5 bits in CX (0..31)
    rts

.killAllBullets: {
    ldx #(numBullets-1)
.loop:
    lda #0 : sta bulletAlive,x
    dex
    bpl loop
    rts }

.handleBullets: {
    lda #(numBullets-1) : sta bulletNum
.loop:
    jsr updateBullet
    jsr drawBullet
    dec bulletNum
    bpl loop
    rts }


.updateBullet: {
    ldx bulletNum
    lda bulletAlive,x
    beq isDead
    ;; isAlive - move...
    lda bulletFX,x : sta theFX
    lda bulletFY,x : sta theFY
    lda bulletCX,x : sta theCX
    lda bulletCY,x : sta theCY
    jsr onArrowWithRepeat
    lda theFX : sta bulletFX,x
    lda theFY : sta bulletFY,x
    lda theCX : sta bulletCX,x
    lda theCY : sta bulletCY,x
    rts
.isDead:
    dec bulletTimer,x
    bne stayDead
    lda #1 : sta bulletAlive,x
    ;; come alive; choose new random position
    jsr randomPos
    lda theFX : sta bulletFX,x
    lda theFY : sta bulletFY,x
    lda theCX : sta bulletCX,x
    lda theCY : sta bulletCY,x
.stayDead:
    rts }


.drawBullet: {
    ldx bulletNum
    lda bulletAlive,x : bne plot
    rts
.plot:
    copy16i action, iterAction
    jmp iterateBulletSpriteData
.action:
    lda (stripPtr),y : jsr overwriteGen : jsr eraseGen
    rts
}


.bulletIsHit: SKIP 1

.postBlitBulletCheck: {
    copy16i nothingPostBlit, postBlitPtr
    lda #(numBullets-1) : sta bulletNum
.loop:
    lda #0 : sta bulletIsHit
    jsr bulletHitCheck
    lda bulletIsHit : beq notHit
    jsr killBulletAndSetRespawn
.notHit:
    dec bulletNum
    bpl loop
    rts
}

.killBulletAndSetRespawn:
    ldx bulletNum
    lda #0 : sta bulletAlive,x ; die
    lda #50 : sta bulletTimer,x ; respawn in 1s
    rts

.bulletHitCheck: {
    ldx bulletNum
    lda bulletAlive,x : bne check
    rts
.check:
    copy16i action, iterAction
    jmp iterateBulletSpriteData
.action:
    lda (stripPtr),y : sta pokeme+1
    ldy #0 : and (theA),y
    .pokeme : cmp #&FF
    { beq notHit : inc bulletIsHit : .notHit }
    rts
}


.iterateBulletSpriteData: {
    lda bulletCX,x : sta theCX
    lda bulletCY,x : sta theCY
    lda bulletFY,x : sta theFY
    jsr calculateAfromXY
    copy16i bulletSpriteData, stripPtrPtr
    lda bulletFX,x : asl a
    tay : lda (stripPtrPtr),y : sta stripPtr
    iny : lda (stripPtrPtr),y : sta stripPtr+1
    ldy #0 : jsr dispatch : jsr down1
    ldy #1 : jsr dispatch : jsr down1
    ldy #2 : jsr dispatch : jsr right4
    ldy #3 : jsr dispatch : jsr up1
    ldy #4 : jsr dispatch : jsr up1
    ldy #5 : jsr dispatch
    rts
.dispatch: jmp (iterAction)
}

;; RED
.bulletSpriteData: { EQUW x0,x1,x2,x3
.x0: EQUB &04,&0e,&04, &00,&00,&00
.x1: EQUB &02,&07,&02, &00,&00,&00
.x2: EQUB &01,&03,&01, &00,&08,&00
.x3: EQUB &00,&01,&00, &08,&0c,&08
}

;;;----------------------------------------------------------------------
;;; hit-flags

.initHitFlags: {
    lda #0
    ldx #0
.loop:
    sta hitFlags,x
    inx
    cpx #maxObjects
    bne loop
    rts }

;;;----------------------------------------------------------------------
;;; misc: stop, sync, print stuff

.stop: { ; byte in Acc
    jsr printHexA
    copy16i msg, msgPtr
    jmp printMessageAndSpin
    .msg EQUS "Stop", 13, 0 }

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

;;;----------------------------------------------------------------------
;;; setupMachine

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
    jsr replaceYellowWithBlue ; easier to see on cyan
    jsr replaceWhiteWithCyan
    rts

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

;;;----------------------------------------------------------------------
;;; dual-space erase

.eraseRun:
    lda #0
    jmp (eraseRunPtr)

eraseNumberBlocks = 130
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
    jsr eraseSpaceCheck ; TODO: remove check when in production!
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

;;;----------------------------------------------------------------------
;;; overwrite plotting

SPLAT = screenEnd-1 ; dev; bug catch

.overwriteRun:
    jmp (overwritePtr)

overwriteNumberBlocks = 150
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
    jsr overwriteSpaceCheck ; TODO: remove check when in production!
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

;;;----------------------------------------------------------------------
;;; hitplot -- detect collision (with red) and set ZP var

.hitplotRun:
    jmp (hitplotPtr)

hitplotNumberBlocks = 120
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
    jsr hitplotSpaceCheck ; TODO: remove check when in production!
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

;;;----------------------------------------------------------------------
;;; hit tables

H = 1
o = 0
;ALIGN &100
.hitTableR: ; if one of the 4 pixels is red -- TODO: compute/gen
    EQUB o,H,H,H,H,H,H,H,H,H,H,H,H,H,H,H
    EQUB o,o,H,H,H,H,H,H,H,H,H,H,H,H,H,H
    EQUB o,H,o,H,H,H,H,H,H,H,H,H,H,H,H,H
    EQUB o,o,o,o,H,H,H,H,H,H,H,H,H,H,H,H
    EQUB o,H,H,H,o,H,H,H,H,H,H,H,H,H,H,H
    EQUB o,o,H,H,o,o,H,H,H,H,H,H,H,H,H,H
    EQUB o,H,o,H,o,H,o,H,H,H,H,H,H,H,H,H
    EQUB o,o,o,o,o,o,o,o,H,H,H,H,H,H,H,H
    EQUB o,H,H,H,H,H,H,H,o,H,H,H,H,H,H,H
    EQUB o,o,H,H,H,H,H,H,o,o,H,H,H,H,H,H
    EQUB o,H,o,H,H,H,H,H,o,H,o,H,H,H,H,H
    EQUB o,o,o,o,H,H,H,H,o,o,o,o,H,H,H,H
    EQUB o,H,H,H,o,H,H,H,o,H,H,H,o,H,H,H
    EQUB o,o,H,H,o,o,H,H,o,o,H,H,o,o,H,H
    EQUB o,H,o,H,o,H,o,H,o,H,o,H,o,H,o,H
    EQUB o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o
.hitTableREnd:
ASSERT ((hitTableREnd-hitTableR) = 256)

;;;----------------------------------------------------------------------
;;; random

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

    lda theFY
    clc : adc theA : sta theA ; adjust A with fineY

    rts

;;;----------------------------------------------------------------------
;;; incremental movement

.onArrowWithRepeat:
    { lda keyU : beq no : jsr up1 : .no }
    { lda keyD : beq no : jsr down1 : .no }
    { lda keyL : beq no : jsr left1 : .no }
    { lda keyR : beq no : jsr right1 : .no }
    rts

.onArrow:
    { lda keyL : beq no : lda lastKeyL : bne no : jsr left1 : .no }
    { lda keyR : beq no : lda lastKeyR : bne no : jsr right1 : .no }
    { lda keyU : beq no : lda lastKeyU : bne no : jsr up1 : .no }
    { lda keyD : beq no : lda lastKeyD : bne no : jsr down1 : .no }
    rts

;;;----------------------------------------------------------------------
;;; left/right...

.left1: {
    lda theFX : bne no
    jsr left4
    lda #4 : sta theFX
.no:
    dec theFX
    rts }

.left4:
    jsr unwrapLine
    dec theCX
    jsr leftA
    rts


.right1: {
    inc theFX : lda theFX : cmp #4  : bne after : lda #0 : sta theFX
    inc theCX : lda theCX : cmp #80 : bne after : lda #0 : sta theCX ; TODO: right4
.after:
    rts }

.right4:
    inc theCX
    jsr rightA
    jsr wrapLine
    rts

;; TODO inline
.leftA:
    lda theA : sec : sbc #8 : sta theA
    lda theA+1     : sbc #0 : sta theA+1
    rts
.rightA:
    lda theA : clc : adc #8 : sta theA
    lda theA+1     : adc #0 : sta theA+1
    rts

.unwrapLine : {
    lda theCX
    bne no
    lda #80 : sta theCX
    jsr downA8
.no:
    rts }

.wrapLine : {
    lda theCX
    cmp #80 : bne no
    lda #0 : sta theCX
    jsr upA8
.no:
    rts }


;;;----------------------------------------------------------------------
;;; up/down...

.up1: { ; FY,CY,A
    lda theFY : bne no
    lda #7 : sta theFY
    lda theA : clc : adc #7 : sta theA
    jmp up8
.no:
    dec theA
    dec theFY
    rts }

.up8: ; CY,A
    jsr unwrapScreen
    dec theCY
    jmp upA8

.upA8: ; A
    lda theA : sec : sbc #&80 : sta theA
    lda theA+1     : sbc #2   : sta theA+1
    rts

.unwrapScreen: { ; CY,A
    lda theCY
    bne no
    lda #32 : sta theCY
    lda theA+1 : clc : adc #HI(screenEnd-screenStart) : sta theA+1
.no:
    rts }


.down1: { ; FY,CY,A
    inc theA
    inc theFY
    lda theFY : cmp #8 : beq cont
    rts
.cont:
    lda #0 : sta theFY
    lda theA : sec : sbc #8 : sta theA
    jmp down8 }

.down8: ; CY,A
    inc theCY
    jsr downA8
    jmp wrapScreen

.downA8: ; A
    lda theA : clc : adc #&80 : sta theA
    lda theA+1     : adc #2   : sta theA+1
    rts

.wrapScreen: { ; CY,A
    lda theCY
    cmp #32 : bne no
    lda #0 : sta theCY
    lda theA+1 : sec : sbc #HI(screenEnd-screenStart) : sta theA+1
.no:
    rts }

;;;----------------------------------------------------------------------
;;; keyboard

.initKeyVars:
    lda #0 : sta keyU
    lda #0 : sta keyD
    lda #0 : sta keyL
    lda #0 : sta keyR
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
    rts

.checkU: {
    lda #0 : sta keyU
    lda #(-keyCodeU-1) : sta &fe4f : lda &fe4f
    BPL no : lda #1 : sta keyU : .no
    rts }

.checkD: {
    lda #0 : sta keyD
    lda #(-keyCodeD-1) : sta &fe4f : lda &fe4f
    BPL no : lda #1 : sta keyD : .no
    rts }

.checkL: {
    lda #0 : sta keyL
    lda #(-keyCodeL-1) : sta &fe4f : lda &fe4f
    BPL no : lda #1 : sta keyL : .no
    rts }

.checkR: {
    lda #0 : sta keyR
    lda #(-keyCodeR-1) : sta &fe4f : lda &fe4f
    BPL no : lda #1 : sta keyR : .no
    rts }

;;;----------------------------------------------------------------------
.end:

print "bytes left: ", screenStart-*

SAVE "Code", start, end
