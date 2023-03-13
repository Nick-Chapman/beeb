
;;; Raster debug for prep time; dont move rocks!
RasterDebugPrepare = FALSE
DontMove = FALSE
DevSpaceCheck = TRUE

;;; keyCodeU = -58  ; up arrow
;;; keyCodeD = -42  ; down arrow
;;; keyCodeL = -26  ; left arrow
;;; keyCodeR = -122 ; right arrow
;;; keyCodeL = -98  ; z
;;; keyCodeR = -67  ; x

keyCodeU = -88    ; semicolon
keyCodeD = -104   ; dot
keyCodeL = -65    ; capslock
keyCodeR = -2     ; ctrl
keyCodeRet = -74  ; return
keyCodeShift = -1 ; shift

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

.keyL SKIP 1
.keyR SKIP 1
.keyRet SKIP 1
.keyShift SKIP 1

.lastKeyL SKIP 1
.lastKeyR SKIP 1
.lastKeyRet SKIP 1
;;; dont need to save Shift

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
    ;lda vsync : bne failSync ; pre-sync check (more harsh)
    { .loop : lda vsync : beq loop }
    ;cmp #2 : bcs failSync; post-sync check (move forgiving)
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

    ;lda #0: sta vsync

    jsr setupMachine
    jsr eraseInit
    jsr initHitFlags

    lda #0 : sta badHit
    copy16i startScene, scenePtr
    copy16i nothingPostBlit, postBlitPtr

    cli

    lda #0 : sta vsync
    jsr mySync

.loop:

    { lda badHit : beq noBadHit : STOP &11 : .noBadHit }

    jsr saveLastKeys
    jsr readKeys

    IF RasterDebugPrepare : lda #3 : sta ula : ENDIF ; blue (prepare scene)
    jsr prepare
    ;jsr prepare ; IDEMPOENT -- nope, does double update
    lda #7 : sta ula ; black

    jsr mySync

    lda #2 : sta ula ; magenta (shows if we are too slow to blit)
    jsr blitScene
    ;;jsr blitScene ; not IDEMPOENT if we blit twice (why?)
    ;;jsr blitScene ; IDEMPOENT if blit 3 times
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
    jsr putShipInMiddle
    jsr spawnRocks
    copy16i mainLevelScene, scenePtr
    rts

.mainLevelScene:
    jsr detectThrust
    jsr detectFirePressed
    jsr detectRotateLeft
    jsr detectRotateRight

    ;; currently we are not doing collision detection for ship
    ;; ship is blitted using the cheap "overwrite" plotting
    ;; bullets are also plotted using "overwrite" plotting
    ;; also: bullets are checked after blitting, to implement bullet collision

    ;; to allow this postBlitBulletCheck to work,
    ;; the bullets must be blitted after the ship is blitted

    ;; However, the code generated in the prepare step is bliited in reverse order
    ;; So we must draw the ship *after* we draw the bullets

    jsr handleBullets ; overwrite (blitted *after* ship)
    jsr handleShip ; overwrite (blitted *before* bullets)
    jsr handleRocks ; hitplot (bliited *last*)
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
;;; detect controls

.detectFirePressed:
    lda #0 : sta fireBullet
    { lda keyRet : beq no : lda lastKeyRet : bne no : inc fireBullet : .no }
    rts

.detectThrust: {
    lda #0 : sta thrustEngine
    lda frames : and #1 : bne done ; every other frame
    { lda keyShift : beq no : inc thrustEngine : .no }
    rts
.done:
    rts }

.detectRotateLeft: {
    ;lda frames : and #1 : bne done
    { lda keyL : beq no : dec theDirection : .no }
    ;{ lda keyL : beq no : lda lastKeyL : bne no : dec theDirection : .no }
.done:
    rts }

.detectRotateRight: {
    ;lda frames : and #1 : bne done
    { lda keyR : beq no : inc theDirection : .no }
    ;{ lda keyR : beq no : lda lastKeyR : bne no : inc theDirection : .no }
.done:
    rts }

.thrustEngine : EQUB 0
.fireBullet : EQUB 0
.theDirection : EQUB 0

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
    ;and #3 : clc : adc #1 : sta speedH,x : sta countH,x
    and #3 : sta speedH,x : sta countH,x

    lda #0 : sta reverseV,x
    jsr getRandom
    lsr a : rol reverseV,x
    ;and #3 : clc : adc #1 : sta speedV,x : sta countV,x
    and #3 : sta speedV,x : sta countV,x

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

;----------------------------------------------------------------------
;;; sprite drawing

.drawRock: {
    ldx rockNum
    lda rockAlive,x : bne plot
    rts
.plot
    copy16i smallMeteor, stripPtrPtr
    copy16i hitplotGen, pokeSpritePlotGen+1
    { lda rockAlive,x : cmp #2 : bne no : copy16i mediumMeteor, stripPtrPtr : .no }
    txa : clc : adc #hitFlags : sta hitme ; me
    lda rockCX,x : sta theCX
    lda rockCY,x : sta theCY
    lda rockFY,x : sta theFY
    jsr calculateAfromXY
    lda rockFX,x : asl a
    jmp drawSprite
}

.drawShip: {
    copy16i shipSpriteData, stripPtrPtr
    copy16i overwriteGen, pokeSpritePlotGen+1 ; use hitplotGen when support ship collision
    lda shipCX : sta theCX
    lda shipCY : sta theCY
    lda shipFY : sta theFY
    jsr calculateAfromXY
    lda shipFX : asl a
    jsr drawSprite
    jmp drawTurret
    }

.drawSprite:
    tay : lda (stripPtrPtr),y : sta stripPtr
    iny : lda (stripPtrPtr),y : sta stripPtr+1
    ldy #0 : lda (stripPtr),y : sta stripCount
    lda #1 : sta dataNum
.loopStrip:
    ldy dataNum : lda (stripPtr),y : sta stripItemCount
    inc dataNum
.loopItem:
    ldy dataNum : lda (stripPtr),y
    beq sprite_nogen : .pokeSpritePlotGen jsr overwriteGen : jsr eraseGen : .sprite_nogen
    inc dataNum
    jsr down1
    dec stripItemCount : bne loopItem
    dec stripCount : beq doneDrawSprite
    ldy dataNum
    lda (stripPtr),y : sta dispatch+1 : iny
    lda (stripPtr),y : sta dispatch+2 : iny
    sty dataNum
    .dispatch : jsr &FFFF
    jmp loopStrip
.doneDrawSprite:
    rts

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
;;; ship

.shipCX SKIP 1
.shipCY SKIP 1
.shipFX SKIP 1
.shipFY SKIP 1

.putShipInMiddle:
    lda #0 : sta shipFX
    lda #0 : sta shipFY
    lda #39 : sta shipCX
    lda #15 : sta shipCY
    rts

.handleShip:
    jsr updateShipPos
    jsr drawShip
    rts

.updateShipPos: {
    lda thrustEngine : beq done
    lda shipFX : sta theFX
    lda shipFY : sta theFY
    lda shipCX : sta theCX
    lda shipCY : sta theCY
    lda theDirection : lsr a : lsr a : and #&f : jsr moveDirection
    lda theFX : sta shipFX
    lda theFY : sta shipFY
    lda theCX : sta shipCX
    lda theCY : sta shipCY
.done:
    rts }

;; YELLOW
.shipSpriteData: { EQUW x0,x1,x2,x3
.x0: EQUB 2
EQUB 3, &e0,&a0,&e0 : EQUW r4u3
EQUB 3, &00,&00,&00
.x1: EQUB 2
EQUB 3, &70,&50,&70 : EQUW r4u3
EQUB 3, &00,&00,&00
.x2: EQUB 2
EQUB 3, &30,&20,&30 : EQUW r4u3
EQUB 3, &80,&80,&80
.x3: EQUB 2
EQUB 3, &10,&10,&10 : EQUW r4u3
EQUB 3, &c0,&40,&c0
}

.r4u3:  jsr right4 : jmp up3

.drawTurret: ; expect CX,CY,FY to be set
    lda shipFX : sta theFX
    jsr positionTurret
    lda theFX : tax
    lda dotData,x
    jsr hitplotGen : jsr eraseGen ;; have to use hitplotGen for the xor
    rts

;; YELLOW
.dotData: EQUB &80, &40, &20, &10

.positionTurret: {
    lda theDirection : lsr a : lsr a : and #&f
    asl a : tay
    lda turretPosition,y : sta pokeme+1
    lda turretPosition+1,y : sta pokeme+2
    .pokeme : jsr &FFFF
    rts }

;;; Turret position is determined as offset from position marked with 'x'
;;; (This is where we end up after drawing the ship)

;;;    ef012
;;;    d...3
;;;    c...4
;;;    b...5
;;;    a9876x

.turretPosition:
EQUW l3u4, l2u4, l1u4, l1u3, l1u2, l1u1, left1, left2
EQUW left3, left4, left5, l5u1, l5u2, l5u3, l5u4, l4u4

.l1u1: jsr left1 : jmp up1
.l1u2: jsr left1 : jmp up2
.l1u3: jsr left1 : jmp up3
.l1u4: jsr left1 : jmp up4
.l2u4: jsr left2 : jmp up4
.l3u4: jsr left3 : jmp up4
.l4u4: jsr left4 : jmp up4
.l5u1: jsr left5 : jmp up1
.l5u2: jsr left5 : jmp up2
.l5u3: jsr left5 : jmp up3
.l5u4: jsr left5 : jmp up4
.nothing: rts


;;;----------------------------------------------------------------------
;;; bullets

numBullets = 4
.bulletAlive : SKIP numBullets ; zero initialization means bullets start dead
.bulletTimer : SKIP numBullets
.bulletCX : SKIP numBullets
.bulletCY : SKIP numBullets
.bulletFX : SKIP numBullets
.bulletFY : SKIP numBullets
.bulletDirection : SKIP numBullets

.bulletNum: SKIP 1

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
    lda bulletAlive,x : beq isDead
    dec bulletTimer,x : beq die
    ;; isAlive; move
    lda bulletFX,x : sta theFX
    lda bulletFY,x : sta theFY
    lda bulletCX,x : sta theCX
    lda bulletCY,x : sta theCY

    lda bulletDirection,x : lsr a : lsr a : and #&f : jsr moveDirection

    lda theFX : sta bulletFX,x
    lda theFY : sta bulletFY,x
    lda theCX : sta bulletCX,x
    lda theCY : sta bulletCY,x
    rts
.die:
    lda #0 : sta bulletAlive,x
.isDead:
    lda fireBullet : beq stayDead
    dec fireBullet ; so only one bullet gets fired

    lda #1 : sta bulletAlive,x ; spawn
    lda #50 : sta bulletTimer,x ; set lifetime

    lda theDirection : sta bulletDirection,x

    lda shipFX : sta theFX
    lda shipFY : sta theFY
    lda shipCX : sta theCX
    lda shipCY : sta theCY

    ;;bullet originates one bullet-move step from center of ship
    lda bulletDirection,x : lsr a : lsr a : and #&f : jsr moveDirection
    lda bulletDirection,x : lsr a : lsr a : and #&f : jsr moveDirection

    lda theFX : sta bulletFX,x
    lda theFY : sta bulletFY,x
    lda theCX : sta bulletCX,x
    lda theCY : sta bulletCY,x

.stayDead:
    rts }

.moveDirection: { ; passed in Acc
    asl a : tay
    lda directions,y : sta pokeme+1
    lda directions+1,y : sta pokeme+2
    .pokeme : jsr &FFFF
    rts }

.directions:
EQUW up3, u3r1, u2r2, r3u1, right3, r3d1, r2d2, d3r1
EQUW down3, d3l1, d2l2, l3d1, left3, l3u1, l2u2, u3l1

.u2r2: jsr up2 : jmp right2
.r2d2: jsr right2 : jmp down2
.d2l2: jsr down2 : jmp left2
.l2u2: jsr left2 : jmp up2

.u3l1: jsr up3 : jmp left1
.u3r1: jsr up3 : jmp right1
.d3l1: jsr down3 : jmp left1
.d3r1: jsr down3 : jmp right1
.l3u1: jsr left3 : jmp up1
.l3d1: jsr left3 : jmp down1
.r3u1: jsr right3 : jmp up1
.r3d1: jsr right3 : jmp down1

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
    ;jsr replaceYellowWithBlue ; easier to see on cyan
    jsr replaceWhiteWithCyan
    rts

;; .replaceYellowWithBlue:
;;     lda #19 : jsr oswrch
;;     lda #2 : jsr oswrch ; logical yellow
;;     lda #4 : jsr oswrch ; physical blue
;;     lda #0 : jsr oswrch
;;     lda #0 : jsr oswrch
;;     lda #0 : jsr oswrch
;;     rts

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
    lda #0 ; this is the zero-black which erases
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
    IF DevSpaceCheck : jsr eraseSpaceCheck : ENDIF
    ;; fill in the generated code
    lda theA     : ldy #1 : sta (erasePtr),y
    lda theA+1   : ldy #2 : sta (erasePtr),y
    rts

IF DevSpaceCheck
.eraseSpaceCheck: {
    lda erasePtr+1 : cmp eraseSpace+1 : bcc fail : bne ok
    lda erasePtr   : cmp eraseSpace   : bcc fail
.ok:
    rts
.fail:
    copy16i msg, msgPtr
    jmp printMessageAndSpin
.msg EQUS "Erase Overflow", 13, 0 }
ENDIF

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
    IF DevSpaceCheck : jsr overwriteSpaceCheck : ENDIF
    ;; fill in the generated code
    .DB}: lda #0 : ldy #1 : sta (overwritePtr),y
    lda theA     : ldy #3 : sta (overwritePtr),y
    lda theA+1   : ldy #4 : sta (overwritePtr),y
    rts

IF DevSpaceCheck
.overwriteSpaceCheck: {
    lda overwritePtr+1 : cmp #HI(overwriteSpace) : bcc fail : bne ok
    lda overwritePtr   : cmp #LO(overwriteSpace) : bcc fail
.ok:
    rts
.fail:
    copy16i msg, msgPtr
    jmp printMessageAndSpin
.msg EQUS "Overwrite Overflow", 13, 0 }
ENDIF

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
    IF DevSpaceCheck : jsr hitplotSpaceCheck : ENDIF
    ;; fill in the generated code
    .DB}: lda #0 : ldy #5 : sta (hitplotPtr),y : ldy #16 : sta (hitplotPtr),y
    lda theA     : ldy #1 : sta (hitplotPtr),y : ldy #18 : sta (hitplotPtr),y
    lda theA+1   : ldy #2 : sta (hitplotPtr),y : ldy #19 : sta (hitplotPtr),y
    lda hitme : ldy #13 : sta (hitplotPtr),y
    ;lda #badHit : sta hitme ; DEV CHECK?
    rts

IF DevSpaceCheck
.hitplotSpaceCheck: {
    lda hitplotPtr+1 : cmp #HI(hitplotSpace) : bcc fail : bne ok
    lda hitplotPtr   : cmp #LO(hitplotSpace) : bcc fail
.ok:
    rts
.fail:
    copy16i msg, msgPtr
    jmp printMessageAndSpin
.msg EQUS "Hitplot Overflow", 13, 0 }
ENDIF

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
.randomOffset SKIP 1

.getRandom:
    inc randomOffset
    ldy randomOffset
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

;;; theCX : coarse-X : 0..79
;;; theFX : fine-X   : 0..3
;;; theCY : coarse-Y : 0..31
;;; theFY : fine-Y   : 0..7

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
;;; left/right...

.left3: jsr left1
.left2: jsr left1
.left1: {
    lda theFX : bne no
    jsr left4
    lda #4 : sta theFX
.no:
    dec theFX
    rts }

.left5: jsr left1
.left4:
    lda theCX
    ; unwrap line
    { bne no : lda #80 : sta theCX : jsr downA8 : .no }
    dec theCX
    lda theA : sec : sbc #8 : sta theA
    lda theA+1     : sbc #0 : sta theA+1
    rts

.right3: jsr right1
.right2: jsr right1
.right1: {
    inc theFX
    lda theFX : cmp #4 : bne no
    lda #0 : sta theFX
    jsr right4
.no:
    rts }

.right4:
    inc theCX
    lda theA : clc : adc #8 : sta theA
    lda theA+1     : adc #0 : sta theA+1
    lda theCX : cmp #80
    ; wrap line
    { bne no : lda #0 : sta theCX : jsr upA8 : .no }
    rts

;;;----------------------------------------------------------------------
;;; up/down...

.up4: jsr up1
.up3: jsr up1
.up2: jsr up1
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


.down3: jsr down1
.down2: jsr down1
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

.saveLastKeys:
    lda keyL : sta lastKeyL
    lda keyR : sta lastKeyR
    lda keyRet : sta lastKeyRet
    rts

.readKeys:
    jsr checkL
    jsr checkR
    jsr checkRet
    jsr checkShift
    rts

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

.checkRet: {
    lda #0 : sta keyRet
    lda #(-keyCodeRet-1) : sta &fe4f : lda &fe4f
    BPL no : lda #1 : sta keyRet : .no
    rts }

.checkShift: {
    lda #0 : sta keyShift
    lda #(-keyCodeShift-1) : sta &fe4f : lda &fe4f
    BPL no : lda #1 : sta keyShift : .no
    rts }

;;;----------------------------------------------------------------------
.end:

print "bytes left: ", screenStart-*

SAVE "Code", start, end
