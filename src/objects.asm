
;;; Objects, with render loop and synced update

;;; TODO
;;; - game logic: (space)start, lives, gameover

;;; TODO
;;; - game logic: level cleared
;;; - more keys: z/x:alternative-turn
;;; - global state for direction, updated by z/x, caps/control
;;; - change ship outline when direction changes
;;; - child rock inherits position(+ random delta) from parent

;;; - bullet inherits position from ship & speed from direction
;;; - support speed on all objects, and update position
;;; - random position when large rocks initially spawn
;;; - child rock inherits speed(+ random delta) from parent
;;; - reinstate ship controls: thrust/decaying speed
;;; - sounds


;;; MOS vectors & zero page use
interruptSaveA = &fc
irq1v = &204

;;; Sheila
ula                         = &fe21
system_VIA_portB            = &fe40
system_VIA_dataDirectionB   = &fe42
system_VIA_dataDirectionA   = &fe43
system_VIA_interruptFlags   = &fe4d
system_VIA_interruptEnable  = &fe4e
system_VIA_portA            = &fe4f

;;; MOS entry points
osasci = &ffe3
oswrch = &ffee

;;; Mode-1
screenStart = &3000
screenEnd = &8000

NUM = 32 ;; Number of objects, indexed consistently using X-register

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copy

macro Copy16iv I,V
    lda #LO(I) : sta V
    lda #HI(I) : sta V+1
endmacro

macro Copy16v A,B
    lda A   : sta B
    lda A+1 : sta B+1
endmacro

;;; Per-object tables of two-byte values are stored with with split LO and HI bytes
macro Copy16ix I,V
    lda #LO(I) : sta V, x
    lda #HI(I) : sta V+NUM, x
endmacro

macro Copy16xv A,B
    lda A,     x : sta B
    lda A+NUM, x : sta B+1
endmacro

macro Copy16xx A,B
    lda A, x     : sta B, x
    lda A+NUM, x : sta B+NUM, x
endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Position, Emit, Space

macro Position X,Y
    lda #31 : jsr osasci
    lda #X : jsr osasci
    lda #Y : jsr osasci
endmacro

macro Emit C
    pha
    lda #C
    jsr emit
    pla
endmacro

macro Space
    Emit ' '
endmacro

macro Puts S
    Copy16iv msg, msgPtr
    jmp after
.msg: EQUS S, 0
.after:
    jsr printString
endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Crash

macro Crash S
    Copy16iv msg, msgPtr
    jsr printString
    jmp spin
.msg:EQUS S, 0
endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; zero page

org &70
.msgPtr skip 2

.theX skip 2 ; plot from Hi byte + hi-bit of LO byte
.theY skip 2 ; plot only from HI byte

.theCX SKIP 1 ; coarse-X : 0..79
.theCY SKIP 1 ; coarse-Y : 0..31
.theFX SKIP 1 ; fine-X   : 0..3
.theFY SKIP 1 ; fine-Y   : 0..7
.theA SKIP 2  ; screen address

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Start

guard screenStart
org &1100

.start:
    jmp main

.spin: jmp spin

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; printString, printHexA, emit

.printString: {
    ldy #0
.loop
    lda (msgPtr),y
    beq done
    jsr osasci
    iny
    bne loop
.done:
    rts
    }

.printHexA: {
    pha
    and #&f0 : lsr a : lsr a : lsr a : lsr a : tay
    lda digits,y
    jsr osasci
    pla
    and #&f : tay
    lda digits,y
    jsr osasci
    rts
.digits: EQUS "0123456789abcdef"
    }

.emit:
    jmp osasci

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init

.mode1:
    lda #22 : jsr oswrch
    lda #1 : jsr oswrch
    rts

.cursorOff:
    lda #23 : jsr oswrch
    lda #1 : jsr oswrch
    lda #0 :
    jsr oswrch : jsr oswrch : jsr oswrch : jsr oswrch
    jsr oswrch : jsr oswrch : jsr oswrch : jsr oswrch
    rts

.setLogicalThreeAsCyan:
    lda #19 : jsr oswrch
    lda #3 : jsr oswrch ; logical three
    lda #6 : jsr oswrch ; physical cyan
    lda #0 : jsr oswrch : jsr oswrch : jsr oswrch
    rts

.selectLogicalTwo:
    lda #17 : jsr oswrch
    lda #2 : jsr oswrch
    rts

.setupColours:
    jsr setLogicalThreeAsCyan
    jsr selectLogicalTwo ; for debug info in yellow
    rts

.setupZeroRts: {
    lda rts : sta 0
.rts:
    rts
    }

.init:
    lda #%01111111 : sta system_VIA_interruptEnable ; disable all interrupts
    lda #%10000010 : sta system_VIA_interruptEnable ; enable just VBlank
    ;; start in keys mode
    lda #%00000011 : sta system_VIA_portB ; set bit 3 to 0
    lda #%01111111 : sta system_VIA_dataDirectionA ; (top bit input)
    jsr mode1
    jsr cursorOff
    jsr setupColours
    jsr setupZeroRts
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sounds

.playSounds:
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keys

.startKeys:

.keySpace EQUB 0
.keyCaps  EQUB 0
.keyCtrl  EQUB 0
.keyShift EQUB 0
.keyEnter EQUB 0

.keyUp    equb 0
.keyDown  equb 0
.keyLeft  equb 0
.keyRight equb 0
.keyTab   equb 0
.keyA     equb 0

.endKeys:
numberKeys = endKeys - startKeys
skip numberKeys ;; space to store LAST state

macro PollKey Code, Key
    lda #0 : sta Key
    lda #(-Code-1) : sta system_VIA_portA : lda system_VIA_portA
    bpl no : lda #1 : sta Key : .no
endmacro

macro Edge Key
    lda Key : sta Key+numberKeys
endmacro

.readKeys:
	;; Caps/Ctrl/Shift are level sensitive keys
    Edge keySpace
    Edge keyEnter
    Edge keyTab ;; DEV: selected object
    Edge keyA ;; DEV: toggle active state

    PollKey -99,  keySpace
    PollKey -65,  keyCaps
    PollKey -2,   keyCtrl
    PollKey -1,   keyShift
    PollKey -74,  keyEnter
    PollKey -97,  keyTab
    PollKey -66,  keyA
    PollKey -58,  keyUp
    PollKey -42,  keyDown
    PollKey -26,  keyLeft
    PollKey -122, keyRight
    rts

macro CheckPress Key ; -> NZ
    lda Key+numberKeys : eor #&ff : and Key
endmacro

.printKeyState:
    lda #'.' : { ldy keySpace: beq no : lda #'B' : .no } : jsr emit
    Space
    lda #'.' : { ldy keyCaps : beq no : lda #'C' : .no } : jsr emit
    lda #'.' : { ldy keyCtrl : beq no : lda #'C' : .no } : jsr emit
    lda #'.' : { ldy keyShift: beq no : lda #'S' : .no } : jsr emit
    lda #'.' : { ldy keyEnter: beq no : lda #'E' : .no } : jsr emit
    Space
    lda #'.' : { ldy keyTab  : beq no : lda #'T' : .no } : jsr emit
    lda #'.' : { ldy keyA    : beq no : lda #'A' : .no } : jsr emit
    Space
    lda #'.' : { ldy keyUp   : beq no : lda #'U' : .no } : jsr emit
    lda #'.' : { ldy keyDown : beq no : lda #'D' : .no } : jsr emit
    lda #'.' : { ldy keyLeft : beq no : lda #'L' : .no } : jsr emit
    lda #'.' : { ldy keyRight: beq no : lda #'R' : .no } : jsr emit
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Calculate screen address from X/Y

.calcA: ; theX/theY --> the{A,FX,CX,FY,CY}
    {
    lda theX+1
    lsr a
    sta theCX
    lsr a : lsr a : lsr a : lsr a
    sta smc_hbOnRow+1

    lda theY+1
    lsr a : lsr a : lsr a
    sta theCY
    asl a : asl a : clc
    adc theCY
    .smc_hbOnRow : adc #&ee
    lsr a
    clc : adc #HI(screenStart)
    sta theA+1

    lda theCY : and #1              ; oddRow
    asl a : asl a : asl a : asl a   ; Xoffset
    eor theCX                       ; Xmod
    asl a : asl a : asl a           ; Xmod*8
    sta smc_alo+1

    lda theY+1
    and #&7 : sta theFY
    clc : .smc_alo : adc #&ee       ; adjust A with fineY
    sta theA

    lda theX ; (only need hi-order bit; rotate into carry)
    asl a
    lda theX+1
    rol a : and #&3 : sta theFX
    rts
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; incremental movement

.left1: {
    lda theFX : bne no
    jsr left4
    lda #4 : sta theFX
.no:
    dec theFX
    rts }

.left4:
    lda theCX
    ; unwrap line
    { bne no : lda #80 : sta theCX : jsr downA8 : .no }
    dec theCX
    lda theA : sec : sbc #8 : sta theA
    { bcs no : dec theA+1 : .no }
    rts

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
    { bcc no : inc theA+1 : .no }
    lda theCX : cmp #80
    ; wrap line
    { bne no : lda #0 : sta theCX : jsr upA8 : .no }
    rts

.upA8: ; A
    lda theA : sec : sbc #&80 : sta theA
    lda theA+1     : sbc #2   : sta theA+1
    rts

.downA8: ; A
    lda theA : clc : adc #&80 : sta theA
    lda theA+1     : adc #2   : sta theA+1
    rts

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

.wrapScreen: { ; CY,A
    lda theCY
    cmp #32 : bne no
    lda #0 : sta theCY
    lda theA+1 : sec : sbc #HI(screenEnd-screenStart) : sta theA+1
.no:
    rts }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Outline descriptions

N = 1
S = 2
E = 4
W = 8
INVISIBLE = 16
START = 32

;;;macro Center : EQUB START : endmacro ; see center of rotation
;;;macro Center : EQUB INVISIBLE : endmacro ; DONT see center of rotation
macro Center : endmacro  ; DONT see center of rotation (and dont even waste a byte!)

NE = N or E
SE = S or E
NW = N or W
SW = S or W

Ni = N or INVISIBLE
Si = S or INVISIBLE
Wi = W or INVISIBLE
SWi = SW or INVISIBLE

.invisible equb INVISIBLE
.asNorth equb N
.asSouth equb S
.asEast equb E
.asWest equb W

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; outlines

.smallRockOutline:
    equb START,E,SE,NE,E,SE,S, SE,S,SW,S,SW, W,NW,SW,W, NW,NE,NW,NW,N,NE, 0

.mediumRockOutline:
    equb START, E,E,NE,NE,E,E,E,E,SE,SE,E,SE,SE, SW,SW,SW,SE,SE,SE,S,S,SW,SW,SW,W,W
    equb NW,NW,SW,SW,W,W,W, NW,NW,NW,NE,NE,NW,NW,NW,N,N,NE,NE, 0

.largeRockOutline:
    equb START
    equb E,E,E,E,E,E,E,E, SE,SE,SE,SE, E,E,E,E,E,E, SE,SE,SE,SE,SE, SW,SW,SW,SW,SW, SE,SE,SE, S,S
    equb SW,SW,SW,SW,SW,SW,SW, W,W,W,W,W, NW,NW,NW,NW, SW,SW,SW, W,W,W, NW,NW,NW
    equb N,N,N,N, NW,NW, N,N,N,N, NE,NE,NE,NE, NW,NW,NW, NE,NE,NE,NE, 0

.shipOutline1: Center
    equb Si,Si, S,W,W,W,SW,N,N, NE,N,N,NE,N,N,NE,N,N,NE, SE,S,S,SE,S,S,SE,S,S,SE,S,S, NW,W,W, 0

.bulletOutline:
    equb START, E, SW, NW, NE, 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; draw

.outlineIndex skip 1
.outlineMotion skip 1

.drawOutline: {
    jsr calcA
    ldy #0
.loop:
    .*SMC_outline : lda &7777, y : beq rts
    iny
    sty outlineIndex
    sta outlineMotion
    bit asNorth : bne north
    bit asSouth : bne south
    jmp ew
.north:
    jsr up1
    jmp ew
.south:
    jsr down1
.ew:
    lda outlineMotion
    bit asEast : bne east
    bit asWest : bne west
    jmp pr
.east:
    jsr right1
    jmp pr
.west:
    jsr left1
.pr:
    lda outlineMotion
    bit invisible : bne nextPoint
    .*SMC_onPoint : jmp &7777
.*nextPoint:
    ldy outlineIndex
    jmp loop
.rts:
    rts
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Object state tables

;;; object kinds & counts by kind
KindShip = 0
KindRock = 1
KindBullet = 2
.countPerKind skip 3

.myKind skip NUM
.myOutline skip 2*NUM
.childA skip NUM
.childB skip NUM

;;; per-object update/render functions
.updateF skip 2*NUM
.renderF skip 2*NUM

;;; per-object plot/unplot routines for draw/onPoint
.myPlot skip 2*NUM
.myUnPlot skip 2*NUM

;;; per-object bits
.isActive skip NUM ; state that is desired for the object; set in update
.isRendered skip NUM ; reflection of activeness when rendered
.isHit skip NUM ; set during render phase; consulted during update

.mySpawnTime skip NUM ; for timed-death of bullet
.myDeathTime skip NUM ; for next-life ship reactivation

;;; logical object position; set during update
.myX skip 2*NUM
.myY skip 2*NUM

;;; rendered object position, to enable unplot
.renderedX skip 2*NUM
.renderedY skip 2*NUM ; we dont need/use HI-byte

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; colour choice & collision detection (onPoint)

.masks:       equb &88, &44, &22, &11
.redMasks:    equb &08, &04, &02, &01
.yellowMasks: equb &80, &40, &20, &10

macro RockPixel ; draw in cyan
    ldy theFX : lda masks,y
    ldy #0 : eor (theA),y : sta (theA),y
endmacro

macro ShipPixel ; draw in yellow
    ldy theFX : lda yellowMasks,y
    ldy #0 : eor (theA),y : sta (theA),y
endmacro

macro BulletPixel ; draw in red
    ldy theFX : lda redMasks,y
    ldy #0 : eor (theA),y : sta (theA),y
endmacro

macro RockHit ; only red hits (sadly can be caused by debug yellow turning red on eor)
    ldy #0 : lda (theA),y
    ldy theFX : and masks,y : eor redMasks,y ; and;eor!
    { bne noHit : inc isHit,x : .noHit } ; bne!
endmacro

macro ShipHit ; red or cyan hits (not yellow, so debug info wont hit us)
    ldy #0 : lda (theA),y
    ldy theFX : and redMasks,y
    { beq noHit : inc isHit,x : .noHit }
endmacro

macro BulletHit ; yellow or cyan hits (not red, so bullets dont interfer)
    ldy #0 : lda (theA),y
    ldy theFX : and yellowMasks,y
    { beq noHit : inc isHit,x : .noHit }
endmacro

;;; When plotting: we first check for collision; then flip the pixel.
;;; When unplotting we do the reverse: we flip the pixel; then check for collision.

.rockPlot:   RockHit : RockPixel : jmp nextPoint
.rockUnPlot: RockPixel : RockHit : jmp nextPoint

.shipPlot:   ShipHit : ShipPixel : jmp nextPoint
.shipUnPlot: ShipPixel : ShipHit : jmp nextPoint

.bulletPlot:   BulletHit : BulletPixel : jmp nextPoint
.bulletUnPlot: BulletPixel : BulletHit : jmp nextPoint

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global state

.frameCounter: skip 1
.lastRenderedFrameObject0 skip 1 ; DEV
.selectedN: skip 1 ; DEV

.livesRemaining skip 1
.wavesCleared skip 1 ; TODO
.score skip 1

.stepSelectedObject:
    inc selectedN
    lda selectedN : cmp #NUM : { bne no : lda #0 : sta selectedN : .no }
    rts

.updateSelectedObjectOnTab:
    CheckPress keyTab
    { beq no : jsr stepSelectedObject : .no }
    rts

.fireAttempted skip 1

MaxBullets = 4

.tryFireOnEnter: {
    CheckPress keyEnter : beq no
    lda countPerKind + KindBullet
    cmp #MaxBullets : bpl no
    lda #1 : sta fireAttempted ; will be cleared by first available bullet
.no:
    rts
    }

.watchForGameOver: {
    lda livesRemaining : bne no
    lda #0 : sta gameIsRunning
.no:
    rts
    }

.updateGlobalWhenRunning:
    jsr updateSelectedObjectOnTab
    jsr tryFireOnEnter
    jsr watchForGameOver
    rts

.offerGameStartMessage:
    Position 10,0
    Puts "Press Space to start"
    rts

.clearGameStartMessage:
    Position 10,0
    Puts "                    "
    rts

.updateGlobalWhenWaiting: {
    CheckPress keySpace : beq no
    jsr clearGameStartMessage
    lda #1 : sta gameIsRunning
    jsr activateObjectsForLevelStart
    rts
.no:
    jsr offerGameStartMessage
    rts
    }

.gameIsRunning skip 1

.updateGlobal:
    lda gameIsRunning : beq updateGlobalWhenWaiting
    jmp updateGlobalWhenRunning

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Objects... DEV movement/control

.moveUp:   dec myY+NUM, x : rts
.moveDown: inc myY+NUM, x : rts

.moveLeft:
    lda myX, x     : sec : sbc #&80                                          : sta myX, x
    lda myX+NUM, x :       sbc   #0 : { cmp #&ff : bne no : lda #159 : .no } : sta myX+NUM, x
    rts

.moveRight:
    lda myX,     x : clc : adc #&80                                          : sta myX, x
    lda myX+NUM, x :       adc   #0 : { cmp #160 : bne no : lda #0   : .no } : sta myX+NUM, x
    rts

.updatePositionIfSelectedAndActive: {
    cpx selectedN : bne no ; not selected
    lda isActive, x : beq no ; not active
    CheckPress keyUp    : { beq no : jsr moveUp    : .no }
    CheckPress keyDown  : { beq no : jsr moveDown  : .no }
    CheckPress keyLeft  : { beq no : jsr moveLeft  : .no }
    CheckPress keyRight : { beq no : jsr moveRight : .no }
.no:
    rts
    }

.toggleActiveness:
    lda isActive, x
    beq spawnObject
    jmp deactivate

.toggleActivenessIfSelected: {
    CheckPress keyA : beq no
    cpx selectedN : bne no
    jsr toggleActiveness
.no:
    rts
    }

.updateObjectDEV:
    jsr toggleActivenessIfSelected
    jsr updatePositionIfSelectedAndActive
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Update logic for game behaviour

.spawnObject: {
    lda isActive, x : bne no ; no, already active
    lda #1 : sta isActive, x
    ldy myKind, x : lda countPerKind, y : clc : adc #1 : sta countPerKind, y
    lda frameCounter : sta mySpawnTime, x
.no:
    rts
    }

.spawnChildren:
    stx restoreX+1
    lda childB, x : pha
    lda childA, x : tax : jsr spawnObject
    pla : tax : jsr spawnObject
    .restoreX : ldx #&77
    rts

.deactivate: {
    lda isActive, x : beq no ; no, already dead
    lda #0 : sta isActive, x
    ldy myKind, x : lda countPerKind, y : sec : sbc #1 : sta countPerKind, y
    lda frameCounter : sta myDeathTime, x
.no:
    rts
    }

.dieAfterTwoSeconds: {
    lda mySpawnTime, x : clc : adc #100
    cmp frameCounter : bpl no
    jsr deactivate
.no:
    rts
    }

.spawnIfFiringAttempted:
    lda fireAttempted : beq no ; not attempted
    lda isActive, x : bne no ; not if we are already active
    jsr spawnObject
    lda #0 : sta fireAttempted ; just one bullet!
.no:
    rts

.bulletUpdate: {
    jsr updateObjectDEV
    jsr spawnIfFiringAttempted
    jsr dieAfterTwoSeconds
    lda isHit, x : beq no
    jsr deactivate
.no:
    rts
    }

.increaseScore:
    sed : clc : adc score : cld ;; decimal mode!
    sta score
    rts

.largeRockUpdate: {
    jsr updateObjectDEV
    lda isHit, x : beq no
    lda isActive, x : beq no
    jsr deactivate
    jsr spawnChildren
    lda #2 : jsr increaseScore
.no:
    rts
    }

.mediumRockUpdate: {
    jsr updateObjectDEV
    lda isHit, x : beq no
    lda isActive, x : beq no
    jsr deactivate
    jsr spawnChildren
    lda #5 : jsr increaseScore
.no:
    rts
    }

.smallRockUpdate: {
    jsr updateObjectDEV
    lda isHit, x : beq no
    lda isActive, x : beq no
    jsr deactivate
    lda #10 : jsr increaseScore
.no:
    rts
    }

.repositionShipInCenter:
    lda #80 : sta myX+NUM, x
    lda #128 : sta myY+NUM, x
    rts

.reactivateShipAfterTwoSeconds: {
    lda gameIsRunning : beq no
    lda isActive, x : bne no
    lda myDeathTime, x : clc : adc #100
    cmp frameCounter : bpl no
    jsr repositionShipInCenter
    jsr spawnObject
.no:
    rts
    }

.shipUpdate: {
    jsr updateObjectDEV
    jsr reactivateShipAfterTwoSeconds
    lda isHit, x : beq no
    lda isActive, x : beq no
    jsr deactivate
    dec livesRemaining
.no:
    rts
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Render object

macro DebugPositionForObject
    lda #31 : jsr osasci
    lda #33 : jsr osasci ; Xpos
    txa : jsr osasci ; Ypos
endmacro

.debugObject:
    DebugPositionForObject
    Space : lda #'.' : { cpx selectedN : bne no : lda #'*' : .no } : jsr emit
    Space
    lda #'.' : { ldy isActive, x : beq no : lda #'a' : .no } : jsr emit
    lda #'.' : { ldy isRendered, x : beq no : lda #'r' : .no } : jsr emit
    lda #'.' : { ldy isHit, x : beq no : lda #'H' : .no } : jsr emit
    rts

.renderObject: {
    ;;jsr debugObject
    lda #0 : sta isHit,x ; clear hit counter
    Copy16xv myOutline, SMC_outline+1

    ;; if we are rendered, unplot...
    lda isRendered, x : beq afterUnplot
    Copy16xv myUnPlot, SMC_onPoint+1
    Copy16xv renderedX, theX
    Copy16xv renderedY, theY
    jsr drawOutline
    lda #0 : sta isRendered, x
.afterUnplot:

	;; if detect hit during unplot, DONT re-plot to avoid incorrect secondary collision
    lda isHit, x : bne afterPlot
    ;; if we are active, plot...
    lda isActive, x : beq afterPlot
    Copy16xv myPlot, SMC_onPoint+1
    Copy16xv myX, theX
    Copy16xv myY, theY
    jsr drawOutline
    ;; remember position at which we rendered
    Copy16xx myX, renderedX
    Copy16xx myY, renderedY
    lda #1 : sta isRendered, x
.afterPlot:
    rts
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IRQ, VBlank syncing

.vsyncNotify equb 0 ; -> 1

.myIRQ: {
    lda system_VIA_interruptFlags : and #2 : bne vblank
    Crash "unexpected interrupt"
    lda #&7f : sta system_VIA_interruptFlags ; ack
    lda interruptSaveA
    rti
.vblank:
    sta system_VIA_interruptFlags ; ack
    lda vsyncNotify : bne crash
    inc vsyncNotify
    lda interruptSaveA
    rti
.crash:
    Crash " SYNC"
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; onSync (update-all)

.updateN: skip 1
.updateObjects: {
    lda #0 : sta updateN
.loop:
    ldx updateN : cpx #NUM : { bne no : rts : .no }
    Copy16xv updateF, dispatch+1
    .dispatch : jsr &7777
    inc updateN
    jmp loop
    }

.onSync:
    inc frameCounter
    jsr playSounds
    jsr readKeys
    ;;Position 34,31 : lda frameCounter : jsr printHexA : Space : lda renderN : jsr printHexA
    ;;Position 1,28 : Space : jsr printKeyState
    jsr updateGlobal
    jsr updateObjects
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Render loop.

;;; show a nice representation of how many frames lag we have
;;; if frameCounter-lastRenderedFrame is 1 (or less) then we have no lag
;;; if 2 then we have a lag of 1, etc. show each unit off lag with a "+"
.printLag: {
    lda frameCounter : sec : sbc lastRenderedFrameObject0
    tay : beq zero
    lda #'+'
.loop:
    dey : beq done : jsr emit : jmp loop
.zero:
    lda #'@' : jsr emit ; we rendered everything and the frame count has not advanced at all!
.done:
    lda #' ' : jsr emit
    lda #' ' : jsr emit
    lda #' ' : jsr emit
    rts
    }

.printObjectCountsByKind:
    Position 28,30
    lda #'s' : jsr emit : lda countPerKind + KindShip : jsr printHexA
    Space : lda #'r' : jsr emit : lda countPerKind + KindRock : jsr printHexA
    Space : lda #'b' : jsr emit : lda countPerKind + KindBullet : jsr printHexA
    rts

.printGameInfo:
    Position 12,30
    lda #'.' : { ldy gameIsRunning: beq no : lda #'R' : .no } : jsr emit
    Space : lda #'L' : jsr emit : lda livesRemaining : jsr printHexA
    Space : lda #'W' : jsr emit : lda wavesCleared : jsr printHexA
    Space : lda #'S' : jsr emit : lda score : jsr printHexA
    ; Space : lda #'.' : { ldy fireAttempted: beq no : lda #'F' : .no } : jsr emit
    rts

.printGlobalState:
    ;;jsr printObjectCountsByKind
    jsr printGameInfo
    rts

.renderN: skip 1
.renderLoop: {
.loop:
    lda vsyncNotify : { beq no : dec vsyncNotify : jsr onSync : .no }
    ldx renderN : cpx #NUM : bne notZeroObject
    ldx #0 : stx renderN
    Position 1,30
    ;;lda frameCounter : sec : sbc lastRenderedFrameObject0 : jsr printHexA : Space
    jsr printLag
    lda frameCounter : sta lastRenderedFrameObject0
    jsr printGlobalState
.notZeroObject:
    Copy16xv renderF, dispatch+1
    .dispatch : jsr &7777
    inc renderN
    jmp loop
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; create

.createObject:
    ;; set generic render behaviour
    Copy16ix renderObject, renderF
    ;; DEV: setup initial HI-byte of position, based on obj#
    txa : and #7 : asl a : asl a : asl a : asl a
    sta myX+NUM, x
    txa : and #%11111000 : asl a : asl a
    sta myY+NUM, x
    rts

.createRock:
    Copy16ix rockPlot, myPlot
    Copy16ix rockUnPlot, myUnPlot
    lda #KindRock : sta myKind, x
    jmp createObject

.createRockS:
    Copy16ix smallRockOutline, myOutline
    Copy16ix smallRockUpdate, updateF
    jmp createRock

.createRockM:
    Copy16ix mediumRockOutline, myOutline
    Copy16ix mediumRockUpdate, updateF
    jmp createRock

.createRockL:
    Copy16ix largeRockOutline, myOutline
    Copy16ix largeRockUpdate, updateF
    jmp createRock

.createShip:
    Copy16ix shipOutline1, myOutline
    Copy16ix shipPlot, myPlot
    Copy16ix shipUnPlot, myUnPlot
    Copy16ix shipUpdate, updateF
    lda #KindShip : sta myKind, x
    jmp createObject

.createBullet:
    Copy16ix bulletOutline, myOutline
    Copy16ix bulletPlot, myPlot
    Copy16ix bulletUnPlot, myUnPlot
    Copy16ix bulletUpdate, updateF
    lda #KindBullet : sta myKind, x
    jmp createObject

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Game init.

.createAllObjects:
    ldx #1 : jsr createRockL : lda #2 : sta childA, x : lda #3 : sta childB, x
    ldx #2 : jsr createRockM : lda #4 : sta childA, x : lda #5 : sta childB, x
    ldx #3 : jsr createRockM : lda #6 : sta childA, x : lda #7 : sta childB, x
    ldx #4 : jsr createRockS
    ldx #5 : jsr createRockS
    ldx #6 : jsr createRockS
    ldx #7 : jsr createRockS

    ldx #9 : jsr createRockL : lda #10 : sta childA, x : lda #11 : sta childB, x
    ldx #10 : jsr createRockM : lda #12 : sta childA, x : lda #13 : sta childB, x
    ldx #11 : jsr createRockM : lda #14 : sta childA, x : lda #15 : sta childB, x
    ldx #12 : jsr createRockS
    ldx #13 : jsr createRockS ; debug print for 13 is strangely/wrongly placed by MOS?
    ldx #14 : jsr createRockS
    ldx #15 : jsr createRockS

    ldx #17 : jsr createRockL : lda #18 : sta childA, x : lda #19 : sta childB, x
    ldx #18 : jsr createRockM : lda #20 : sta childA, x : lda #21 : sta childB, x
    ldx #19 : jsr createRockM : lda #22 : sta childA, x : lda #23 : sta childB, x
    ldx #20 : jsr createRockS
    ldx #21 : jsr createRockS
    ldx #22 : jsr createRockS
    ldx #23 : jsr createRockS

    ldx #24 : jsr createShip : stx selectedN
    ldx #25 : jsr createBullet
    ldx #26 : jsr createBullet
    ldx #27 : jsr createBullet
    ldx #28 : jsr createBullet

    rts

.activateObjectsForLevelStart:
    ;; 3 large rocks
    ldx #1 : jsr spawnObject
    ldx #9 : jsr spawnObject
    ldx #17 : jsr spawnObject
	;; and the ship
    ldx #24
    jsr repositionShipInCenter
    jsr spawnObject
    lda #3 : sta livesRemaining
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; main

.main:
    jsr init
    jsr createAllObjects
    Copy16iv myIRQ, irq1v ; initialise sync
    jmp renderLoop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.end:
print "bytes remaining: ", screenStart-*
SAVE "Code", start, end
