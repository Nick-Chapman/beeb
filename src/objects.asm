
;;; Objects, with render loop and synced update

;;; DONE
;;; - randomness: large rock spawn position and speed
;;; - randomness: speed delta on crack

;;; TODO
;;; - track when object has moved; avoid unplot/plot when unmoved
;;; - switch to own emit (faster; fix code 13 issue)
;;; - other performance improvement?
;;; - sound!
;;; - game logic: (space)start, lives, gameover, level cleared, startup banner
;;; - keys: z/x: alternative-turn

Debug = FALSE
DebugObjects = FALSE

NUM = 32 ;; Number of objects, indexed consistently using X-register

ShipObjectNum = 0
MaxBullets = 4

MomentumIndex = 7  ; Higher is more momentum (and increased max speed)
ImpulseIndex = 4 ; Higher is lower thrust (and decreased max speed)

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

TopLinesNotUsed = 2 ; must be even; used for high score etc

MaxCoarseX = 80
MaxCoarseY = 32 - TopLinesNotUsed

BytesPerLine = MaxCoarseX * 8 ; 640

;;; Mode-1
screenStart = &3000
lineTwoStart = screenStart + (TopLinesNotUsed * BytesPerLine)
screenSize = MaxCoarseY * BytesPerLine

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copy, B=A

macro Copy16iv A,B
    lda #LO(A) : sta B
    lda #HI(A) : sta B+1
endmacro

;;; Per-object tables of two-byte values are stored with with split LO and HI bytes
;;; indexed using reg-X, from 0..NUM-1
macro Copy16ix A,B
    lda #LO(A) : sta B, x
    lda #HI(A) : sta B+NUM, x
endmacro

macro Copy16xv A,B
    lda A,     x : sta B
    lda A+NUM, x : sta B+1
endmacro

macro Copy16xx A,B
    lda A, x     : sta B, x
    lda A+NUM, x : sta B+NUM, x
endmacro

;; for when one object inherits from another
macro Copy16xy A,B
    lda A, x     : sta B, y
    lda A+NUM, x : sta B+NUM, y
endmacro

macro Add16vv A, B ; A = A+B
    clc
    lda A
    adc B
    sta A
    lda A+1
    adc B+1
    sta A+1
endmacro

macro Add16xv A, B ; A = A+B
    clc
    lda A, x
    adc B
    sta A, x
    lda A+NUM, x
    adc B+1
    sta A+NUM, x
endmacro

macro Add16xx A, B ; A = A+B
    clc
    lda A, x
    adc B, x
    sta A, x
    lda A+NUM, x
    adc B+NUM, x
    sta A+NUM, x
endmacro

macro Add16yx A, B ; A = A+B
    clc
    lda A, y
    adc B, x
    sta A, y
    lda A+NUM, y
    adc B+NUM, x
    sta A+NUM, y
endmacro

macro Sub16vx A, B ; A = A-B
    sec
    lda A
    sbc B, x
    sta A
    lda A+1
    sbc B+NUM, x
    sta A+1
endmacro

;;; spelt with "k" to avoid clash with "inc" op
macro Inkrement16v A ; A = A+1
    clc
    lda A
    adc #1
    sta A
    lda A+1
    adc #0
    sta A+1
endmacro

macro Halve V
    lda V+1 : cmp #&80 : ror a : sta V+1
    lda V              : ror a : sta V
endmacro

macro Invert16x A ; A = -A
    sec
    lda #0
    sbc A, x
    sta A, x
    lda #0
    sbc A+NUM, x
    sta A+NUM, x
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
    jsr selectLogicalTwo ; for debug info in yellow. default is cyan
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

.keySpace EQUB 0 ; TODO: start game
.keyCaps  EQUB 0
.keyCtrl  EQUB 0
.keyShift EQUB 0
.keyEnter EQUB 0

.keyK     equb 0
.keyW     equb 0
.keyN     equb 0

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
    Edge keySpace
    Edge keyEnter

    Edge keyK
    Edge keyW
    Edge keyN

    PollKey -99,  keySpace
    PollKey -65,  keyCaps
    PollKey -2,   keyCtrl
    PollKey -1,   keyShift
    PollKey -74,  keyEnter

    PollKey -71,  keyK
    PollKey -34,  keyW
    PollKey -86,  keyN

    rts

macro CheckPress Key ; -> NZ
    lda Key+numberKeys : eor #&ff : and Key
endmacro

.printKeyState:
    Position 15,0
    lda #'.' : { ldy keyCaps : beq no : lda #'L' : .no } : jsr emit
    lda #'.' : { ldy keyCtrl : beq no : lda #'C' : .no } : jsr emit
    lda #'.' : { ldy keySpace: beq no : lda #'B' : .no } : jsr emit
    lda #'.' : { ldy keyShift: beq no : lda #'S' : .no } : jsr emit
    lda #'.' : { ldy keyEnter: beq no : lda #'E' : .no } : jsr emit
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
    clc : adc #HI(lineTwoStart)
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
    lda theA : sec : sbc #LO(BytesPerLine) : sta theA
    lda theA+1     : sbc #HI(BytesPerLine) : sta theA+1
    rts

.downA8: ; A
    lda theA : clc : adc #LO(BytesPerLine) : sta theA
    lda theA+1     : adc #HI(BytesPerLine) : sta theA+1
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
    lda #MaxCoarseY : sta theCY
    lda theA+1 : clc : adc #HI(screenSize) : sta theA+1
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
    cmp #MaxCoarseY : bne no
    lda #0 : sta theCY
    lda theA+1 : sec : sbc #HI(screenSize) : sta theA+1
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

.bulletOutline:
    equb START, E, SW, NW, NE, 0

.shipOutline1: Center
    equb Si,Si, S,W,W,W,SW,N,N, NE,N,N,NE,N,N,NE,N,N,NE, SE,S,S,SE,S,S,SE,S,S,SE,S,S, NW,W,W, 0

.shipOutline2: Center
    equb SWi, S,W,Wi,W,NE,N,NE,N,NE,N,NE,NE,N,NE,NE, S,S,S,S,S,S,S,S,S,S,S,S,S, NW,W,W, 0

.shipOutline3: Center
    equb Wi, SW,W,Wi,W, NE,NE,NE,NE,NE,NE,NE,NE,NE,NE, S,S,S,SW,S,S,S,S,SW,S,S,S,S,S, Ni,NW,W,NW,W, 0

.shipOutline4: Center
    equb Si, SW,W,NW,Wi,W, NE,NE,NE,NE,NE,E,NE,NE,NE,NE,E, S,SW,S,S,SW,S,S,SW,S,S,SW,S,S, Ni,NW,W, 0

.shipOutline5: Center
    equb SWi, SW,NW,NW,W, NE,E,NE,E,NE,NE,E,NE,E,NE,E, S,SW,S,SW,SW,S,SW,S,SW,S,SW, N,NW, 0

.shipOutlines:
    EQUW shipOutline1,shipOutline2,shipOutline3,shipOutline4
    EQUW shipOutline5,shipOutline4,shipOutline3,shipOutline2


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
;;; Global state

.frameCounter: skip 1

.score skip 2

;;; only needed for Ship
.theThrustX skip 2
.theThrustY skip 2
.theAccX skip 2
.theAccY skip 2

.fireAttempted skip 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Object kinds

KindNull = 0
KindShip = 1
KindRock = 2
KindBullet = 3
NUMK = 4

.kindCount skip NUMK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Object state tables

;;; set during creation
.myKind skip NUM
.myOutline skip 2*NUM ; for ship: changed during update
.myPlot skip 2*NUM
.myUnPlot skip 2*NUM

.myUpdateF skip 2*NUM
.myActivateF skip 2*NUM
.myDeactivateF skip 2*NUM

;;; set during update
.myPosX skip 2*NUM
.myPosY skip 2*NUM
.myDirection skip NUM
.mySpeedX skip 2*NUM
.mySpeedY skip 2*NUM
.isActive skip NUM

.myTimer skip NUM ; for timed-death of bullet

;;; only for large/medium rocks
.childA skip NUM
.childB skip NUM

;;; set during rendering
.isHit skip NUM ; set during render phase; consulted during update
.isRendered skip NUM ; reflection of activeness when rendered
.renderedX skip 2*NUM
.renderedY skip 2*NUM ; we dont need/use HI-byte
.renderedDirection skip NUM
.renderedOutline skip 2*NUM

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
;;; ship direction

QuarterTurn = 8
HalfTurn = 2*QuarterTurn
FullCircle = 4*QuarterTurn

.setOrientation:
    and #%00011100 ; determine "Octrant" (1/8-of-a-turn-sector)
    tay ; 0,4,8,12,16,20,24,28
    lda orientations,y : sta asNorth : iny
    lda orientations,y : sta asEast : iny
    lda orientations,y : sta asSouth : iny
    lda orientations,y : sta asWest
    rts

;;; orientations table; each line is 4 bytes
;;; and represents the transformation for each 1/8-of-a-turn
.orientations:
    equb N,E,S,W
    equb E,N,W,S
    equb W,N,E,S
    equb S,E,N,W
    equb S,W,N,E
    equb W,S,E,N
    equb E,S,W,N
    equb N,W,S,E

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Render object

macro DebugPositionForObject
    lda #31 : jsr osasci
    lda #36 : jsr osasci ; Xpos
    txa : clc : adc #3 : jsr osasci ; Ypos
endmacro

.debugObject: {
    cpx #10 : beq done; dont debug object 10 because line 13 position is weird
    lda myKind, x : beq done ; kind not set, so not a real object
    DebugPositionForObject
    lda #'.' : { ldy isActive, x : beq no : lda #'a' : .no } : jsr emit
    lda #'.' : { ldy isRendered, x : beq no : lda #'r' : .no } : jsr emit
    lda #'.' : { ldy isHit, x : beq no : lda #'H' : .no } : jsr emit
.done:
    rts
    }

.renderObject: {
    IF DebugObjects : jsr debugObject : ENDIF
    lda #0 : sta isHit,x ; clear hit counter
    ;; if we are rendered, unplot...
    lda isRendered, x : beq afterUnplot
    lda renderedDirection, x : jsr setOrientation
    Copy16xv renderedOutline, SMC_outline+1
    Copy16xv renderedX, theX
    Copy16xv renderedY, theY
    Copy16xv myUnPlot, SMC_onPoint+1
    jsr drawOutline
    lda #0 : sta isRendered, x
.afterUnplot:
    ;; if detect hit during unplot, DONT re-plot to avoid incorrect secondary collision
    lda isHit, x : bne afterPlot
    ;; if we are active, plot...
    lda isActive, x : beq afterPlot
    lda myDirection, x : jsr setOrientation
    Copy16xv myOutline, SMC_outline+1
    Copy16xv myPosX, theX
    Copy16xv myPosY, theY
    Copy16xv myPlot, SMC_onPoint+1
    jsr drawOutline
    ;; remember position/outline/direction at which we rendered
    Copy16xx myPosX, renderedX
    Copy16xx myPosY, renderedY
    Copy16xx myOutline, renderedOutline
    lda myDirection, x : sta renderedDirection, x
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
    IF NOT(Debug) : lda vsyncNotify : bne crash : ENDIF
    inc vsyncNotify
    lda interruptSaveA
    rti
.crash:
    Crash " SYNC"
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; printLag

;;; show a nice representation of how many frames lag we have
;;; if frameCounter-lastRenderedFrame is 1 (or less) then we have no lag
;;; if 2 then we have a lag of 1, etc. show each unit off lag with a "+"

.lastRenderedFrameObject0 skip 1

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
    lda frameCounter : sta lastRenderedFrameObject0
    rts
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; print game and DEV info

.printScore:
    Position 1,0
    ;;lda #'.' : { ldy gameIsRunning: beq no : lda #'R' : .no } : jsr emit
    ;;Space : lda #'L' : jsr emit : lda livesRemaining : jsr printHexA
    ;;Space : lda #'W' : jsr emit : lda sheetNumber : jsr printHexA
    ;;Space : lda #'S' : jsr emit
    lda score+1 : jsr printHexA : lda score,x : jsr printHexA : Emit '0'
    ;;Space : lda #'.' : { ldy fireAttempted: beq no : lda #'F' : .no } : jsr emit
    ;;Space : lda #'D' : jsr emit : lda myDirection+ShipObjectNum : jsr printHexA
    rts

ShipSpeedX = mySpeedX + ShipObjectNum
ShipSpeedY = mySpeedY + ShipObjectNum

.printObjectCountsByKind:
    lda #'s' : jsr emit : lda kindCount + KindShip : jsr printHexA
    Space : lda #'r' : jsr emit : lda kindCount + KindRock : jsr printHexA
    Space : lda #'b' : jsr emit : lda kindCount + KindBullet : jsr printHexA
    rts

.printThrustInfo:
    Position 28, 0 : Emit 'a'
    Space : lda theAccX+1 : jsr printHexA : lda theAccX : jsr printHexA
    Space : lda theAccY+1 : jsr printHexA : lda theAccY : jsr printHexA
    Position 28, 1  : Emit 'v'
    Space : lda ShipSpeedX+NUM : jsr printHexA : lda ShipSpeedX : jsr printHexA
    Space : lda ShipSpeedY+NUM : jsr printHexA : lda ShipSpeedY : jsr printHexA
    rts

.printGlobalState:
    jsr printScore
    Position 1,1 : jsr printLag ;; alway print lag for now
    ;;IF Debug : Position 15,1 : jsr printObjectCountsByKind : ENDIF ;; not that useful
    IF Debug : jsr printThrustInfo : ENDIF
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Randomness

.randomBytes:
equb &22,&52,&6a,&51,&a7,&35,&26,&bc,&ce,&54,&e8,&56,&60,&af,&45,&04
equb &ce,&65,&54,&70,&df,&d4,&36,&b1,&7c,&0f,&0d,&dd,&1f,&66,&bd,&98
equb &7e,&a0,&8e,&36,&27,&5a,&9b,&31,&7e,&70,&48,&65,&6f,&39,&45,&60
equb &db,&4f,&fb,&ba,&e4,&7a,&a7,&a7,&96,&f0,&b0,&e6,&a8,&e9,&99,&bb
equb &10,&6f,&28,&02,&dc,&79,&bc,&b3,&18,&18,&81,&cc,&bb,&b3,&e0,&ff
equb &8b,&4f,&11,&e0,&f2,&1b,&ff,&7a,&ee,&37,&c5,&ca,&9d,&57,&ba,&c4
equb &cd,&65,&b5,&43,&f7,&5c,&82,&10,&d2,&8c,&5e,&b0,&c5,&aa,&c6,&1a
equb &bd,&a4,&3a,&f7,&37,&0f,&5c,&5f,&63,&61,&93,&0a,&05,&54,&21,&7a
equb &b2,&c3,&fe,&3f,&74,&a6,&5c,&3e,&ca,&1b,&5c,&26,&57,&ef,&01,&32
equb &f9,&ff,&82,&b4,&ee,&df,&7c,&d6,&2f,&f2,&e5,&20,&84,&3b,&a6,&d0
equb &ac,&2a,&88,&c3,&9b,&01,&81,&9f,&a5,&3a,&c4,&fa,&fc,&6d,&d4,&46
equb &e2,&f6,&7d,&39,&63,&0a,&97,&6d,&b9,&9a,&97,&71,&f8,&ea,&ff,&7f
equb &85,&bc,&88,&06,&3b,&30,&c5,&3f,&33,&3a,&67,&d7,&a7,&f7,&f7,&83
equb &8f,&f6,&ae,&f1,&1f,&07,&3b,&a6,&0b,&3b,&b3,&9b,&f9,&f4,&67,&fe
equb &1a,&c0,&ce,&41,&cc,&26,&13,&b8,&64,&c0,&77,&42,&00,&9f,&63,&e2
equb &70,&3b,&a5,&0d,&f2,&13,&e8,&72,&9b,&e0,&ad,&7e,&aa,&8e,&d0,&f5
assert *-randomBytes = 256

.randomOffset SKIP 1

.getRandomByte: { ; -->A
    inc randomOffset
    sty restoreY+1
    ldy randomOffset
    lda randomBytes,y
    .restoreY : ldy #&77
    rts
    }

macro RandomByte
    jsr getRandomByte
endmacro

macro RandomBit
	RandomByte : and #1
endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; general object behaviour

.activate: {
    lda isActive, x : bne no
    lda #1 : sta isActive, x
    ldy myKind, x : lda kindCount, y : clc : adc #1 : sta kindCount, y
    Copy16xv myActivateF, dispatch+1
    .dispatch : jsr &7777
.no:
    rts
    }

.deactivate: {
    lda isActive, x : beq no
    lda #0 : sta isActive, x
    ldy myKind, x : lda kindCount, y : sec : sbc #1 : sta kindCount, y
    Copy16xv myDeactivateF, dispatch+1
    .dispatch : jsr &7777
.no:
    rts
    }

.deactivateIfHit: {
    lda isActive, x : beq no
    lda isHit, x : beq no
    jsr deactivate
.no:
    rts
    }

.updatePositionFromSpeed:
    Add16xx myPosX, mySpeedX
    Add16xx myPosY, mySpeedY
    lda myPosX+NUM, x
    jsr modMaximumX
    sta myPosX+NUM, x
    lda myPosY+NUM, x
    jsr modMaximumY
    sta myPosY+NUM, x
    rts

.modMaximumX: {
    m = 160
    x = 9
    clc : adc #x
    { cmp #(m+x) : bcc no : sbc #m : .no } ; wrap
    { cmp #x : bcs no : adc #m : .no } ; unwrap
    sec : sbc #x
.done:
    rts
    }

.modMaximumY: {
    m = 8 * MaxCoarseY
    x = 7
    clc : adc #x
    { cmp #(m+x) : bcc no : sbc #m : .no } ; wrap
    { cmp #x : bcs no : adc #m : .no } ; unwrap
    sec : sbc #x
.done:
    rts
    }

.updateGenericObject:
    jsr deactivateIfHit
    jsr updatePositionFromSpeed
    rts

.setCentralPosition:
    lda #80 : sta myPosX+NUM, x
    lda #128 : sta myPosY+NUM, x
    rts

.inheritPosition: ; obX --> obY
    Copy16xy myPosX, myPosX
    Copy16xy myPosY, myPosY
    rts

.startTimer:
    lda frameCounter : sta myTimer, x
    rts

;;; TODO: dont be slower on higher levels
macro Slower : lsr a : endmacro
;macro Slower : endmacro

.setRandomSpeed:
    lda #0
    sta mySpeedX+NUM, x
    sta mySpeedY+NUM, x
    RandomByte : Slower : lsr a : sta mySpeedX, x
    RandomByte : Slower         : sta mySpeedY, x
    RandomBit : { beq no : Invert16x mySpeedX : .no }
    RandomBit : { beq no : Invert16x mySpeedY : .no }
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rock behaviour

.setRandomPosForLargeRock:
    lda #0 : sta myPosY, x
    RandomByte : sta myPosY+NUM, x
    lda #0 : sta myPosX, x
    RandomByte : lsr a : sta myPosX+NUM, x : ror myPosX, x
    ;; 0..127 --> 0..59 / 92..159 (make channel for ship in stanting position)
    lda myPosX+NUM, x
    { cmp #60 : bmi no : clc : adc #32 : .no } ; 60 chosen by visual inspection
    sta myPosX+NUM, x
    rts

.activateRockL:
    jsr setRandomPosForLargeRock
    jsr setRandomSpeed
    rts


.setRandomSpeedY: {stx restoreX+1 : tya : tax : jsr setRandomSpeed : .restoreX : ldx #&77 : rts }

.inheritSpeedWithRandomDelta: ; obX --> obY
    jsr setRandomSpeedY
    Add16yx mySpeedX, mySpeedX
    Add16yx mySpeedY, mySpeedY
    rts

.inheritSpeedAndPosition:
    jsr inheritPosition
    jsr inheritSpeedWithRandomDelta
    rts

.activateY: {stx restoreX+1 : tya : tax : jsr activate : .restoreX : ldx #&77 : rts }

.spawnChildren:
    lda childA, x : tay : jsr inheritSpeedAndPosition : jsr activateY
    lda childB, x : tay : jsr inheritSpeedAndPosition : jsr activateY
    rts

.increaseScore:
    sed ;; decimal mode
    clc : adc score : sta score
    lda #0 : adc score+1 : sta score+1
    cld ;; back to binary mode
    rts

.deactivateRockL:
    jsr spawnChildren
    lda #2 : jsr increaseScore
    rts

.deactivateRockM:
    jsr spawnChildren
    lda #5 : jsr increaseScore
    rts

.deactivateRockS:
    lda #&10 : jsr increaseScore
    rts

.updateRock:
    jmp updateGenericObject

.createRock:
    lda #KindRock : sta myKind, x
    Copy16ix rockPlot, myPlot
    Copy16ix rockUnPlot, myUnPlot
    Copy16ix updateRock, myUpdateF
    rts

.createRockL:
    Copy16ix largeRockOutline, myOutline
    Copy16ix activateRockL, myActivateF
    Copy16ix deactivateRockL, myDeactivateF
    jmp createRock

.createRockM:
    Copy16ix mediumRockOutline, myOutline
    Copy16ix deactivateRockM, myDeactivateF
    jmp createRock

.createRockS:
    Copy16ix smallRockOutline, myOutline
    Copy16ix deactivateRockS, myDeactivateF
    jmp createRock

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; trig...

macro mm B
    equb B
    ;print ~B
endmacro

.cosTabLow:
    FOR i, -QuarterTurn, FullCircle - 1
    mm LO(SIN((i*PI)/2/QuarterTurn) * 256)
    NEXT
.cosTabHigh:
    FOR i, -QuarterTurn, FullCircle - 1
    mm HI(SIN((i*PI)/2/QuarterTurn) * 256)
    NEXT

sinTabLow = cosTabLow + QuarterTurn
sinTabHigh = cosTabHigh + QuarterTurn

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ship behaviour

.setShipOutlineFromDirection:
    lda myDirection, x
    and #7 ; take 3 bits (8 outlines in sequence)
    asl a ; index Y for outline table must be even
    tay
    lda shipOutlines, y  : sta myOutline, x
    lda shipOutlines+1,y : sta myOutline+NUM, x
    rts

.modulateDirection:
    lda myDirection, x
    and #(FullCircle - 1)
    sta myDirection, x
    rts

.updateDirection:
    jsr turnLeftOnCaps
    jsr turnRightOnCtrl
    jsr modulateDirection
    jsr setShipOutlineFromDirection
    rts

.setThrust: {
    CheckPress keyShift : beq no
    ldy myDirection, x
    lda sinTabHigh, y : sta theThrustX+1
    lda sinTabLow, y : sta theThrustX
    lda cosTabHigh, y : sta theThrustY+1
    lda cosTabLow, y : sta theThrustY
    rts
.no:
    Copy16iv 0, theThrustX
    Copy16iv 0, theThrustY
    rts
    }

.halveThrust:
    Halve theThrustY
.halveThrustX:
    Halve theThrustX
    rts

macro InkrementIfNegative V
    lda V+1 : bmi no
    Inkrement16v V
.no:
endmacro

.decayDrag:
    InkrementIfNegative theAccX
    InkrementIfNegative theAccY
    Halve theAccX
    Halve theAccY
    rts

.computeAcceleration:
    jsr setThrust
    IF ImpulseIndex > 0 : FOR i, 1, ImpulseIndex : jsr halveThrust : NEXT
    ENDIF
    jsr halveThrustX ; Y-axis is at twice precision of X
    Copy16iv 0, theAccX
    Copy16iv 0, theAccY
    Sub16vx theAccX, mySpeedX
    Sub16vx theAccY, mySpeedY
    IF MomentumIndex > 0 : FOR i, 1, MomentumIndex : jsr decayDrag : NEXT
    ENDIF
    Add16vv theAccX, theThrustX
    Add16vv theAccY, theThrustY
    rts

.updateSpeedFromAcceleration:
    Add16xv mySpeedX, theAccX
    Add16xv mySpeedY, theAccY
    rts

.updateShip:
    jsr updateDirection
    jsr computeAcceleration
    jsr updateSpeedFromAcceleration
    jmp updateGenericObject

.createShip:
    lda #KindShip : sta myKind, x
    Copy16ix shipPlot, myPlot
    Copy16ix shipUnPlot, myUnPlot
    Copy16ix updateShip, myUpdateF
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bullet behaviour

.inheritPositionFromShip:
    txa : tay
    ldx #ShipObjectNum
    jsr inheritPosition
    tya : tax
    rts

.setBulletSpeed: ;; TODO macro trig lookup
    ldy myDirection+ShipObjectNum
    lda sinTabLow, y : sta mySpeedX, x
    lda sinTabHigh, y : sta mySpeedX+NUM, x
    ;; Y-axis is at twice precision of X, so we must double
    lda cosTabLow, y : asl a : sta mySpeedY, x
    lda cosTabHigh, y : rol a : sta mySpeedY+NUM, x
    rts

.doubleSpeed:
    lda mySpeedX, x : asl a : sta mySpeedX, x
    lda mySpeedX+NUM, x : rol a : sta mySpeedX+NUM, x
    lda mySpeedY, x : asl a : sta mySpeedY, x
    lda mySpeedY+NUM, x : rol a : sta mySpeedY+NUM, x
    rts

.activateBullet:
    jsr inheritPositionFromShip
    jsr setBulletSpeed
    jsr doubleSpeed
    jsr updatePositionFromSpeed
    jsr updatePositionFromSpeed
    jsr updatePositionFromSpeed
    jsr startTimer
    rts

.spawnIfFiringAttempted:
    lda fireAttempted : beq no ; not attempted
    lda isActive, x : bne no ; not if we are already active
    jsr activate
    lda #0 : sta fireAttempted ; just one bullet!
.no:
    rts

.dieWhenTimerExpires: {
    lda myTimer, x : clc : adc #50 ; not so long that we can shoot ourselve!
    cmp frameCounter : bpl no
    jsr deactivate
.no:
    rts
    }

.updateBullet:
    jsr spawnIfFiringAttempted
    jsr dieWhenTimerExpires
    jmp updateGenericObject

.createBullet:
    lda #KindBullet : sta myKind, x
    Copy16ix bulletOutline, myOutline
    Copy16ix bulletPlot, myPlot
    Copy16ix bulletUnPlot, myUnPlot
    Copy16ix updateBullet, myUpdateF
    Copy16ix activateBullet, myActivateF
    rts

.createRockFamily:
    jsr createRockL : txa : clc : adc #1 : sta childA, x : adc #1 : sta childB, x : inx
    jsr createRockM : txa : clc : adc #2 : sta childA, x : adc #1 : sta childB, x : inx
    jsr createRockM : txa : clc : adc #2 : sta childA, x : adc #1 : sta childB, x : inx
    jsr createRockS : inx
    jsr createRockS : inx
    jsr createRockS : inx
    jsr createRockS : inx
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; create

.createAllObjects: {
    ldx #ShipObjectNum : jsr createShip ; 0
    ldx #1 : jsr createRockFamily ; 1--7
    ldx #8 : jsr createRockFamily ; 8--14
    ldx #15 : jsr createRockFamily ; 15--21
    txa : clc : adc #MaxBullets : sta smc_cond+1
.loopB:
    jsr createBullet ; 22--25
    inx
    .smc_cond : cpx #&77
    bne loopB
    rts
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; game logic

.startNewWave:
    ;; TODO: be more principled in discovering large rock numbers
    ldx #1 : jsr activate
    ldx #8: jsr activate
    ldx #15: jsr activate
    rts

.killAllRocks: {
    ldx #0
.loop:
    cpx #NUM : beq done
    lda myKind, x
    cmp #KindRock : bne next
    jsr deactivate
.next:
    inx
    jmp loop
.done:
    rts
    }

.activateShip:
    ldx #ShipObjectNum
    jsr setCentralPosition
    lda #0 : sta myDirection, x
    Copy16ix 0, mySpeedX
    Copy16ix 0, mySpeedY
    jsr activate
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; control

.turnLeftOnCaps: {
    lda frameCounter : and #1 : beq no ; on even frames
    CheckPress keyCaps : beq no
    dec myDirection, x
.no:
    rts
    }

.turnRightOnCtrl: {
    lda frameCounter : and #1 : beq no ; on even frames
    CheckPress keyCtrl : beq no
    inc myDirection, x
.no:
    rts
    }

.killAllRocksOnK: {
    CheckPress keyK : beq no
    jsr killAllRocks
.no:
    rts
    }

.startNewWaveOnW: {
    CheckPress keyW : beq no
    jsr startNewWave
.no:
    rts
    }

.newLifeOnN: {
    CheckPress keyN : beq no
    jsr activateShip
.no:
    rts
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; global behaviour

.tryFireOnEnter: {
    CheckPress keyEnter : beq no
    lda kindCount + KindBullet
    cmp #MaxBullets : bpl no
    ;; TODO: cant fire when ship is not active
    lda #1 : sta fireAttempted ; will be cleared by first available bullet
.no:
    rts
    }

.updateGlobal:
    jsr tryFireOnEnter
    jsr killAllRocksOnK
    jsr startNewWaveOnW
    jsr newLifeOnN
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; onSync (update-all)

.updateN: skip 1
.updateObjects: {
    lda #0 : sta updateN
.loop:
    ldx updateN : cpx #NUM : { bne no : rts : .no }
    Copy16xv myUpdateF, dispatch+1
    .dispatch : jsr &7777
    inc updateN
    jmp loop
    }

.onSync:
    inc frameCounter
    jsr playSounds
    jsr readKeys
    ;; if Debug : jsr printKeyState : endif ;; not that useful
    jsr updateGlobal
    jsr updateObjects
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Render loop.

.renderN: skip 1
.renderLoop: {
.loop:
    lda vsyncNotify : { beq no : dec vsyncNotify : jsr onSync : .no }
    ldx renderN : cpx #NUM : bne notZeroObject
    ldx #0 : stx renderN
    jsr printGlobalState
.notZeroObject:
    inc randomOffset ;; better randomness; necessary?
    jsr renderObject
    inc renderN
    jmp loop
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; main

.main:
    jsr init
    jsr createAllObjects
    Copy16iv myIRQ, irq1v ; initialise sync
    jsr startNewWave
    jsr activateShip
    jmp renderLoop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.end:
print "bytes remaining: ", screenStart-*
SAVE "Code", start, end
