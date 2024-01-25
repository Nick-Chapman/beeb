
;;; Objects, with render loop and synced update

;;; TODO
;;; - debug keys: k/w/n - control kill all rocks / start wave / new life
;;; - randomness: large rock spawn position / speed delta on crack
;;; - track when object has moved; avoid unplot/plot when unmoved
;;; - switch to own emit (faster; fix code 13 issue)
;;; - other performance improvement?
;;; - sound!
;;; - game logic: (space)start, lives, gameover, level cleared, startup banner
;;; - keys: z/x: alternative-turn

Debug = FALSE

NUM = 28 ;; Number of objects, indexed consistently using X-register

ShipObjectNum = 0
MaxBullets = 4

MomentumIndex = 6  ; Higher is more momentum (and increased max speed)
ImpulseIndex = 3 ; Higher is lower thrust (and decreased max speed)

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
    Edge keySpace
    Edge keyEnter
    Edge keyTab ;; DEV: selected object
    Edge keyA ;; DEV: toggle active state

    Edge keyUp
    Edge keyDown
    Edge keyLeft
    Edge keyRight

    ;Edge keyCaps
    ;Edge keyCtrl

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
.selectedN: skip 1 ; TODO: remove

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
    lda #33 : jsr osasci ; Xpos
    txa : clc : adc #3 : jsr osasci ; Ypos
endmacro

.debugObject: {
    cpx #10 : beq done; dont debug object 10 because line 13 position is weird
    lda myKind, x : beq done ; kind not set, so not a real object
    DebugPositionForObject
    Space : lda #'.' : { cpx selectedN : bne no : lda #'*' : .no } : jsr emit
    Space
    lda #'.' : { ldy isActive, x : beq no : lda #'a' : .no } : jsr emit
    lda #'.' : { ldy isRendered, x : beq no : lda #'r' : .no } : jsr emit
    lda #'.' : { ldy isHit, x : beq no : lda #'H' : .no } : jsr emit
.done:
    rts
    }

.renderObject: {
    IF Debug : jsr debugObject : ENDIF
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
    Position 8,0 : jsr printLag ;; alway print lag for now
    ;;IF Debug : Position 15,0 : jsr printObjectCountsByKind : ENDIF
    IF Debug : jsr printThrustInfo : ENDIF
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
    ;;Position 34,31 : lda frameCounter : jsr printHexA : Space : lda renderN : jsr printHexA
    ;;Position 1,28 : Space : jsr printKeyState
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
    jsr renderObject
    inc renderN
    jmp loop
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; control

.turnLeftOnCaps: {
    lda frameCounter : and #1 : beq no ; on even frames
    CheckPress keyCaps : beq no
    dec myDirection, x
.no
    rts
    }

.turnRightOnCtrl: {
    lda frameCounter : and #1 : beq no ; on even frames
    CheckPress keyCtrl : beq no
    inc myDirection, x
.no
    rts
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; update (DEV)

Acc = 25

.accelerateUp:
    lda mySpeedY,     x : sec : sbc #Acc : sta mySpeedY, x
    lda mySpeedY+NUM, x :       sbc #0   : sta mySpeedY+NUM, x
    rts

.accelerateDown:
    lda mySpeedY,     x : clc : adc #Acc : sta mySpeedY, x
    lda mySpeedY+NUM, x :       adc #0   : sta mySpeedY+NUM, x
    rts

.accelerateLeft:
    lda mySpeedX,     x : sec : sbc #Acc : sta mySpeedX, x
    lda mySpeedX+NUM, x :       sbc #0   : sta mySpeedX+NUM, x
    rts

.accelerateRight:
    lda mySpeedX,     x : clc : adc #Acc : sta mySpeedX, x
    lda mySpeedX+NUM, x :       adc #0   : sta mySpeedX+NUM, x
    rts

.updateSpeedIfSelected: {
    cpx selectedN : bne no ; not selected
    CheckPress keyUp    : { beq no : jsr accelerateUp    : .no }
    CheckPress keyDown  : { beq no : jsr accelerateDown  : .no }
    CheckPress keyLeft  : { beq no : jsr accelerateLeft  : .no }
    CheckPress keyRight : { beq no : jsr accelerateRight : .no }
.no:
    rts
    }

.stepSelectedObject:
    inc selectedN
    lda selectedN : cmp #NUM : { bne no : lda #0 : sta selectedN : .no }
    rts

.updateSelectedObjectOnTab:
    CheckPress keyTab
    { beq no : jsr stepSelectedObject : .no }
    rts

.toggleActiveness:
    lda isActive, x
    beq activate
    jmp deactivate

.toggleActivenessIfSelected: {
    CheckPress keyA : beq no
    cpx selectedN : bne no
    jsr toggleActiveness
.no:
    rts
    }

.setPositionFromId: ; DEV - used for large rock
    txa
    and #3
    clc : adc #1 ; one column over
    asl a : asl a : asl a : asl a
    sta myPosX+NUM, x
    txa : and #%11111100
    clc : adc #8 ; two rows down
    asl a : asl a : asl a
    sta myPosY+NUM, x
    rts

.setInitialSlowSpeed: ; DEV - used for large rock.
    lda #10 : sta mySpeedX, x
    lda #100 : sta mySpeedY, x
    lda #0 : sta mySpeedX+NUM, x
    lda #0 : sta mySpeedY+NUM, x
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; global behaviour

.tryFireOnEnter: {
    CheckPress keyEnter : beq no
    lda kindCount + KindBullet
    cmp #MaxBullets : bpl no
    lda #1 : sta fireAttempted ; will be cleared by first available bullet
.no:
    rts
    }

.updateGlobal:
    ;; jsr updateSelectedObjectOnTab
    jsr tryFireOnEnter
    rts

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
    jsr toggleActivenessIfSelected
    jsr updateSpeedIfSelected
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

.inheritSpeed: ; obX --> obY
    Copy16xy mySpeedX, mySpeedX
    Copy16xy mySpeedY, mySpeedY
    rts

.startTimer:
    lda frameCounter : sta myTimer, x
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rock behaviour

.accelerateRightY: {stx restoreX+1 : tya : tax : jsr accelerateRight : .restoreX : ldx #&77 : rts }
.accelerateDownY: {stx restoreX+1 : tya : tax : jsr accelerateDown : .restoreX : ldx #&77 : rts }

.inheritSpeedAndPositionA:
    jsr inheritPosition
    jsr inheritSpeed
    jsr accelerateRightY ;; TODO: change by random component
    rts

.inheritSpeedAndPositionB:
    jsr inheritPosition
    jsr inheritSpeed
    jsr accelerateDownY ;; TODO: then can treat A/B the same
    rts

.activateY: {stx restoreX+1 : tya : tax : jsr activate : .restoreX : ldx #&77 : rts }

.spawnChildren:
    lda childA, x : tay : jsr inheritSpeedAndPositionA : jsr activateY
    lda childB, x : tay : jsr inheritSpeedAndPositionB : jsr activateY
    rts

.activateRockL: ;; TODO: set (random values) in level restart
    jsr setPositionFromId
    jsr setInitialSlowSpeed
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

.activateShip:
    jsr setCentralPosition
    rts

.createShip:
    lda #KindShip : sta myKind, x
    Copy16ix shipPlot, myPlot
    Copy16ix shipUnPlot, myUnPlot
    Copy16ix updateShip, myUpdateF
    Copy16ix activateShip, myActivateF
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
    jsr createRockM : txa : clc : adc #1 : sta childA, x : adc #1 : sta childB, x : inx
    jsr createRockM : txa : clc : adc #1 : sta childA, x : adc #1 : sta childB, x : inx
    jsr createRockS : inx
    jsr createRockS : inx
    jsr createRockS : inx
    jsr createRockS : inx
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; create

.createAllObjects: {
    ldx #ShipObjectNum : jsr createShip ; 0
    ldx #2 : jsr createRockFamily ; 2--8
    ldx #11 : jsr createRockFamily ; 11-17
    ldx #19
.loopB:
    jsr createBullet ; 19,20,21,22
    inx
    cpx #(MaxBullets+19)
    bne loopB
    rts
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; game logic

.startLevel:
    ldx #ShipObjectNum : jsr activate ;; the ship
    ldx #2 : jsr activate ;; first large rock
    ldx #11: jsr activate ;; second large rock
    rts

;;; TODO: DEV: kill level / reset level / new life

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; main

.main:
    jsr init
    jsr createAllObjects
    Copy16iv myIRQ, irq1v ; initialise sync
    jsr startLevel
    jmp renderLoop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.end:
print "bytes remaining: ", screenStart-*
SAVE "Code", start, end
