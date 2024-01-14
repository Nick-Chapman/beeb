
;;; New Year. New fun.
;;; Determine Acceleration via Thrust/Drag.

;;; Time is represented in "frame" units = 1/50s

;;; Screen Pixel Positions (X,Y) are represented as UNSIGNED in 2 bytes
;;; Y: 0..255 (+8 binary points)
;;; X: 0..319 (+7 binary points)

ShipMomentumIndex = 7 ; Higher is more momentum (and increased max speed)

;;; Speed (X,Y) is represented as SIGNED in 2 bytes
;;; With ShipMomentumIndex = 6, max-speed ranges +/- &40 (steps/frame)
;;; With ShipMomentumIndex = 7, max-speed ranges +/- &80 (steps/frame)
;;; ShipMomentumIndex > 7 causes overflow and instant direction reversal

;;; Speed is scaled when applied to positions
;;; We scale X by one more than Y because X-positions have only 7 binary points, not 8
SpeedScaleY = 4 ; Higher is slower
SpeedScaleX = SpeedScaleY + 1

;;; The resulting maximum ship-speed approaches +/- 2^(Momentum - SpeedScaleY) pixels/frame

;;; We resolve 8 distinct angles per QuarterTurn (for outline & thrust direction)

QuarterTurn = 8
HalfTurn = 2*QuarterTurn
ThreeQuarters = 3*QuarterTurn
FullCircle = 4*QuarterTurn

RotSpeedScaleIndex = 1 ; higher is a slower turn
RotSpeedScale = 2^RotSpeedScaleIndex

SyncAssert = TRUE
RasterDebugBlit = TRUE ; magenta
RasterDebugPrepare = FALSE ; blue
RasterDebugTextInfo = FALSE ; green

white = 0
cyan = 1
magenta = 2
blue = 3
yellow = 4
green = 5
red = 6
black = 7

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

screenStart = &3000

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macros

macro copy16i I,V
    lda #LO(I) : sta V
    lda #HI(I) : sta V+1
endmacro

macro copy16v A,B
    lda A : sta B
    lda A+1 : sta B+1
endmacro

macro Emit C
	lda #C
    jsr emit
endmacro

macro Space
	lda #' '
    jsr emit
endmacro

macro Position X,Y
    copy16i screenStart+16*(Y*40+X), emitPos
endmacro

macro STOP S
    copy16i msg, msgPtr
    jsr printString
    jmp spin
.msg:EQUS S, 0
endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parameter Stack

macro PushA
    dex
    sta 0,x
endmacro

macro PushY
    dex
    sty 0,x
endmacro

macro PopA
    lda 0,x
    inx
endmacro

macro PopY
    ldy 0,x
    inx
endmacro

macro PushLit V
    lda #V : PushA
endmacro

macro PushVar8 V
    lda V
    PushA
endmacro

macro PopVar8 V
    PopA
    sta V
endmacro

macro PushVar16 V
    PushVar8 V+1
    PushVar8 V
endmacro

macro PopVar16 V
	PopVar8 V
	PopVar8 V+1
endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Zero Page

guard &100
org &70
.msgPtr SKIP 2 ;; TODO: kill

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Start

guard screenStart
org &1100

.start:
    jmp main

.spin: jmp spin

.main: {
    jsr prepFont4col
    jsr initVia
    jsr mode1
    jsr cursorOff
    jsr replaceWhiteWithCyan
    copy16i myIRQ, irq1v
    ldx #&90 ; End of stack; grows downwards
    jsr syncVB
.loop:
    jsr readKeys

    ;; prepare...
    IF RasterDebugPrepare : lda #blue : sta ula : ENDIF
    jsr preparePhase
    lda #black : sta ula

    ;;jsr syncVB ;; dev

    ;; text-info...
    IF RasterDebugTextInfo : lda #green : sta ula : ENDIF
    jsr textInfo
    lda #black : sta ula

    jsr syncVB ;; correct

    ;; blit...
    IF RasterDebugBlit : lda #magenta : sta ula : ENDIF
    jsr blitPhase
    lda #black : sta ula

    inc frameCounter
    jmp loop
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Blit

.blitPhase:
    jsr processEraseBuf
    jsr processPlotBuf
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Prepare

.preparePhase:
    jsr updateState
    jmp preparePlotAndSubsequentErase

.preparePlotAndSubsequentErase: ; ( -- )
    jsr resetEraseBuf
    jsr resetPlotBuf
    lda #yellowMask : PushA
    PushVar16 posX
    PushVar8 posY+1 ; only hi
    jsr getDirection
    jsr setOrientationFromDirection
    copy16i onPoint, SMC_doPoint+1 : jsr drawShape
    PopA : PopA : PopA : PopA
    rts

.getDirection: ; ( -- d )
    PushVar8 direction
    IF RotSpeedScaleIndex > 0 : FOR i, 1, RotSpeedScaleIndex : lsr 0,x : NEXT
    ENDIF
    rts

.onPoint:
    jsr erasePointAt ; prepared for next frame
    jmp colPointAt

.erasePointAt: ; ( xx y -- xx y )
    jsr calcA ; ( xx y aa fineXmask )
    inx ; drop fineXmask
    ;;jmp eraseAt ; TODO fallthrough
.eraseAt:  ; ( aa -- )
    lda 1,x : jsr pushEraseBuf ;hi -- hi comes first in buffer
    lda 0,x : jsr pushEraseBuf ;lo
    inx : inx
    rts

.colPointAt: ; ( c xx y -- c xx y )
    jsr calcA ; ( c xx y aa fineXmask )
    lda 0,x
    and 6,x
    sta 0,x
    ;;jmp eorAt ; TODO fallthrough
.eorAt: { ; ( aa d -- )
    lda 2,x : jsr pushPlotBuf ; a-hi -- hi comes first in buffer
    lda 1,x : jsr pushPlotBuf ; a-lo
    lda 0,x : jsr pushPlotBuf ; d
    inx : inx : inx
    rts
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; State

.frameCounter EQUB 0

;;; start North; angles increase clockwise (0 is East)
.direction EQUB (RotSpeedScale*ThreeQuarters)

.accelX SKIP 2
.accelY SKIP 2
.speedX SKIP 2
.speedY SKIP 2
.posX EQUB 0, 75
.posY EQUB 0, 128

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Update State

macro Mod160 V ; V <- V `mod` 160
    PushVar8 V
    PopA
    jsr mod160
    PushA
    PopVar8 V
endmacro

macro PlusEq16 A, B ; A <- A+B
    PushVar16 B
    PushVar16 A
    jsr plus
    PopVar16 A
endmacro

.updateState:
    jsr updateDirection ; in response to Caps/Ctrl
    jsr updateDirectionFromArrows ; in response to Arrow keys (for dev/xdebug)
    Jsr modulateDirection

    ;; X...
    jsr thrustX
    PushVar16 speedX
    jsr decayMomentum
    jsr minus
    PopVar16 accelX
    PlusEq16 speedX, accelX
    PushVar16 speedX
    IF SpeedScaleX > 0 : FOR i, 1, SpeedScaleX : jsr decay1 : NEXT
    ENDIF
    PushVar16 posX
    jsr plus
    PopVar16 posX
    Mod160 posX+1

    ;; Y...
    jsr thrustY
    PushVar16 speedY
    jsr decayMomentum
    jsr minus
    PopVar16 accelY
    PlusEq16 speedY, accelY
    PushVar16 speedY
    IF SpeedScaleY > 0 : FOR i, 1, SpeedScaleY : jsr decay1 : NEXT
    ENDIF
    PushVar16 posY
    jsr plus
    PopVar16 posY

    rts

.updateDirection: {
    PushVar8 direction
    jsr turnLeft : beq no
    jsr turnRight : bne done ; both
    dec 0,x
    jmp done
    .no : jsr turnRight : beq done ; neither
    inc 0,x
.done:
    PopVar8 direction
    rts
    }

.turnLeft:
    ;;lda keyCapsLAST : bne noTurn ;;edge-direction
    lda keyCaps
    rts
.turnRight:
    ;;lda keyCtrlLAST : bne noTurn ;;edge-direction
    lda keyCtrl
    rts
.noTurn:
    lda #0
    rts

.updateDirectionFromArrows: {
    QT = QuarterTurn * RotSpeedScale
    { lda keyRight : beq no : lda #0 : sta direction : .no }
    { lda keyDown : beq no : lda #QT : sta direction : .no }
    { lda keyLeft : beq no : lda #(2*QT) : sta direction : .no }
    { lda keyUp : beq no : lda #(3*QT) : sta direction : .no }
    rts
    }

.modulateDirection:
    lda direction
    and #(FullCircle * RotSpeedScale - 1)
    sta direction
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Thrust

.thrustX: { ; ( -- nn )
    jsr areWeThrusting : beq noThrust
    jsr getDirection
    jmp cos
.noThrust:
    jmp pushZero
    }

.thrustY: { ; ( -- nn )
    jsr areWeThrusting : beq noThrust
    jsr getDirection
    jmp sin
.noThrust:
    jmp pushZero
    }

.pushZero: ; ( -- nn )
    lda #0 : PushA : PushA
    rts

.areWeThrusting:
    {
    lda keyShift : bne done
    ;lda keyLeft : bne done
    ;lda keyRight : bne done
    ;lda keyUp : bne done
    ;lda keyDown : bne done
.done:
    rts
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Trig...

.cos: ; ( n -- nn )
    jsr turnNinety
    jmp sin

.turnNinety: ; ( n -- n )
    lda 0,x
    clc : adc #QuarterTurn
    sta 0,x
    rts

.sin: ; ( n -- nn )
    jsr mirrorB
    PopA : pha
    jsr mirrorA
    lda 1,x
    and #%00000111 ; fix quadrant
    sta 1,x
    jsr invertQuadrantMaybe
    jsr sinTableLookup
    pla : PushA
    jsr invertMaybe
    rts

.pushZeroByte: ; ( -- n )
    lda #0 : PushA
    rts

.mirrorB: ; ( n -- n p )
    lda 0,x
    and #HalfTurn
    PushA
    rts

.mirrorA: ; ( n -- n p )
    lda 0,x
    and #QuarterTurn
    PushA
    rts

.invertQuadrantMaybe: ; ( n p -- n )
    inx
    lda &ff,x
    bne invertQuadrant
    rts

.invertQuadrant: ; ( n -- n )
    lda #(QuarterTurn)
    sec : sbc 0,x
    sta 0,x
    rts

;; .swap: ; ( n n -- n n )
;;     lda 0,x
;;     ldy 1,x
;;     sta 1,x
;;     sty 0,x
;;     rts

;; .drop: ; ( n -- )
;;     inx
;;     rts

.sinTableLookup: ; ( n -- nn )
    lda 0,x
    cmp #QuarterTurn
    beq push256
    PopY
    jsr pushZeroByte ; res-hi
    lda sinTab, y
    PushA
    rts

.push256: ; ( n -- nn )
    inx
    lda #1 : PushA
    lda #0 : PushA
    rts

macro mm B
    EQUB B
    ;;PRINT B
endmacro

.sinTab:
    FOR i, 0, QuarterTurn-1
    mm INT(SIN((i*PI)/2/QuarterTurn) * 256)
    NEXT

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Arithmetic

.decayMomentum: ; ( tt vv -- aa )
    IF ShipMomentumIndex > 0 : FOR i, 1, ShipMomentumIndex : jsr decay1 : NEXT
    ENDIF
    rts

;;; TODO: how exactly do decay1 and halve differ?
.decay1: { ; ( vv -- vv )
    lda 1,x ;h
    bmi no
    jsr increment
.no:
    jmp halve
    }

.increment: ; ( vv -- vv ) ; TODO better imp!
    jsr pushOne
    jmp plus

.pushOne
    lda #0 : PushA : lda #1 : PushA
    rts

.halve: ; ( aa -- bb )
    lda 1,x
    cmp #&80 ; signed!
    ror 1,x ; hi
    ror 0,x ; lo
    rts

.plus: ; ( pp qq -- rr )
    clc
    lda 2, x ; PL
    adc 0, x ; QL
    sta 2, x
    lda 3, x ; PH
    adc 1, x ; QH
    sta 3, x
    inx : inx
    rts

.minus: ; ( pp qq -- rr )
    sec
    lda 2, x ; PL
    sbc 0, x ; QL
    sta 2, x
    lda 3, x ; PH
    sbc 1, x ; QH
    sta 3, x
    inx : inx
    rts

.invertMaybe: { ; ( nn p -- nn )
    inx : lda &ff,x
    beq done
    jmp invert
.done:
    rts }

.invert: ; ( qq -- rr ) ; zero-minus ; TODO: better imp
    sec
    lda #0
    sbc 0, x ; QL
    sta 0, x
    lda #0
    sbc 1, x ; QH
    sta 1, x
    rts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Outlines

N = 1
S = 2
E = 4
W = 8
INVISIBLE = 16

NE = N or E
SE = S or E
NW = N or W
SW = S or W

Ni = N or INVISIBLE
Si = S or INVISIBLE
Wi = W or INVISIBLE
SWi = SW or INVISIBLE

;;;macro Center : EQUB 32 : endmacro ; see center of rotation
;;;macro Center : EQUB 16 : endmacro ; DONT see center of rotation
macro Center : endmacro  ; DONT see center of rotation (and dont even waste a byte!)

.outline1: Center
    EQUB Si,Si, S,W,W,W,SW,N,N, NE,N,N,NE,N,N,NE,N,N,NE, SE,S,S,SE,S,S,SE,S,S,SE,S,S, NW,W,W, 0

.outline2: Center
    EQUB SWi, S,W,Wi,W,NE,N,NE,N,NE,N,NE,NE,N,NE,NE, S,S,S,S,S,S,S,S,S,S,S,S,S, NW,W,W, 0

.outline3: Center
    EQUB Wi, SW,W,Wi,W, NE,NE,NE,NE,NE,NE,NE,NE,NE,NE, S,S,S,SW,S,S,S,S,SW,S,S,S,S,S, Ni,NW,W,NW,W, 0

.outline4: Center
    EQUB Si, SW,W,NW,Wi,W, NE,NE,NE,NE,NE,E,NE,NE,NE,NE,E, S,SW,S,S,SW,S,S,SW,S,S,SW,S,S, Ni,NW,W, 0

.outline5: Center
    EQUB SWi, SW,NW,NW,W, NE,E,NE,E,NE,NE,E,NE,E,NE,E, S,SW,S,SW,SW,S,SW,S,SW,S,SW, N,NW, 0

.outlines:
    EQUW outline1,outline2,outline3,outline4
    EQUW outline5,outline4,outline3,outline2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Blit

cyanMask = &ff
yellowMask = &f0
redMask = &0f
blackMask = &00

.setOrientationFromDirection: ; ( d -- )

    PopA ; direction
    pha

    ;; Set outline...
    and #7 ; take 3 bits (8 outlines in sequence)
    asl a ; index Y for outline table must be even
    tay
    lda outlines,y : sta SMC_outline+1
    lda outlines+1,y : sta SMC_outline+2

    ;; Set orientation...
    pla ; direction
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
    EQUB W,N,E,S
    EQUB S,E,N,W
    EQUB S,W,N,E
    EQUB W,S,E,N
    EQUB E,S,W,N
    EQUB N,W,S,E
    EQUB N,E,S,W
    EQUB E,N,W,S

.invisible EQUB INVISIBLE
.asNorth SKIP 1
.asSouth SKIP 1
.asEast SKIP 1
.asWest SKIP 1

.drawShape: { ; ( c xx y -- c xx y )
    ldy #0
.loop:
    ;;cpy #1 : beq done ;; DEV HACK - just see one point
    .*SMC_outline : lda &eeee, y : beq done
    bit asNorth : bne north
    bit asSouth : bne south
    jmp ew
.north: dec 0,x : jmp ew
.south: inc 0,x : jmp ew
.ew:
    pha
    bit asEast : bne east
    bit asWest : bne west
    jmp pr
.east:
    lda 1,x
    clc : adc #128
    sta 1,x
    bcc pr
    lda 2,x
    clc : adc #1 : jsr mod160
    sta 2,x
    jmp pr
.west:
    lda 1,x
    sec : sbc #128
    sta 1,x
    bcs pr
    lda 2,x
    sec : sbc #1 : jsr mod160
    sta 2,x
    jmp pr
.pr:
    pla
    bit invisible : bne next
    sty SMC_y + 1
    .*SMC_doPoint : jsr &eeee
    .SMC_y : ldy #&ee
.next:
    iny
    bne loop
.done:
    rts
    }

.mod160: {
    m = 160
    x = 9
    clc : adc #x
    { cmp #(m+x) : bcc no : sbc #m : .no } ; wrap
    { cmp #x : bcs no : adc #m : .no } ; unwrap
    sec : sbc #x
.done:
    rts
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; calculate screen address from X/Y

.calcA: { ; ( xx y -- xx y a-hi a-lo fineXmask )
    lda 0,x ; y
    pha : and #&7 : sta theFY : pla
    lsr a : lsr a : lsr a : sta theCY

    lda 1,x ; x-lo (need just hi-order bit; get it in carry)
    cmp #&80

    lda 2,x ; x-hi
    pha : rol a : and #&3 : sta theFX : pla
    lsr a : sta theCX
    ;; TODO: inline calculateAfromXY ; kill "the"* vars & simplfy!
    jsr calculateAfromXY
    lda theA+1 ; a-hi
    PushA
    lda theA ; a-lo
    PushA
    ldy theFX ; TODO: avoid using y ?
    lda masks,y
    PushA
    rts
    .masks : EQUB &88, &44, &22, &11
    }

.theCX SKIP 1 ; coarse-X : 0..79
.theCY SKIP 1 ; coarse-Y : 0..31
.theFX SKIP 1 ; fine-X   : 0..3
.theFY SKIP 1 ; fine-Y   : 0..7
.theA  SKIP 2 ; screen address of the 8x8 char

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keyboard input

.keyShift EQUB 0
.keyEnter EQUB 0
.keyCaps  EQUB 0
.keyCtrl  EQUB 0
.keyUp    EQUB 0
.keyDown  EQUB 0
.keyLeft  EQUB 0
.keyRight EQUB 0

;;;.keyCapsLAST  EQUB 0
;;;.keyCtrlLAST  EQUB 0

macro PollKey CODE, VAR
    lda #0 : sta VAR
    lda #(-CODE-1) : sta system_VIA_portA : lda system_VIA_portA
    bpl no : lda #1 : sta VAR : .no
endmacro

.readKeys:
    ;;;lda keyCaps : sta keyCapsLAST
    ;;;lda keyCtrl : sta keyCtrlLAST
    PollKey -1,   keyShift
    PollKey -74,  keyEnter
    ;;PollKey -97,  keyCaps ;; Use Tab instead of CapsLock
    PollKey -65,  keyCaps ;; TODO Also Z (and Tab)
    PollKey -2,   keyCtrl ;; TODO Also X
    PollKey -58,  keyUp
    PollKey -42,  keyDown
    PollKey -26,  keyLeft
    PollKey -122, keyRight
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Text Info (debug)

macro PrHexByte VAR
    lda VAR
    jsr printHexA
endmacro

macro PrHexWord VAR
    PrHexByte VAR+1
    PrHexByte VAR
endmacro

.textInfo:
    {
    ;;jmp dev ;; TEMP

    lda frameCounter : and #1 : bne p12 : jmp p34
.p12:
    lda frameCounter : and #2 : bne p1 : jmp p2
.p34:
    lda frameCounter : and #2 : bne p3 : jmp p4
.p1:
    Position 1,28 : Emit 'A' : Space : PrHexWord accelX : Space : PrHexWord accelY
    rts
.p2:
    Position 1,27 : Emit 'D' : Space : lda direction : jsr printHexA
    ;Position 0,25 : Space : jsr printKeyState
    rts
.p3:
    Position 1,29 : Emit 'S' : Space : PrHexWord speedX : Space : PrHexWord speedY
    rts
.p4:
    Position 1,30 : Emit 'P' : Space : PrHexWord posX : Space : PrHexWord posY
    rts

.dev:
    ;; dev...
    Position 1,31
    ;lda frameCounter : jsr printHexA
    ;Space : txa : jsr printHexA ; debug param stack bugs
    ;Space : lda ptrPlotBuf : jsr printHexA
    ;Space : lda ptrEraseBufA : jsr printHexA
    ;Space : lda ptrEraseBufB : jsr printHexA
    jmp p2
    rts
    }

.printKeyState:
    lda #'.' : { ldy keyCaps : beq no : lda #'A' : .no } : jsr emit
    lda #'.' : { ldy keyCtrl : beq no : lda #'C' : .no } : jsr emit
    lda #'.' : { ldy keyShift: beq no : lda #'S' : .no } : jsr emit
    Space
    lda #'.' : { ldy keyLeft : beq no : lda #'L' : .no } : jsr emit
    lda #'.' : { ldy keyRight: beq no : lda #'R' : .no } : jsr emit
    lda #'.' : { ldy keyUp   : beq no : lda #'U' : .no } : jsr emit
    lda #'.' : { ldy keyDown : beq no : lda #'D' : .no } : jsr emit
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Print (debug)

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
    jsr emit
    pla
    and #&f : tay
    lda digits,y
    jsr emit
    rts
.digits: EQUS "0123456789abcdef"
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IRQ, VBlank syncing

.vsyncNotify EQUB 0

.myIRQ: {
    lda system_VIA_interruptFlags : and #2 : bne vblank
    STOP "unexpected interrupt"
    lda #&7f : sta system_VIA_interruptFlags ; ack
    lda interruptSaveA
    rti
.vblank:
    sta system_VIA_interruptFlags ; ack
    inc vsyncNotify
    lda interruptSaveA
    rti
    }

.syncVB: {
    IF SyncAssert : lda vsyncNotify : bne failSync1 : ENDIF ; pre-sync check (more harsh)
    { .loop : lda vsyncNotify : beq loop }
    IF SyncAssert : cmp #2 : bcs failSync2 : ENDIF ; post-sync check (move forgiving)
    lda #0 : sta vsyncNotify
    rts
.failSync1:
    STOP "Too slow to sync(1)"
.failSync2:
    STOP "Too slow to sync(2)"
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initialization

.initVia:
    lda #%01111111 : sta system_VIA_interruptEnable ; disable all interrupts
    lda #%10000010 : sta system_VIA_interruptEnable ; enable just VBlank
    ;; poll keyboard via system VIA portA
    ;; data directionA: bottom 7 bits output (key to poll); top bit input (is it pressed?)
    lda #%01111111 : sta system_VIA_dataDirectionA
    lda #%00001111 : sta system_VIA_dataDirectionB ; allow write to addressable latch
    lda #%00000011 : sta system_VIA_portB ; set bit 3 to 0
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
    lda #3 : jsr oswrch ; logical white
    lda #6 : jsr oswrch ; physical cyan
    lda #0 : jsr oswrch
    lda #0 : jsr oswrch
    lda #0 : jsr oswrch
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Erase Buffer (unified)

.resetEraseBuf:
    {
    lda frameCounter : and #1 : beq odd ; reversed
    jmp resetEraseBufA
.odd:
    jmp resetEraseBufB
    }

.pushEraseBuf: ;; ( A --> )
    {
    pha
    lda frameCounter : and #1 : beq odd ;; reversed
    pla
    jmp pushEraseBufA
.odd:
    pla
    jmp pushEraseBufB
    }

.processEraseBuf:
    {
    lda frameCounter : and #1 : bne odd
    jmp processEraseBufA
.odd:
    jmp processEraseBufB
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Erase Buffer (A)

.ptrEraseBufA SKIP 1
{ .before: align 256 : print "bytes wasted before align EraseBufA: ", *-before }
.EraseBufA SKIP 256

.resetEraseBufA:
    lda #0
    sta ptrEraseBufA
    rts

.pushEraseBufA: ;; ( A --> )
    {
    ldy ptrEraseBufA
    sta EraseBufA,y
    iny
    beq overflow ; TODO: make dev time check only
    sty ptrEraseBufA
    rts
.overflow:
    STOP "pushEraseBufA overflow"
    }

.processEraseBufA:
    {
    ldy ptrEraseBufA
    lda #0 ; no true hi screen byte can be zero
    sta EraseBufA,y
    ldy #0
.loop:
    lda EraseBufA,y : beq done : iny : sta SMC+2 ;hi
    lda EraseBufA,y            : iny : sta SMC+1 ;lo
    lda #blackMask
    .SMC : sta &eeee
    jmp loop
.done:
    rts
}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Erase Buffer (B)

.ptrEraseBufB SKIP 1
{ .before: align 256 : print "bytes wasted before align EraseBufB: ", *-before }
.EraseBufB SKIP 256

.resetEraseBufB:
    lda #0
    sta ptrEraseBufB
    rts

.pushEraseBufB: ;; ( A --> )
    {
    ldy ptrEraseBufB
    sta EraseBufB,y
    iny
    beq overflow ; TODO: make dev time check only
    sty ptrEraseBufB
    rts
.overflow:
    STOP "pushEraseBufB overflow"
    }

.processEraseBufB:
    {
    ldy ptrEraseBufB
    lda #0 ; no true hi screen byte can be zero
    sta EraseBufB,y
    ldy #0
.loop:
    lda EraseBufB,y : beq done : iny : sta SMC+2 ;hi
    lda EraseBufB,y            : iny : sta SMC+1 ;lo
    lda #blackMask
    .SMC : sta &eeee
    jmp loop
.done:
    rts
}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Plot Buffer

.ptrPlotBuf SKIP 1
{ .before: align 256 : print "bytes wasted before align PlotBuf: ", *-before }
.PlotBuf SKIP 256

.resetPlotBuf:
    lda #0
    sta ptrPlotBuf
    rts

.pushPlotBuf: ;; ( A --> )
    {
    ldy ptrPlotBuf
    sta PlotBuf,y
    iny
    beq overflow ; TODO: make dev time check only
    sty ptrPlotBuf
    rts
.overflow:
    STOP "pushPlotBuf overflow"
    }

.processPlotBuf:
    {
    ldy ptrPlotBuf
    lda #0 ; no true hi screen byte can be zero
    sta PlotBuf,y
    ldy #0
.loop:
    lda PlotBuf,y : beq done : iny : sta r+2 : sta w+2
    lda PlotBuf,y            : iny : sta r+1 : sta w+1
    lda PlotBuf,y            : iny
    .r eor &eeee
    .w sta &eeee
    jmp loop
.done:
    rts
}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (own) emit
;;; Prepare font tables for 4 colour mode.
;;; Each char gets 16 bytes of data, instead of 8 as in the original data

numberOfCharsInFont = 96 ; 128-' '
{ .before: align 256 : print "bytes wasted before font4colTab: ", *-before }
.font4colTab: skip (numberOfCharsInFont * 16)

macro Add16 V
    lda V   : clc : adc #16 : sta V
    lda V+1 :       adc #0  : sta V+1
endmacro

macro Add8 V
    lda V   : clc : adc #8 : sta V
    lda V+1 :       adc #0 : sta V+1
endmacro

.emitPos SKIP 2

.prepFont4col: {
    ldx #1
.loopX:
    ldy #0
.loopY:
    .romData : lda &c000, y
    pha
    and #&f0
    { sta smc+1 : lsr a : lsr a : lsr a : lsr a : .smc : ora #&ee }
    .fontTab1 : sta font4colTab,y
    pla
    and #&f
    { sta smc+1 : asl a : asl a : asl a : asl a : .smc : ora #&ee }
    .fontTab2 : sta font4colTab+8,y
    iny
    cpy #8 ;; bytes per char of original data
    beq doneY
    jmp loopY
.doneY:
    inx
    cpx #numberOfCharsInFont
    beq doneX
    Add8 romData+1
    Add16 fontTab1+1
    Add16 fontTab2+1
    jmp loopX
.doneX:
    rts
    }

.emit:
    {
    ldy #0 : sty font+1 : sty font+2
    sec : sbc #' '
    asl a : rol font+2
    asl a : rol font+2
    asl a : rol font+2
    asl a : rol font+2
    sta font+1

	;; if font4colTab were not aligned:
    ;; lda font+1 : clc : adc #LO(font4colTab) : sta font+1
    ;; lda font+2 :       adc #HI(font4colTab) : sta font+2
	;; but it is aligned, so:
    clc : lda font+2 : adc #HI(font4colTab) : sta font+2

    copy16v emitPos, pos1+1
    Add16 emitPos
    ldy #15
.loop:
    .font : lda &eeee,y
    .pos1 : sta &eeee,y
    dey
    bpl loop
    rts
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.end:
print "bytes remaining: ", screenStart-*
SAVE "Code", start, end
