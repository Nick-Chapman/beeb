
;;; New Year. New fun.
;;; Determine Acceleration via Thrust/Drag.

;;; Possible values for Impulse/Momentum: 2/5, 3/5, 3/6
;;; Impulse = 0 ;; Thrust = 1/2**Impulse
Momentum = 7 ;; Drag = 1/2**Momentum
SpeedScale = 4

;;; MaxSpeed ~= 2**(Momentum - Impulse - SpeedScale) pixels/frame

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

macro Puts S
    copy16i msg, msgPtr
    jmp after
.msg: EQUS S, 0
.after: jsr printString
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
.ptr SKIP 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Start

guard screenStart
org &1100

.start:
    jmp main

.spin: jmp spin

.main: {
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
    jsr textInfo ;; DEV : if after sync to see full size of erase/blit
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
    jsr prepareErase
    jsr preparePlot
    rts

.prepareErase: ; ( -- )
    jsr resetEraseBuf
    PushVar16 lastPosX
    PushVar8 lastPosY+1 ; only hi
    PushVar8 lastDirection
    jsr setOrientationFromDirection
    copy16i erasePointAt, SMC_doPoint+1 : jsr drawShape
    PopA : PopA : PopA
    rts

.erasePointAt: ; ( xx y -- xx y )
    jsr calcA ; ( xx y aa fineXmask )
    inx ; drop fineXmask
    jmp eraseAt

.eraseAt:  ; ( aa -- )
    lda 1,x : jsr pushEraseBuf ;hi -- hi comes first in buffer
    lda 0,x : jsr pushEraseBuf ;lo
    inx : inx
    rts

.preparePlot: ; ( -- )
    jsr resetPlotBuf
    lda #yellowMask : PushA
    PushVar16 posX
    PushVar8 posY+1 ; only hi
    PushVar8 direction
    jsr setOrientationFromDirection
    copy16i colPointAt, SMC_doPoint+1 : jsr drawShape
    PopA : PopA : PopA : PopA
    rts

.colPointAt: ; ( c xx y -- c xx y ) -- TODO: reorder x y c ?
    jsr calcA ; ( c xx y aa fineXmask )
    lda 0,x
    and 6,x
    sta 0,x
    jmp eorAt

.eorAt: { ; ( aa d -- )
    lda 2,x : jsr pushPlotBuf ; a-hi -- hi comes first in buffer
    lda 1,x : jsr pushPlotBuf ; a-lo
    lda 0,x : jsr pushPlotBuf ; d
    inx : inx : inx
    rts
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; State

;;; We resolve only 8 distinct angles per QuarterTurn (for outline & thrust direction)
AnglesResolvedPerQT = 8

;;; But to control ship rotation speed, we scale that. 0 is too fast; 2 is two slow
RotSpeedIndex = 1

macro ScaleDirection
    IF RotSpeedIndex > 0 : FOR i, 1, RotSpeedIndex : lsr a : NEXT
    ENDIF
endmacro

QuarterTurn = AnglesResolvedPerQT * (2^RotSpeedIndex)
HalfTurn = QuarterTurn + QuarterTurn
ThreeQuarters = QuarterTurn + HalfTurn

.frameCounter EQUB 0

.direction EQUB ThreeQuarters ; start North; angles increase clockwise (0 is East)

.accelX SKIP 2
.accelY SKIP 2
.speedX SKIP 2
.speedY SKIP 2
.posX EQUB 0, 75
.posY EQUB 0, 128

;; (last) copies of position & direction
.lastPosX SKIP 2
.lastPosY SKIP 2
.lastDirection SKIP 1

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
    jsr savePos
    jsr updateDirection ; in response to Caps/Ctrl
    jsr updateDirectionFromArrows ; in response to Arrow keys (for dev/xdebug)

    ;; X...
    jsr thrustX
    PushVar16 speedX
    jsr decayMomentum
    jsr minus
    PopVar16 accelX
    PlusEq16 speedX, accelX
    PushVar16 speedX
    IF SpeedScale > 0 : FOR i, 1, SpeedScale+1 : jsr decay1 : NEXT ;; +1 because X
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
    IF SpeedScale > 0 : FOR i, 1, SpeedScale : jsr decay1 : NEXT
    ENDIF
    PushVar16 posY
    jsr plus
    PopVar16 posY

    rts

.savePos: ; ( -- ) only the position high-bytes
    copy16v posX, lastPosX
    copy16v posY, lastPosY ; dont need to copy the lo-byte, but hey
    lda direction : sta lastDirection
    rts

SpeedTurn = 1 ;; TODO: DIE

.updateDirection: {
    PushVar8 direction
    lda keyCaps : beq no
    lda keyCtrl : bne done ; both
    FOR i,1,SpeedTurn : dec 0,x : NEXT
    jmp done
    .no : lda keyCtrl : beq done ; neither
    FOR i,1,SpeedTurn : inc 0,x : NEXT
.done:
    PopVar8 direction
    rts
    }

.updateDirectionFromArrows:
    { lda keyRight : beq no : lda #0 : sta direction : .no }
    { lda keyDown : beq no : lda #QuarterTurn : sta direction : .no }
    { lda keyLeft : beq no : lda #HalfTurn : sta direction : .no }
    { lda keyUp : beq no : lda #ThreeQuarters : sta direction : .no }
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Thrust

.thrustX: { ; ( -- nn )
    jsr areWeThrusting : beq noThrust
    PushVar8 direction
    jsr cos
    rts
.noThrust:
    jmp pushZero
    }

.thrustY: { ; ( -- nn )
    jsr areWeThrusting : beq noThrust
    PushVar8 direction
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
    lda #(QuarterTurn-1)
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
    PopA
    and #(QuarterTurn-1)
    ScaleDirection
    tay
    jsr pushZeroByte ; res-hi
    lda sinTab, y
    PushA
    rts

macro mm B
    EQUB B
    ;;PRINT B
endmacro

.sinTab:
    FOR i, 0, AnglesResolvedPerQT-1
    mm INT(SIN((i*PI)/2/AnglesResolvedPerQT) * 256)
    NEXT

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Arithmetic

.decayMomentum: ; ( tt vv -- aa )
    IF Momentum > 0 : FOR i, 1, Momentum : jsr decay1 : NEXT
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

.decrement: ; ( vv -- vv ) ; TODO better imp! -- Any callers?
    jsr pushOne
    jmp minus

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
    ;; direction scaled -> angle
    ScaleDirection
    pha

    ;; Set outline...
    and #7 ; take 3 bits (8 outlines in sequence)
    asl a ; index Y for outline table must be even
    tay
    lda outlines,y : sta SMC_outline+1
    lda outlines+1,y : sta SMC_outline+2

    ;; Set orientation...
    pla ; angle
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

macro PollKey CODE, VAR
    lda #0 : sta VAR
    lda #(-CODE-1) : sta system_VIA_portA : lda system_VIA_portA
    bpl no : lda #1 : sta VAR : .no
endmacro

.readKeys:
    PollKey -1,   keyShift
    PollKey -74,  keyEnter
    PollKey -65,  keyCaps
    PollKey -2,   keyCtrl
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

    ;Space : lda frameCounter : jsr printHexA
    ;Space : txa : jsr printHexA ; debug param stack bugs
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
    ;;IF SyncAssert : lda vsyncNotify : bne failSync1 : ENDIF ; pre-sync check (more harsh) ;; TODO
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
;;; Erase Buffer

.ptrEraseBuf SKIP 1
{ .before: align 256 : print "bytes wasted before align EraseBuf: ", *-before }
.EraseBuf SKIP 256

.resetEraseBuf:
    lda #0
    sta ptrEraseBuf
    rts

.pushEraseBuf: ;; ( A --> ) -- TODO: macroize?
    {
    ldy ptrEraseBuf
    sta EraseBuf,y
    iny
    beq overflow ; TODO: make dev time check only
    sty ptrEraseBuf
    rts
.overflow:
    STOP "pushEraseBuf overflow"
    }

.processEraseBuf:
    {
    ldy ptrEraseBuf
    lda #0 ; no true hi screen byte can be zero
    sta EraseBuf,y
    ldy #0
.loop:
    lda EraseBuf,y : beq done : iny : sta SMC+2 ;hi
    lda EraseBuf,y            : iny : sta SMC+1 ;lo
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

.pushPlotBuf: ;; ( A --> ) -- TODO: macroize?
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

macro Add8 V
    lda V   : clc : adc #8 : sta V
    lda V+1 :       adc #0 : sta V+1
endmacro

.emitPos SKIP 2

.emit:
    {
    ldy #0 : sty font+1 : sty font+2
    sec : sbc #32
    asl a : rol font+2
    asl a : rol font+2
    asl a : rol font+2
    sta font+1
    lda font+2 : clc : adc #&c0
    sta font+2
    copy16v emitPos, pos1+1
    Add8 emitPos
    copy16v emitPos, pos2+1
    Add8 emitPos
    ldy #0
.loop:
    .font : lda &eeee,y
    pha
    and #&f0
    { sta smc+1 : lsr a : lsr a : lsr a : lsr a : .smc : ora #&ee }
    .pos1 : sta &eeee,y
    pla
    and #&f
    ;asl a : asl a : asl a : asl a
    { sta smc+1 : asl a : asl a : asl a : asl a : .smc : ora #&ee }
    .pos2 : sta &eeee,y
    iny
    cpy #8 : beq done
    jmp loop
.done:
    rts
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.end:
print "bytes remaining: ", screenStart-*
SAVE "Code", start, end
