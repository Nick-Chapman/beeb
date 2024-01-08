
;;; New Year. New fun.
;;; Determine Acceleration via Thrust/Drag.

;;; Possible values for Impulse/Momentum: 2/5, 3/5, 3/6
Impulse = 2 ;; Thrust = 1/2**Impulse
Momentum = 5 ;; Drag = 1/2**Momentum
;;; MaxSpeed ~= 2**(Momentum - Impulse)

SpeedTurn = 4

SyncAssert = TRUE
RasterDebugBlit = TRUE
RasterDebugPrepare = FALSE
RasterDebugTextInfo = FALSE

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

macro Emit C
	lda #C
    jsr osasci
endmacro

macro Space
	Emit ' '
endmacro

macro Position X,Y
    lda #31 : jsr osasci
    lda #X : jsr osasci
    lda #Y : jsr osasci
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

    ;;jsr syncVB ; earlier for DEV, so can see full size of green/magenta erase/blit

    IF RasterDebugPrepare : lda #blue : sta ula : ENDIF
    jsr prepare
    lda #black : sta ula

    IF RasterDebugTextInfo : lda #red : sta ula : ENDIF
    jsr textInfo
    lda #black : sta ula

    jsr syncVB ; proper location

    IF RasterDebugBlit : lda #green : sta ula : ENDIF
    jsr eraseRun
    IF RasterDebugBlit : lda #magenta : sta ula : ENDIF
    jsr plotRun

    lda #black : sta ula

    inc frameCounter
    jmp loop
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; State

QuarterTurn = 64
HalfTurn = QuarterTurn + QuarterTurn
ThreeQuarters = QuarterTurn + HalfTurn

.frameCounter EQUB 0

.direction EQUB ThreeQuarters ; angles increase clockwise

.accelX SKIP 2
.accelY SKIP 2
.speedX SKIP 2
.speedY SKIP 2
.posX EQUW &9000
.posY EQUW &e000

;; (last) copies of (just)high-byte of pos, for erasing
.lastPosX SKIP 1
.lastPosY SKIP 1
.lastDirection SKIP 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Prepare

macro PlusEq16 A, B ; A <- A+B
    PushVar16 B
    PushVar16 A
    jsr plus
    PopVar16 A
endmacro

macro ComputeAccel Accel, Thrust, Speed
    jsr Thrust
    IF Impulse > 0 : FOR i, 1, Impulse : jsr halve : NEXT
    ENDIF
    PushVar16 Speed
    jsr decay5
    PopVar16 Accel
endmacro

.prepare:
    jsr savePos ; save where we were before update
    jsr updateDirection ; in repose to key caps/ctrl key presses
    jsr setAccel ; in response to a key presses & existing speed
    jsr updateSpeed ; using accel
    jsr updatePos ; using speed
    rts

.savePos: ; ( -- ) only the position high-bytes
    lda posX+1 : sta lastPosX
    lda posY+1 : sta lastPosY
    lda direction : sta lastDirection
    rts

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

.setAccel: ; ( -- )
    ComputeAccel accelX, thrustX, speedX
    ComputeAccel accelY, thrustY, speedY
    rts

.updateSpeed: ; ( -- )
    PlusEq16 speedX, accelX
    PlusEq16 speedY, accelY
    rts

.updatePos: ; ( -- )
    PlusEq16 posX, speedX
    PlusEq16 posY, speedY
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Thrust

.thrustX: { ; ( -- nn )
    lda keyShift : beq noThrust
    PushVar8 direction
    jmp cos
.noThrust:
    jmp pushZero
    }

.thrustY: { ; ( -- nn )
    lda keyShift : beq noThrust
    PushVar8 direction
    jmp sin
.noThrust:
    jmp pushZero
    }

.pushZero: ; ( -- nn )
    lda #0 : PushA : PushA
    rts

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

.sinTableLookup: ; ( n -- nn )
    PopA
    and #(QuarterTurn-1)
    tay
    jsr pushZeroByte ; res-hi
    lda sinTab, y
    PushA
    rts

.pushZeroByte: ; ( -- n )
    lda #0 : PushA
    rts

.mirrorB: ; ( n -- n p )
    lda 0,x
    and #&80
    PushA
    rts

.mirrorA: ; ( n -- n p )
    lda 0,x
    and #&40
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

macro mm B
    EQUB B
    ;PRINT B
endmacro

.sinTab: FOR i, 0, QuarterTurn-1 : mm INT(SIN((i*PI)/128) * 256) : NEXT
sinTabSize = *-sinTab
ASSERT sinTabSize = 64

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Arithmetic

.decay5: ; ( tt vv -- aa )
    IF Momentum > 0 : FOR i, 1, Momentum : jsr decay1 : NEXT
    ENDIF
    jsr minus
    rts

.decay1: { ; ( vv -- vv )
    lda 1,x ;h
    bmi no
    jsr pushOne : jsr plus ; TODO increment
.no:
    jmp halve
    }

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

macro Center : EQUB 32 : endmacro ; see center of rotation
;;;macro Center : EQUB 16 : endmacro ; DONT see center of rotation
;;;macro Center : endmacro  ; DONT see center of rotation (and dont even waste a byte!)

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

.eraseRun: ; ( -- )
    PushVar8 lastPosX
    PushVar8 lastPosY
    PushVar8 lastDirection
    jsr setOrientationFromDirection
    copy16i erasePointAt, SMC_doPoint+1 : jsr drawShape
    PopA : PopA
    rts

.plotRun: ; ( -- )
    lda #yellowMask : PushA
    PushVar8 posX+1
    PushVar8 posY+1
    PushVar8 direction
    jsr setOrientationFromDirection
    copy16i colPointAt, SMC_doPoint+1 : jsr drawShape
    PopA : PopA : PopA
    rts

.setOrientationFromDirection: ; ( d -- )

    ;; Set outline...
    lda 0,x
    lsr a : lsr a : lsr a ; each outline covers 8 angles
    and #7 ; take 3 bits (8 outlines in sequence)
    asl a ; index Y for outline table must be even
    tay
    lda outlines,y : sta SMC_outline+1
    lda outlines+1,y : sta SMC_outline+2

    ;; Set orientation...
    PopA ; direction
    lsr a : lsr a : lsr a
    and #%00011100
    tay ; 0,4,8,12,16,20,24,28
    lda orientations,y : sta n : iny
    lda orientations,y : sta e : iny
    lda orientations,y : sta s : iny
    lda orientations,y : sta w
    rts

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
.n SKIP 1
.s SKIP 1
.e SKIP 1
.w SKIP 1

.drawShape: { ; ( c x y -- c x y )
    ldy #0
.loop:
    ;;cpy #1 : beq done ;; DEV HACK - just see one point
    .*SMC_outline : lda &eeee, y : beq done
    bit n : bne north
    bit s : bne south
    jmp ew
.north: dec 0,x : jmp ew
.south: inc 0,x : jmp ew
.ew:
    bit e : bne east
    bit w : bne west
    jmp pr
.east: inc 1,x : jmp pr
.west: dec 1,x : jmp pr
.pr:
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

.colPointAt: ; ( c x y -- c x y ) -- TODO: reorder x y c ?
    jsr calcA ; ( c x y aa fineXmask )
    lda 0,x
    and 5,x
    sta 0,x
    jmp eorAt

.eorAt: { ; ( aa d -- )
    PopA : sta SMC_d + 1
    PopA : sta SMC_r + 1 : sta SMC_w + 1
    PopA : sta SMC_r + 2 : sta SMC_w + 2
    .SMC_d lda #&ee
    .SMC_r eor &eeee
    .SMC_w sta &eeee
    rts
    }

.erasePointAt: ; ( x y -- x y )
    jsr calcA ; ( x y aa fineXmask )
    inx ; drop fineXmask
    jmp eraseAt

.eraseAt: { ; ( aa -- )
    PopA : sta SMC +1
    PopA : sta SMC +2
    lda #blackMask
    .SMC sta &eeee
    rts
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; calculate screen address from X/Y

.calcA: { ; ( x y -- x y a-hi a-lo fineXmask )
    ;;PopA ; y
    lda 0,x
    pha : and #&7 : sta theFY : pla
    lsr a : lsr a : lsr a : sta theCY
    ;;PopA ; x
    lda 1,x
    pha : and #&3 : sta theFX : pla
    lsr a : lsr a : sta theCX
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
    ;Position 1,27 : Emit 'A' : Space : PrHexWord accelX : Space : PrHexWord accelY
    ;Position 1,28 : Emit 'S' : Space : PrHexWord speedX : Space : PrHexWord speedY
    Position 1,30 : Emit 'D' : Space : lda direction : jsr printHexA
    ;Position 1,28 : Emit 'P' : Space : PrHexWord posX : Space : PrHexWord posY
    ;lda frameCounter : jsr printHexA
    ;Position 1,31 : txa : jsr printHexA ; debug param stack bugs
    ;Space : jsr printKeyState
    rts

.printKeyState:
    lda #'.' : { ldy keyCaps : beq no : lda #'A' : .no } : jsr osasci
    lda #'.' : { ldy keyCtrl : beq no : lda #'C' : .no } : jsr osasci
    lda #'.' : { ldy keyShift : beq no : lda #'S' : .no } : jsr osasci
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
    jsr osasci
    pla
    and #&f : tay
    lda digits,y
    jsr osasci
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
.end:
print "bytes remaining: ", screenStart-*
SAVE "Code", start, end
