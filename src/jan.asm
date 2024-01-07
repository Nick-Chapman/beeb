
;;; New Year. New fun.
;;; Determine Acceleration via Thrust/Drag.

;;; Possible values for Impulse/Momentum: 2/5, 3/5, 3/6
Impulse = 3 ;; Thrust = 1/2**Impulse
Momentum = 6 ;; Drag = 1/2**Momentum
;;; MaxSpeed ~= 2**(Momentum - Impulse)

SyncAssert = TRUE
RasterDebugPrepare = TRUE
RasterDebugBlit = TRUE

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

cyan = &ff
yellow = &f0
red = &0f

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
    jsr plotRun
.loop:
    jsr readKeys
    IF RasterDebugPrepare : lda #3 : sta ula : ENDIF ; blue (prepare)
    jsr prepare
    lda #7 : sta ula ; black
    jsr textInfo
    jsr syncVB
    IF RasterDebugBlit : lda #2 : sta ula : ENDIF ; magenta (blit)
    jsr blitScene
    lda #7 : sta ula ; black
    inc frameCounter
    jmp loop
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; State

.frameCounter EQUB 0

.direction EQUB 0 ; angles increase clockwise

.accelX SKIP 2
.accelY SKIP 2
.speedX SKIP 2
.speedY SKIP 2
.posX EQUW &1100
.posY EQUW &2200

;; (last) copies of (just)high-byte of pos, for erasing
.lastPosX SKIP 1
.lastPosY SKIP 1

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
    rts

.updateDirection: {
    PushVar8 direction
    lda keyCaps : beq no
    lda keyCtrl : bne done ; both
    dec 0,x : jmp done
    .no : lda keyCtrl : beq done ; neither
    inc 0,x
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
    clc : adc #32
    sta 0,x
    rts

.sin: ; ( n -- nn )
    jsr mirrorB
    PopA : pha
	jsr mirrorA
    jsr thirtyOneMinusMaybe
    jsr sinTableLookup
    jsr pushZeroByte ; HI byte of resulting answer
    jsr swap
    pla : PushA
    jsr invertMaybe
    rts

.sinTableLookup: ; ( n -- n ) -- TODO (return nn)
    lda 0,x ; 0--31
    and #31
    tay
    ;jsr pushZeroByte ;;TODO
    lda sinTab, y
    sta 0,x
    rts

;;; TODO inline?...
.pushZeroByte: ; ( -- n )
    lda #0 : PushA
    rts

.mirrorB: ; ( n -- n p )
    lda 0,x
    and #&40
    PushA
    rts

.mirrorA: ; ( n -- n p )
    lda 0,x
    and #&20
    PushA
    rts

.thirtyOneMinusMaybe: ; ( n p -- n )
    inx
    lda &ff,x
    bne thirtyOneMinus
    rts

.thirtyOneMinus: ; ( n -- n )
    lda #31
    sec : sbc 0,x
    sta 0,x
    rts

.swap: ; ( n n -- n n )
    lda 0,x
    ldy 1,x
    sta 1,x
    sty 0,x
    rts

macro mm B
    EQUB B
    ;PRINT B
endmacro

.sinTab: FOR i, 0, 31 : mm INT(SIN((i*PI)/64) * 256) : NEXT
sinTabSize = *-sinTab
ASSERT sinTabSize = 32

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
;;; Blit

.blitScene:
    jsr eraseRun
    jmp plotRun

.eraseRun: ; ( -- ) -- erase is same as plot, except using last pos
    PushVar8 lastPosX
    PushVar8 lastPosY
    jmp drawShape

.plotRun: ; ( -- )
    PushVar8 posX+1
    PushVar8 posY+1
    jmp drawShape

macro Cyan : jsr cyanPointAt : endmacro
macro Yel : jsr yellowPointAt : endmacro
macro IX : inc 1,x : endmacro
macro IY : inc 0,x : endmacro
macro DX : dec 1,x : endmacro
macro DY : dec 0,x : endmacro

.drawShape: ; ( x y -- ) -- draw a simple shape
    Cyan
    IX : Yel : IX
    Yel : IY : Yel : IY
    Yel : DX : Yel : DX
    Yel : DY : Yel
    PopA : PopA
    rts

.redPointAt: ; ( x y -- x y )
    jsr calcA ; ( a-hi a-lo fineXmask )
    PopA ; fineXmask
    and #red
    PushA
    jmp eorAt

.yellowPointAt: ; ( x y -- x y )
    jsr calcA ; ( a-hi a-lo fineXmask )
    PopA ; fineXmask
    and #yellow
    PushA
    jmp eorAt

.cyanPointAt: ; ( x y -- x y )
    jsr calcA ; ( a-hi a-lo fineXmask )
    jmp eorAt

.eorAt: { ; ( aa d -- )
    PopY ; d
    PopA ; a-lo
    sta SMC_r +1
    sta SMC_w +1
    PopA ; a-hi
    sta SMC_r +2
    sta SMC_w +2
    tya ; d
    .SMC_r eor &eeee
    .SMC_w sta &eeee
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
    ldy theFX
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
    Position 1,26 : Emit 'A' : Space : PrHexWord accelX : Space : PrHexWord accelY
    Position 1,27 : Emit 'S' : Space : PrHexWord speedX : Space : PrHexWord speedY
    ;Position 1,28 : Emit 'P' : Space : PrHexWord posX : Space : PrHexWord posY
    Position 1,30
    ;lda frameCounter : jsr printHexA
    txa : jsr printHexA ; debug param stack bugs
    Space : lda direction : jsr printHexA
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
