
;;; New Year. New fun.
;;; Determine Acceleration via Thrust/Drag.

Impulse = 2 ;; Thrust = 1/2**Impulse
Momentum = 5 ;; Drag = 1/2**Momentum
;;; MaxSpeed ~= 2**(Momentum - Impulse)

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

macro Space
	lda #' '
    jsr osasci
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
    ldx &90 ; End of stack; grows downwards
    jsr syncVB
    jsr plotRun
.loop:
    jsr readKeys
    jsr textInfo
    IF RasterDebugPrepare : lda #3 : sta ula : ENDIF ; blue (prepare)
    jsr prepare
    lda #7 : sta ula ; black
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
    FOR i, 1, Impulse : jsr halve : NEXT
    PushVar16 Speed
    jsr decay5
    PopVar16 Accel
endmacro

.prepare:
    jsr savePos ; save where we were before update
    jsr setAccel ; in response to a key presses & existing speed
    jsr updateSpeed ; using accel
    jsr updatePos ; using speed
    rts

.savePos: ; ( -- ) only the position high-bytes
    lda posX+1 : sta lastPosX
    lda posY+1 : sta lastPosY
    rts

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

.decay5: ; ( tt vv -- aa )
    FOR i, 1, Momentum : jsr decay1 : NEXT
    jsr minus
    rts

.decay1: { ; ( vv -- vv )
    lda 1,x ;h
    bmi no
    jsr pushOne ; TODO: inline
    jsr plus ; TODO change to inc
.no:
    jmp halve
    }

.pushOne
    lda #0 : PushA : lda #1 : PushA
    rts

macro DetermineThrust Decrease, Increase
    lda Increase : beq no
    lda Decrease : bne pushZero ; both
    jmp pushOneE8 ; TODO inline
    .no : lda Decrease : beq pushZero ; neither
    jmp pushMinusOneE8 ; TODO inline
endmacro

.thrustX:
    DetermineThrust keyLeft, keyRight

.thrustY:
    DetermineThrust keyUp, keyDown

.pushZero:
    lda #0 : PushA : PushA
    rts

.pushOneE8
    lda #1 : PushA : lda #0 : PushA
    rts

.pushMinusOneE8
    lda #&ff : PushA : lda #0 : PushA
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

.drawShape: ; ( x y -- ) -- draw a simple tricolour L-shape
    lda #red : PushA : jsr pointAt
    inc 0,x
    lda #yellow : PushA : jsr pointAt
    inc 1,x
    lda #cyan : PushA : jsr pointAt
    PopY
    PopY
    rts

.pointAt: ; ( x y col -- x y )
    PopA ; col
    sta SMC +1
    jsr calcA ; ( a-hi a-lo fineXmask )
    PopA ; fineXmask
    .SMC : and #&33
    PushA ; col & fineXmask --> d
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
    .SMC_r eor &3333
    .SMC_w sta &3333
    rts
    }

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
    Position 1,27 : Puts "A: " : PrHexWord accelX : Space : PrHexWord accelY
    Position 1,28 : Puts "V: " : PrHexWord speedX : Space : PrHexWord speedY
    Position 1,30 : jsr printKeyState
    rts

.printKeyState:
    lda #'.' : { ldy keyLeft  : beq no : lda #'L' : .no } : jsr osasci
    lda #'.' : { ldy keyRight : beq no : lda #'R' : .no } : jsr osasci
    lda #'.' : { ldy keyUp    : beq no : lda #'U' : .no } : jsr osasci
    lda #'.' : { ldy keyDown  : beq no : lda #'D' : .no } : jsr osasci
    lda #' '                                              : jsr osasci
    lda #'.' : { ldy keyShift : beq no : lda #'S' : .no } : jsr osasci
    lda #'.' : { ldy keyEnter : beq no : lda #'E' : .no } : jsr osasci
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
    and #&f0 : lsr a : lsr a : lsr a : lsr a : tax
    lda digits,x
    jsr osasci
    pla
    and #&f : tax
    lda digits,x
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
    lda vsyncNotify : bne failSync1 ; pre-sync check (more harsh)
    { .loop : lda vsyncNotify : beq loop }
    cmp #2 : bcs failSync2 ; post-sync check (move forgiving)
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
