;;; Key scanning test example

interruptSaveA = &fc
irq1v = &204

system_VIA_portB           = &fe40
system_VIA_dataDirectionB  = &fe42
system_VIA_dataDirectionA  = &fe43

system_VIA_interruptFlags  = &fe4d
system_VIA_interruptEnable = &fe4e
system_VIA_portA           = &fe4f

osasci = &ffe3
oswrch = &ffee
osbyte = &fff4

screenStart = &3000
screenEnd = &8000
stack = &100

macro STOP N
    lda #N : jsr stop
endmacro

macro copy16i I,V
    lda #LO(I) : sta V
    lda #HI(I) : sta V+1
endmacro

macro Puts S
    copy16i msg, msgPtr
    jmp after
.msg: EQUS S, 0
.after:
    jsr printMessage
endmacro

macro Position X,Y
    lda #31 : jsr osasci
    lda #X : jsr osasci
    lda #Y : jsr osasci
endmacro

GUARD &80
GUARD stack
ORG &70

.msgPtr SKIP 2

GUARD screenStart
ORG &1100

.start:
    jmp main

.myIrq:
    lda system_VIA_interruptFlags : and #2 : bne vblank
    ;STOP &33
    ;lda #&7f : sta system_VIA_interruptFlags
    lda interruptSaveA
    rti
.vblank:
    sta system_VIA_interruptFlags
    inc vsyncNotify
    lda interruptSaveA
    rti

.vsyncNotify SKIP 1

.syncVB: {
    ;lda vsyncNotify : bne failSync
.loop:
    lda vsyncNotify : beq loop
    lda #0 : sta vsyncNotify
    rts
.failSync:
    Puts "Too slow"
    jmp spin
    }

.main: {

    jsr mode1
    jsr cursorOff
    ;; wait a bit so b-em can finish the boot-up beep
    ;jsr mos_syncVB
    ;jsr mos_syncVB
    ;jsr mos_syncVB

    lda #%01111111 : sta system_VIA_interruptEnable ; disable all interrupts
    lda #%10000010 : sta system_VIA_interruptEnable ; enable just VBlank

    ;sei
    copy16i myIrq, irq1v
    ;cli

    ;; poll keyboard via system VIA portA
    ;; data directionA: bottom 7 bits output (key to poll); top bit input (is it pressed?)
    lda #%01111111 : sta system_VIA_dataDirectionA
    lda #%00001111 : sta system_VIA_dataDirectionB ; allow write to addressable latch
    lda #%00000011 : sta system_VIA_portB ; set bit 3 to 0

.loop:
    jsr syncVB

;; macro Poll CODE,VAR
;;     lda #0 : sta VAR
;;     lda #(-CODE-1) : sta system_VIA_portA : lda system_VIA_portA
;;     bpl no : lda #1 : sta VAR : .no
;; endmacro

    ldx #0
.more:
    lda #0 : sta keys,x
    stx system_VIA_portA : lda system_VIA_portA
    bpl no : lda #1 : sta keys,x : .no
    inx
    bne more

    Position 1,1 : Puts "Frame    : " : lda frameCount  : jsr printHexA

macro See CODE,CHR
    { lda #'.' : ldx #(-CODE-1) : ldy keys,x : beq no : lda #CHR : .no : jsr osasci }
endmacro

macro SeeSpecial MES,CODE
    Puts MES : ldx #(-CODE-1) : lda keys,x : jsr printHexA
endmacro

    Position 1,3 : SeeSpecial "Tab      : ", -97
    Position 1,4 : SeeSpecial "CapsLock : ", -65
    Position 1,5 : SeeSpecial "Shift    : ", -1
    Position 1,6 : SeeSpecial "Control  : ", -2

    Position 1,8 : SeeSpecial "Return   : ", -74

    Position 1,10 : SeeSpecial "Up       : ", -58
    Position 1,11 : SeeSpecial "Down     : ", -42
    Position 1,12 : SeeSpecial "Left     : ", -26
    Position 1,14 : SeeSpecial "Right    : ", -122 ; problem at position 13 ??


    Position 1,17

    See -17,'Q'
    See -34,'W'
    See -35,'E'
    See -52,'R'
    See -36,'T'
    See -69,'Y'
    See -54,'U'
    See -38,'I'
    See -55,'O'
    See -56,'P'

    Position 2,19

    See -66,'A'
    See -82,'S'
    See -51,'D'
    See -68,'F'
    See -84,'G'
    See -85,'H'
    See -70,'J'
    See -71,'K'
    See -87,'L'

    Position 3,21

    See -98,'Z'
    See -67,'X'
    See -83,'C'
    See -100,'V'
    See -101,'B'
    See -86,'N'
    See -102,'M'

    inc frameCount
    jmp loop }

;;; some non-zero-page variables

.frameCount SKIP 1

.keys SKIP 256

.mos_syncVB: lda #19 : jmp osbyte

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

.printMessage: {
    ldy #0
.loop
    lda (msgPtr),y
    beq done
    jsr osasci
    iny
    bne loop
.done:
    rts }

.stop:
    jsr printHexA
    Puts "Stop,"
    jmp spin

.spin :  {
    ;jsr printHexA
    Puts "SPIN"
.loop:
    jmp loop }

.end:
print "bytes left: ", screenStart-*
SAVE "Code", start, end
