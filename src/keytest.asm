;;; Key scanning test example

system_VIA_portB           = &fe40
system_VIA_dataDirectionB  = &fe42
system_VIA_dataDirectionA  = &fe43
system_VIA_interruptEnable = &fe4e
system_VIA_portA           = &fe4f

osasci = &ffe3
oswrch = &ffee
osbyte = &fff4

screenStart = &3000
screenEnd = &8000
stack = &100

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

macro Poll CODE,VAR

    lda #0 : sta VAR
    lda #(-CODE-1) : sta system_VIA_portA : lda system_VIA_portA
    bpl no : lda #1 : sta VAR : .no
endmacro

.main: {

    jsr mode1
    jsr cursorOff
    jsr syncVB : jsr syncVB ; wait a bit so b-em can finish  the boot-up beep

    lda #%01111111 : sta system_VIA_interruptEnable ; disable all interrupts
    lda #%10000010 : sta system_VIA_interruptEnable ; enable VSync

    ;; poll keyboard via system VIA portA
    ;; data directionA: bottom 7 bits output (key to poll); top bit input (is it pressed?)
    lda #%01111111 : sta system_VIA_dataDirectionA
    lda #%00001111 : sta system_VIA_dataDirectionB ; allow write to addressable latch
    lda #%00000011 : sta system_VIA_portB ; set bit 3 to 0

.loop:
    jsr syncVB

    Poll -98, keyZ
    Poll -67, keyX
    Poll -65, keyCapsLock
    Poll -2, keyCtrl
    Poll -1, keyShift
    Poll -74, keyReturn

    Position 1,1 : Puts "Frame    : " : lda frameCount  : jsr printHexA

    Position 1,3 : Puts "Z        : " : lda keyZ        : jsr printHexA
    Position 1,4 : Puts "X        : " : lda keyX        : jsr printHexA
    Position 1,5 : Puts "CapsLock : " : lda keyCapsLock : jsr printHexA
    Position 1,6 : Puts "Ctrl     : " : lda keyCtrl     : jsr printHexA
    Position 1,7 : Puts "Shift    : " : lda keyShift    : jsr printHexA
    Position 1,8 : Puts "Return   : " : lda keyReturn   : jsr printHexA

    inc frameCount
    jmp loop }

;;; some non-zero-page variables

.frameCount SKIP 1

.keyZ SKIP 1
.keyX SKIP 1
.keyCapsLock SKIP 1
.keyCtrl SKIP 1
.keyShift SKIP 1
.keyReturn SKIP 1

.spin : jmp spin

.mode1:
    lda #22 : jsr oswrch
    lda #1 : jsr oswrch
    rts

.syncVB:
    lda #19 : jsr osbyte
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

.end:
print "bytes left: ", screenStart-*
SAVE "Code", start, end
