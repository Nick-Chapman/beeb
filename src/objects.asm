
;;; Objects, with render loop and synced update

screenStart = &3000

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copy

macro Copy16i I,V
    lda #LO(I) : sta V
    lda #HI(I) : sta V+1
endmacro

macro Copy16v A,B
    lda A : sta B
    lda A+1 : sta B+1
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Crash

macro Crash S
    Copy16i msg, msgPtr
    jsr printString
    jmp spin
.msg:EQUS S, 0
endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; zero page

org &70
.msgPtr skip 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Start

guard screenStart
org &1100

.start:
    jmp main

;;; increase the image size, so the switch-on beep has time to finish while loading
skip 1000

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
    jsr emit
    pla
    and #&f : tay
    lda digits,y
    jsr emit
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

.init:
    lda #%01111111 : sta system_VIA_interruptEnable ; disable all interrupts
    lda #%10000010 : sta system_VIA_interruptEnable ; enable just VBlank
    jsr mode1
    jsr cursorOff
    jsr replaceWhiteWithCyan
    Copy16i myIRQ, irq1v
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Objects...

NumberObjects = 3

;;; more tables: postions, aciveNow, activeNext, lockedToArrows(DEV)

.updateOb: ; X:ob#
    Space : Emit 'u' : txa : jsr printHexA
    rts

.renderOb: ; X:ob#
    Space : Emit 'r' : txa : jsr printHexA
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Update everything.

.updateF skip 2*NumberObjects
.updateN: skip 1
.updateEverything: {
    lda #0 : sta updateN
.loop:
    ldx updateN : cpx #NumberObjects : { bne no : rts : .no }
    txa : asl a : tay
    lda updateF,   y : sta dispatch+1
    lda updateF+1, y : sta dispatch+2
    .dispatch : jsr &7777
    inc updateN
    jmp loop
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
;;; Render loop.

.renderF skip 2*NumberObjects
.renderN: skip 1
.renderLoop: {
.loop:
    lda vsyncNotify : { beq no : dec vsyncNotify : jsr updateEverything : .no }
    lda renderN : cmp #NumberObjects : { bne no : lda #0 : sta renderN : .no }
    tax : asl a : tay
    lda renderF,   y : sta dispatch+1
    lda renderF+1, y : sta dispatch+2
    .dispatch : jsr &7777
    inc renderN
    jmp loop
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Game init.

.setupZeroRts:
    lda rts : sta 0
.rts:
    rts

.initObjects:
    ;; Setup some objects...
    Copy16i updateOb, updateF+0
    Copy16i renderOb, renderF+0
    Copy16i updateOb, updateF+2
    Copy16i renderOb, renderF+2
    Copy16i updateOb, updateF+4
    Copy16i renderOb, renderF+4
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; main

.main:
    jsr setupZeroRts
    jsr init
    jsr initObjects
    jmp renderLoop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.end:
print "bytes remaining: ", screenStart-*
SAVE "Code", start, end
