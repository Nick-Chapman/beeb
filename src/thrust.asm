
;;; Explore fractional and signed calculations for thrust.

;;;----------------------------------------------------------------------
;;; MOS vectors & zero page use

interruptSaveA = &fc
irq1v = &204

;;;----------------------------------------------------------------------
;;; Shelia

ula                         = &fe21

system_VIA_portB            = &fe40
system_VIA_dataDirectionB   = &fe42
system_VIA_dataDirectionA   = &fe43

system_VIA_interruptFlags   = &fe4d
system_VIA_interruptEnable  = &fe4e
system_VIA_portA            = &fe4f

;;;----------------------------------------------------------------------
;;; MOS entry points

osasci = &ffe3
oswrch = &ffee
osbyte = &fff4

;;;----------------------------------------------------------------------
;;; 16bit macros

macro copy16i I,V
    lda #LO(I) : sta V
    lda #HI(I) : sta V+1
endmacro

macro copy16v A,B
    lda A : sta B
    lda A+1 : sta B+1
endmacro

;;;----------------------------------------------------------------------
;;; Print macros

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

;;;----------------------------------------------------------------------
;;; Zero page

GUARD &100 ; stack
ORG &70
.msgPtr SKIP 2

;;;----------------------------------------------------------------------
;;; Program code

screenStart = &3000
screenEnd = &8000

GUARD screenStart
ORG &1100

.start:
    jmp main

.spin: jmp spin

;;;----------------------------------------------------------------------
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
.mySync: {
    ;;lda vsyncNotify : bne failSync1 ; pre-sync check (more harsh)
    { .loop : lda vsyncNotify : beq loop }
    ;;cmp #2 : bcs failSync2 ; post-sync check (move forgiving)
    lda #0 : sta vsyncNotify
    rts
.failSync1:
    STOP "Too slow to sync(1)"
.failSync2:
    STOP "Too slow to sync(2)"
}

;;;----------------------------------------------------------------------
;;; initialize

.initialize: {
    lda #%01111111 : sta system_VIA_interruptEnable ; disable all interrupts
    lda #%10000010 : sta system_VIA_interruptEnable ; enable just VBlank
    ;; poll keyboard via system VIA portA
    ;; data directionA: bottom 7 bits output (key to poll); top bit input (is it pressed?)
    lda #%01111111 : sta system_VIA_dataDirectionA
    lda #%00001111 : sta system_VIA_dataDirectionB ; allow write to addressable latch
    lda #%00000011 : sta system_VIA_portB ; set bit 3 to 0
    jsr mode1
    jsr cursorOff
    jsr replaceWhiteWithCyan
    copy16i myIRQ, irq1v
    rts
}

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

;;;----------------------------------------------------------------------
;;; printing

.printString: {
    ldy #0
.loop
    lda (msgPtr),y
    beq done
    jsr osasci
    iny
    bne loop
.done:
    rts }

.printHexA: {
    ;;pha : lda #'[' : jsr osasci : pla
    pha
    and #&f0 : lsr a : lsr a : lsr a : lsr a : tax
    lda digits,x
    jsr osasci
    pla
    and #&f : tax
    lda digits,x
    jsr osasci
    ;;pha : lda #']' : jsr osasci : pla
    rts
.digits EQUS "0123456789abcdef" }

;;;----------------------------------------------------------------------
;;; keyboard

;;.keyReturn SKIP 1
;;.lastReturn SKIP 1
;;.edgeReturn SKIP 1

.keyUp SKIP 1
.lastUp SKIP 1
.edgeUp SKIP 1

.keyDown SKIP 1
.lastDown SKIP 1
.edgeDown SKIP 1

.keyShift SKIP 1
.lastShift SKIP 1
.edgeShift SKIP 1

macro PollKey CODE, VAR
    lda #0 : sta VAR
    lda #(-CODE-1) : sta system_VIA_portA : lda system_VIA_portA
    bpl no : lda #1 : sta VAR : .no
endmacro

.readKeys:
    ;; save current key values for edge-triggered keys
    lda keyUp : sta lastUp
    lda keyDown : sta lastDown
    lda keyShift : sta lastShift
    ;; poll new key values
    PollKey -1,   keyShift
    PollKey -58,  keyUp
    PollKey -42,  keyDown
    ;; PollKey -74, keyReturn
    ;; compute key edges
    ldy #0 : { lda keyUp    : beq no : lda lastUp    : bne no : ldy #1 : .no } : sty edgeUp
    ldy #0 : { lda keyDown  : beq no : lda lastDown  : bne no : ldy #1 : .no } : sty edgeDown
    ldy #0 : { lda keyShift : beq no : lda lastShift : bne no : ldy #1 : .no } : sty edgeShift
    rts

;;;----------------------------------------------------------------------
;;; Main loop

.Speed : EQUB 0
.PositionL : EQUB 0
.PositionH : EQUB &33

.Flags : EQUB 0

;; ;; Unsigned 16bit Position, signed 8bit Speed
;; .v1_updatePositionWithSpeed: { .start:
;;     ldy #0 : lda Speed : { bpl no : ldy #&ff : .no } : sty pokeme+1
;;     clc : adc PositionL : sta PositionL
;;     lda PositionH : .pokeme : adc #7 : sta PositionH ; dummy 7 will be overwritten
;;     rts
;;     print "#v1=",*-start
;; }
;; .v2_updatePositionWithSpeed: { .start:
;;     lda Speed
;;     clc : adc PositionL : sta PositionL
;;     bcs carry
;; .nocarry:
;;     ;; positive Speed -> do nothing; negative Speed -> decrement high-byte
;;     lda Speed : bpl done
;;     dec PositionH
;;     rts
;; .carry:
;;     ;; positive Speed -> increment high-byte; negative Speed -> do nothing
;;     lda Speed : bmi done
;;     inc PositionH
;; .done:
;;     rts
;;     print "#v2=",*-start
;; }

.v3_updatePositionWithSpeed: { .start:
    lda Speed : clc : adc PositionL : sta PositionL
    php : pla : sta Flags
    rts
    print "#v3=",*-start
}

.main: {
    jsr initialize
.loop:
    jsr readKeys
    { lda edgeUp : beq no : inc Speed : .no }
    { lda edgeDown : beq no : dec Speed : .no }
    { lda edgeShift : beq no : jsr v3_updatePositionWithSpeed : .no } ;; TODO: make unconditional

    jsr mySync

    Position 1,1 : Puts "Up    : " : lda keyUp : jsr printHexA
    Position 1,2 : Puts "Down  : " : lda keyDown : jsr printHexA
    Position 1,3 : Puts "Shift : " : lda keyShift : jsr printHexA

    Position 1,5
    Puts "Speed    : " : lda Speed : jsr printHexA

    Position 1,7
    Puts "Position : " : lda PositionH : jsr printHexA
    Puts " " : lda PositionL : jsr printHexA

    Position 1,9
    Puts "Flags : " : lda Flags : jsr printHexA

    jmp loop
}

;;;----------------------------------------------------------------------
.end:
print "bytes left: ", screenStart-*
SAVE "Code", start, end
