;;; Play with Noise...

;;; MOS entry points
osasci = &ffe3
oswrch = &ffee

screenStart = &3000

;;; MOS vectors & zero page use
interruptSaveA = &fc
irq1v = &204

;;; Sheila
ula                         = &fe21
system_VIA_portB            = &fe40
;system_VIA_dataDirectionB   = &fe42
system_VIA_dataDirectionA   = &fe43
system_VIA_interruptFlags   = &fe4d
system_VIA_interruptEnable  = &fe4e
system_VIA_portA            = &fe4f

;;soundChipData = &fe41
soundChipData = system_VIA_portA

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macros

macro Emit C
    lda #C
    jsr emit
endmacro

macro Space
	lda #' '
    jsr emit
endmacro

macro Position X,Y
    lda #31 : jsr osasci
    lda #X : jsr osasci
    lda #Y : jsr osasci
endmacro

macro copy16i I,V
    lda #LO(I) : sta V
    lda #HI(I) : sta V+1
endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Zero Page

guard &100
org &70

.msgPtr SKIP 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Start

guard screenStart
org &1100

.start:
    jmp main

.spin: jmp spin

;;; increase the image size, so the switch-on beep has time to finish while loading
skip 1000

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Printing

.emit:
    jmp osasci

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

macro crash S
    copy16i msg, msgPtr
    jsr printString
    jmp spin
.msg:EQUS S, 0
endmacro

macro puts S
    copy16i msg, msgPtr
    jmp after
.msg: EQUS S, 0
.after:
    jsr printString
endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initialization

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IRQ, VBlank syncing

.vsyncNotify EQUB 0

.myIRQ: {
    lda system_VIA_interruptFlags : and #2 : bne vblank
    jsr spin ;;STOP "unexpected interrupt"
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
    { .loop : lda vsyncNotify : beq loop }
    lda #0 : sta vsyncNotify
    rts
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VIA - init/switch sound/keys

.enableOnlyVBlank:
    lda #%01111111 : sta system_VIA_interruptEnable ; disable all interrupts
    lda #%10000010 : sta system_VIA_interruptEnable ; enable just VBlank

    ;; start in keys mode
    lda #%00000011 : sta system_VIA_portB ; set bit 3 to 0
    lda #%01111111 : sta system_VIA_dataDirectionA ; (top bit input)
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keyboard input

.keyShift EQUB 0
.keyEnter EQUB 0
.keyEnterLAST EQUB 0
.keyShiftLAST EQUB 0

macro PollKey CODE, VAR
    lda #0 : sta VAR
    lda #(-CODE-1) : sta system_VIA_portA : lda system_VIA_portA
    bpl no : lda #1 : sta VAR : .no
endmacro

.readKeys:
    lda keyEnter : sta keyEnterLAST
    lda keyShift : sta keyShiftLAST
    PollKey -1,   keyShift
    PollKey -74,  keyEnter
    rts

.printKeyState:
    lda #'.' : { ldy keyShift: beq no : lda #'S' : .no } : jsr emit
    lda #'.' : { ldy keyEnter: beq no : lda #'E' : .no } : jsr emit
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; main

;;.frameCounter skip 1

.main: {
    jsr enableOnlyVBlank
    jsr mode1
    jsr cursorOff
    copy16i myIRQ, irq1v
    Position 1,1 : puts "Meteors tick-tock. Shift to speed up."
.loop
    ;;inc frameCounter
    Position 1,3
    ;;lda frameCounter : jsr printHexA
    Space : jsr printKeyState
    Space : jsr printC0state
    jsr readKeys
    jsr syncVB
    jsr doChannelZero
    jmp loop
    }

macro DEL20 : nop:nop:nop:nop:nop:nop:nop:nop:nop:nop : endmacro

.pulseWriteSound:
    ;; switch to sound mode
    lda #%11111111 : sta system_VIA_dataDirectionA ; (All bits output)
    lda #%00001011 : sta system_VIA_portB ; set bit 3 to 1
    ;; pulse sound write
    lda #%00000000 : sta system_VIA_portB
    DEL20
    lda #%00001000 : sta system_VIA_portB
    DEL20
    ;; back to keys mode
    lda #%00000011 : sta system_VIA_portB ; set bit 3 to 0
    lda #%01111111 : sta system_VIA_dataDirectionA ; (top bit input)
    rts

macro SEND
    sta soundChipData
    jsr pulseWriteSound
endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tick/tock on channel 0

.c0wait equb 50 ; initial pause of 50 (1 sec)
.c0playing equb 0 ; start off
.c0tockPhaseNext equb 0 ; start on tick
.c0period equb 23 ; initial period (between tick/tock)

.printC0state:
    lda c0wait : jsr printHexA
    Space : lda c0playing : jsr printHexA
    Space : lda c0tockPhaseNext : jsr printHexA
    Space : lda c0period : jsr printHexA
    rts

.doChannelZero:
    jsr c0speedUpOnShift
    lda c0playing : bne c0step
    jmp c0maybeStart

.c0maybeStart:
    dec c0wait
    beq c0start
    rts

.c0step:
    dec c0wait
    beq c0stop
    rts

.c0start:
    jsr c0sound
    lda #2 : sta c0wait
    inc c0playing
    rts

.c0stop:
    jsr c0quiet
    dec c0playing
    rts

.c0sound:
    lda c0tockPhaseNext
    bne c0tock
    jmp c0tick

.c0tick:
    lda #&89 : SEND
    lda #&3b : SEND
    lda #&90 : SEND
    inc c0tockPhaseNext
    rts

.c0tock:
    lda #&82 : SEND
    lda #&3f : SEND
    lda #&90 : SEND
    dec c0tockPhaseNext
    rts

.c0quiet:
    lda #&9f : SEND
    lda c0period : sta c0wait
    rts

.c0speedUpOnShift: {
    lda keyShiftLAST : bne done
    lda keyShift : beq done
    jsr c0decreasePeriod
.done:
    rts
    }

.c0decreasePeriod: {
    lda c0period : cmp #6 : beq no ; minimum period of 6
    dec c0period
.no:
    rts }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.end:
print "code size: ", *-start
print "bytes remaining: ", screenStart-*
SAVE "Code", start, end
