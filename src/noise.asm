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

soundChipWrite = system_VIA_portB
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
    rts

.doKeys:
    ;; poll keyboard via system VIA portA
    ;; bottom 7 bits output (key to poll); top bit input (is it pressed?)
    lda #%01111111 : sta system_VIA_dataDirectionA
    lda #%00000011 : sta system_VIA_portB ; set bit 3 to 0
    rts

.doSound:
    lda #%11111111 : sta system_VIA_dataDirectionA ; (ALL bit output)
    lda #%00001011 : sta system_VIA_portB ; set bit 3 to 1
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keyboard input

.keyShift EQUB 0
.keyEnter EQUB 0
.keyEnterLAST EQUB 0

macro PollKey CODE, VAR
    lda #0 : sta VAR
    lda #(-CODE-1) : sta system_VIA_portA : lda system_VIA_portA
    bpl no : lda #1 : sta VAR : .no
endmacro

.readKeys:
    lda keyEnter : sta keyEnterLAST
    jsr doKeys
    PollKey -1,   keyShift
    PollKey -74,  keyEnter
    rts

.printKeyState:
    lda #'.' : { ldy keyShift: beq no : lda #'S' : .no } : jsr emit
    lda #'.' : { ldy keyEnter: beq no : lda #'E' : .no } : jsr emit
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; main

.frameCounter skip 1

.main: {
    jsr enableOnlyVBlank
    jsr mode1
    jsr cursorOff
    copy16i myIRQ, irq1v
    Position 1,1 : puts "toggle volume with enter key"
.loop
    inc frameCounter ; used for note
    Position 1,3
    lda frameCounter : jsr printHexA
    Space : jsr printKeyState
    jsr readKeys
    jsr adjustVolumeMaybe
    jsr syncVB
    jsr sound
    jmp loop
    }

macro DEL20 : nop:nop:nop:nop:nop:nop:nop:nop:nop:nop : endmacro

macro SEND :
    lda #0 : sta soundChipWrite
    DEL20
    lda #8 : sta soundChipWrite
    DEL20
endmacro

.sound:
    jsr doSound

    ;; tone on channel 1 (latch)
    lda #%10100000
    sta soundChipData
    SEND

    ;; tone (extra data)
    lda frameCounter
    lsr a : lsr a

    sta soundChipData
    SEND

    rts


.volumeToggle equb 0

.adjustVolumeMaybe: {
    lda keyEnterLAST : bne done
    lda keyEnter : beq done
    ;; we got an edge on enter, so...
    lda volumeToggle
    eor #1
    sta volumeToggle
    bne loud ; switch to loud
    jmp silent ; switch to silent
.done:
    rts
    }

.loud:
    jsr doSound
    ;; max volume on channel 1
    lda #%10110000 : sta soundChipData
    SEND
    rts

.silent:
    jsr doSound
    ;; minimum volume on channel 1
    lda #%10111111 : sta soundChipData
    SEND
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.end:
print "code size: ", *-start
print "bytes remaining: ", screenStart-*
SAVE "Code", start, end
