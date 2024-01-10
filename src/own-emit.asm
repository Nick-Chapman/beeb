
Mine = TRUE

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
system_VIA_dataDirectionB   = &fe42
system_VIA_dataDirectionA   = &fe43
system_VIA_interruptFlags   = &fe4d
system_VIA_interruptEnable  = &fe4e
system_VIA_portA            = &fe4f

white = 0
cyan = 1
magenta = 2
blue = 3
yellow = 4
green = 5
red = 6
black = 7

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

macro Copy16i I,V
    lda #LO(I) : sta V
    lda #HI(I) : sta V+1
endmacro

macro Copy16v A,B
    lda A : sta B
    lda A+1 : sta B+1
endmacro

macro Add8 V
    lda V   : clc : adc #8 : sta V
    lda V+1 :       adc #0 : sta V+1
endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Zero Page

guard &100
org &70

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Start

guard screenStart
org &1100

.start:
    jmp main

.spin: jmp spin

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emit

.emitPos skip 2
.ownEmit:
    {
    ldy #0 : sty font+1 : sty font+2
    sec : sbc #32
    asl a : rol font+2
    asl a : rol font+2
    asl a : rol font+2
    sta font+1
    lda font+2 : clc : adc #&c0
    sta font+2
    Copy16v emitPos, pos1+1
    Add8 emitPos
    Copy16v emitPos, pos2+1
    Add8 emitPos
    ldy #0
.loop:
    .font : lda &eeee,y
    pha
    and #&f0
    ;{ sta smc+1 : lsr a : lsr a : lsr a : lsr a : .smc : ora #&ee }
    .pos1 : sta &eeee,y
    pla
    and #&f
    asl a : asl a : asl a : asl a
    ;{ sta smc+1 : asl a : asl a : asl a : asl a : .smc : ora #&ee }
    .pos2 : sta &eeee,y
    iny
    cpy #8 : beq done
    jmp loop
.done:
    rts
    }

.emit:
    IF Mine : ELSE : jmp osasci : ENDIF
    IF Mine : jmp ownEmit : ENDIF

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; printHexA

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
;;; main

.frameCounter skip 1

.main: {
    jsr initVia
    jsr mode1
    jsr cursorOff
    Copy16i myIRQ, irq1v
    lda #0
    sta frameCounter
.loop:
    jsr syncVB
    Copy16i screenStart+(16*(30*40+1)), emitPos
    Position 1,30
    lda frameCounter
    jsr printHexA
    Space
    lda frameCounter
    jsr printHexA
    Space
    lda frameCounter
    jsr printHexA
    Space

    lda #blue : sta ula
    lda frameCounter
    jsr printHexA
    lda #black : sta ula

    Space

    lda #green : sta ula
    lda frameCounter
    jsr printHexA
    lda #black : sta ula

    Space
    Emit 'H'
    Emit 'e'
    Emit 'y'
    Emit '!'
    inc frameCounter
    jmp loop
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
    ;;IF SyncAssert : lda vsyncNotify : bne failSync1 : ENDIF ; pre-sync check (more harsh)
    { .loop : lda vsyncNotify : beq loop }
    ;;IF SyncAssert : cmp #2 : bcs failSync2 : ENDIF ; post-sync check (move forgiving)
    lda #0 : sta vsyncNotify
    rts
;; .failSync1:
;;     STOP "Too slow to sync(1)"
;; .failSync2:
;;     STOP "Too slow to sync(2)"
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.end:
print "code size: ", *-start
print "bytes remaining: ", screenStart-*
SAVE "Code", start, end
