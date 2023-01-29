oswrch = &ffee
osbyte = &fff4

ORG 0

.mypos SKIP 1
.lastpos SKIP 1
.xdown SKIP 1

;.vsync          SKIP 1
.angle          SKIP 2
.speed          SKIP 2

;;; Hacking demo to figure out what is needed or keyboard scanning!

ORG &1900

.start
    jmp go

; something in here is needed to make the keyscanning work
.irq
    LDA &FE4D
    AND #2
    BNE irqvsync
.irqtimer
    LDA #&40
    STA &FE4D
    ;INC vsync
    LDA &FC
    RTI
.irqvsync
    STA &FE4D
    ;; LDA #LO(timerlength):STA &FE44
    ;; LDA #HI(timerlength):STA &FE45
    LDA &FC
    RTI

.go:
    \\ Set up hardware state and interrupts

    ;SEI
    ;LDX #&FF:TXS                ; reset stack
    ;STX &FE44:STX &FE45
    LDA #&7F:STA &FE4E          ; disable all interrupts (KEEP)
    ;STA &FE43                   ; set keyboard data direction

    ;LDA #&C2:STA &FE4E          ; enable VSync and timer interrupt
    ;LDA #&0F:STA &FE42          ; set addressable latch for writing

    LDA #3:STA &FE40            ; keyboard write enable (KEEP)
    ;LDA #0:STA &FE4B            ; timer 1 one shot mode

    LDA #LO(irq):STA &204
    LDA #HI(irq):STA &205       ; set interrupt handler

    ;mode 1
    lda #22 : jsr oswrch
    lda #1 : jsr oswrch


    LDA #0:STA angle:STA angle+1
    ;STA vsync
    STA speed
    LDA #1:STA speed+1

    \\ Enable interrupts, ready to start the main loop

    ;CLI

    lda #0: sta mypos : sta lastpos

.mainloop

    ;; show mypos and xdown on screen...
    ldx lastpos
    lda #&00 : sta &4000,x
    ldx mypos
    lda #&ff : sta &4000,x
    lda xdown:beq no:lda #&00:sta &4400:lda #&ff:sta &4407:jmp join:.no:lda #&ff:sta &4400:lda #&0:sta &4407:.join


    ;lda #19 : jsr osbyte ; normal vsync
    jsr pause
 ;; .waitingforvsync
 ;;     LDA vsync:BEQ waitingforvsync
 ;;     LDA #0:STA vsync

    ; set angle from speed
    CLC:LDA angle:ADC speed:STA angle
    LDA angle+1:ADC speed+1:STA angle+1

    ; Check keypresses; update speed
    LDA #0 : sta xdown
    LDA #66:STA &FE4F:LDA &FE4F:BPL notx
    inc xdown : CLC:LDA speed:ADC #16:STA speed:BCC notx:INC speed+1:.notx
    LDA #97:STA &FE4F:LDA &FE4F:BPL notz
    SEC:LDA speed:SBC #16:STA speed:BCS notz:DEC speed+1:.notz

    ; set mypos from angle hi
    lda mypos : sta lastpos
    lda angle+1 : sta mypos

    JMP mainloop


.pause:
    pha : txa : pha : tya : pha
    ldx #30
.loop_x:
    ldy #255
.loop_y:
    dey
    bne loop_y
    dex
    bne loop_x
    pla : tay : pla : tax : pla
    rts

.end

SAVE "Code", start, end
