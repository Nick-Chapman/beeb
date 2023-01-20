
oswrch = &FFEE
;osasci = &ffe3
osrdch = &fff0
osbyte = &fff4

org &70

.var SKIP 1

org &1900

;ORG &1100

.start:
    lda #0 : sta var


    SEI
    LDX #&FF:TXS                ; reset stack
    STX &FE44:STX &FE45
    LDA #&7F
    STA &FE4E          ; disable all interrupts
    STA &FE43                   ; set keyboard data direction
    LDA #&C2:STA &FE4E          ; enable VSync and timer interrupt
    LDA #&0F:STA &FE42          ; set addressable latch for writing
    LDA #3:STA &FE40            ; keyboard write enable
    LDA #0:STA &FE4B            ; timer 1 one shot mode
    ;LDA #LO(irq):STA &204
    ;LDA #HI(irq):STA &205       ; set interrupt handler

    CLI
.L:
    jsr vsync
    lda var : jsr printByte
    lda #' ' : jsr oswrch

    LDA #66:STA &FE4F:LDA &FE4F:BPL notx ; 66 = 0100 0010, col 2, row 4
    INC var:.notx
    LDA #97:STA &FE4F:LDA &FE4F:BPL notz ; 97 = 0110 0001, col 1, row 6
    DEC var:.notz
    LDA #112:STA &FE4F:LDA &FE4F:BMI exit ; 112 = 0111 0000, col 0, row 7

    jmp L
    rts

.exit:
    rts
    ;JMP (&FFFC)

.printByte:
    pha
    lsr a : lsr a : lsr a : lsr a
    jsr printNibble
    pla
    and #15
    jsr printNibble
    rts

.printNibble:
    tax
    lda nibChars,x : jsr oswrch
    rts

.nibChars:
    EQUS "0123456789abcdef"

.vsync: 
    ;pha : txa : pha : tya : pha
    lda #19 : jsr osbyte
    ;pla : tay : pla : tax : pla
    rts

.end:

SAVE "Code", start, end
