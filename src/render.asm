
;;; Rework plot and render code to use multiple buffers.

SyncAssert = TRUE

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

white = 0
cyan = 1
magenta = 2
blue = 3
yellow = 4
green = 5
red = 6
black = 7

macro Ula col : lda #col : sta ula : endmacro
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copy

macro Copy16i I,V
    lda #LO(I) : sta V
    lda #HI(I) : sta V+1
endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; zero page

org &70
.msgPtr skip 2

.bufO skip 1
.bufP skip 2

.theX skip 2 ; plot form Hi byte + hi-bit of LO byte
.theY skip 2 ; plot only from HI byte
.theA skip 2
.theFineXmask skip 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Start

guard screenStart
org &1100

.start:
    jmp main

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
    Emit '.'
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
;;; IRQ, VBlank syncing

.vsyncNotify EQUB 0

.myIRQ: {
    lda system_VIA_interruptFlags : and #2 : bne vblank
    Crash "unexpected interrupt"
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
    IF SyncAssert : lda vsyncNotify : bne failSync1 : ENDIF ; pre-sync check (more harsh)
    { .loop : lda vsyncNotify : beq loop }
    IF SyncAssert : cmp #2 : bcs failSync2 : ENDIF ; post-sync check (move forgiving)
    lda #0 : sta vsyncNotify
    rts
.failSync1:
    Crash "Too slow to sync(1)"
.failSync2:
    Crash "Too slow to sync(2)"
    }

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
;;; Buffers

;;; TODO: different sizes for different buffers
;;; for now, all have same size
bufferSize = 44*3+1

.b0: skip bufferSize
.b1: skip bufferSize
.b2: skip bufferSize
.b3: skip bufferSize
.b4: skip bufferSize
.buffers: EQUW b0, b1, b2, b3, b4 ; starting addresses
.buffersEnd:
numberOfBuffers = (buffersEnd-buffers)/2
PRINT numberOfBuffers
.offsets: skip numberOfBuffers ; TODO: do we need to track all these?

.debugBufferInfo:
    Space : ldy #0 : lda offsets,y : jsr printHexA
    Space : ldy #1 : lda offsets,y : jsr printHexA
    Space : ldy #2 : lda offsets,y : jsr printHexA
    Space : ldy #3 : lda offsets,y : jsr printHexA
    Space : ldy #4 : lda offsets,y : jsr printHexA
    rts

.selectBuffer: ; X:buf# --> bufP (use A,Y)
    txa : asl a : tay
    lda buffers,   y : sta bufP
    lda buffers+1, y : sta bufP+1
    rts

.openBstart: ; X:buf# --> bufP/bufO (use A)
    jsr selectBuffer
    ;;lda offsets, x
    lda #0
    sta bufO
    rts

;;; TODO: do we need to push bufO back into offsets???
;;; Once we are done filling a buffer, we process from the start anyway!
;;; only reason is to see how many writes we made
.closeB: ; X:buf#, bufO (use A)
    lda bufO : sta offsets,x
    rts

.overflow:
    lda bufO : jsr printHexA
    Crash "-overflow"

macro WriteB ; bufO/bufP, A:data (use y)
    ;Emit 'w'
    ;Space : pha : jsr printHexA : pla
    ldy bufO
    inc bufO
    cpy #bufferSize : { bne ok : jmp overflow : .ok } ;; TODO: dev only
    sta (bufP),y
endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Calculate screen address from X/Y

.calcA: ; theX/theY --> theA
    {
    lda theX+1
    lsr a
    sta smc_cx+1
    lsr a : lsr a : lsr a : lsr a
    sta smc_hbOnRow+1

    lda theY+1
    lsr a : lsr a : lsr a
    sta smc_cyA+1
    sta smc_cyB+1
    asl a : asl a : clc
    .smc_cyA : adc #&ee
    .smc_hbOnRow : adc #&ee
    lsr a
    clc : adc #HI(screenStart)
    sta theA+1

    .smc_cyB : lda #&ee : and #1    ; oddRow
    asl a : asl a : asl a : asl a   ; Xoffset
    .smc_cx : eor #&ee              ; Xmod
    asl a : asl a : asl a           ; Xmod*8
    sta smc_alo+1

    lda theY+1
    and #&7
    clc : .smc_alo : adc #&ee       ; adjust A with fineY
    sta theA

    lda theX ; (only need hi-order bit; rotate into carry)
    asl a
    lda theX+1
    rol a : and #&3
    tay
    lda masks,y
    sta theFineXmask

    rts

.masks:
    EQUB &88, &44, &22, &11
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mod160 -- TODO: do better!

.mod160: {
    m = 160
    x = 9
    clc : adc #x
    { cmp #(m+x) : bcc no : sbc #m : .no } ; wrap
    { cmp #x : bcs no : adc #m : .no } ; unwrap
    sec : sbc #x
.done:
    rts
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; outlines

N = 1
S = 2
E = 4
W = 8
INVISIBLE = 16
START = 32

NE = N or E
SE = S or E
NW = N or W
SW = S or W

.smallRockOutline: ; 22 (&16) points
    equb START,E,SE,NE,E,SE,S, SE,S,SW,S,SW, W,NW,SW,W, NW,NE,NW,NW,N,NE, 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; draw

.invisible equb INVISIBLE
.asNorth equb N
.asSouth equb S
.asEast equb E
.asWest equb W

.theN skip 1 ; TODO zero page

.rts:
    rts

.drawShape: { ; into open buffer; (starting)theX/theY -- get modified (use theN)
    ldx #0
.loop:
    ;Emit 'D'
    .*SMC_outline : lda &7777, x : beq rts
    inx
    stx theN

    bit asNorth : bne north
    bit asSouth : bne south
    jmp ew
.north:
    ;Emit 'N'
    dec theY+1
    jmp ew
.south:
    ;Emit 'S'
    inc theY+1
    ;;jmp ew
.ew:
    pha
    bit asEast : bne east
    bit asWest : bne west
    jmp pr
.east:
    ;Emit 'E'
    lda theX
    clc : adc #128
    sta theX
    bcc pr
    lda theX+1
    clc : adc #1 : jsr mod160
    sta theX+1
    jmp pr
.west:
    ;Emit 'W'
    lda theX
    sec : sbc #128
    sta theX
    bcs pr
    lda theX+1
    sec : sbc #1 : jsr mod160
    sta theX+1
    ;;jmp pr
.pr:
    pla
    bit invisible : bne next
    jsr calcA ;; TODO be incremental
    lda theA+1 : WriteB ; A-hi first
    lda theA : WriteB
    lda theFineXmask : WriteB
.next:
    ldx theN
    jmp loop
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Plot

.plotRock: ;X:buf#
    {
    Copy16i smallRockOutline, SMC_outline+1
    jsr drawShape
    rts
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Render

.renderWithEor: ; X:buf#
    {
    jsr selectBuffer
    ldy #0 ; read from start
.loop:
    ;Emit 'R'
    lda (bufP),y : beq done : iny : sta r+2 : sta w+2
    lda (bufP),y            : iny : sta r+1 : sta w+1
    lda (bufP),y            : iny
    .r eor &7777 ;; TODO: use y-indexed; avoid SMC ?
    .w sta &7777
    jmp loop
.done:
    rts
    }


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; main/loop

;;.frameCounter skip 1

.info:
    ;;Position 1,30 : lda frameCounter : jsr printHexA
    rts

;;; track positions of rocks with a single number
.rock1 skip 1
.rock2 skip 1
.rock3 skip 1
.rock4 skip 1

.initPositions:
    lda #10 : sta rock1
    lda #10 : sta rock2
    rts
    
.updatePositions:
    inc rock1
    inc rock2
    inc rock3 : inc rock3
    dec rock4
    rts

B1 = 1
B2 = 2
B3 = 3

.plotRock1: ; move vertical
    lda #0 : sta theX
    lda #50 : sta theX+1
    lda rock1 : sta theY+1
    jmp plotRock

.plotRock2: ; move horizontal
    lda #0 : sta theX
    lda rock2 : sta theX+1
    lda #50 : sta theY+1
    jmp plotRock

.plotRock3: ; move vertical
    lda #0 : sta theX
    lda #100 : sta theX+1
    lda rock3 : sta theY+1
    jmp plotRock
	
.plotRock4: ; move horizontal
    lda #0 : sta theX
    lda rock4 : sta theX+1
    lda #100 : sta theY+1
    jmp plotRock


.plotFirstHalf:
    jsr plotRock1
    jsr plotRock2
    lda #0 : WriteB
    rts
	
.plotSecondHalf:
    jsr plotRock3
    jsr plotRock4
    lda #0 : WriteB
    rts
    
.render12:
    Ula magenta
    ldx #B1
    jsr renderWithEor
    ldx #B2
    jsr renderWithEor
    Ula black
    rts

.render23:
    Ula magenta
    ldx #B2
    jsr renderWithEor
    ldx #B3
    jsr renderWithEor
    Ula black
    rts

.render31:
    Ula magenta
    ldx #B3
    jsr renderWithEor
    ldx #B1
    jsr renderWithEor
    Ula black
    rts
    
.main: {
    jsr init
    jsr initPositions


.loop:
    jsr updatePositions
    ldx #B1 : jsr openBstart
    jsr plotFirstHalf
    jsr syncVB
    jsr render12
    
    jsr updatePositions
    ldx #B2 : jsr openBstart
    jsr plotSecondHalf
    jsr syncVB
    jsr render23

    jsr updatePositions
    ldx #B3 : jsr openBstart
    jsr plotFirstHalf
    jsr syncVB
    jsr render31
	
    jsr updatePositions
    ldx #B1 : jsr openBstart
    jsr plotSecondHalf
    jsr syncVB
    jsr render12
	
    jsr updatePositions
    ldx #B2 : jsr openBstart
    jsr plotFirstHalf
    jsr syncVB
    jsr render23

    jsr updatePositions
    ldx #B3 : jsr openBstart
    jsr plotSecondHalf
    jsr syncVB
    jsr render31

    jmp loop
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.end:
print "bytes remaining: ", screenStart-*
SAVE "Code", start, end
