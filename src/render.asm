
;;; Rework plot and render code to use multiple buffers.

SyncAssert = TRUE

screenStart = &3000
screenEnd = &8000

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
;;macro Ula col : endmacro

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
;;; zero page

org &70
.msgPtr skip 2

.screenP skip 2

.bufP skip 2

.theX skip 2 ; plot from Hi byte + hi-bit of LO byte
.theY skip 2 ; plot only from HI byte

.theCX SKIP 1 ; coarse-X : 0..79
.theCY SKIP 1 ; coarse-Y : 0..31
.theFX SKIP 1 ; fine-X   : 0..3
.theFY SKIP 1 ; fine-Y   : 0..7
.theA SKIP 2  ; screen address of the 8x8 char

.outlineIndex skip 1
.outlineMotion skip 1

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
;;; Keyboard input

.keyUp    EQUB 0
.keyDown  EQUB 0
.keyLeft  EQUB 0
.keyRight EQUB 0

.keyUpLAST    EQUB 0
.keyDownLAST  EQUB 0
.keyLeftLAST  EQUB 0
.keyRightLAST EQUB 0

macro PollKey CODE, VAR
    lda #0 : sta VAR
    lda #(-CODE-1) : sta system_VIA_portA : lda system_VIA_portA
    bpl no : lda #1 : sta VAR : .no
endmacro

.readKeys:
    ;lda keyLeft : sta keyLeftLAST
    ;lda keyRight : sta keyRightLAST
    ;lda keyUp : sta keyUpLAST
    ;lda keyDown : sta keyDownLAST
    PollKey -58,  keyUp
    PollKey -42,  keyDown
    PollKey -26,  keyLeft
    PollKey -122, keyRight
    rts

.printKeyState:
    ;Space
    lda #'.' : { ldy keyLeft : beq no : lda #'L' : .no } : jsr emit
    lda #'.' : { ldy keyRight: beq no : lda #'R' : .no } : jsr emit
    lda #'.' : { ldy keyUp   : beq no : lda #'U' : .no } : jsr emit
    lda #'.' : { ldy keyDown : beq no : lda #'D' : .no } : jsr emit
    rts

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

    ;; start in keys mode
    lda #%00000011 : sta system_VIA_portB ; set bit 3 to 0
    lda #%01111111 : sta system_VIA_dataDirectionA ; (top bit input)

    jsr mode1
    jsr cursorOff
    jsr replaceWhiteWithCyan
    Copy16i myIRQ, irq1v
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buffers

bufferSize = 255
.b0: skip bufferSize
.b1: skip bufferSize
.b2: skip bufferSize
.buffers: EQUW b0, b1, b2
.buffersEnd:
numberOfBuffers = (buffersEnd-buffers)/2
.offsets: skip numberOfBuffers

.overflow:
    Crash "overflow"

macro WriteB ; bufP, X:buf#, A:data (uses y)
    ldy offsets,x
    inc offsets,x
    cpy #bufferSize : { bne ok : jmp overflow : .ok } ;; TODO: dev only
    sta (bufP),y
endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Calculate screen address from X/Y (ORIG, and faster!)

.calcA: ; theX/theY --> the{A,FX,CX,FY,CY}
    {
    lda theX+1
    lsr a
    sta theCX
    lsr a : lsr a : lsr a : lsr a
    sta smc_hbOnRow+1

    lda theY+1
    lsr a : lsr a : lsr a
    sta theCY
    asl a : asl a : clc
    adc theCY
    .smc_hbOnRow : adc #&ee
    lsr a
    clc : adc #HI(screenStart)
    sta theA+1

    lda theCY : and #1              ; oddRow
    asl a : asl a : asl a : asl a   ; Xoffset
    eor theCX                       ; Xmod
    asl a : asl a : asl a           ; Xmod*8
    sta smc_alo+1

    lda theY+1
    and #&7 : sta theFY
    clc : .smc_alo : adc #&ee       ; adjust A with fineY
    sta theA

    lda theX ; (only need hi-order bit; rotate into carry)
    asl a
    lda theX+1
    rol a : and #&3 : sta theFX

    rts
    }

.masks:
    EQUB &88, &44, &22, &11


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; incremental movement -- copied from game3

.left1: {
    lda theFX : bne no
    jsr left4
    lda #4 : sta theFX
.no:
    dec theFX
    rts }

.left4:
    lda theCX
    ; unwrap line
    { bne no : lda #80 : sta theCX : jsr downA8 : .no }
    dec theCX
    lda theA : sec : sbc #8 : sta theA
    { bcs no : dec theA+1 : .no }
    rts

.right1: {
    inc theFX
    lda theFX : cmp #4 : bne no
    lda #0 : sta theFX
    jsr right4
.no:
    rts }

.right4:
    inc theCX
    lda theA : clc : adc #8 : sta theA
    { bcc no : inc theA+1 : .no }
    lda theCX : cmp #80
    ; wrap line
    { bne no : lda #0 : sta theCX : jsr upA8 : .no }
    rts

.upA8: ; A -- TODO:inline?
    lda theA : sec : sbc #&80 : sta theA
    lda theA+1     : sbc #2   : sta theA+1
    rts

.downA8: ; A -- TODO:inline?
    lda theA : clc : adc #&80 : sta theA
    lda theA+1     : adc #2   : sta theA+1
    rts

.up1: { ; FY,CY,A
    lda theFY : bne no
    lda #7 : sta theFY
    lda theA : clc : adc #7 : sta theA
    jmp up8
.no:
    dec theA
    dec theFY
    rts }

.up8: ; CY,A
    jsr unwrapScreen
    dec theCY
    jmp upA8

.unwrapScreen: { ; CY,A
    lda theCY
    bne no
    lda #32 : sta theCY
    lda theA+1 : clc : adc #HI(screenEnd-screenStart) : sta theA+1
.no:
    rts }

.down1: { ; FY,CY,A
    inc theA
    inc theFY
    lda theFY : cmp #8 : beq cont
    rts
.cont:
    lda #0 : sta theFY
    lda theA : sec : sbc #8 : sta theA
    jmp down8 }

.down8: ; CY,A
    inc theCY
    jsr downA8
    jmp wrapScreen

.wrapScreen: { ; CY,A
    lda theCY
    cmp #32 : bne no
    lda #0 : sta theCY
    lda theA+1 : sec : sbc #HI(screenEnd-screenStart) : sta theA+1
.no:
    rts }

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

.smallRockOutline:
    equb START,E,SE,NE,E,SE,S, SE,S,SW,S,SW, W,NW,SW,W, NW,NE,NW,NW,N,NE, 0

.mediumRockOutline:
    equb START, E,E,NE,NE,E,E,E,E,SE,SE,E,SE,SE, SW,SW,SW,SE,SE,SE,S,S,SW,SW,SW,W,W
    equb NW,NW,SW,SW,W,W,W, NW,NW,NW,NE,NE,NW,NW,NW,N,N,NE,NE, 0


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; draw

.invisible equb INVISIBLE
.asNorth equb N
.asSouth equb S
.asEast equb E
.asWest equb W


.rts:
    rts

.drawOutline: { ; into open buffer; (starting)theX/theY -- get modified (use outlineIndex)
    jsr calcA
    ldx #0
.loop:
    ;cpx #1 : beq rts ;; DEV HACK - just see one point
    .*SMC_outline : lda &7777, x : beq rts
    inx
    stx outlineIndex
    sta outlineMotion
    bit asNorth : bne north
    bit asSouth : bne south
    jmp ew
.north:
    jsr up1 ; TODO inline
    jmp ew
.south:
    jsr down1 ; TODO inline
.ew:
    lda outlineMotion
    bit asEast : bne east
    bit asWest : bne west
    jmp pr
.east:
    jsr right1 ; TODO: inline
    jmp pr
.west:
    jsr left1 ; TODO: inline
.pr:
    lda outlineMotion
    bit invisible : bne next

    .*SMC_drawBuffer : ldx #&77 ; BUFFER

    lda theA+1 : WriteB ; A-hi first
    lda theA : WriteB

    ldy theFX
    lda masks,y
    WriteB  ;; TODO: pick buffer based on mask!!!
.next:
    ldx outlineIndex
    jmp loop
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; control

.nope: lda #0 : rts
.edgeLeft: lda keyLeftLAST : bne nope : lda keyLeft : rts
.edgeRight: lda keyRightLAST : bne nope : lda keyRight : rts
.edgeUp: lda keyUpLAST : bne nope : lda keyUp : rts
.edgeDown: lda keyDownLAST : bne nope : lda keyDown : rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; main/loop

;;; track positions of rocks

.rock1X skip 2
.rock1Y skip 2
.rock2X skip 2
.rock2Y skip 2
.rock3X skip 2
.rock3Y skip 2
.rock4X skip 2
.rock4Y skip 2

.initPositions:
    Copy16i &3000, rock1X
    Copy16i &6000, rock1Y
    Copy16i &5000, rock2X
    Copy16i &9000, rock2Y
    Copy16i &2000, rock3X
    Copy16i &B000, rock3Y
    Copy16i &4000, rock4X
    Copy16i &D000, rock4Y
    rts

.moveLeft:
    lda theX   : sec : sbc #&80                                          : sta theX
    lda theX+1 :       sbc   #0 : { cmp #&ff : bne no : lda #159 : .no } : sta theX+1
    rts

.moveRight:
    lda theX   : clc : adc #&80                                        : sta theX
    lda theX+1 :       adc   #0 : { cmp #160 : bne no : lda #0 : .no } : sta theX+1
    rts

.rock1Up: dec rock1Y+1 : rts
.rock1Down: inc rock1Y+1 : rts

.rock1Left:
    Copy16v rock1X, theX
    jsr moveLeft
    Copy16v theX, rock1X
    rts

.rock1Right:
    Copy16v rock1X, theX
    jsr moveRight
    Copy16v theX, rock1X
    rts

.rock2Up: dec rock2Y+1 : rts
.rock2Down: inc rock2Y+1 : rts

.rock2Left:
    Copy16v rock2X, theX
    jsr moveLeft
    Copy16v theX, rock2X
    rts

.rock2Right:
    Copy16v rock2X, theX
    jsr moveRight
    Copy16v theX, rock2X
    rts

.rock3Up: dec rock3Y+1 : rts
.rock3Down: inc rock3Y+1 : rts

.rock3Left:
    Copy16v rock3X, theX
    jsr moveLeft
    Copy16v theX, rock3X
    rts

.rock3Right:
    Copy16v rock3X, theX
    jsr moveRight
    Copy16v theX, rock3X
    rts

.rock4Up: dec rock4Y+1 : rts
.rock4Down: inc rock4Y+1 : rts

.rock4Left:
    Copy16v rock4X, theX
    jsr moveLeft
    Copy16v theX, rock4X
    rts

.rock4Right:
    Copy16v rock4X, theX
    jsr moveRight
    Copy16v theX, rock4X
    rts

.updatePositions:
    jsr edgeUp : { beq no : jsr rock1Up : .no }
    jsr edgeDown : { beq no : jsr rock1Down : .no }
    jsr edgeLeft : { beq no : jsr rock1Left : .no }
    jsr edgeRight : { beq no : jsr rock1Right : .no }
    jsr edgeUp : { beq no : jsr rock2Up : .no }
    jsr edgeDown : { beq no : jsr rock2Down : .no }
    jsr edgeLeft : { beq no : jsr rock2Left : .no }
    jsr edgeRight : { beq no : jsr rock2Right : .no }
    jsr edgeUp : { beq no : jsr rock3Up : .no }
    jsr edgeDown : { beq no : jsr rock3Down : .no }
    jsr edgeLeft : { beq no : jsr rock3Left : .no }
    jsr edgeRight : { beq no : jsr rock3Right : .no }
    jsr edgeUp : { beq no : jsr rock4Up : .no }
    jsr edgeDown : { beq no : jsr rock4Down : .no }
    jsr edgeLeft : { beq no : jsr rock4Left : .no }
    jsr edgeRight : { beq no : jsr rock4Right : .no }
    rts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Plot

.plotRockM:
    Copy16i mediumRockOutline, SMC_outline+1
    jmp drawOutline

.plotRockS:
    Copy16i smallRockOutline, SMC_outline+1
    jmp drawOutline

.plotRock1:
    Copy16v rock1X, theX
    Copy16v rock1Y, theY
    jmp plotRockS

.plotRock2:
    Copy16v rock2X, theX
    Copy16v rock2Y, theY
    jmp plotRockS

.plotRock3:
    Copy16v rock3X, theX
    Copy16v rock3Y, theY
    jmp plotRockM

.plotRock4:
    Copy16v rock4X, theX
    Copy16v rock4Y, theY
    jmp plotRockM

.plotFirstHalf:
    jsr plotRock1
    jsr plotRock3
    Ula black
    rts

.plotSecondHalf:
    jsr plotRock2
    jsr plotRock4
    rts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Render

.renderWithEor: { ; X:buf#

    txa : asl a : tay
    lda buffers,   y : sta smc0+1 : sta smc1+1 : sta smc2+1 : sta smc3+1
    lda buffers+1, y : sta smc0+2 : sta smc1+2 : sta smc2+2 : sta smc3+2

    ;; Close the buffer (add the terminating null at offset)
    ldy offsets,x
    lda #0
    .smc0 : sta &7777,y

    ldx #0 ; buffer index
    ldy #0
.loop:
    .smc1 : lda &7777,x : beq done : inx : sta screenP+1
    .smc2 : lda &7777,x            : inx : sta screenP
    .smc3 : lda &7777,x            : inx
    eor (screenP),y
    sta (screenP),y
    jmp loop
.done:
    rts
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; main/loop

.frameNumber skip 1

.semiPlot: {
    lda frameNumber : and #1 : bne odd
    jmp plotFirstHalf
.odd:
    jmp plotSecondHalf
    }

.selectBuf: ;X:buf#
    ;; reset the buffer offset to the start of the buffer
    lda #0
    sta offsets,x
    ;; save X so draw routine can access the offset
    stx SMC_drawBuffer+1
    ;; open the buffer  for writing
    txa : asl a : tay
    lda buffers,   y : sta bufP
    lda buffers+1, y : sta bufP+1
    rts

.phase equb 2 ; cycles 2,1,0

.nextPhase: {
    dec phase
    bpl done
    lda #2 : sta phase
.done:
    rts
    }

.selectPlotBufferForPhase:
    ldx phase
    jmp selectBuf

.renderForPhase:
    ;; render most recent buffer plotted in. & one from 2 steps ago
    { lda phase : cmp #2 : beq no : ldx #0 : jsr renderWithEor : .no }
    { lda phase : cmp #0 : beq no : ldx #1 : jsr renderWithEor : .no }
    { lda phase : cmp #1 : beq no : ldx #2 : jsr renderWithEor : .no }
    rts

.main: {
    jsr init
    jsr initPositions
.loop:

    inc frameNumber
    jsr nextPhase
    jsr readKeys
    jsr updatePositions
    jsr selectPlotBufferForPhase

    Ula blue
    jsr semiPlot
    Ula black

    jsr syncVB

    Ula magenta
    jsr renderForPhase
    Ula black

    jmp loop
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.end:
print "bytes remaining: ", screenStart-*
SAVE "Code", start, end
