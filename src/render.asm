 
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

.bufO skip 1
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
    ;;IF SyncAssert : lda vsyncNotify : bne failSync1 : ENDIF ; pre-sync check (more harsh)
    { .loop : lda vsyncNotify : beq loop }
    ;;IF SyncAssert : cmp #2 : bcs failSync2 : ENDIF ; post-sync check (move forgiving)
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

;;; TODO: different sizes for different buffers
;;; for now, all have same size
bufferSize = 200

.b0: skip bufferSize
.b1: skip bufferSize
.b2: skip bufferSize
.buffers: EQUW b0, b1, b2 ; starting addresses
.buffersEnd:
numberOfBuffers = (buffersEnd-buffers)/2
PRINT numberOfBuffers
.offsets: skip numberOfBuffers ; TODO: do we need to track all these?

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
;;; TODO: when we have sep buffers per mask, then we will have multiple 'open' at once
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
;;; Calculate screen address from X/Y (ORIG, and faster!)

.calcA: ; theX/theY --> the{A,FX,CX,FY,CY}
    {
    lda theX+1
    lsr a
    sta smc_cx+1 : sta theCX
    lsr a : lsr a : lsr a : lsr a
    sta smc_hbOnRow+1

    lda theY+1
    lsr a : lsr a : lsr a
    sta theCY
    sta smc_cyA+1 ; TODO: dont bother with smc since I am maintaining zero-page vars
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
;;; info
    
;; .debugBufferInfo:
;;     Space : ldy #0 : lda offsets,y : jsr printHexA
;;     Space : ldy #1 : lda offsets,y : jsr printHexA
;;     Space : ldy #2 : lda offsets,y : jsr printHexA
;;     Space : ldy #3 : lda offsets,y : jsr printHexA
;;     Space : ldy #4 : lda offsets,y : jsr printHexA
;;     rts

;; .info:
;;     Ula green
;;     Position 1,1
;;     jsr printKeyState
;;     ;;Space : lda theX+1 : jsr printHexA : lda theX : jsr printHexA
;;     ;;Space : lda theY+1 : jsr printHexA : lda theY : jsr printHexA
;;     Space : lda theCX : jsr printHexA : Space : lda theFX : jsr printHexA
;;     Space : lda theCY : jsr printHexA : Space : lda theFY : jsr printHexA
;;     Space : lda theA+1 : jsr printHexA : lda theA : jsr printHexA
;;     Ula black
;;     rts
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; main/loop

;;; track positions of rocks

.rock1X skip 2
.rock1Y skip 2
.rock2X skip 2
.rock2Y skip 2
    
.initPositions:
    Copy16i &3000, rock1X
    Copy16i &6000, rock1Y
    Copy16i &5000, rock2X
    Copy16i &9000, rock2Y
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

.updatePositions:
    jsr edgeUp : { beq no : jsr rock1Up : .no }
    jsr edgeDown : { beq no : jsr rock1Down : .no }
    jsr edgeLeft : { beq no : jsr rock1Left : .no }
    jsr edgeRight : { beq no : jsr rock1Right : .no }
    jsr edgeUp : { beq no : jsr rock2Up : .no }
    jsr edgeDown : { beq no : jsr rock2Down : .no }
    jsr edgeLeft : { beq no : jsr rock2Left : .no }
    jsr edgeRight : { beq no : jsr rock2Right : .no }
    rts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Plot

.plotRock: ;X:buf#
    {
    Copy16i mediumRockOutline, SMC_outline+1
    jsr drawOutline
    rts
    }

.plotRock1:
    Copy16v rock1X, theX
    Copy16v rock1Y, theY
    jsr calcA
    jmp plotRock
    
.plotRock2:
    Copy16v rock2X, theX
    Copy16v rock2Y, theY
    jsr calcA
    jmp plotRock

.plotFirstHalf:
    Ula blue
    jsr plotRock1
    lda #0 : WriteB
    Ula black
    rts

.plotSecondHalf:
    Ula blue
    jsr plotRock2
    lda #0 : WriteB
    Ula black
    rts


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
    
B1 = 0
B2 = 1
B3 = 2

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; main/loop

.main: {
    jsr init
    jsr initPositions

.loop:

    jsr readKeys
    jsr updatePositions
    ldx #B1 : jsr openBstart
    jsr plotFirstHalf
    jsr syncVB
    jsr render12

    jsr readKeys
    jsr updatePositions
    ldx #B2 : jsr openBstart
    jsr plotSecondHalf
    jsr syncVB
    jsr render23

    jsr readKeys
    jsr updatePositions
    ldx #B3 : jsr openBstart
    jsr plotFirstHalf
    jsr syncVB
    jsr render31

    jsr readKeys
    jsr updatePositions
    ldx #B1 : jsr openBstart
    jsr plotSecondHalf
    jsr syncVB
    jsr render12

    jsr readKeys
    jsr updatePositions
    ldx #B2 : jsr openBstart
    jsr plotFirstHalf
    jsr syncVB
    jsr render23

    jsr readKeys
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
