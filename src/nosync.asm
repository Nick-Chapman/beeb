
;;; Experiment with no VB syncing...
;;; i.e. dont split drawing into plot/render phases
;;; no need for buffers
;;; just blit the screen whenever we choose; screen artifacts be dammed

;;; IDEA: Perhaps we should only loose syncing for screen drawing!
;;; But continue to sync when updaing positions to control game-object speed
;;; ???
    
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

;;macro Ula col : lda #col : sta ula : endmacro
macro Ula col : endmacro

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

.drawOutline: {
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

    ldy theFX
    lda masks,y
    ldy #0
    eor (theA),y
    sta (theA),y

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

.updatePositions1:
    jsr edgeUp : { beq no : jsr rock1Up : .no }
    jsr edgeDown : { beq no : jsr rock1Down : .no }
    jsr edgeLeft : { beq no : jsr rock1Left : .no }
    jsr edgeRight : { beq no : jsr rock1Right : .no }
    rts

.updatePositions2:
    jsr edgeUp : { beq no : jsr rock2Up : .no }
    jsr edgeDown : { beq no : jsr rock2Down : .no }
    jsr edgeLeft : { beq no : jsr rock2Left : .no }
    jsr edgeRight : { beq no : jsr rock2Right : .no }
    rts

.updatePositions3:
    jsr edgeUp : { beq no : jsr rock3Up : .no }
    jsr edgeDown : { beq no : jsr rock3Down : .no }
    jsr edgeLeft : { beq no : jsr rock3Left : .no }
    jsr edgeRight : { beq no : jsr rock3Right : .no }
	rts

.updatePositions4:
    jsr edgeUp : { beq no : jsr rock4Up : .no }
    jsr edgeDown : { beq no : jsr rock4Down : .no }
    jsr edgeLeft : { beq no : jsr rock4Left : .no }
    jsr edgeRight : { beq no : jsr rock4Right : .no }
    rts

.updatePositions:
    jsr updatePositions1
    jsr updatePositions2
    jsr updatePositions3
    jsr updatePositions4
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Plot

.plotRockM:
    Copy16i mediumRockOutline, SMC_outline+1
    jmp drawOutline

.plotRockS:
    Copy16i smallRockOutline, SMC_outline+1
    jmp drawOutline

.last_rock1X skip 2
.last_rock1Y skip 2
.last_rock2X skip 2
.last_rock2Y skip 2
.last_rock3X skip 2
.last_rock3Y skip 2
.last_rock4X skip 2
.last_rock4Y skip 2

.plotRock1:
    Copy16v rock1X, last_rock1X
    Copy16v rock1Y, last_rock1Y
    Copy16v rock1X, theX
    Copy16v rock1Y, theY
    jmp plotRockS

.plotRock2:
    Copy16v rock2X, last_rock2X
    Copy16v rock2Y, last_rock2Y
    Copy16v rock2X, theX
    Copy16v rock2Y, theY
    jmp plotRockS

.plotRock3:
    Copy16v rock3X, last_rock3X
    Copy16v rock3Y, last_rock3Y
    Copy16v rock3X, theX
    Copy16v rock3Y, theY
    jmp plotRockM

.plotRock4:
    Copy16v rock4X, last_rock4X
    Copy16v rock4Y, last_rock4Y
    Copy16v rock4X, theX
    Copy16v rock4Y, theY
    jmp plotRockM


.unplotRock1:
    Copy16v last_rock1X, theX
    Copy16v last_rock1Y, theY
    jmp plotRockS

.unplotRock2:
    Copy16v last_rock2X, theX
    Copy16v last_rock2Y, theY
    jmp plotRockS

.unplotRock3:
    Copy16v last_rock3X, theX
    Copy16v last_rock3Y, theY
    jmp plotRockM

.unplotRock4:
    Copy16v last_rock4X, theX
    Copy16v last_rock4Y, theY
    jmp plotRockM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; main/loop

.frameNumber skip 1

.main: {
    jsr init
    jsr initPositions

    jsr plotRock1
    jsr plotRock2
    jsr plotRock3
    jsr plotRock4

.loop:

    inc frameNumber
    jsr info
    jsr readKeys
    ;jsr syncVB
    jsr unplotRock1
    jsr updatePositions
    jsr plotRock1

    inc frameNumber
    jsr info
    jsr readKeys
    ;jsr syncVB
    jsr unplotRock2
    jsr updatePositions
    jsr plotRock2

    inc frameNumber
    jsr info
    jsr readKeys
    ;jsr syncVB
    jsr unplotRock3
    jsr updatePositions
    jsr plotRock3

    inc frameNumber
    jsr info
    jsr readKeys
    ;jsr syncVB
    jsr unplotRock4
    jsr updatePositions
    jsr plotRock4

    jmp loop
    }

.info:
    Ula green
    Position 1,1
    Space : lda frameNumber : jsr printHexA
    Ula black
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.end:
print "bytes remaining: ", screenStart-*
SAVE "Code", start, end
