
;;; Objects, with render loop and synced update

;;; DONE
;;; - control rocks with arrows (tab/caps - select arrow-lock)
;;; - tab/shift - toggle rock activeness/visibility (last/this bit)
;;; - reinstate drawing: unplot/plot, consulting this/last active-bit
;;; - position of selected object(s) controlled by arrows
;;; - different outlines: medium, small rock
;;; - debug: show frames-since-last-rendered per object
;;; - object/outline for ship; render in yellow
;;; - object/outline for bullets; render in red
;;; - object hit bit: updated by collision detection in render phase
;;; - collision logic: bullet hits ship/rock (& itself); rock hits ship
;;; - large rock outline. woohoo!

;;; TODO
;;; - collision logic: rock must not collide with itself!

;;; - hit logic: hit object becomes inactive; other objects activated
;;; - child rock inherits position(+ random delta) from parent
;;; - full rock destruction logic: large -> 2medium -> 4small

;;; - random position when spawn (become active)
;;; - rock movement controlled by speed
;;; - random speed when spawned (become active)
;;; - child rock inherits speed(+ random delta) from parent

;;; - reinstate ship controls: thrust/direction
;;; - bullet firing (spawn) & bullet death on timer (state: frameCounter when spawned)

;;; - sounds
;;; - scoring
;;; - game logic: start, level cleared, gameover


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

;;; Mode-1
screenStart = &3000
screenEnd = &8000

cyanMask = &ff
yellowMask = &f0
redMask = &0f
blackMask = &00

NUM = 8 ;; Number of objects, indexed consitently using X-register

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copy

;;; expect y = 2*x (where x = obj#)

macro Copy16iv I,V
    lda #LO(I) : sta V
    lda #HI(I) : sta V+1
endmacro

macro Copy16v A,B
    lda A   : sta B
    lda A+1 : sta B+1
endmacro

;;; Per-object tables of two-byte values are stored with with split LO and HI bytes
macro Copy16ix I,V
    lda #LO(I) : sta V,               x
    lda #HI(I) : sta V+NUM, x
endmacro

macro Copy16xv A,B
    lda A,     x : sta B
    lda A+NUM, x : sta B+1
endmacro

macro Copy16xx A,B
    lda A, x     : sta B, x
    lda A+NUM, x : sta B+NUM, x
endmacro

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
    Copy16iv msg, msgPtr
    jsr printString
    jmp spin
.msg:EQUS S, 0
endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; zero page

org &70
.msgPtr skip 2

.theX skip 2 ; plot from Hi byte + hi-bit of LO byte
.theY skip 2 ; plot only from HI byte

.theCX SKIP 1 ; coarse-X : 0..79
.theCY SKIP 1 ; coarse-Y : 0..31
.theFX SKIP 1 ; fine-X   : 0..3
.theFY SKIP 1 ; fine-Y   : 0..7
.theA SKIP 2  ; screen address of the 8x8 char

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Start

guard screenStart
org &1100

.start:
    jmp main

;;; increase the image size, so the switch-on beep has time to finish while loading
skip 1000

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

.setupZeroRts: {
    lda rts : sta 0
.rts:
    rts
    }

.init:
    lda #%01111111 : sta system_VIA_interruptEnable ; disable all interrupts
    lda #%10000010 : sta system_VIA_interruptEnable ; enable just VBlank
    ;; start in keys mode
    lda #%00000011 : sta system_VIA_portB ; set bit 3 to 0
    lda #%01111111 : sta system_VIA_dataDirectionA ; (top bit input)
    jsr mode1
    jsr cursorOff
    jsr replaceWhiteWithCyan
    jsr setupZeroRts
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sounds

.playSounds:
    ;; TODO!
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keys

.startKeys:

.keyCaps  EQUB 0
.keyCtrl  EQUB 0
.keyShift EQUB 0
.keyEnter EQUB 0

.keyUp    equb 0
.keyDown  equb 0
.keyLeft  equb 0
.keyRight equb 0
.keyTab   equb 0

.endKeys:
numberKeys = endKeys - startKeys
skip numberKeys ;; space to store LAST state

macro PollKey Code, Key
    lda #0 : sta Key
    lda #(-Code-1) : sta system_VIA_portA : lda system_VIA_portA
    bpl no : lda #1 : sta Key : .no
endmacro

macro Edge Key
    lda Key : sta Key+numberKeys
endmacro

.readKeys:
	;; Caps/Ctrl/Shift are level sensitive keys
    Edge keyCtrl ;; For Debug/Dev - switch arrow lock
    Edge keyShift ;; For Debug/Dev - switch active state
    Edge keyEnter
    Edge keyTab
    ;; We may or may not want (debugging) arrow keys to be edge sensitive
    ;;Edge keyUp
    ;;Edge keyDown
    ;;Edge keyLeft
    ;;Edge keyRight

    PollKey -65,  keyCaps
    PollKey -2,   keyCtrl
    PollKey -1,   keyShift
    PollKey -74,  keyEnter
    PollKey -97,  keyTab
    PollKey -58,  keyUp
    PollKey -42,  keyDown
    PollKey -26,  keyLeft
    PollKey -122, keyRight
    rts

macro CheckPress Key ; -> NZ
    lda Key+numberKeys : eor #&ff : and Key
endmacro

.printKeyState:
    lda #'.' : { ldy keyCaps : beq no : lda #'A' : .no } : jsr emit
    lda #'.' : { ldy keyCtrl : beq no : lda #'C' : .no } : jsr emit
    lda #'.' : { ldy keyShift: beq no : lda #'S' : .no } : jsr emit
    lda #'.' : { ldy keyEnter: beq no : lda #'E' : .no } : jsr emit
    Space
    lda #'.' : { ldy keyTab  : beq no : lda #'T' : .no } : jsr emit
    Space
    lda #'.' : { ldy keyUp   : beq no : lda #'U' : .no } : jsr emit
    lda #'.' : { ldy keyDown : beq no : lda #'D' : .no } : jsr emit
    lda #'.' : { ldy keyLeft : beq no : lda #'L' : .no } : jsr emit
    lda #'.' : { ldy keyRight: beq no : lda #'R' : .no } : jsr emit
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

.upA8: ; A
    lda theA : sec : sbc #&80 : sta theA
    lda theA+1     : sbc #2   : sta theA+1
    rts

.downA8: ; A
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
;;; Outline movement control

N = 1
S = 2
E = 4
W = 8
INVISIBLE = 16
START = 32

;;;macro Center : EQUB START : endmacro ; see center of rotation
;;;macro Center : EQUB INVISIBLE : endmacro ; DONT see center of rotation
macro Center : endmacro  ; DONT see center of rotation (and dont even waste a byte!)

NE = N or E
SE = S or E
NW = N or W
SW = S or W

Ni = N or INVISIBLE
Si = S or INVISIBLE
Wi = W or INVISIBLE
SWi = SW or INVISIBLE

.invisible equb INVISIBLE
.asNorth equb N
.asSouth equb S
.asEast equb E
.asWest equb W

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; draw

.outlineIndex skip 1
.outlineMotion skip 1

.drawOutline: {
    jsr calcA
    lda #0 : sta isHit,x
    ldy #0
.loop:
    .*SMC_outline : lda &7777, y : beq rts
    iny
    sty outlineIndex
    sta outlineMotion
    bit asNorth : bne north
    bit asSouth : bne south
    jmp ew
.north:
    jsr up1
    jmp ew
.south:
    jsr down1
.ew:
    lda outlineMotion
    bit asEast : bne east
    bit asWest : bne west
    jmp pr
.east:
    jsr right1
    jmp pr
.west:
    jsr left1
.pr:
    lda outlineMotion
    bit invisible : bne nextPoint
    .*SMC_onPoint : jmp &7777
.*nextPoint:
    ldy outlineIndex
    jmp loop
.rts:
    rts
    }

macro SetOutlineAndOnPoint
    Copy16xv myOutline, SMC_outline+1
    Copy16xv myOnPoint, SMC_onPoint+1
endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global state

.frameCounter: skip 1

.selectedN: equb 1

.stepSelectedObject:
    inc selectedN
    lda selectedN : cmp #NUM : { bne no : lda #0 : sta selectedN : .no }
    rts

.updateSelectedObjectOnTab:
    CheckPress keyTab
    { beq no : jsr stepSelectedObject : .no }
    rts

.updateGlobalState:
    jsr updateSelectedObjectOnTab
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Objects... state tables & update

.isActive skip NUM ; state that is desired for the object
.isArrowLocked skip NUM ; move using arrows for dev/debuf

.isHit skip NUM ; set during render phase

;; current position (double prec)
.obX skip 2*NUM
.obY skip 2*NUM

macro ToggleA
	eor #1 : and #1
endmacro

.updateActivenessOnShift: {
    CheckPress keyShift : beq no
    cpx selectedN : bne no
    lda isActive, x : ToggleA : sta isActive, x ; spawn/die
.no:
    rts
    }

.updateArrowLockOnCtrl: {
    CheckPress keyCtrl : beq no
    cpx selectedN : bne no
    lda isArrowLocked, x : ToggleA : sta isArrowLocked, x
.no:
    rts
    }

.moveUp:   dec obY+NUM, x : rts
.moveDown: inc obY+NUM, x : rts

.moveLeft:
    lda obX, x     : sec : sbc #&80                                          : sta obX, x
    lda obX+NUM, x :       sbc   #0 : { cmp #&ff : bne no : lda #159 : .no } : sta obX+NUM, x
    rts

.moveRight:
    lda obX,     x : clc : adc #&80                                          : sta obX, x
    lda obX+NUM, x :       adc   #0 : { cmp #160 : bne no : lda #0   : .no } : sta obX+NUM, x
    rts

.updatePositionIfArrowLocked: {
    lda isArrowLocked, x : beq no
    CheckPress keyUp    : { beq no : jsr moveUp    : .no }
    CheckPress keyDown  : { beq no : jsr moveDown  : .no }
    CheckPress keyLeft  : { beq no : jsr moveLeft  : .no }
    CheckPress keyRight : { beq no : jsr moveRight : .no }
.no:
    rts
    }

.updateObject:
    ;;Position 1,30 : Emit 'U' : txa : jsr printHexA
    jsr updateActivenessOnShift
    jsr updateArrowLockOnCtrl
    jsr updatePositionIfArrowLocked
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Render object

;;; object was active on last render, so we drew it on screen
.lastActive skip NUM

;;; position we drew object on screen (double prec)
.lastX skip 2*NUM
.lastY skip 2*NUM ; TODO: dont need/use HI-byte

.lastRenderedFrame skip NUM

macro DebugPositionForObject
    lda #31 : jsr osasci
    lda #30 : jsr osasci
    txa : jsr osasci
endmacro

.debugObject:
    DebugPositionForObject
    lda frameCounter : sec : sbc lastRenderedFrame, x : jsr printHexA
    Space : lda #'.' : { cpx selectedN : bne no : lda #'*' : .no } : jsr emit
    Space : lda #'.' : { ldy isArrowLocked, x : beq no : lda #'M' : .no } : jsr emit
    Space : lda #'.' : { ldy isHit, x : beq no : lda #'H' : .no } : jsr emit
    ;;Space : lda #'0' : { ldy isActive, x : beq no : lda #'1' : .no } : jsr emit
    ;;lda #'0' : { ldy lastActive, x : beq no : lda #'1' : .no } : jsr emit
    rts

.renderObject: {
    ;;Position 1,30 : Emit 'r' : txa : jsr printHexA
    jsr debugObject
    SetOutlineAndOnPoint
    ;; unplot (if we were active when last rendered)
    lda lastActive, x : beq afterUnplot
    Copy16xv lastX, theX
    Copy16xv lastY, theY
    jsr drawOutline
.afterUnplot:
    ;; (re)plot if (we are to be active now)
    lda isActive, x : beq afterPlot
    Copy16xv obX, theX
    Copy16xv obY, theY
    jsr drawOutline
    ;; remember position at which we rendered
    Copy16xx obX, lastX
    Copy16xx obY, lastY
.afterPlot:
    ;; remember our activeness state
    lda isActive, x : sta lastActive, x
    lda frameCounter : sta lastRenderedFrame, x
    rts
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IRQ, VBlank syncing

.vsyncNotify equb 0 ; -> 1

.myIRQ: {
    lda system_VIA_interruptFlags : and #2 : bne vblank
    Crash "unexpected interrupt"
    lda #&7f : sta system_VIA_interruptFlags ; ack
    lda interruptSaveA
    rti
.vblank:
    sta system_VIA_interruptFlags ; ack
    lda vsyncNotify : bne crash
    inc vsyncNotify
    lda interruptSaveA
    rti
.crash:
    Crash " SYNC"
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; onSync

.updateF skip 2*NUM
.updateN: skip 1

.updateObjects: {
    lda #0 : sta updateN
.loop:
    ldx updateN : cpx #NUM : { bne no : rts : .no }
    Copy16xv updateF, dispatch+1
    .dispatch : jsr &7777
    inc updateN
    jmp loop
    }

.onSync:
    inc frameCounter
    jsr playSounds
    jsr readKeys
    ;;Position 1,1 : lda frameCounter : jsr printHexA
    ;; Space : jsr printKeyState
    jsr updateGlobalState
    jsr updateObjects
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Render loop.

.renderF skip 2*NUM
.renderN: skip 1
.renderLoop: {
.loop:
    lda vsyncNotify : { beq no : dec vsyncNotify : jsr onSync : .no }
    ldx renderN : cpx #NUM : { bne no : ldx #0 : stx renderN : .no }
    Copy16xv renderF, dispatch+1
    .dispatch : jsr &7777
    inc renderN
    jmp loop
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; outlines

.smallRockOutline:
    equb START,E,SE,NE,E,SE,S, SE,S,SW,S,SW, W,NW,SW,W, NW,NE,NW,NW,N,NE, 0

.mediumRockOutline:
    equb START, E,E,NE,NE,E,E,E,E,SE,SE,E,SE,SE, SW,SW,SW,SE,SE,SE,S,S,SW,SW,SW,W,W
    equb NW,NW,SW,SW,W,W,W, NW,NW,NW,NE,NE,NW,NW,NW,N,N,NE,NE, 0

.largeRockOutline:
    equb START
    equb E,E,E,E,E,E,E,E, SE,SE,SE,SE, E,E,E,E,E,E, SE,SE,SE,SE,SE, SW,SW,SW,SW,SW, SE,SE,SE, S,S
    equb SW,SW,SW,SW,SW,SW,SW, W,W,W,W,W, NW,NW,NW,NW, SW,SW,SW, W,W,W, NW,NW,NW
    equb N,N,N,N, NW,NW, N,N,N,N, NE,NE,NE,NE, NW,NW,NW, NE,NE,NE,NE, 0

.shipOutline1: Center
    equb Si,Si, S,W,W,W,SW,N,N, NE,N,N,NE,N,N,NE,N,N,NE, SE,S,S,SE,S,S,SE,S,S,SE,S,S, NW,W,W, 0

.bulletOutline:
    equb START, E, SW, NW, NE, 0


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; colour choice & collision detection (onPoint)

.myOutline skip 2*NUM
.myOnPoint skip 2*NUM

.yellow_onPoint: {
    ;; get/stash the existing byte at this screen address
    ldy #0 : lda (theA),y
    sta existing+1
	;; get the fine-x pixel-plot-position mask
    ldy theFX : lda masks,y
    ;; and combine with a mask for the colours we consider as hitting us
    ;;.*SMC_hitMask
    and #&ff ; detect hits with anything
    bit existing+1
    { beq noHit : lda #1 : sta isHit,x : .noHit }
	;; get the fine-x pixel-plot-position mask (again)
    ldy theFX : lda masks,y
    ;; combine this with the colour mask for the colour we wish to plot
    ;;.*SMC_colourMask
    and #yellowMask ;redMask
    ;; eor the byte to be plotted with the existing byte
    .existing : eor #77
    ldy #0 : sta (theA),y
    jmp nextPoint ; not rts!
    }

.red_onPoint: {
    ;; get/stash the existing byte at this screen address
    ldy #0 : lda (theA),y
    sta existing+1
	;; get the fine-x pixel-plot-position mask
    ldy theFX : lda masks,y
    ;; and combine with a mask for the colours we consider as hitting us
    ;;.*SMC_hitMask
    and #&ff ; detect hits with anything
    bit existing+1
    { beq noHit : lda #1 : sta isHit,x : .noHit }
	;; get the fine-x pixel-plot-position mask (again)
    ldy theFX : lda masks,y
    ;; combine this with the colour mask for the colour we wish to plot
    ;;.*SMC_colourMask
    and #redMask
    ;; eor the byte to be plotted with the existing byte
    .existing : eor #77
    ldy #0 : sta (theA),y
    jmp nextPoint ; not rts!
    }

.cyan_onPoint: {
    ;; get/stash the existing byte at this screen address
    ldy #0 : lda (theA),y
    sta existing+1
	;; get the fine-x pixel-plot-position mask
    ldy theFX : lda masks,y
    ;; and combine with a mask for the colours we consider as hitting us
    ;;.*SMC_hitMask
    and #&ff ; detect hits with anything
    bit existing+1
    { beq noHit : lda #1 : sta isHit,x : .noHit }
	;; get the fine-x pixel-plot-position mask (again)
    ldy theFX : lda masks,y
    ;; combine this with the colour mask for the colour we wish to plot
    ;;.*SMC_colourMask
    and #cyanMask ;; TODO: not necesary!
    ;; eor the byte to be plotted with the existing byte
    .existing : eor #77
    ldy #0 : sta (theA),y
    jmp nextPoint ; not rts!
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; create

.createObject:
    ;; set generic update/render behaviour
    Copy16ix renderObject, renderF
    Copy16ix updateObject, updateF
    ;; start everything active
    inc isActive, x
    ;; object 2 is initially controlled by arrows
    { cpx #2 : bne no : inc isArrowLocked, x : .no }
    ;; setup initial HI-byte of position, based on obj#
    txa : asl a : asl a : asl a : asl a
    sta obX+NUM, x
    asl a
    sta obY+NUM, x
    rts

.createRock:
    Copy16ix cyan_onPoint, myOnPoint
    jmp createObject

.createRockS: Copy16ix smallRockOutline, myOutline : jmp createRock
.createRockM: Copy16ix mediumRockOutline, myOutline : jmp createRock
.createRockL: Copy16ix largeRockOutline, myOutline : jmp createRock

.createShip:
    Copy16ix shipOutline1, myOutline ;; TODO: multple outlines!
    Copy16ix yellow_onPoint, myOnPoint
    jmp createObject

.createBullet:
    Copy16ix bulletOutline, myOutline
    Copy16ix red_onPoint, myOnPoint
    jmp createObject
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Game init.

.initObjects:
    ;;ldx #0 : jsr createRockM
    ldx #1 : jsr createRockL
    ldx #2 : jsr createRockL
    ldx #3 : jsr createBullet
    ldx #4 : jsr createShip
    ldx #5 : jsr createRockM
    ldx #6 : jsr createRockS
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; main

.main:
    jsr init
    jsr initObjects
    Copy16iv myIRQ, irq1v ; initialise sync
    jmp renderLoop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.end:
print "bytes remaining: ", screenStart-*
SAVE "Code", start, end
