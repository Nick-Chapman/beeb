
;;; Objects, with render loop and synced update

;;; DONE
;;; - control rocks with arrows (tab/caps - select arrow-lock)
;;; - tab/shift - toggle rock activeness/visibility (last/this bit)

;;; TODO
;;; - reinstate drawing: unplot/plot, consulting this/last active-bit
;;; - position of selected object(s) controlled by arrows
;;; - different outlines: medium, small rock
;;; - debug: show frames-since-last-rendered per object
;;; - random position when spawn (become active)
;;; - object/outline for ship; render in yellow
;;; - objects/outlines for bullets; render in red

;;; - object hit bit, dev: tab/return (no longer need debug/dev control of activeness)
;;; - hit logic: hit object becomes inactive; other objects activated
;;; - child rock inherits position(+ random delta) from parent
;;; - encode large rock outline
;;; - full rock destruction logic: large -> 2medium -> 4small

;;; - collision detection in render phase: controls hit bit
;;; - full collision logic: bullet kills ship/rock (& itself); rock kills ship

;;; - rock movement controlled by speed
;;; - random speed when spawned (become active)
;;; - child rock inherits speed(+ random delta) from parent

;;; - reinstate ship controls: thrust/direction
;;; - bullet firing (spawn) & bullet death on timer (state: frameCounter when spawned)

;;; - sounds
;;; - scoring
;;; - game logic: start, level cleared, gameover

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
;;; zero page

org &70
.msgPtr skip 2

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

.init:
    lda #%01111111 : sta system_VIA_interruptEnable ; disable all interrupts
    lda #%10000010 : sta system_VIA_interruptEnable ; enable just VBlank
    jsr mode1
    jsr cursorOff
    jsr replaceWhiteWithCyan
    Copy16i myIRQ, irq1v
    ;; start in keys mode
    lda #%00000011 : sta system_VIA_portB ; set bit 3 to 0
    lda #%01111111 : sta system_VIA_dataDirectionA ; (top bit input)
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
    Edge keyCaps ;; For Debug/Dev - switch arrow lock
    Edge keyShift ;; For Debug/Dev - switch active state
    Edge keyEnter
    Edge keyTab
    ;; We may or may not want (debugging) arrow keys to be edge sensitive
    Edge keyUp
    Edge keyDown
    Edge keyLeft
    Edge keyRight

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
    lda #'.' : { ldy keyLeft : beq no : lda #'L' : .no } : jsr emit
    lda #'.' : { ldy keyRight: beq no : lda #'R' : .no } : jsr emit
    lda #'.' : { ldy keyUp   : beq no : lda #'U' : .no } : jsr emit
    lda #'.' : { ldy keyDown : beq no : lda #'D' : .no } : jsr emit
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global state

.selectedN: equb 1

.stepSelectedObject:
    inc selectedN
    lda selectedN : cmp #NumberObjects : { bne no : lda #0 : sta selectedN : .no }
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

NumberObjects = 3

;;; tables...
.isArrowLocked skip NumberObjects
.isActiveNext skip NumberObjects
.isActiveNow skip NumberObjects

.placeHolderPos skip NumberObjects ;; TODO placeholder. needs x/y and double precision

macro ToggleA
	eor #1 : and #1
endmacro

.updateActivenessOnShift: {
    CheckPress keyShift : beq no
    cpx selectedN : bne no
    lda isActiveNext, x : ToggleA : sta isActiveNext, x
.no:
    rts
    }

.updateArrowLockOnCaps: {
    CheckPress keyCaps : beq no
    cpx selectedN : bne no
    lda isArrowLocked, x : ToggleA : sta isArrowLocked, x
.no:
    rts
    }

.updatePositionIfArrowLocked: {
    lda isArrowLocked, x : beq no
    ;; any arrow key for DEV
    CheckPress keyLeft : bne yes
    CheckPress keyRight : bne yes
    CheckPress keyUp : bne yes
    CheckPress keyDown : bne yes
.no:
    rts
.yes:
    inc placeHolderPos, x
    rts
    }

.updateOb: ; X:ob#
    Position 1,30 : Emit 'U' : txa : jsr printHexA
    jsr updateActivenessOnShift
    jsr updateArrowLockOnCaps
    jsr updatePositionIfArrowLocked
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Render object

macro DebugPositionForObject
    lda #31 : jsr osasci
    lda #25 : jsr osasci
    txa : jsr osasci
endmacro

.debugObject: ; X:ob#
    DebugPositionForObject
    txa : jsr printHexA
    Space : lda #'.' : { cpx selectedN : bne no : lda #'*' : .no } : jsr emit
    Space : lda #'0' : { ldy isActiveNext, x : beq no : lda #'1' : .no } : jsr emit
    lda #'0' : { ldy isActiveNow, x : beq no : lda #'1' : .no } : jsr emit
    Space : lda #'0' : { ldy isArrowLocked, x : beq no : lda #'1' : .no } : jsr emit
    Space : lda placeHolderPos, x : jsr printHexA
    rts

.renderOb:
    Position 1,30 : Emit 'r' : txa : jsr printHexA
    jsr debugObject
    ;; TODO:
    ;; - IF WAS active, plot/(UN)render object outline at LAST position
    ;; - IF WILLBE active, plot/render object outline at position
    ;; - copy the position rendered at to LAST position
    lda isActiveNext, x : sta isActiveNow, x
    rts

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
    ;;lda vsyncNotify : bne crash ;; TODO: like to reinstate
    inc vsyncNotify
    lda interruptSaveA
    rti
.crash:
    Crash " SYNC"
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; onSync

.frameCounter: skip 1

.updateF skip 2*NumberObjects
.updateN: skip 1

.updateObjects: {
    lda #0 : sta updateN
.loop:
    ldx updateN : cpx #NumberObjects : { bne no : rts : .no }
    txa : asl a : tay
    lda updateF,   y : sta dispatch+1
    lda updateF+1, y : sta dispatch+2
    .dispatch : jsr &7777
    inc updateN
    jmp loop
    }

.onSync:
    inc frameCounter
    jsr playSounds
    jsr readKeys
    Position 1,1 : lda frameCounter : jsr printHexA : Space : jsr printKeyState
    jsr updateGlobalState
    jsr updateObjects
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Render loop.

.renderF skip 2*NumberObjects
.renderN: skip 1
.renderLoop: {
.loop:
    lda vsyncNotify : { beq no : dec vsyncNotify : jsr onSync : .no }
    lda renderN : cmp #NumberObjects : { bne no : lda #0 : sta renderN : .no }
    tax : asl a : tay
    lda renderF,   y : sta dispatch+1
    lda renderF+1, y : sta dispatch+2
    .dispatch : jsr &7777
    inc renderN
    jmp loop
    }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Game init.

.setupZeroRts:
    lda rts : sta 0
.rts:
    rts

.initObjects:
    ;; Setup some objects...
    Copy16i updateOb, updateF+0
    Copy16i renderOb, renderF+0
    Copy16i updateOb, updateF+2
    Copy16i renderOb, renderF+2
    Copy16i updateOb, updateF+4
    Copy16i renderOb, renderF+4
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; main

.main:
    jsr setupZeroRts
    jsr init
    jsr initObjects
    jmp renderLoop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.end:
print "bytes remaining: ", screenStart-*
SAVE "Code", start, end
