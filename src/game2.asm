ula = &fe21
osasci = &ffe3
oswrch = &ffee
osbyte = &fff4

ORG &70

.theA SKIP 2
.mesPtr SKIP 2

.overwritePtr SKIP 2
.xorplotPtr SKIP 2

.erasePtr SKIP 2
.eraseRunPtr SKIP 2
.eraseSwitcher SKIP 2

ORG &1900

.start:
    jmp main

;----------------------------------------------------------------------
; main loop

.examplePos SKIP 1 ; for first example drive

.main: {
    lda #0 : sta examplePos
    jsr setupMachine
    jsr eraseInit
.loop:
    jsr prepareScene

    ;lda #3 : sta ula ; blue
    jsr syncDelay
    lda #7 : sta ula ; black

    ;lda #2 : sta ula ; magenta
    jsr blitScene
    lda #7 : sta ula ; black

    jsr eraseFlip
    jmp loop
    rts }

.blitScene:
    jsr eraseRun
    jsr overwriteRun
    jsr xorplotRun
    rts

.prepareScene:
    jsr overwriteReInit
    jsr xorplotReInit
    ;; two fixed in place bars
    lda #&33 : sta theA : lda #&40 : sta theA+1
    lda #&f0 : jsr overwriteGen : jsr eraseGen ; yellow
    lda #&53 : sta theA : lda #&40 : sta theA+1
    lda #&0f : jsr overwriteGen : jsr eraseGen ; red
    ;; moving cyan block
    ldx #32
    jsr eraseGen
.SPLAT_TO_CRASH:
{ .loop:
    txa
    clc : adc examplePos
    sta theA : lda #&40 : sta theA+1
    lda #&ff : jsr xorplotGen : jsr eraseGen
    dex
    bne loop }
    inc examplePos
    rts

;----------------------------------------------------------------------
; misc

.syncDelay:
    lda #19 : jsr osbyte
    rts

.printMessageAndSpin: {
    ldy #0
.loop
    lda (mesPtr),y
    beq spin
    jsr osasci
    iny
    bne loop
.spin:
    jmp spin }

;----------------------------------------------------------------------
; setupMachine

.setupMachine:
    jsr mode1
    jsr cursorOff
    jsr replaceWhiteWithCyan
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

.replaceWhiteWithCyan:
    lda #19 : jsr oswrch
    lda #7 : jsr oswrch
    lda #6 : jsr oswrch
    lda #0 : jsr oswrch
    lda #0 : jsr oswrch
    lda #0 : jsr oswrch
    rts

;----------------------------------------------------------------------
; dual-space erase

.eraseRun:
    lda #&0 ;50 ; yellow for dev
    jmp (eraseRunPtr)

eraseNumberBlocks = 50
macro eraseTemplate
    sta SPLAT_TO_CRASH ; SCREEN-ADDRESS(1,2)
endmacro

.eraseSpaceA:
eraseTemplate ; one copy for size
eraseBlockSize = *-eraseSpaceA
FOR i, 1, eraseNumberBlocks-1 : eraseTemplate : NEXT
.eraseSpaceA_End:
    rts
.eraseSpaceB:
FOR i, 1, eraseNumberBlocks : eraseTemplate : NEXT
.eraseSpaceB_End:
    rts

.eraseGen:
    ;; decrement ptr one block
    lda erasePtr : sec : sbc #eraseBlockSize : sta erasePtr
    { bcs noHiDec : dec erasePtr+1 : .noHiDec }
    ;; check we are within the space
    jsr eraseSpaceCheck
    ;; fill in the generated code
    lda theA     : ldy #1 : sta (erasePtr),y
    lda theA+1   : ldy #2 : sta (erasePtr),y
    rts

.eraseSpaceCheck: {
    lda erasePtr+1 : cmp eraseSpace+1 : bcc fail : bne ok
    lda erasePtr   : cmp eraseSpace   : bcc fail
.ok:
    rts
.fail:
    lda #LO(msg) : sta mesPtr
    lda #HI(msg) : sta mesPtr+1
    jmp printMessageAndSpin
.msg EQUS "Erase Overflow", 13, 0 }

.eraseFlip:
    jmp (eraseSwitcher)
    rts

.eraseSpace SKIP 2

.eraseInit:
    lda #LO(eraseSpaceB_End) : sta erasePtr
    lda #HI(eraseSpaceB_End) : sta erasePtr+1
.eraseSwitcherA:
    lda erasePtr : sta eraseRunPtr
    lda erasePtr+1 : sta eraseRunPtr+1
    lda #LO(eraseSpaceA) : sta eraseSpace
    lda #HI(eraseSpaceA) : sta eraseSpace+1
    lda #LO(eraseSpaceA_End) : sta erasePtr
    lda #HI(eraseSpaceA_End) : sta erasePtr+1
    lda #LO(eraseSwitcherB) : sta eraseSwitcher
    lda #HI(eraseSwitcherB) : sta eraseSwitcher+1
    rts

.eraseSwitcherB:
    lda erasePtr : sta eraseRunPtr
    lda erasePtr+1 : sta eraseRunPtr+1
    lda #LO(eraseSpaceB) : sta eraseSpace
    lda #HI(eraseSpaceB) : sta eraseSpace+1
    lda #LO(eraseSpaceB_End) : sta erasePtr
    lda #HI(eraseSpaceB_End) : sta erasePtr+1
    lda #LO(eraseSwitcherA) : sta eraseSwitcher ; TODO: macro for 16bit cp
    lda #HI(eraseSwitcherA) : sta eraseSwitcher+1
    rts

;----------------------------------------------------------------------
; overwrite

.overwriteRun:
    jmp (overwritePtr)

overwriteNumberBlocks = 50
macro overwriteTemplate
    lda #&ff ; DATA-BYTE(1)
    sta SPLAT_TO_CRASH ; SCREEN-ADDRESS(3,4)
endmacro

.overwriteSpace:
overwriteTemplate ; one copy for size
overwriteBlockSize = *-overwriteSpace
FOR i, 1, overwriteNumberBlocks-1 : overwriteTemplate : NEXT
.overwriteSpaceEnd:
    rts

.overwriteGen: ; byte to overwrite in ACC
    {sta DB+1 ; save byte to overwrite until after the ptr decrement
    ;; decrement ptr one block
    lda overwritePtr : sec : sbc #overwriteBlockSize : sta overwritePtr
    { bcs noHiDec : dec overwritePtr+1 : .noHiDec }
    ;; check we are within the space
    jsr overwriteSpaceCheck
    ;; fill in the generated code
    .DB}: lda #0 : ldy #1 : sta (overwritePtr),y
    lda theA     : ldy #3 : sta (overwritePtr),y
    lda theA+1   : ldy #4 : sta (overwritePtr),y
    rts

.overwriteSpaceCheck: {
    lda overwritePtr+1 : cmp #HI(overwriteSpace) : bcc fail : bne ok
    lda overwritePtr   : cmp #LO(overwriteSpace) : bcc fail
.ok:
    rts
.fail:
    lda #LO(msg) : sta mesPtr
    lda #HI(msg) : sta mesPtr+1
    jmp printMessageAndSpin
.msg EQUS "Overwrite Overflow", 13, 0 }

.overwriteReInit:
    lda #LO(overwriteSpaceEnd) : sta overwritePtr
    lda #HI(overwriteSpaceEnd) : sta overwritePtr+1
    rts

;----------------------------------------------------------------------
; xorplot (WIP)

.xorplotRun:
    jmp (xorplotPtr)

xorplotNumberBlocks = 50
macro xorplotTemplate
    lda SPLAT_TO_CRASH ; SCREEN-ADDRESS(1,2)
    eor #&ff           ; DATA-BYTE(4)
    sta SPLAT_TO_CRASH ; SCREEN-ADDRESS(6,7)
endmacro

.xorplotSpace:
xorplotTemplate ; one copy for size
xorplotBlockSize = *-xorplotSpace
FOR i, 1, xorplotNumberBlocks-1 : xorplotTemplate : NEXT
.xorplotSpaceEnd:
    rts

.xorplotGen: ; byte to xorplot in ACC
    {sta DB+1 ; save byte to xorplot until after the ptr decrement
    ;; decrement ptr one block
    lda xorplotPtr : sec : sbc #xorplotBlockSize : sta xorplotPtr
    { bcs noHiDec : dec xorplotPtr+1 : .noHiDec }
    ;; check we are within the space
    jsr xorplotSpaceCheck
    ;; fill in the generated code
    .DB}: lda #0 : ldy #4 : sta (xorplotPtr),y
    lda theA     : ldy #1 : sta (xorplotPtr),y : ldy #6 : sta (xorplotPtr),y
    lda theA+1   : ldy #2 : sta (xorplotPtr),y : ldy #7 : sta (xorplotPtr),y
    rts

.xorplotSpaceCheck: {
    lda xorplotPtr+1 : cmp #HI(xorplotSpace) : bcc fail : bne ok
    lda xorplotPtr   : cmp #LO(xorplotSpace) : bcc fail
.ok:
    rts
.fail:
    lda #LO(msg) : sta mesPtr
    lda #HI(msg) : sta mesPtr+1
    jmp printMessageAndSpin
.msg EQUS "Xorplot Overflow", 13, 0 }

.xorplotReInit:
    lda #LO(xorplotSpaceEnd) : sta xorplotPtr
    lda #HI(xorplotSpaceEnd) : sta xorplotPtr+1
    rts

;----------------------------------------------------------------------
.end:
SAVE "Code", start, end
