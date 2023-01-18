
osasci = &ffe3
oswrch = &ffee
osbyte = &fff4

org &70

.tmp SKIP 1
.spriteNumber SKIP 1
.spriteHeight SKIP 1
.coarseY SKIP 1
.oddRow SKIP 1
;; .onRow SKIP 1
;; .maxRow SKIP 1

.screenPtr SKIP 2
.patPtr SKIP 2

guard &1B00
guard &3000

org &1900

.start: {
    jsr switchMode1
    jsr cursorOff
    jsr replaceWhiteWithCyan
    jsr grid
    lda #1 : sta spriteNumber
.loop:
    jsr setVarsForSprite
    jsr computeVarsForSprite
    jsr vsync : jsr plot ; show

    ;jmp spin ; stop
    jsr pause

    jsr setVarsForSprite
    jsr computeVarsForSprite
    jsr bumpSpriteY
    ;jsr bumpSpriteH
    jsr vsync : jsr plot ; erase

    jmp loop
    }
.spin:
    jmp spin

.pause: {
    ldx #50
.loop:
    jsr vsync
    dex
    bne loop
    rts }

.vsync
    pha : txa : pha : tya : pha
    lda #19 : jsr osbyte
    pla : tay : pla : tax : pla
    rts

.switchMode1:
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


screenStart = &3000

;; mark a grid on screen to check p=splite positioning
;; mark pixel at top-left of every 4x8 cell
.grid: {
    lda #LO(screenStart) : sta screenPtr
    lda #HI(screenStart) : sta screenPtr+1
    ldy #0
.next:
    lda #&88
    sta (screenPtr),y
    clc
    lda screenPtr : adc #8 : sta screenPtr
    lda screenPtr+1 : adc #0 : sta screenPtr+1
    cmp #&80
    bmi next
    rts }


.sprite_pattern_lo:
    EQUB 0
    EQUB LO(blocky)

.sprite_pattern_hi:
    EQUB 0
    EQUB HI(blocky)

;; example pattern: red/yellow
.blocky:
    EQUB &e0, &a0, &e0, &a0
    EQUB &0e, &0a, &0e, &0a
    EQUB &e0, &a0, &e0, &a0
    EQUB &0e, &0a, &0e, &0a
    EQUB &e0, &a0, &e0, &a0
    EQUB &0e, &0a, &0e, &0a
    EQUB &e0, &a0, &e0, &a0
    EQUB &0e, &0a, &0e, &0a

.sprite_height: ; in pixels (at least 1)
    EQUB 0
    EQUB 24

.sprite_coarseY: ; 0..31
    EQUB 0
    EQUB 28

;; .fineY: ; 0..7
;;     EQUB 0
;;     EQUB 1

;; .coarseX: ; 0..79
;;     EQUB 0
;;     EQUB 1

;; .fineX: ; 0..3 -- picks which shifted-version of a patttern to use
;;     EQUB 0
;;     EQUB 1

;; .sprite_width: ; in 4-pix strips
;;     EQUB 0
;;     EQUB 1

.bumpSpriteY:
    ldx spriteNumber
    lda sprite_coarseY,x
    sec : adc #0 : and #31
    sta sprite_coarseY,x
    rts

.bumpSpriteH:
    ldx spriteNumber
    lda sprite_height,x
    and #31
    sec : adc #0
    sta sprite_height,x
    rts

.setVarsForSprite:
    ldx spriteNumber
    lda sprite_pattern_lo,x : sta patPtr
    lda sprite_pattern_hi,x : sta patPtr+1
    lda sprite_height,x : sta spriteHeight
    lda sprite_coarseY,x : sta coarseY
    rts

.computeVarsForSprite:
    lda coarseY : and #1 : sta oddRow
    lda coarseY : clc : asl a : asl a : adc coarseY : lsr a ; (coarseY*5)/2
    clc : adc #HI(screenStart) : sta screenPtr+1
    lda oddRow : clc : asl a : asl a : asl a : asl a : asl a : asl a : asl a
    sta screenPtr
    ;; lda coarseY : sta onRow
    ;; lda spriteHeight : sec : sbc #1 : clc : lsr a : lsr a : lsr a
    ;; clc : adc coarseY : sta maxRow
    rts

.plot: {
    ldy #0
    ldx #7
    ;ldy spriteHeight : dey
    ;set x from height: 7 -> 7, 8 -> 0, 9 -> 1, 10->2
    ;tya : and #7 : tax
.loopP:
    lda (screenPtr),y
    eor (patPtr),y
    ;lda (patPtr),y ; for dev, splat the existing col
    sta (screenPtr),y
    dex
    bmi nextRow
.afterNextRow
    ;;dey : bpl loop
    iny
    cpy spriteHeight : bne loopP
    rts
.nextRow:
    ldx #7
    clc
    lda screenPtr   : adc #120 : sta screenPtr
    lda screenPtr+1 : adc   #2
    cmp #&80 ; TODO this is wrong!
    bpl wrapScreen
    sta screenPtr+1
    ;inc onRow
    jmp afterNextRow
.wrapScreen:
    jmp spin
    sec : sbc #&50
    sta screenPtr+1
    ;; lda spriteHeight : clc : asl a : asl a : asl a
    ;; adc screenPtr+1
    jmp afterNextRow
}


.end:

SAVE "Code", start, end
