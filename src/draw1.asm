
; Plotting example

oswrch = &ffee

org &2000

black = 0
red = 1
yellow = 2
white = 3

.start:
    jsr switch_mode1
    jsr cursor_off
    lda #white
    ;jsr border
    ;jsr wash_screen_with_colours
    jsr animate
.spin:
    jmp spin

.animate:
    lda #yellow
    ldy #100
    ldx #50
.aloop
    jsr small_m
    jsr pause
    jsr small_m
    inx : inx : iny
    jmp aloop
    rts

.small_m
    ;pha : txa : pha : tya : pha
    ;lda #white

    dey : inx : jsr plot_dot
    dey : inx : jsr plot_dot
    inx : jsr plot_dot
    iny : inx : jsr plot_dot
    dey : inx : jsr plot_dot
    inx : jsr plot_dot
    iny : inx : jsr plot_dot
    iny : jsr plot_dot
    iny : inx : jsr plot_dot
    iny : jsr plot_dot
    iny : dex : jsr plot_dot
    iny : jsr plot_dot
    iny : dex : jsr plot_dot
    dex : jsr plot_dot
    dey : dex : jsr plot_dot
    iny : dex : jsr plot_dot
    dex : jsr plot_dot
    dey : dex : jsr plot_dot
    dey : inx : jsr plot_dot
    dey : dex : jsr plot_dot
    dey : dex : jsr plot_dot
    dey : jsr plot_dot

    ;pla : tay : pla : tax : pla
    rts

.border
    ldx #1
.next_x
    ldy #255
    jsr plot_dot
    ldy #0
    jsr plot_dot
    inx
    bne next_x
    jsr plot_dot

    ldy #1
.next_y
    ldx #0
    jsr plot_dot
    ldx #255
    jsr plot_dot
    iny
    bne next_y

    ldy #255
    jsr plot_dot

    rts

.wash_screen_with_colours
    lda #yellow
    jsr wash
    lda #white ; to show as red
    jsr wash
    lda #red ; to clear to black
    jsr wash
    rts

.wash: ; takes colour in Acc
    pha
    ldy #1
.next_row:
    ldx #1
.next:
    pla : pha
    jsr plot_dot
    inx
    txa : cmp #255 : bne next
    tya : clc : adc #1 : tay : cmp #255 : bne next_row
    pla
    rts

;; .beep:
;;     lda #7 : jsr oswrch
;;     rts

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

.switch_mode1:
    lda #22 : jsr oswrch
    lda #1 : jsr oswrch
    rts

.cursor_off:
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

;; x [0-255]
;; y [0-255]
;;
;; brick_row = y/8 [0-31]
;; half_brick_on_row = x/64 [0-3 ; 4-never]
;; odd_row = (brick_row % 2) [0;1]
;; half_brick_offset = odd_row << 6 [0;64]
;;
;; A_hi = &30 + (5 * brick_row + half_brick_on_row) / 2 [&30-&7F]
;; A_lo = (x ^ half_brick_offset) % 128 / 4 * 8 + y%8 [0-255]

p = &70
tmp = &72

.plot_dot
    pha

    tya : lsr a : lsr a : lsr a : sta tmp ; brick_row
    asl a : asl a : clc : adc tmp : sta tmp ; 5*brick_row
    txa : lsr a : lsr a : lsr a : lsr a : lsr a : lsr a ; half_brick_on_row
    clc : adc tmp : lsr a
    clc : adc #&30
    sta p+1 ; A_hi

    tya : lsr a : lsr a : lsr a : and #1 ; odd_row
    asl a : asl a : asl a : asl a : asl a : asl a : sta tmp ; half_brick_offset
    txa : and #127 : eor tmp : lsr a : lsr a : asl a : asl a : asl a : sta tmp
    tya : and #7
    clc : adc tmp
    sta p ; A_lo

    pla : pha
    and #2 : asl a : asl a : asl a
    sta tmp
    pla : pha
    and #1 : ora tmp ; col pattern [&00, &01, &10, &11]

    sta tmp
    txa : and #1
    bne no1
    asl tmp
.no1
    txa : and #2
    bne no2
    asl tmp : asl tmp
.no2
    ;; tmp has colour byte mask shifted to correct pixel

    tya : pha
    ldy #0
    lda (p),y
    eor tmp
    sta (p),y
    pla : tay

    pla
    rts


.end
save "code", start, end
