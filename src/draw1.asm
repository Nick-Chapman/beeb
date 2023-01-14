
; Plotting example

ula = &fe21

oswrch = &ffee
osbyte = &fff4

black = 0
red = 1
yellow = 2
cyan = 3

org 0 ;&70 ; could be 0

.tmp SKIP 1
.X SKIP 1
.Y SKIP 1

.p SKIP 2
.q SKIP 2
.dp SKIP 2

.fineX SKIP 1
.fineY SKIP 1
.fineYsplit SKIP 1

.ob1X SKIP 1
.ob1Y SKIP 1
.ob2X SKIP 1
.ob2Y SKIP 1

org &2000

.start:
    jsr switch_mode1
    jsr cursor_off
    jsr replace_white_with_cyan
    lda #cyan
    jsr animate

.spin:
    ;rts ; early return
    jmp spin

.vsync
    pha : txa : pha : tya : pha
    lda #19 : jsr osbyte
    pla : tay : pla : tax : pla
    rts

.raster_show_on
    pha
    ;lda #&00+(1EOR7) : sta ula
    lda #6 : sta ula
    pla
    rts

.raster_show_off
    pha
    ;lda #&00+(0EOR7) : sta ula
    lda #7 : sta ula
    pla
    rts

.old_small_m
    ldx X
    ldy Y : iny : iny
    lda #cyan
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
    rts

.pause:
    pha : txa : pha : tya : pha
    ldx #130
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

.replace_white_with_cyan
    lda #19 : jsr oswrch
    lda #7 : jsr oswrch
    lda #6 : jsr oswrch
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

;----------------------------------------------------------------------
;;; draw sprites as a single unit sharing the calculations

oooo = &00
ooox = &11
ooxo = &22
ooxx = &33
oxoo = &44
oxox = &55
oxxo = &66
oxxx = &77
xooo = &88
xoox = &99
xoxo = &aa
xoxx = &bb
xxoo = &cc
xxox = &dd
xxxo = &ee
xxxx = &ff

;; small meteor data... (unshifted)
.sprite_data0:

EQUB    ooxx
EQUB    oxoo
EQUB    xooo
EQUB    xooo
EQUB    oxoo
EQUB    ooxo
EQUB    oxoo
EQUB    ooxx

EQUB    oxxo
EQUB    xoox
EQUB    ooox
EQUB    oooo
EQUB    oooo
EQUB    ooox
EQUB    xoox
EQUB    oxxo

EQUB    oooo
EQUB    oooo
EQUB    oooo
EQUB    xooo
EQUB    xooo
EQUB    oooo
EQUB    oooo
EQUB    oooo

;; small meteor data... (shifted 1)
.sprite_data1:

EQUB    ooox
EQUB    ooxo
EQUB    oxoo
EQUB    oxoo
EQUB    ooxo
EQUB    ooox
EQUB    ooxo
EQUB    ooox

EQUB    xoxx
EQUB    oxoo
EQUB    oooo
EQUB    oooo
EQUB    oooo
EQUB    oooo
EQUB    oxoo
EQUB    xoxx

EQUB    oooo
EQUB    xooo
EQUB    xooo
EQUB    oxoo
EQUB    oxoo
EQUB    xooo
EQUB    xooo
EQUB    oooo

;; small meteor data... (shifted 2)
.sprite_data2:

EQUB    oooo
EQUB    ooox
EQUB    ooxo
EQUB    ooxo
EQUB    ooox
EQUB    oooo
EQUB    ooox
EQUB    oooo

EQUB    xxox
EQUB    ooxo
EQUB    oooo
EQUB    oooo
EQUB    oooo
EQUB    xooo
EQUB    ooxo
EQUB    xxox

EQUB    xooo
EQUB    oxoo
EQUB    oxoo
EQUB    ooxo
EQUB    ooxo
EQUB    oxoo
EQUB    oxoo
EQUB    xooo

;; small meteor data... (shifted 3)
.sprite_data3:

EQUB    oooo
EQUB    oooo
EQUB    ooox
EQUB    ooox
EQUB    oooo
EQUB    oooo
EQUB    oooo
EQUB    oooo

EQUB    oxxo
EQUB    xoox
EQUB    oooo
EQUB    oooo
EQUB    xooo
EQUB    oxoo
EQUB    xoox
EQUB    oxxo

EQUB    xxoo
EQUB    ooxo
EQUB    ooxo
EQUB    ooox
EQUB    ooox
EQUB    ooxo
EQUB    ooxo
EQUB    xxoo

.sprite_data_p:
    EQUW sprite_data0, sprite_data1, sprite_data2, sprite_data3


.new_small_m:

    lda Y : lsr a : lsr a : lsr a : sta tmp ; brick_row
    asl a : asl a : clc : adc tmp : sta tmp ; 5*brick_row
    lda X : lsr a : lsr a : lsr a : lsr a : lsr a : lsr a ; half_brick_on_row
    clc : adc tmp : lsr a
    clc : adc #&30
    sta p+1 ; A_hi

    lda Y : lsr a : lsr a : lsr a : and #1 ; odd_row
    asl a : asl a : asl a : asl a : asl a : asl a : sta tmp ; half_brick_offset
    lda X : and #127 : eor tmp : lsr a : lsr a : asl a : asl a : asl a : sta tmp

    ;; fine y-offset [0-7]
    lda Y : and #7 : sta fineY
    ;lda #0

    clc : adc tmp
    sta p ; A_lo

    ;; fine x-offset [0-3]
    lda X : and #3 : clc : asl a : tax ; asl because words are 2 bytes
    ;ldx #0

    lda sprite_data_p,x
    sta dp
    lda sprite_data_p+1,x
    sta dp+1

    clc
    lda p   : adc #120 : sta q ; 120==128-8 !
    lda p+1 : adc   #2 :
    sec : cmp #&80 : bcc noCycleScreen : sbc #&50
.noCycleScreen
    sta q+1

    lda #8 : sec : sbc fineY : sta fineYsplit

    ldx #3 ; number of sprite columns
    ldy #0

.render_loop
    cpy fineYsplit : bpl noR1

    lda (dp),y
    eor (p),y
    sta (p),y
.noR1
    cpy fineYsplit : bmi noR2

    lda (dp),y
    eor (q),y
    sta (q),y
.noR2

    iny
    cpy #8 : bne render_loop
    dex : bne next_col
    lda X : sec : sbc #8 : sta X ; restore X after doing 2(#cols-1)x adc#4
    rts

.next_col
    ldy #0
    lda p : clc : adc #8 : sta p
    lda p+1 : adc #0 : sta p+1
    lda q : clc : adc #8 : sta q
    lda q+1 : adc #0 : sta q+1
    lda dp : clc : adc #8 : sta dp
    ;lda dp+1 : clc : adc #0 : sta dp+1 ; needed if sprite data crosses a page

    lda X : clc : adc #4 : sta X : bcc noColWrap

    sec
    ;lda p   : sbc #128 : sta p
    lda p+1 : sbc #2 : sta p+1
    sec
    ;lda q   : sbc #128 : sta q
    lda q+1 : sbc #2 : sta q+1

.noColWrap
    jmp render_loop


.init_world
    lda #50 : sta ob1X
    lda #100 : sta ob1Y
    lda #70 : sta ob2X
    lda #130 : sta ob2Y
    rts

.update_world
    inc ob1X : inc ob1X : inc ob1Y
    inc ob2X : dec ob2Y
    rts

.display_world
    lda ob1X : sta X
    lda ob1Y : sta Y
    jsr new_small_m
    ;; lda ob2X : sta X
    ;; lda ob2Y : sta Y
    ;; jsr new_small_m
    rts

.animate
    jsr init_world
    jsr display_world ; on0
.aloop
    jsr vsync
    jsr raster_show_on
    jsr display_world ; off
    jsr update_world
    jsr display_world ; on
    jsr raster_show_off
    jmp aloop

.end
save "code", start, end
