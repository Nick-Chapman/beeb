OSRDCH = &ffe0
OSWRCH = &ffe3

mp = &70
ip = &72

org $2000

.start
    lda #(prog MOD 256)
    sta ip
    lda #(prog DIV 256)
    sta ip+1
    lda #(mem MOD 256)
    sta mp
    lda #(mem DIV 256)
    sta mp+1
    ldy #0
    lda #0
    ldx #&FF
.zeromem
    sta mem,X
    dex
    bne zeromem
    sta mem
    jmp fetch
.advance
    jsr incip
.fetch
    lda (ip),Y
    bne left
    rts
.left
    cmp #ASC("<") : bne right
    dec mp
    jmp advance
.right
    cmp #ASC(">") : bne plus
    inc mp
    jmp advance
.plus
    cmp #ASC("+") : bne minus
    lda (mp),Y:clc:adc#1:sta (mp),Y
    jmp advance
.minus
    cmp #ASC("-") : bne comma
    lda (mp),Y:sec:sbc#1:sta (mp),Y
    jmp advance
.comma
    cmp #ASC(",") : bne dot
    jsr OSRDCH
    sta (mp),Y
    jmp advance
.dot
    cmp #ASC(".") : bne open
    lda (mp),Y
    jsr OSWRCH

.open
    cmp #ASC("[") : bne close
    lda (mp),Y
    bne advance
    ldx #1
.forward
    jsr incip
    lda (ip),Y
    cmp #ASC("[") : bne forward2
    inx
    jmp forward
.forward2
    cmp #ASC("]") : bne forward
    dex
    bne forward
    jmp advance

.close
    cmp #ASC("]") : bne advance
    lda (mp),Y
    beq advance
    ldx #1
.backward
    jsr decip
    lda (ip),Y
    cmp #ASC("]") : bne backward2
    inx
    jmp backward
.backward2
    cmp #ASC("[") : bne backward
    dex
    bne backward
    jmp advance
.incip
    inc ip
    lda ip
    bne incip2
    inc ip+1
.incip2
    rts
.decip
    lda ip
    bne decip2
    dec ip+1
.decip2
    dec ip
    rts
.prog
EQUS ">++++++++++>+>+[[+++++[>++++++++<-]>.<++++++[>--------<-]+<<<]>.>>[[-]<[>+<-]>>[<<+>+>-]<[>+<-[>+<-[>+<-[>+<-[>+<-[>+<- [>+<-[>+<-[>+<-[>[-]>+>+<<<-[>+<-]]]]]]]]]]]+>>>]<<<]"
EQUB 0
.end
.mem

save "code", start, end
