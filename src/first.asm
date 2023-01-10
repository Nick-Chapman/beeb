
; First _from-scratch_ example

osasci = &FFE3

org &2000

.start:
    lda #'N'
    jsr osasci
.end:
    rts

save "code", start, end
