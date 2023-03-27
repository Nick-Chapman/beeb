
import Asm
import qualified Data.ByteString as ByteString (pack,putStr)

main :: IO ()
main = do
  -- TODO: take origin on command line
  let bytes = assemble 0x2000 asm
  ByteString.putStr (ByteString.pack bytes)
 where

-- TODO: suprsing asm type
 asm = Asm.mdo

  mp <- allocateZP
  _ <- allocateZP -- comment out this line to break things
  ip <- allocateZP
  _ <- allocateZP

  lda (lo prog)
  sta (ZeroPage ip)
  lda (hi prog)
  sta (ZeroPage (ip+1))
  lda (lo mem)
  sta (ZeroPage mp)
  lda (hi mem)
  sta (ZeroPage (mp+1))
  ldy (immediate 0)
  lda (immediate 0)
  ldx (immediate 0xff)

  zeromem <- labelCode
  sta (IndexedX mem)
  dex
  bne zeromem
  sta (Absolute mem)
  jmp fetch

  advance <- labelEntry
  jsr incip

  fetch <- labelCode
  lda (IndirectY ip)
  bne left
  rts

  left <- labelEntry
  cmp_c '<' ; bne right
  dec (ZeroPage mp)
  jmp advance

  right <- labelEntry
  cmp_c '>' ; bne plus
  inc (ZeroPage mp)
  jmp advance

  plus <- labelEntry
  cmp_c '+' ; bne minus
  lda (IndirectY mp)
  clc
  adc (immediate 1)
  sta (IndirectY mp)
  jmp advance

  minus <- labelEntry
  cmp_c '-' ; bne comma
  lda (IndirectY mp)
  sec
  sbc (immediate 1)
  sta (IndirectY mp)
  jmp advance

  comma <- labelEntry
  cmp_c ',' ; bne dot
  jsr osrdch
  sta (IndirectY mp)
  jmp advance

  dot <- labelEntry
  cmp_c '.' ; bne open
  lda (IndirectY mp)
  cmp_c '\n' ; bne print
  clc
  adc (immediate 3)

  print <- labelCode
  jsr oswrch
  jmp advance

  open <- labelEntry
  cmp_c '[' ; bne close
  lda (IndirectY mp)
  bne advance
  ldx (immediate 1)

  forward <- labelCode
  jsr incip
  lda (IndirectY ip)
  cmp_c '[' ; bne forward2
  inx
  jmp forward

  forward2 <- labelEntry
  cmp_c ']' ; bne forward
  dex
  bne forward
  jmp advance

  close <- labelEntry

  cmp_c ']' ; bne advance
  lda (IndirectY mp)
  beq done
  ldx (immediate 1)

  backward <- labelCode
  jsr decip
  lda (IndirectY ip)
  cmp_c ']' ; bne backward2
  inx
  jmp backward

  backward2 <- labelEntry
  cmp_c '[' ; bne backward
  dex
  bne backward
  done <- labelCode
  jmp advance

  incip <- labelEntry
  inc (ZeroPage ip)
  lda (ZeroPage ip)
  bne incip2
  inc (ZeroPage (ip+1))
  incip2 <- labelCode
  rts

  decip <- labelEntry
  lda (ZeroPage ip)
  bne decip2
  dec (ZeroPage (ip+1))
  decip2 <- labelCode
  dec (ZeroPage ip)
  rts

  prog <- labelData
  equs ">++++++++++>+>+[[+++++[>++++++++<-]>.<++++++[>--------<-]+<<<]>.>>[[-]<[>+<-]>>[<<+>+>-]<[>+<-[>+<-[>+<-[>+<-[>+<-[>+<- [>+<-[>+<-[>+<-[>[-]>+>+<<<-[>+<-]]]]]]]]]]]+>>>]<<<]"
  equb [0]

  mem <- labelData
  Asm.pure ()

osrdch :: MemAddr a -- TODO: type to capture these routines
osrdch = 0xffe0

oswrch :: MemAddr a
oswrch = 0xffe3
