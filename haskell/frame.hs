
import Prelude hiding (pure,and)
import Asm
import Data.Word (Word8)
import qualified Data.ByteString as ByteString (pack,putStr)

main :: IO ()
main = ByteString.putStr (ByteString.pack bytes)

bytes :: [Word8]
bytes = assemble 0x2000 $ Asm.mdo

  msgPtr <- allocateZP
  _ <- allocateZP -- second byte for the pointer
  -- TODO: want a type error if fails to allocate two ZP bytes for a ptr
  -- and so it overlaps with the following allocation
  frameCount <- allocateZP

  let
    -- TODO: can we put a type annotation here?
    puts str = Asm.mdo
      copy16i msg msgPtr
      jmp after
      msg <- labelData; equs str; equb [0]
      after <- labelEntry
      jsr printMessage

  jmp main
  --jmp main -- TEST: Is type error for unreachable code. Good

  --frameCount <- labelData; equb [0]

  _mos_syncVB <- labelEntry
  lda (immediate 19)
  jmp osbyte
  --rts -- TEST: bug if added

  _mode1 <- labelEntry
  lda (immediate 22); jsr oswrch
  lda (immediate 1) ; jsr oswrch
  rts

  _cursorOff <- labelEntry
  lda (immediate 23); jsr oswrch
  lda (immediate 1) ; jsr oswrch
  lda (immediate 0) ; jsr oswrch
  lda (immediate 0) ; jsr oswrch
  lda (immediate 0) ; jsr oswrch
  lda (immediate 0) ; jsr oswrch
  lda (immediate 0) ; jsr oswrch
  lda (immediate 0) ; jsr oswrch
  lda (immediate 0) ; jsr oswrch
  lda (immediate 0) ; jsr oswrch
  rts

  printMessage <- makePrintMessage msgPtr

  main <- labelEntry

  jsr _mode1
  jsr _cursorOff

  loop <- labelCode
  jsr _mos_syncVB
  position 1 1; puts "Frame : "; lda (ZeroPage frameCount); jsr printHexA
  inc (ZeroPage frameCount)
  jmp loop

  printHexA <- makePrintHexA

  pure ()

makePrintHexA :: Asm ('Data v1) ('Data v2) (MemAddr (State a x y s))
makePrintHexA = Asm.mdo
  entry <- labelEntry
  pha ; lda (immChar '['); jsr osasci
  pla -- TODO: comment should be type err -- but not because osasci is too unspecific
  pha
  and (immediate 0xf0)
  lsr_a; lsr_a; lsr_a; lsr_a; tax
  lda (IndexedX digits)
  jsr osasci
  pla
  and (immediate 0x0f); tax
  lda (IndexedX digits)
  jsr osasci
  pha; lda (immChar ']'); jsr osasci; pla
  --pha -- TEST: this is a type error
  rts
  digits <- labelData
  equs "0123456789abcdef"
  pure entry

makePrintMessage :: ZpAddr a -> Asm ('Data v1) ('Data v2) (MemAddr ('Code ('Cpu a x1 o ('Cons ('ReturnAddr ('Cpu a x1 ('Data Word8) s)) s))))
makePrintMessage msgPtr = Asm.mdo
  entry <- labelEntry
  ldy (immediate 0)
  loop <- labelCode
  lda (IndirectY msgPtr)
  beq done
  jsr osasci
  iny
  bne loop
  done <- labelCode
  rts
  pure entry

position :: Word8 -> Word8 -> Asm (State a1 x1 y1 s1) (State a2 x2 y2 s2) ()
position x y = Asm.do
  lda (immediate 31); jsr osasci
  lda (immediate x); jsr osasci
  lda (immediate y); jsr osasci

osasci,oswrch,osbyte :: MemAddr (State a1 x1 y1 s1)
osasci = 0xffe3
oswrch = 0xffee
osbyte = 0xfff4


copy16i :: MemAddr v -> ZpAddr a -> Asm (State o x y s) (State a x y s) ()
copy16i a v = Asm.do
  lda (lo a) ; sta (ZeroPage v)
  lda (hi a) ; sta (ZeroPage (v+1))
