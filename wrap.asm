;;; Wraps a binary file as a disc image
org &2000
.start
incbin BinFile
.end
save "Code", start, end
