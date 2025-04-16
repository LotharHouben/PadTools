FUNCTION READASCIILIST, filename 
  l=list()
  openr, unit, filename, ERROR = o_err, /GET_LUN
  if (o_err NE 0 ) then begin
     printtocon, "Error while trying to open file " + filename+ ":" + !ERR_STRING
     return, l
  END
  str = ''
  count = 0ll
  WHILE ~ EOF(unit) DO BEGIN
     READF, unit, str
     l.add, str
     count = count + 1
  ENDWHILE
  FREE_LUN, unit
  RETURN, l
  CLOSE, unit
END


FUNCTION ReadBlockDataToList, filename, TOARRAY=toarray, HEADER=header, SEPARATOR=separator, TOFLOAT=tofloat
  if not(keyword_set(separator)) THEN separator=" "
  l=list()
  openr, unit, filename, ERROR = o_err, /GET_LUN
  if (o_err NE 0 ) then begin
     printtocon, "Error while trying to open file " + filename+ ":" + !ERR_STRING
     return, l
  END
  str = ''
  count = 0ll
  if keyword_set(header) THEN BEGIN
     IF ~ EOF(unit) THEN BEGIN
        ;; read a header line
        READF, unit, str
        header=STRSPLIT(str,separator,/EXTRACT)
        count += 1
     END
  END
  ;; now read data
  WHILE ~ EOF(unit) DO BEGIN
     READF, unit, str
     IF keyword_set(tofloat) THEN BEGIN
       l.add, FLOAT(STRSPLIT(STRTRIM(str,2),separator,/EXTRACT))
     END ELSE BEGIN
        l.add, List(STRSPLIT(STRTRIM(str,2),separator,/EXTRACT),/EXTRACT)
     END
     count = count + 1
  ENDWHILE
  FREE_LUN, unit
  CLOSE, unit
  IF keyword_set(toarray) THEN RETURN, l.ToArray() ELSE RETURN, l
END
