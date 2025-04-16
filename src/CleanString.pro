FUNCTION CleanString, x
  if (N_ELEMENTS(x) EQ 0) THEN return, ""
 N=SIZE(x)
 IF (N[0] GT 0) THEN return, "" ;; no vector context
 IF (N[1] EQ 7) THEN BEGIN
    IF (x EQ '') THEN return, x ;; empty STRING
 END
 return, STRCOMPRESS(STRING(x, /PRINT), /REMOVE_ALL)
END
