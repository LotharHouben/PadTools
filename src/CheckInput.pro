Function CheckInput, in, MATCHLIST=matchlist, CAPITALIZE=capitalize, CLEARWHITESPACE=clearwhitespace
;; checks an input value or string
;; keywords:
;; MATCHLIST= an optianal list of values to match
;;                      if empty then the input value is not checked against a possible value
;;                      it is only capitalized and whitespaces are removed when keywords are set
;;
;; returns 1 if match and or the cleared value in in  
;;
;;
  IF keyword_set(CLEARWHITESPACE) THEN BEGIN
     in=STRTRIM(in,2)
  END
  IF keyword_set(capitalize) THEN BEGIN
     in=STRUPCASE(in)
  END
  IF keyword_set(matchlist) then BEGIN
     ;; try to match with list
     IF (WHERE(matchlist EQ in) LT 0) THEN return, 0
  END
  return, 1
END

;; Example
;; mlist=list('x','X','y','Y')
;; val='  z'
;; if CheckInput(val,matchlist=mlist,/CAPITALIZE, /CLEARWHITSPACE)
;; THEN print, "Match: return value is "+in ELSE print, "No match"

