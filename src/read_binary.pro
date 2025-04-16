
PRO ReadImageToArray, FileName, A, Offset, flag
;;
;; reads image data from a file into an array
;;
;; FileName = String - complete filename inluding path
;; A        = Array - dimensions and type must be set!
;; Offset   = Integer - number of bytes to skip
;; flag     = Boolean - specifies, whether bytes will be 
;;                                 swapped (1) or not (0)
;;
  IF NOT(GetDebugFlag()) THEN BEGIN
     CATCH, Error_status
     IF (Error_status NE 0) THEN BEGIN
        PrintToCon, "% ReadImageToArray:  Fatal error "
        PrintToCon, "%   Error status  - " + STRING(error_status)
        PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
        CATCH, /Cancel
        ;; if ((Imlun GE 99) AND (Imlun LE 128)) THEN FREE_LUN, Imlun
        return
     END
  END 
  OPENR, Imlun, FileName, /GET_LUN
  POINT_LUN, Imlun, Offset
  ;; printtocon, '% ReadImageToArray: reading ' + FileName
  READU, Imlun, A
  FREE_LUN, Imlun
  IF flag THEN A = Swap_Endian(A)
  return
END

