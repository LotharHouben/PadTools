FUNCTION Read3DData, filename, offset, DimX, DimY, DimZ, dtype, VERBOSE=verbose, SWAPENDIAN=swapendian
 error = 1
; CATCH, Error_status
 ;IF (Error_status NE 0) THEN BEGIN
 ;    Print, "% Read3DData:  Fatal error "
 ;    Print, "%   Error status  - " + STRING(error_status)
 ;    Print, "%   Error message - " + !ERROR_STATE.MSG
 ;    CATCH, /Cancel
 ;    return, 0
 ;END

;; XConsole_PushState
;; XConsole_Busy
  openr, LUN, filename, ERROR = o_err, /GET_LUN
  if (o_err NE 0 ) then begin
	print, "error while trying to open file " + filename+ ":" + !ERR_STRING
	return, 1
  endif 
 IF keyword_set(swapendian) THEN swapendian = 1 ELSE swapendian = 0


  case dtype of
	1 : BEGIN
             APtr=PTR_NEW(BYTARR(DimX,DimY,DimZ))
             i_data = ASSOC(LUN, BYTARR(DimX,DimY,DimZ),offset)	
            END
	2 : BEGIN
             APtr=PTR_NEW(INTARR(DimX,DimY,DimZ))
             i_data = ASSOC(LUN, INTARR(DimX,DimY,DimZ),offset)	
            END
	3 : BEGIN
             APtr=PTR_NEW(LONARR(DimX,DimY,DimZ))
             i_data = ASSOC(LUN, LONARR(DimX,DimY,DimZ),offset)	
            END
	4 : BEGIN
             APtr=PTR_NEW(FLTARR(DimX,DimY,DimZ))
             i_data = ASSOC(LUN, FLTARR(DimX,DimY,DimZ),offset)	
            END
	5 : BEGIN
             APtr=PTR_NEW(DBLARR(DimX,DimY,DimZ))
             i_data = ASSOC(LUN, DBLARR(DimX,DimY,DimZ),offset)	
            END
	6 : BEGIN
             APtr=PTR_NEW(COMPLEXARR(DimX,DimY,DimZ))
             i_data = ASSOC(LUN, COMPLEXARR(DimX,DimY,DimZ),offset)	
            END
	9 : BEGIN
             APtr=PTR_NEW(DCOMPLEXARR(DimX,DimY,DimZ))
             i_data = ASSOC(LUN, DCOMPLEXARR(DimX,DimY,DimZ),offset)	
            END
	12 : BEGIN
             APtr=PTR_NEW(UINTARR(DimX,DimY,DimZ))
             i_data = ASSOC(LUN, UINTARR(DimX,DimY,DimZ),offset)	
          END
        13 : BEGIN ;; Unsigned Long 32bit
             APtr=PTR_NEW(ULONARR(DimX,DimY,DimZ))
             i_data = ASSOC(LUN, ULONARR(DimX,DimY,DimZ),offset)	
          END
        14 : BEGIN ;; Long 64bit
             APtr=PTR_NEW(LON64ARR(DimX,DimY,DimZ))
             i_data = ASSOC(LUN, LON64ARR(DimX,DimY,DimZ),offset)	
          END
        15 : BEGIN ;; Unsigned Long 64bit
             APtr=PTR_NEW(ULON64ARR(DimX,DimY,DimZ))
             i_data = ASSOC(LUN, ULON64ARR(DimX,DimY,DimZ),offset)	
          END
	else: BEGIN
                print, "error: invalid data type " + MyString(dtype)
                close, LUN
                free_lun, LUN
                ;; XConsole_PopState
                return, 1 
              END
  endcase
;;STOP
;transfer data from file to memory
  if (swapendian EQ 1) then BEGIN
      *(APtr) = SWAP_ENDIAN(i_data(0))
  ENDIF ELSE BEGIN
      *(APtr) = i_data(0)
  ENDELSE
  *APtr=REFORM(*APtr,DimX,DimY,DimZ)
  close, LUN
  free_lun, LUN
;;   XConsole_PopState
 return, APtr
END

