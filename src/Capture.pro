Pro Capture, p
;;
;; the screen capture routine
;; 
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    PrintToCon, "% Capture:  Fatal error "
    PrintToCon, "%   Error status  - " + STRING(error_status)
    PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
    XConsole_Idle
    CATCH, /Cancel
    ErrMsg, !ERROR_STATE.MSG
    return
END
IF Ptr_Valid(p) THEN BEGIN
   ;; get current window
   pdata=(*p).datap
   IF NOT(PTR_VALID(pdata)) THEN BEGIN
      PrintToCon, "% Capture:  Data pointer is invalid."
   END
    WSET, (*pdata).window
    image=tvrd(CHANNEL=1, TRUE=1)
    N=Size(image)
    dec=0
    DEVICE, GET_DECOMPOSED=dec
    if dec THEN BEGIN
       im=0.2989*image[0,*,*]+0.5870*image[1,*,*]+0.1140*image[2,*,*]
       ;; rgb vector, how to compose grayscale
       STOP
       ThrowImage, REFORM(im)
    END ELSE ThrowImage, image
    WSET, (*pdata).window
END
XConsole_Idle
return
END
