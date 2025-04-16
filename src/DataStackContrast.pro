FUNCTION CumulativeHistogramEqual, image, MINTH=minth, MAXTH=maxth
  IF not(keyword_set(minth)) THEN minth=0.1
  IF not(keyword_set(maxth)) THEN maxth=0.9
  h=histogram(ALOG(image),MIN=0,bin=0.1) ;; log scale histogram
  h=h/Total(h)
  hcumu=Total(h,/CUMULATIVE)
  res=WHERE((hcumu GT minth) AND (hcumu LT maxth), count)
  IF (count GT 0) THEN BEGIN
     return, exp(1)^[res[0]*0.1,res[N_Elements(res)-1]*0.1]
  END ELSE BEGIN
     printtocon, "% CumulativeHistogramEqual: Error during histogram equalization"
     return, [0.,1.]
  END
END


PRO SetStackContrastValues, hilo, STACKP=stackp 
;;
;; STACKP: can be used to choose a stack different from the current stack
;; 
;; HILO: array of hi and lo cut-off values
;;
IF NOT(GetDebugFlag()) THEN BEGIN
   CATCH, Error_status
   IF (Error_status NE 0) THEN BEGIN
      Printtocon, "% SetStackContrastMode: Fatal error "
      Printtocon, "% Error status - " + STRING(error_status)
      Printtocon, "% Error message - " + !ERROR_STATE.MSG 
      CATCH, /Cancel
      return
END 
END  
IF NOT(keyword_set(stackp)) THEN BEGIN
   ptr=GetRootP()
   IF PTR_VALID(ptr) THEN BEGIN
      stackp=(*ptr).current
   END
END
IF (PTR_VALID(stackp) AND PTR_VALID((*stackp).datap)) THEN BEGIN
   (*(*stackp).datap).contrast=hilo
END ELSE BEGIN
   ;; no current stack, revert to defaults
   PrintToCon, "% SetStackContrastValues: Invalid stack pointer. Ignoring."
END
END 


FUNCTION GetStackContrastValues, STACKP=stackp 
;; returns low and high cut-off values
;; HILO: if return value is 'mode' the hilo contains a two element
;;       array of hi and lo cut-off values 
IF NOT(GetDebugFlag()) THEN BEGIN
   CATCH, Error_status
   IF (Error_status NE 0) THEN BEGIN
      Printtocon, "% GetStackContrastMode: Fatal error "
      Printtocon, "% Error status - " + STRING(error_status)
      Printtocon, "% Error message - " + !ERROR_STATE.MSG 
      CATCH, /Cancel
return, 'auto'
END 
END 
IF NOT(keyword_set(stackp)) THEN BEGIN
   ptr=GetRootP()
   IF PTR_VALID(ptr) THEN BEGIN
      stackp=(*ptr).current
   END
END
IF PTR_VALID(stackp) THEN pp=(*stackp).datap
IF PTR_Valid(pp) THEN BEGIN
   return, (*pp).contrast
END ELSE BEGIN
   ;; no current stack, revert to defaults
   PrintToCon, "% GetStackContrastValues: Invalid stack pointer, returning default values."
   return, [0,1]
END
END 



FUNCTION GetStackContrastMode, STACKP=stackp, SUBMODE=submode, ROI=roi, SRANGE=srange, HILO=hilo 
;; returns 'auto' or 'manual'
;; STACKP: can be used to choose a stack different from the current stack
;; SUBMODE: if return value is 'auto' the submode vcan take the values
;;          'sdev', 'mdev' or 'minmax' or 'diff' or histequal
;; ROI: can take the values 'quarter' or 'full' or 'sparse'
;; SRANGE: for submode 'sdev' the multiplicator
;; HILO: if return value is 'mode' the hilo contains a two element
;;       array of hi and lo cut-off values
;;
;; NOTE -- All data is obtained from the meta data that is going along with the data stack
;;         None of the data is read from the user interface here
;;         The user interface is responsible to update the meta data first using the
;;         SetStackContrastMode and SetStackContrastValues first
;;  
IF NOT(GetDebugFlag()) THEN BEGIN
   CATCH, Error_status
   IF (Error_status NE 0) THEN BEGIN
      Printtocon, "% GetStackContrastMode: Fatal error "
      Printtocon, "% Error status - " + STRING(error_status)
      Printtocon, "% Error message - " + !ERROR_STATE.MSG 
      CATCH, /Cancel
return, 'auto'
END 
END 
IF NOT(keyword_set(stackp)) THEN BEGIN
   ptr=GetRootP()
   IF PTR_VALID(ptr) THEN BEGIN
      stackp=(*ptr).current
   END
END
IF (PTR_VALID(stackp) AND PTR_VALID((*stackp).datap)) THEN BEGIN
     pp=(*stackp).datap
     IF (keyword_set(sdevrange)) THEN sdevrange=(*pp).contrastsdev
     IF (keyword_set(hilo)) THEN hilo=(*pp).contrast
     IF (keyword_set(roi)) THEN roi=(*pp).contrastroi
     IF (keyword_set(submode)) THEN submode=(*pp).contrastsubmode
     return, (*pp).contrastmode
END ELSE BEGIN
   ;; no current stack, revert to defaults
   PrintToCon, "% GetStackContrastMode: Invalid stack pointer, returning default values."
   IF (keyword_set(hilo)) THEN hilo=[0.,1.]
   IF (keyword_set(sdevrange)) THEN sdevrange=2.
   IF (keyword_set(roi)) THEN roi='quarter'
   IF (keyword_set(submode)) THEN submode='minmax'
   return, 'auto'
END 
END 


PRO SetStackContrastMode, STACKP=stackp, AUTO=auto, MANUAL=manual, SDEV=sdev, MDEV=mdev, MINMAX=minmax, QUARTER=quarter, FULL=full, SPARSE=sparse, SRANGE=srange, HILO=hilo, DIFF=diff, HISTEQUAL=histequal
;; Set info contrast info data in data stack structure field
;; Auto: auto contrast
;; Auto-Submodes: sdev 
IF NOT(GetDebugFlag()) THEN BEGIN
;; then the error handler will be ignored and the program stops at the ;; error, here in the line x=(*p)
;;
;; install the error handler if the debug flag is not set
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
;; on error print the error in the console ...
Printtocon, "% SetStackContrastMode: Fatal error "
Printtocon, "% Error status - " + STRING(error_status)
Printtocon, "% Error message - " + !ERROR_STATE.MSG ;; and display a dialog that the user has to acknowledge 
;; deinstall the error handler
CATCH, /Cancel
;; and return immediately to the calling level
return
END 
END 
IF NOT(keyword_set(stackp)) THEN BEGIN
   ptr=GetRootP() 
   p=(*ptr).current 
   pp=(*p).datap
END ELSE pp=(*stackp).datap
IF keyword_set(manual) THEN (*pp).contrastmode='manual'
IF keyword_set(auto) THEN (*pp).contrastmode='auto'
IF keyword_set(sdev) THEN  (*pp).contrastsubmode='sdev'
IF keyword_set(mdev) THEN  (*pp).contrastsubmode='mdev'
IF keyword_set(histequal) THEN  (*pp).contrastsubmode='histequal'
IF keyword_set(minmax) THEN  (*pp).contrastsubmode='minmax'
IF keyword_set(diff) THEN  (*pp).contrastsubmode='diff'
IF keyword_set(quarter) THEN  (*pp).contrastroi='quarter'
IF keyword_set(full) THEN  (*pp).contrastroi='full'
IF keyword_set(sparse) THEN  (*pp).contrastroi='sparse'
IF keyword_set(diff) THEN  (*pp).contrastroi='diff'
IF keyword_set(srange) THEN  (*pp).contrastroi=ABS(FLOAT(srange))
IF keyword_set(hilo) THEN  BEGIN
   (*pp).contrastmode='manual'
   (*pp).contrast=hilo
END
return
END


FUNCTION SetAutoContrast, image, ROI=roi, FRAMEROI=frameroi, SUBMODE=submode, SRANGE=sd
  ;;
  ;; set hi lo values for contrast cut off
  ;;
  ;; This routine is called in two instamces only
  ;; (1) in 'auto' mode
  ;; (2) in manual mode when the user called the auto scaling to minmax or diff
  ;;
  lo=0. & hi=1. ;; don't change values if something fails
  hilo=[lo,hi]
  IF NOT(keyword_set(submode)) THEN submode='sdev'
  IF NOT(keyword_set(sd)) THEN sd=2.
  IF NOT(keyword_set(frameroi)) THEN frameroi='quarter'
  IF not(keyword_set(roi)) THEN BEGIN
     N=SIZE(image, /DIMENSION, /L64)
     CASE frameroi OF
        'sparse': BEGIN
           ;; determine sparse grid 
           Npix=N(0)*N(1)
           IF (NPix GT 1024) THEN BEGIN
              divisor=CEIL((ALOG(NPix)))
              Ind=FINDGEN(FLOOR(N(1)*N(2)/FLOAT(divisor)))*divisor
              ctrsimage=reform(image, NPix)
              ctrsimage=ctrsimage(Ind)
           END ELSE ctrsimage=image
           
           ctrsimage=image(Randcoord)
        END
        'quarter': BEGIN
           roi=[ROUND(N(0)/4.),ROUND(N(0)/4.)+ROUND(N(0)/2.)-1,ROUND(N(1)/4.),ROUND(N(1)/4.)+ROUND(N(1)/2.)-1]
           ctrsimage=image[roi(0):roi(1),roi(2):roi(3)]
        END
        'diff': BEGIN
           ;; exclude central quarter
           mask=BYTARR(N[0],N[1])
           roi=[ROUND(1*N(0)/8.),ROUND(1*N(0)/8.)+ROUND(3*N(0)/4.)-1,ROUND(1*N(1)/8.),ROUND(1*N(1)/8.)+ROUND(3*N(1)/4.)-1]
           mask[roi(0):roi(1),roi(2):roi(3)]=1
           roi=[ROUND(7*N(0)/16.),ROUND(7*N(0)/16.)+ROUND(N(0)/8.)-1,ROUND(7*N(1)/16.),ROUND(7*N(1)/16.)+ROUND(N(1)/8.)-1]
           mask[roi(0):roi(1),roi(2):roi(3)]=0           
          
           ctrsimage=image[WHERE(mask EQ 1)]
        END
        ELSE: ctrsimage=image
     END
  END ELSE BEGIN
     ctrsimage=image[roi(0):roi(1),roi(2):roi(3)]
  END ;; not(keyword_set(roi))
  CASE submode OF
     'minmax': BEGIN
        lo=MIN(ctrsimage,MAX=hi)
        hilo=[lo,hi]
     END
     'sdev': BEGIN
        result=Moment(ctrsimage,MAXMOMENT=2)
        sdev=Sqrt(result(1))
        lo=result(0)-sd*sdev
        hi=result(0)+sd*sdev
        hilo=[lo,hi]
     END
     'diff': BEGIN
        result=Moment(ctrsimage,MAXMOMENT=2)
        sdev=Sqrt(result(1))
        hi=result(0)+sd*sdev
        lo=0.
        hilo=[lo,hi]
     END
     'mdev': BEGIN
        result=Moment(ctrsimage,MDEV=mdev)
        lo=result(0)-mdev
        hi=result(0)+mdev
        hilo=[lo,hi]
     END
     'histequal': BEGIN
        hilo=CumulativeHistogramEqual(ctrsimage, MINTH=0.1, MAXTH=0.95)
     END
     ELSE: BEGIN
        ;; unknown submode
        lo=MIN(ctrsimage,MAX=hi)
        hilo=[lo,hi]
     END
  END
  ;;print, "% EvaluateContrast: Setting contrast values to ", [lo,hi]
  return, hilo
END 
