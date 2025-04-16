PRO ScalarArithmetics, e, LOG=log
    IF keyword_Set(log) THEN BEGIN
        ;; calculate log values
        ;; data may contain zeros
        eps=1E-12
        B=WHERE((*e) GT eps, count, COMPLEMENT=comp)
        IF (count GT 0) THEN BEGIN
           (*e)[B]=ALOG(FLOAT((*e)[B])+1.)
           (*e)[comp]=0.
        END 
     END
END
   

PRO ContrastChange,NEGATIVE=negative, MINMAX=minmax, INVERT=invert, MAXTOZERO=maxtozero, LOG=log
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    Printtocon, "% ContrastChange:    Fatal error "
    Printtocon, "%   Error status  - " + STRING(error_status)
    Printtocon, "%   Error message - " + !ERROR_STATE.MSG
    ErrMsg, !ERROR_STATE.MSG
    CATCH, /Cancel
    XConsole_PopState
    return
END
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     printtocon, "% ContrastChange: Root pointer is invalid" 
     return
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% ContrastChange: current stack pointer is invalid" 
     return
  END
  e=(*c).datap
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% ContrastChange: current data pointer is invalid" 
     return
  END
  name=(*c).name
  IF keyword_Set(negative) THEN name="Negative("+(*c).name+")"
  IF keyword_Set(minmax) THEN name="Minmax("+(*c).name+")"
  IF keyword_Set(invert) THEN name="Inverse("+(*c).name+")"
  IF keyword_Set(maxtozero) THEN name="MaxToZero("+(*c).name+")"
  IF keyword_Set(log) THEN name="Log("+(*c).name+")"
  printtocon, "% ContrastChange: creating " + name 
  ;; STOP
  pp=DataList_CreateElement(ptr, name)
  ;; make sure we will not get an overflow when adding data 
  (*pp).type=4 ;; floating type
  (*pp).SzX=(*e).SzX
  (*pp).SzY=(*e).SzY
  (*pp).SzZ=(*e).SzZ
  (*pp).xsamp=(*e).xsamp
  (*pp).ysamp=(*e).ysamp
  (*pp).zsamp=(*e).zsamp
  (*pp).slice=(*e).slice
  (*pp).zcoord=(*e).zcoord
  (*pp).contrastmode="auto"
  IF NOT(keyword_set(nodatacopy)) THEN BEGIN
     ppd=(Data_GetEmptyArrayP(pp))
     IF NOT(PTR_VALID(ppd)) THEN BEGIN
        printtocon, "% InvertStack: failed to create data array." 
        return
     END
     (*pp).data=ppd
     IF keyword_Set(negative) THEN BEGIN
        ;; determine min and max
        ;; preserve mean value
        minstack=Min((*(*e).data), MAX=maxstack)
        meanstack=MEAN((*(*e).data))
        (*ppd)=maxstack+meanstack-FLOAT((*(*e).data))
     END 
     IF keyword_Set(minmax) THEN BEGIN
        ;; determine min and max
        minstack=Min((*(*e).data), MAX=maxstack)
        (*ppd)=FLOAT((*(*e).data))-minstack
     END 
     IF keyword_Set(maxtozero) THEN BEGIN
        ;; determine min and max
        maxstack=Max((*(*e).data))
        B=WHERE((*(*e).data) EQ maxstack)
        (*ppd)=FLOAT((*(*e).data))
        (*ppd)[B]=0.
     END 
     IF keyword_Set(invert) THEN BEGIN
        ;; determine min and max
        (*ppd)=1./(FLOAT((*(*e).data)))
     END 
     IF keyword_Set(log) THEN BEGIN
        ;; calculate log values
        ;; data may contain zeros
        eps=1E-12
        B=WHERE((*(*e).data) GT eps, count, COMPLEMENT=comp)
        IF (count GT 0) THEN BEGIN
           (*ppd)[B]=ALOG(FLOAT((*(*e).data)[B])+1.)
           (*ppd)[comp]=0.
        END 
     END 
  END
  current=(*ptr).current
  (*current).name=name
  CreateWindow
  TVDisplay
  Update_XTabControl
END 


PRO ContrastReplace, MEANFORZERO=meanforzero
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    Printtocon, "% ContrastReplace:    Fatal error "
    Printtocon, "%   Error status  - " + STRING(error_status)
    Printtocon, "%   Error message - " + !ERROR_STATE.MSG
    ErrMsg, !ERROR_STATE.MSG
    CATCH, /Cancel
    XConsole_PopState
    return
END
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     printtocon, "% ContrastReplace: Root pointer is invalid" 
     return
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% ContrastReplace: current stack pointer is invalid" 
     return
  END
  e=(*c).datap
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% ContrastReplace: current data pointer is invalid" 
     return
  END
  name=(*c).name
  name="ContrastReplace("+(*c).name+")"
  printtocon, "% ContrastReplace: creating " + name 
  ;; STOP
  pp=DataList_CreateElement(ptr, name)
  ;; make sure we will not get an overflow when adding data 
  (*pp).type=4 ;; floating type
  (*pp).SzX=(*e).SzX
  (*pp).SzY=(*e).SzY
  (*pp).SzZ=(*e).SzZ
  (*pp).xsamp=(*e).xsamp
  (*pp).ysamp=(*e).ysamp
  (*pp).zsamp=(*e).zsamp
  (*pp).slice=(*e).slice
  (*pp).zcoord=(*e).zcoord
  (*pp).contrastmode="auto"
  IF NOT(keyword_set(nodatacopy)) THEN BEGIN
     ppd=(Data_GetEmptyArrayP(pp))
     IF NOT(PTR_VALID(ppd)) THEN BEGIN
        printtocon, "% ContrastReplace: failed to create data array." 
        return
     END
     (*pp).data=ppd
     IF keyword_Set(meanforzero) THEN BEGIN
        ;; replace zeros by slice mean for each slice
        For k=0,(*e).SzX DO BEGIN
           im=REFORM((*(*e).data)[k,*,*])
           Zeroes=WHERE(im EQ 0, cnt, COMPLEMENT=nonzeroes)
           IF (cnt GT 0) THEN BEGIN
              meanslice=MEAN((*(*e).data)[nonzeroes])
              im[zeroes]=meanslice
           END
           (*ppd)[k,*,*]=im
        END
     END 
    
  END
  current=(*ptr).current
  (*current).name=name
  CreateWindow
  TVDisplay
  Update_XTabControl
END 
