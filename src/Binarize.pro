PRO BinarizeStack
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    Printtocon, "% BinarizeStack:    Fatal error "
    Printtocon, "%   Error status  - " + STRING(error_status)
    Printtocon, "%   Error message - " + !ERROR_STATE.MSG
    ErrMsg, !ERROR_STATE.MSG
    CATCH, /Cancel
    XConsole_PopState
    return
END
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     printtocon, "% BinarizeStack: Root pointer is invalid" 
     return
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% BinarizeStack: current stack pointer is invalid" 
     return
  END
  e=(*c).datap
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% BinarizeStack: current data pointer is invalid" 
     return
  END
  ;;
  name="Binary("+(*c).name+")"
  printtocon, "% BinarizeStack: creating " + name 
  ;; STOP
  pp=DataList_CreateElement(ptr, name)
  ;; make sure we will not get an overflow when adding data 
  (*pp).type=1 ;; Byte type
  (*pp).SzX=(*e).SzX
  (*pp).SzY=(*e).SzY
  (*pp).SzZ=(*e).SzZ
  (*pp).xsamp=(*e).xsamp
  (*pp).ysamp=(*e).ysamp
  (*pp).zsamp=(*e).zsamp
  (*pp).slice=(*e).slice
  (*pp).zcoord=(*e).zcoord
  (*pp).contrastmode="auto"
  ppd=(Data_GetEmptyArrayP(pp))
  IF NOT(PTR_VALID(ppd)) THEN BEGIN
     printtocon, "% BinarizeStack: failed to create data array." 
     return
  END
  (*pp).data=ppd
  CreateWindow
  TVDisplay
  Update_XTabControl
  a=STRARR(2,1)
  a[0,0]="Threshold value "
  a[1,0]=MyString((*e).contrast[0])
  IF (XSimpleDataField(a, TITLE="Threshold for Binarization") EQ 1) THEN BEGIN
     threshold=FLOAT(a[1,0])
     ;; initialize progress bar
     pminv=[1] & pmaxv=[(*e).SzX] & ptext=["Binarizing Image Nr."]
     InitProgressBar, MINV=pminv, MAXV=pmaxv, TEXT=ptext
     tmp=TEMPORARY(REFORM((*(*e).data)[0,*,*]))
     For i=0,((*e).SzX-1) DO BEGIN
        ProgressBarSet, 0, i, TEXT=MyString(i)
        tmp=REFORM((*(*e).data)[i,*,*])
        B=WHERE(tmp GE threshold, COMPLEMENT=C)
        tmp(B)=1 & tmp(C)=0
        (*(*pp).data)[i,*,*]=BYTE(tmp)
     END
     DestroyProgressBar
  END
END
