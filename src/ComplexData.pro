
PRO ComplexDataOp, Menu=menu
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     print, "% PaddVol: Root pointer is invalid" 
     return
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     print, "% ComplexDataOp: current stack pointer is invalid" 
     return
  END
  e=(*c).datap
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     print, "% ComplexDataOp: current data pointer is invalid" 
     return
  END
  IF (NOT(((*e).type EQ 6) OR ((*e).type EQ 9))) THEN BEGIN
     print, "% ComplexDataOp: Current data is not complex valued." 
     return
  END
  IF keyword_set(menu) THEN BEGIN
;; ++++++++++++++++++++++++++++++
  ;; FFT parameters
  ;; ++++++++++++++++++++++++++++++
     selFn=["Real Part","Imaginary Part","Amplitude","Power","Complex Conjugate"]
     Op=XMChoiceDialog(selFn,"Choose Option")
     IF (Op EQ "") THEN BEGIN
        printtocon, "% ComplexDataOp: cancelled"
        return
     END  
  END
  ;;Create a new stack
  name=MyString(Op)+"("+(*e).id+")"
  pp=DataList_CreateElement(ptr, name)
  ;; make sure we will not get an overflow when adding data 
  (*pp).type=4
  CASE Op OF
     "Complex Conjugate": BEGIN
        (*pp).type=6
     END
     ELSE: (*pp).type=4
  END
  ;; get dimensions for the projection image stack dimensions
  (*pp).SzZ= (*e).SzZ
  (*pp).zsamp=(*e).zsamp 
  (*pp).SzX=(*e).SzX
  (*pp).SzY=(*e).SzY
  (*pp).xsamp=(*e).xsamp 
  (*pp).ysamp=(*e).xsamp
  ppd=(Data_GetEmptyArrayP(pp))
  IF NOT(PTR_VALID(ppd)) THEN BEGIN
     print, "% ComplexDataOp: Failed to create data array for the projected images." 
     return
  END
  (*pp).data=ppd
  ;; Don't accumulate errors, don't do incremental tilt
  i=0
  ;;
  Case Op OF
     "Real Part": BEGIN
        (*ppd)=Real_Part(*(*e).data)
        printtocon, "% ComplexDataOp: Extracting real part."
     END
     "Imaginary Part": BEGIN
        (*ppd)=Imaginary(*(*e).data)
        printtocon, "% ComplexDataOp: Extracting imaginary part."
     END
     "Amplitude": BEGIN
        (*ppd)=ABS(*(*e).data)
        printtocon, "% ComplexDataOp: Extracting amplitude."
     END
     "Power": BEGIN
        (*ppd)=((*(*e).data)*(*(*e).data))
        printtocon, "% ComplexDataOp: Extracting power."
     END
     "Complex Conjugate": BEGIN
        (*ppd)=Conj((*(*e).data))
        printtocon, "% ComplexDataOp: Extracting complex conjugate."
     END
     ELSE: BEGIN
        printtocon, "% ComplexDataOp: Unknown transformation ("+MyString(Op)+")"
        return
     END
  END
  ;; current stack is *pp, display the projections on the screen  
  (*pp).slice=(*e).slice
  (*pp).zcoord=(*e).zcoord
  (*pp).contrastmode="auto"
  CreateWindow
  TVDisplay
  Update_XTabControl
END 

