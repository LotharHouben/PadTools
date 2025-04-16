PRO StackArithmetics
  IF (GetDebugFlag()) THEN BEGIN
  END ELSE BEGIN
     CATCH, Error_status
     IF (Error_status NE 0) THEN BEGIN
        Print, "% StackArithmetics: Fatal error "
        Print, "%   Error status  - " + STRING(error_status)
        Print, "%   Error message - " + !ERROR_STATE.MSG
        ErrMsg, !ERROR_STATE.MSG
        CATCH, /Cancel
        return
     END
  END
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     printtocon, "% StackArithmetics: Root pointer is invalid" 
     return
  END
  ;; ++++++++++++++++++++++++++++++++
  ;; update the image list 
  ;; first select data pointer
  ;; ++++++++++++++++++++++++++++++++
  pdata=XListSelector(Title="Data stack A: "); 
  IF (NOT(PTR_VALID(pdata))) THEN BEGIN
     printtocon, "% StackArithmetics: Pointer to data stack A is invalid." 
     return
  END
  data=(*pdata).datap
  ;; +++++++++++++++++++++++++++++++++++++++++
  ;; and determine the projection data pointer
  ;; +++++++++++++++++++++++++++++++++++++++++
  pproj=XListSelector(Title="Data stack B: "); 
  IF (NOT(PTR_VALID(pproj))) THEN BEGIN
     printtocon, "% StackArithmetics: Pointer to data stack B is invalid" 
     return
  END
  proj=(*pproj).datap
  ;; ++++++++++++++++++++++++++++++++++
  ;; check sizes
  ;; convention: image index is index 0
  ;; ++++++++++++++++++++++++++++++++++
  IF ((*proj).SzX NE (*data).SzX) THEN BEGIN
     printtocon, "% StackArithmetics: Mismatch in x-dimension." 
     printtocon, "%      returning"
     return
  END
  IF ((*proj).SzY NE (*data).SzY) THEN BEGIN
     printtocon, "% StackArithmetics: Mismatch in y-dimension." 
     printtocon, "%      returning"
     return
  END
  IF ((*proj).SzZ NE (*data).SzZ) THEN BEGIN
     printtocon, "% StackArithmetics: Mismatch in z-dimension." 
     printtocon, "%      returning"
     return
  END
  ;;
  ;; ++++++++++++++++++++++++++++++
  ;; operator parameters
  ;; ++++++++++++++++++++++++++++++
  selFn=["+","-","/","*"]
  Op=XMChoiceDialog(selFn,"Choose Arithmetic Operator")
  IF (Op EQ "") THEN BEGIN
     printtocon, "% StackArithmetics: cancelled"
     return
  END
  ;; ---------------------------
  ;; initialize progress bar
  ;; ---------------------------
  pminv=[0] & pmaxv=[1] & ptext=["Processing slice"]
  InitProgressBar, MINV=pminv, MAXV=pmaxv, TEXT=ptext
  ;; ---------------------------
  ;; Contrast and Correlation ROI
  ;; ---------------------------
  identifier="("+(*pdata).name+Op+(*pproj).name+")"
  pp=DataList_CreateElement(ptr, identifier)
  ;; set the dimensions of the data array
  (*pp).SzX=(*data).SzX & (*pp).SzY=(*data).SzY & (*pp).SzZ=(*data).SzZ
  ;; set the data type, here: float
  (*pp).type=4
  ;; allocate memory and get pointer ppd to the array
  ppd=(Data_GetEmptyArrayP(pp))
  IF NOT(PTR_VALID(ppd)) THEN BEGIN
     print, "% StackArithmetics: Failed to create data array." 
     print, "%     Returning." 
     return
  END
  ;; strore array pointer in data container
  (*pp).data=ppd
  ;; now process the data 
  CASE Op OF
     '+': (*ppd)=(*(*data).data)+(*(*proj).data)
     '-': (*ppd)=(*(*data).data)-(*(*proj).data)
     '/': (*ppd)=(*(*data).data)/(*(*proj).data)
     '*': (*ppd)=(*(*data).data)*(*(*proj).data)
     ELSE: BEGIN
        print, "% StackArithmetics: Failed to create data array." 
        print, "%     Returning." 
     END
  END
  ;;
  (*pp).zcoord=(*data).zcoord
  ;; choose center slice for display
  (*pp).slice=(*data).slice
  ;; leave contrastmode as 'auto'
  (*pp).contrastmode="auto"
  ;; create the disply window
  CreateWindow
  ;; display the array slice in the window
  TVDisplay  
  ;; update the GUI to show your data container and the container info
  Update_XTabControl
  
  DestroyProgressBar

END 

