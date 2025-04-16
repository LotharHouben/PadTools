PRO ThrowImage, x, TITLE=title, BIN=bin, SAMP=samp, UNIT=unit
  IF (GetDebugFlag()) THEN BEGIN
  END ELSE BEGIN
     CATCH, Error_status
     IF (Error_status NE 0) THEN BEGIN
        Print, "% ThrowImage:    Fatal error "
        Print, "%   Error status  - " + STRING(error_status)
        Print, "%   Error message - " + !ERROR_STATE.MSG
        CATCH, /Cancel
        IF NOT(GetCommandLineMode()) THEN ErrMsg, !ERROR_STATE.MSG ELSE Exit, status=1
        return
     END 
  END
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     printtocon, "% ThrowImage: Root pointer is invalid" 
     return
  END
  name="Image"
  IF N_Elements(title) THEN name=title
  
  CASE SIZE(x,/N_DIMENSIONS) OF
     2: BEGIN
        N=Size(x,/DIMENSIONS)
        dim=[N[0],N[1],1]
     END
     3: BEGIN
        dim=Size(x,/DIMENSIONS)
     END
     ELSE: BEGIN
        printtocon, "% ThrowImage: Invalid dimension."       
     return
     END
  END
  pp=DataList_CreateElement(ptr, name)
   (*pp).id=name
   (*pp).type=SIZE(x,/TYPE)
   (*pp).SzX=dim[0]
   (*pp).SzY=dim[1]
   (*pp).SzZ=dim[2]
   IF keyword_set(samp) THEN BEGIN
      (*pp).xsamp=samp[0]
      (*pp).ysamp=samp[1]
      (*pp).zsamp=samp[2]
   END
   IF keyword_set(unit) THEN BEGIN
      (*pp).xunit=unit[0]
      (*pp).yunit=unit[1]
      (*pp).zunit=unit[2]
   END
   ppd=PTR_NEW(x)
   IF NOT(PTR_VALID(ppd)) THEN BEGIN
      printtocon, "% ThrowImage: Failed to create data array." 
      return
   END
   (*pp).data=ppd
   (*pp).zcoord=3
   (*pp).slice=0
   if keyword_set(bin) THEN BEGIN
      (*pp).BINNING=bin
      (*pp).BINNINGX=bin
      (*pp).BINNINGY=bin
   END ELSE BEGIN
      bin=TVBIN(dim[0],dim[1], /MAXIMISE, maxfrac=0.4)
   END
   CreateWindow, BIN=bin, TITLE=title
   SetStackContrastMode, /AUTO, /SDEV, /QUARTER
   TVDisplay
   s=GetAutoContrastValues(v) & (*pp).contrast=s
   Update_XTabControl
END 
