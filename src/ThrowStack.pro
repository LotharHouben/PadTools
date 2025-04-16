PRO ThrowStack, pdata, fname, Extra=extra, TITLE=title, BIN=bin, SAMP=samp, UNIT=unit
  IF NOT(GetDebugFlag()) THEN BEGIN
     CATCH, Error_status
     IF (Error_status NE 0) THEN BEGIN
        PrintToCon, "% ThrowStack:  Fatal error "
        PrintToCon, "%   Error status  - " + STRING(error_status)
        PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
        ErrMsg, !ERROR_STATE.MSG
        CATCH, /Cancel
        return
     END
  END
IF PTR_VALID(pdata) THEN BEGIN
  ptr=GetRootP() ;; datalistpointer
  names=DataList_GetNames(ptr)
  Selected=WHERE(names EQ fname)
  IF (Selected EQ -1) THEN BEGIN
     e=DataList_CreateElement(ptr, fname)
  END ELSE BEGIN
     i=2
     REPEAT BEGIN
       altname=fname+ "" + MyString(i)
       Selected=WHERE(names EQ altname)
       i=i+1
     END UNTIL (Selected EQ -1)
     e=DataList_CreateElement(ptr, altname)
  END
  IF PTR_VALID(e) THEN BEGIN
     ;;
     ;; open file to read header
     ;;
     N=Size(*pdata)
     printtocon, "% ThrowStack: header information for " + fname
     DimZ=N[3]
     printtocon, "%    Number of images : " + MyString(DimZ)
     DimX=N[1]
     DimY=N[2]
     printtocon, "%    Dimensions       : " + MyString(DimX)+ " x " + MyString(DimY)
     type=SIZE(*pdata, /TYPE)
     printtocon, "%    Data type        : " + MyString(type)
     

     (*e).data=pdata
     IF (PTR_VALID((*e).data)) THEN BEGIN
        (*e).SzX=DimX & (*e).SzY=DimY & (*e).SzZ=DimZ
        (*e).id=fname
        (*e).type=type
        (*e).slice=FIX((*e).SzZ/2)
        (*e).zcoord=3
        (*e).contrastmode="auto"
        (*e).contrastsubmode="minmax"
        (*e).contrastroi='full'
        IF keyword_set(extra) THEN (*e).extra=extra
        if keyword_set(bin) THEN BEGIN
           (*e).BINNING=bin
           (*e).BINNINGX=bin
           (*e).BINNINGY=bin
        END
        IF keyword_set(samp) THEN BEGIN
           (*e).xsamp=samp[0]
           (*e).ysamp=samp[1]
           (*e).zsamp=samp[2]
        END
        IF keyword_set(unit) THEN BEGIN
           (*e).xunit=unit[0]
           (*e).yunit=unit[1]
           (*e).zunit=unit[2]
        END
        CreateWindow, BIN=bin, TITLE=title
        TVDisplay
        s=GEtAutoContrastValues(v)
        (*e).CONTRAST=s
        Update_XTabControl
        ;; Make sure contrast values in fix mode are set properly
        
     End
  END 
END
END  
