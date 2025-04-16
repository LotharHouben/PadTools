Pro ExtractSlice
IF (GetDebugFlag()) THEN BEGIN
END ELSE BEGIN
   CATCH, Error_status
   IF (Error_status NE 0) THEN BEGIN
      Printtocon, "% ExtractSlice:    Fatal error "
      Printtocon, "%   Error status  - " + STRING(error_status)
      Printtocon, "%   Error message - " + !ERROR_STATE.MSG
      ErrMsg, !ERROR_STATE.MSG
      CATCH, /Cancel
      XConsole_PopState
      return
   END
END
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     printtocon, "% ExtractSlice: Root pointer is invalid" 
     return
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% ExtractSlice: current stack pointer is invalid" 
     return
  END
  e=(*c).datap
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% ExtractSlice: current data pointer is invalid" 
     return
  END
  name="Slice("+(*c).name+","+MyString((*e).zcoord)+":"+MyString((*e).slice)+")"
  pp=DataList_CreateElement(ptr, name)
  ;; make sure we will not get an overflow when adding data 
  (*pp).type=(*e).type
  ;;
  CASE (*e).zcoord OF
     1: BEGIN
        (*pp).SzX=(*e).SzY
        (*pp).SzY=(*e).SzZ
        (*pp).SzZ=1
        (*pp).xsamp=(*e).ysamp
        (*pp).ysamp=(*e).zsamp
        (*pp).zsamp=1.
     END
     2: BEGIN
        (*pp).SzX=(*e).SzX
        (*pp).SzY=(*e).SzZ
        (*pp).SzZ=1
        (*pp).xsamp=(*e).xsamp
        (*pp).ysamp=(*e).zsamp
        (*pp).zsamp=1.
     END
     3: BEGIN
        (*pp).SzX=(*e).SzX
        (*pp).SzY=(*e).SzY
        (*pp).SzZ=1
        (*pp).xsamp=(*e).xsamp
        (*pp).ysamp=(*e).zsamp
        (*pp).zsamp=1.
     END
     ELSE:
  END
  ;;
  (*pp).slice=0
  (*pp).zcoord=3
  (*pp).contrastmode="auto"
  ppd=(Data_GetEmptyArrayP(pp))
  (*pp).data=ppd
  IF (PTR_VALID(ppd)) THEN BEGIN
     CASE (*e).zcoord OF
        1: BEGIN
           (*ppd)[*,*,0]=(*(*e).data)[(*e).slice,*,*]
        END
        2: BEGIN
           (*ppd)[*,*,0]=(*(*e).data)[*,(*e).slice,*]
        END
        3: BEGIN
           (*ppd)[*,*,0]=(*(*e).data)[*,*,(*e).slice]
        END
        ELSE:
     END
  END

  CreateWindow
  Update_XTabControl
  TVDisplay


END

PRO CutVolume
XConsole_PushState
XConsole_Busy
CATCH, Error_status
IF NOT(GetDebugFlag()) THEN BEGIN
   IF (Error_status NE 0) THEN BEGIN
      Printtocon, "% CutVolume:    Fatal error "
      Printtocon, "%   Error status  - " + STRING(error_status)
      Printtocon, "%   Error message - " + !ERROR_STATE.MSG
      ErrMsg, !ERROR_STATE.MSG
      CATCH, /Cancel
      XConsole_PopState
      return
   END
END 
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     printtocon, "% CutVolume: Root pointer is invalid" 
      XConsole_PopState
     return
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% CutVolume: current stack pointer is invalid" 
      XConsole_PopState
     return
  END
  e=(*c).datap
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% CutVolume: current data pointer is invalid" 
      XConsole_PopState
     return
  END

  ;; --------------------------------------------------------
  ;; ROI for the square error in the difference image DiffROI
  CASE (*e).zcoord OF
     1: BEGIN
        CutROI=[FIX((*e).SzY*1/4),FIX((*e).SzY*3/4),FIX((*e).SzZ*1/4),FIX((*e).SzZ*3/4)]
     END
     2: BEGIN
        CutROI=[FIX((*e).SzX*1/4),FIX((*e).SzX*3/4),FIX((*e).SzZ*1/4),FIX((*e).SzZ*3/4)]
     END
     3: BEGIN
        CutROI=[FIX((*e).SzX*1/4),FIX((*e).SzX*3/4),FIX((*e).SzY*1/4),FIX((*e).SzY*3/4)]
     END
     ELSE: BEGIN
        printtocon, "% CutVolume: Invalid stack projection." 
        XConsole_PopState
        return
     END
  END
  XConsole_WaitingForInput
  roilist=VolumePicker3D(ERRSTATE=errstate)
  
   if NOT(errstate EQ 0) THEN BEGIN 
     print, "% CutVolume: Cancelled." 
     XConsole_PopState
     return
  END
   roiarray=ROIListToArray(roilist)
   NRoiarray=SIZE(roiarray)

   nROI=NRoiarray[1]
      XConsole_Busy

   k=0
   BEGIN
      SzX=roiarray[k,1]-roiarray[k,0]+1
      SzY=roiarray[k,3]-roiarray[k,2]+1
      SzZ=roiarray[k,5]-roiarray[k,4]+1
      name="Cut("+(*c).name+")"
      printtocon, "% CutVolume: creating " + name 
      ;; STOP
      pp=DataList_CreateElement(ptr, name)
      (*pp).type=(*e).type
      ;; predefine new stack size
      
  printtocon, "% CutVolume: New stack size ("+MyString(SzX)+","+ MyString(SzY)+","+ MyString(SzZ)+")"
  (*pp).SzX=SzX
  (*pp).SzY=SzY
  (*pp).SzZ=SzZ
  (*pp).xsamp=(*e).xsamp
  (*pp).ysamp=(*e).ysamp
  (*pp).zsamp=(*e).zsamp
  (*pp).slice=(*e).slice
  (*pp).zcoord=(*e).zcoord
  (*pp).contrastmode="auto"
  ;; 
  ppd=(Data_GetEmptyArrayP(pp))
  IF NOT(PTR_VALID(ppd)) THEN BEGIN
     printtocon, "% CutVolume: Failed to create data array." 
     XConsole_PopState
     return
  END
  (*pp).data=ppd
  (*ppd)[*,*,*]=(*(*e).data)[roiarray[k,0]:roiarray[k,1],roiarray[k,2]:roiarray[k,3],roiarray[k,4]:roiarray[k,5]]
  current=(*ptr).current
  (*current).name=name
  CreateWindow
  Update_XTabControl
  TVDisplay
END
  XConsole_PopState
END 


PRO CutVolume2D
XConsole_PushState
XConsole_Busy
CATCH, Error_status
IF NOT(GetDebugFlag()) THEN BEGIN
   IF (Error_status NE 0) THEN BEGIN
      Printtocon, "% CutVolume2D:    Fatal error "
      Printtocon, "%   Error status  - " + STRING(error_status)
      Printtocon, "%   Error message - " + !ERROR_STATE.MSG
      ErrMsg, !ERROR_STATE.MSG
      CATCH, /Cancel
      XConsole_PopState
      return
   END
END 
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     printtocon, "% CutVolume2D: Root pointer is invalid" 
      XConsole_PopState
     return
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% CutVolume2D: current stack pointer is invalid" 
      XConsole_PopState
     return
  END
  e=(*c).datap
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% CutVolume2D: current data pointer is invalid" 
      XConsole_PopState
     return
  END

  ;; --------------------------------------------------------
  ;; ROI for the square error in the difference image DiffROI
  CASE (*e).zcoord OF
     1: BEGIN
        CutROI=[FIX((*e).SzY*1/4),FIX((*e).SzY*3/4),FIX((*e).SzZ*1/4),FIX((*e).SzZ*3/4)]
     END
     2: BEGIN
        CutROI=[FIX((*e).SzX*1/4),FIX((*e).SzX*3/4),FIX((*e).SzZ*1/4),FIX((*e).SzZ*3/4)]
     END
     3: BEGIN
        CutROI=[FIX((*e).SzX*1/4),FIX((*e).SzX*3/4),FIX((*e).SzY*1/4),FIX((*e).SzY*3/4)]
     END
     ELSE: BEGIN
        printtocon, "% CutVolume2D: Invalid stack projection." 
        XConsole_PopState
        return
     END
  END
  XConsole_WaitingForInput
  roi=CutROI
  IF (GetROI(c,(*e).zcoord,roi) GT 0) THEN CutROI=roi ELSE BEGIN
     print, "% CutVolume2D: Cancelled." 
     XConsole_PopState
     return
  END
  XConsole_Busy
  ;; 
  name="Cut("+(*c).name+")"
  printtocon, "% CutVolume2D: creating " + name 
  ;; STOP
  pp=DataList_CreateElement(ptr, name)
  (*pp).type=(*e).type
  ;; predefine new stack size
  SzX=(*e).SzX & SzY=(*e).SzY & SzZ=(*e).SzZ 
  CASE (*e).zcoord OF
     1: BEGIN
        SzY=roi[1]-roi[0]+1
        IF ((SzY LT 1) Or (SzY GT (*e).SzY)) THEN BEGIN
           SzY=(*e).SzY & roi[0]=0 & roi[1]=(*e).SzY-1
        END      
        SzZ=roi[3]-roi[2]+1
        IF ((SzZ LT 1) Or (SzZ GT (*e).SzZ)) THEN BEGIN
           SzZ=(*e).SzZ & roi[2]=0 & roi[3]=(*e).SzZ-1
        END      
     END
     2: BEGIN
        SzX=roi[1]-roi[0]+1
        IF ((SzX LT 1) Or (SzX GT (*e).SzX)) THEN BEGIN
           SzX=(*e).SzX & roi[0]=0 & roi[1]=(*e).SzX-1
        END      
        SzZ=roi[3]-roi[2]+1
        IF ((SzZ LT 1) Or (SzZ GT (*e).SzZ)) THEN BEGIN
           SzZ=(*e).SzZ & roi[2]=0 & roi[3]=(*e).SzZ-1
        END      
     END
     3: BEGIN
        SzX=roi[1]-roi[0]+1
        IF ((SzX LT 1) Or (SzX GT (*e).SzX)) THEN BEGIN
           SzX=(*e).SzX & roi[0]=0 & roi[1]=(*e).SzX-1
        END      
        SzY=roi[3]-roi[2]+1
        IF ((SzY LT 1) Or (SzY GT (*e).SzY)) THEN BEGIN
           SzY=(*e).SzY & roi[2]=0 & roi[3]=(*e).SzY-1
        END      
     END
     ELSE: 
  END
  ;;
  printtocon, "% CutVolume2D: New stack size ("+MyString(SzX)+","+ MyString(SzY)+","+ MyString(SzZ)+")"
  (*pp).SzX=SzX
  (*pp).SzY=SzY
  (*pp).SzZ=SzZ
  (*pp).xsamp=(*e).xsamp
  (*pp).ysamp=(*e).ysamp
  (*pp).zsamp=(*e).zsamp
  (*pp).slice=(*e).slice
  (*pp).zcoord=(*e).zcoord
  (*pp).contrastmode="auto"
  ;; 
  ppd=(Data_GetEmptyArrayP(pp))
  IF NOT(PTR_VALID(ppd)) THEN BEGIN
     printtocon, "% CutVolume2D: Failed to create data array." 
     XConsole_PopState
     return
  END
  (*pp).data=ppd
  CASE (*e).zcoord OF
     1: BEGIN 
        For i=0,(SzX-1) DO BEGIN
           (*ppd)[i,*,*]=(*(*e).data)[i,roi[0]:roi[1],roi[2]:roi[3]]
        END
     END
     2: BEGIN 
        For i=0,(SzY-1) DO BEGIN
           (*ppd)[*,i,*]=(*(*e).data)[roi[0]:roi[1],i,roi[2]:roi[3]]
        END
     END
     3: BEGIN 
        For i=0,(SzZ-1) DO BEGIN
           (*ppd)[*,*,i]=(*(*e).data)[roi[0]:roi[1],roi[2]:roi[3],i]
        END
     END
     ELSE: 
  END
  current=(*ptr).current
  (*current).name=name
  CreateWindow
  TVDisplay
  Update_XTabControl
  XConsole_PopState
END 
