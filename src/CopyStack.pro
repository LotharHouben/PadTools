PRO CopyStack, NODATACOPY=nodatacopy, TITLE=title
IF NOT(GetDebugFlag()) THEN BEGIN
   CATCH, Error_status
   IF (Error_status NE 0) THEN BEGIN
      Printtocon, "% CopyStack:    Fatal error "
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
     printtocon, "% CopyStack: Root pointer is invalid" 
     return
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% CopyStack: current stack pointer is invalid" 
     return
  END
  e=(*c).datap
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% CopyStack: current data pointer is invalid" 
     return
  END
  IF not(keyword_set(title)) THEN name="Cp("+(*c).name+")" ELSE name=title
  printtocon, "% CopyStack: creating " + name 
  ;; STOP
  pp=DataList_CreateElement(ptr, name)
  ;; make sure we will not get an overflow when adding data 
  (*pp).type=(*e).type
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
        printtocon, "% CopyStack: failed to create data array." 
        return
     END
     (*pp).data=ppd
     (*ppd)=(*(*e).data)
  END
  current=(*ptr).current
  (*current).name=name
  ;;
  ;; duplicate PAD object if exist
  o=(*e).extra
  IF OBJ_VALID(o) THEN BEGIN
    empaddata=obj_new('EMPADObj',(*e).id,FRAMEX=(*e).SzX,FRAMEY=(*e).SzY)
    empaddata.SetScanSize, o->GetScanSize(/X), o->GetScanSize(/Y)
    empaddata.SetScanSampling, o->GetScanSampling(/X), o->GetScanSampling(/Y)
    empaddata.SetDetectorSampling,  o->GetDetectorSampling(/X), o->GetDetectorSampling(/Y)
    empaddata.SetDataPointer, ppd
    (*pp).extra=empaddata 
  END  
  ;; copy Radon geometry if present
  ;; IF OBJ_VALID((*e).radongeometry) THEN BEGIN 
 ;;    printtocon, "% CopyStack: Copying Radon transform geometry data." 
     ;; geometry is defined for the stack
 ;;    TAxisP=(*e).radongeometry
 ;;    (*pp).radongeometry=OBJ_NEW('TiltSeriesObj')
 ;;    TAxisCP=(*pp).radongeometry
 ;;    v=TAxisP->get_rho()
 ;;    IF (SIZE(v, /N_DIMENSIONS) GT 0) THEN BEGIN
 ;;       TAxisCP->set_rho, AXSTR=TAxisP->get_axis_label(/RHO), PRESET=[-FLOOR(((*e).SzY)/2),1.,(*e).SzY]
 ;;    END ELSE BEGIN
 ;;       TAxisP->set_rho, AXSTR=TAxisP->get_axis_label(/RHO), VALUES=v
 ;;    END
 ;;    TAxisCP->set_stack, AXSTR=TAxisP->get_axis_label(/STACK), NUM=TAxisP->get_stack()
 ;;    v=TAxisP->get()
 ;;    IF (SIZE(v, /N_DIMENSIONS) GT 0) THEN BEGIN
 ;;       TAxisCP->set, AXSTR=TAxisP->get_axis_label(/TILT), VALUES=v
 ;;    END ELSE BEGIN
 ;;            TAxisCP->set, AXSTR=TAxisP->get_axis_label(/TILT), PRESET=[-FLOOR(((*e).SzX)/2),(*e).xsamp,(*e).SzX]
 ;;    END
     ;; STOP
;;  END ELSE BEGIN
;;     ;; geometry is not defined for this stack 
;;     (*pp).radongeometry=OBJ_NEW('TiltSeriesObj')
 ;;    TAxisCP=(*pp).radongeometry
 ;;    TAxisCP->set_rho, AXSTR='y',PRESET=[-FLOOR(((*e).SzY)/2),1.,(*e).SzY]
 ;;    TAxisCP->set_stack, AXSTR='z', NUM=(*e).SzZ
 ;;    TAxisCP->set, AXSTR='x',PRESET=[-FLOOR(((*e).SzX)/2),(*e).xsamp,(*e).SzX]
  ;; END
  
  CreateWindow
  IF NOT(keyword_set(nodatacopy)) THEN BEGIN
     TVDisplay
  END 
  Update_XTabControl
END 
