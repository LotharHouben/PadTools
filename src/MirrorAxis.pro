FUNCTION XAxisDialog, axisA, TITLE=title
;;XConsole_PushState
;;XConsole_WaitingForInput
IF NOT(GetDebugFlag()) THEN BEGIN
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    Printtocon, "% XAxisDialog:    Fatal error "
    Printtocon, "%   Error status  - " + STRING(error_status)
    Printtocon, "%   Error message - " + !ERROR_STATE.MSG
    ErrMsg, !ERROR_STATE.MSG
    CATCH, /Cancel
    XConsole_PopState
    return, 0
 END
END
if not(keyword_set(title)) then title='Mirror Axis'
result=1 ;; set return result to error
base=widget_base(Title=title, /Column, TAB_MODE = 1)
frame1 = WIDGET_BASE(base,/Column)
row11 = WIDGET_BASE(frame1,/Row)
text=Widget_Label(row11, VALUE=title)
tilttogglebase = WIDGET_BASE(row11, /Row, /EXCLUSIVE)
direction_id=INTARR(3)
direction_id[0] = WIDGET_BUTTON(tilttogglebase, VALUE="x", UVALUE="tiltx", /NO_RELEASE)
direction_id[1] = WIDGET_BUTTON(tilttogglebase, VALUE="y", UVALUE="tilty", /NO_RELEASE)
direction_id[2] = WIDGET_BUTTON(tilttogglebase, VALUE="z", UVALUE="tiltz", /NO_RELEASE)
CASE axisA OF
"y": i=1
"z": i=2
ELSE: i=0
END
WIDGET_CONTROL, direction_id[i], /SET_BUTTON

c=WIDGET_BASE(base, /ROW)
apply = WIDGET_BUTTON(c, value=' Apply ', UVALUE='apply')
cancel = WIDGET_BUTTON(c, value=' Cancel ', UVALUE='cancel')

WIDGET_CONTROL, base, /REALIZE
 
quit=0
REPEAT BEGIN
  ev = WIDGET_Event(base)
  WIDGET_CONTROL, ev.id, GET_UVALUE = uv
  CASE uv OF
     'tiltx': BEGIN
        axisA="x"
     END
     'tilty': BEGIN
        axisA="y"
     END
     'tiltz': BEGIN
        axisA="z"
     END
     'apply': BEGIN
           result=0
           quit=1
     END
     'cancel': BEGIN
        quit=1
        result=1
     END
     ELSE: 
  END
ENDREP UNTIL (quit EQ 1)


WIDGET_CONTROL, base, /DESTROY
;;XConsole_PopState
return, result
END



PRO MirrorAxis, AXIS=axis, INPLACE=inplace
  ;; axis = "x","y","z"
  ;; if not set then there is a dialog
IF NOT(GetDebugFlag()) THEN BEGIN
   CATCH, Error_status
   IF (Error_status NE 0) THEN BEGIN
      Printtocon, "% MirrorAxis:    Fatal error "
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
     printtocon, "% MirrorAxis: Root pointer is invalid" 
     return
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% MirrorAxis: current stack pointer is invalid" 
     return
  END
  e=(*c).datap
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% MirrorAxis: current data pointer is invalid" 
     return
  END
  IF NOT(keyword_set(axis)) THEN BEGIN
     CASE (*e).zcoord OF
        0: axisA='x'
        1: axisA='y'
        2: axisA='z'
        ELSE: axisA='x'
     END
     IF (XAxisDialog(axisA) GT 0) THEN BEGIN
        printtocon, "% MirrorAxis: cancelled" 
        return
     END
  END ELSE axisA=axis
  ;;
  if not(keyword_set(inplace)) THEN BEGIN
  name="Mirror("+(*e).id+","+axisA+")"
  printtocon, "% MirrorAxis: Creating new stack " + name 
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
  ppd=(Data_GetEmptyArrayP(pp))
  IF NOT(PTR_VALID(ppd)) THEN BEGIN
     printtocon, "% MirrorAxis: failed to create data array for the Radon transform images" 
     return
  END
  current=(*ptr).current
  (*current).name=name
  (*pp).data=ppd
  Case axisA of
     'x': BEGIN 
        (*ppd)=REVERSE((*(*e).data),1)
     END
     'y':BEGIN
        (*ppd)=REVERSE((*(*e).data),2)
     END
     'z':BEGIN
        (*ppd)=REVERSE((*(*e).data),3)
     END
  END
  CreateWindow
END ELSE BEGIN
    Case axisA of
     'x': BEGIN 
        (*(*e).data)=REVERSE((*(*e).data),1, /OVERWRITE)
     END
     'y':BEGIN
       (*(*e).data)=REVERSE((*(*e).data),2, /OVERWRITE)
     END
     'z':BEGIN
        (*(*e).data)=REVERSE((*(*e).data),3, /OVERWRITE)
     END
  END
END
  TVDisplay
  Update_XTabControl
END 


PRO CorrectDiffOrientation, EMPAD=empad
;; mirror x axis
  IF NOT(GetDebugFlag()) THEN BEGIN
   CATCH, Error_status
   IF (Error_status NE 0) THEN BEGIN
      Printtocon, "% CorrectDiffOrientation:    Fatal error "
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
     printtocon, "% CorrectDiffOrientation: Root pointer is invalid" 
     return
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% CorrectDiffOrientation: current stack pointer is invalid" 
     return
  END
  e=(*c).datap
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% CorrectDiffOrientation: current data pointer is invalid" 
     return
  END
  ppd=(*e).data ;; the data pointer
  tmp=(*ppd)
  IF keyword_set(empad) THEN BEGIN
     minv=[0]
     maxv=[2]
     mybar=obj_new('ProgressBar', MINV=minv,MAXV=maxv,TEXT=["Stage"], STATUS="EMPAD Rotation Compensation", FRAMESTYLE=2)
     mybar->Set, 0, 1, TEXT="1"
     mybar->SetStatus, "Flipping x ..."
     tmp=REVERSE(tmp,1) ;; flip x
     mybar->Set, 0, 1, TEXT="2"
     mybar->SetStatus, "Rotating -90 deg ..."
     tmp=ROTATE(tmp,3)  ;; rotate -90 degrees (or +270 degrees)
     obj_destroy, mybar
  END
  (*e).id="CorrDiffOrient("+(*e).id+")"
  tt=(*e).SzX
  (*e).SzX=(*e).SzY
  (*e).SzY=tt
  tt=(*e).xsamp
  (*e).xsamp=(*e).ysamp
  (*e).ysamp=tt
  tt=(*e).binningx
  (*e).binningx=(*e).binningy
  (*e).binningy=tt
  tt=(*e).xunit
  (*e).xunit=(*e).yunit
  (*e).yunit=tt
  ;; close window
  WDELETE, (*e).Window
  (*e).window=-1
  PTR_Free, (*e).data
  (*e).data=PTR_NEW(tmp)
  ;; STOP
  CreateWindow, BIN=(*e).binning, TITLE=(*e).id
  TVDisplay
  Update_XTabControl
END 

