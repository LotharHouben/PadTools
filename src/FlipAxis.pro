FUNCTION XFlipAxisDialog, axisA, axisB
;;XConsole_PushState
;;XConsole_WaitingForInput
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    Printtocon, "% XFlipAxis:    Fatal error "
    Printtocon, "%   Error status  - " + STRING(error_status)
    Printtocon, "%   Error message - " + !ERROR_STATE.MSG
    ErrMsg, !ERROR_STATE.MSG
    CATCH, /Cancel
    XConsole_PopState
    return, 0
END

result=1 ;; set return result to error
title='Flip Axis'
base=widget_base(Title=title, /Column, TAB_MODE = 1)
frame1 = WIDGET_BASE(base,/Column)
row11 = WIDGET_BASE(frame1,/Row)
text=Widget_Label(row11, VALUE='Flip Axis A:')
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
row12 =  WIDGET_BASE(frame1, /ROW) 
text=Widget_Label(row12, VALUE='with Axis B: ')
tilttogglebase = WIDGET_BASE(row12, /Row, /EXCLUSIVE)
direction_id=INTARR(3)
direction_id[0] = WIDGET_BUTTON(tilttogglebase, VALUE="x", UVALUE="stackx", /NO_RELEASE)
direction_id[1] = WIDGET_BUTTON(tilttogglebase, VALUE="y", UVALUE="stacky", /NO_RELEASE)
direction_id[2] = WIDGET_BUTTON(tilttogglebase, VALUE="z", UVALUE="stackz", /NO_RELEASE)
CASE axisB OF
"y": i=1
"z": i=2
ELSE: i=0
END
WIDGET_CONTROL, direction_id[i], /SET_BUTTON
c=WIDGET_BASE(base, /ROW)
apply = WIDGET_BUTTON(c, value='Accept', UVALUE='apply')
cancel = WIDGET_BUTTON(c, value='Cancel', UVALUE='cancel')

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
     'stackx': BEGIN
        axisB="x"
     END
     'stacky': BEGIN
        axisB="y"
     END
     'stackz': BEGIN
        axisB="z"
     END
     'apply': BEGIN
        IF ((axisA NE axisB)) THEN BEGIN 
           result=0
           quit=1
        END ELSE BEGIN
           dummy=Dialog_Info(["Conflict in axis selection.","Please check."], /WARNING)
        END
        
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



PRO FlipAxis
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    Printtocon, "% FlipAxis:    Fatal error "
    Printtocon, "%   Error status  - " + STRING(error_status)
    Printtocon, "%   Error message - " + !ERROR_STATE.MSG
    ErrMsg, !ERROR_STATE.MSG
    CATCH, /Cancel
    XConsole_PopState
    return
END
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     printtocon, "% FlipAxis: Root pointer is invalid" 
     return
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% FlipAxis: current stack pointer is invalid" 
     return
  END
  e=(*c).datap
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% FlipAxis: current data pointer is invalid" 
     return
  END
  axisA="x"
  axisB="y"
  IF (XFlipAxisDialog(axisA, AxisB) GT 0) THEN BEGIN
     printtocon, "% FlipAxis: cancelled" 
     return
  END
  ;;
  name="FlipAxis("+(*e).id+","+axisA+"<->"+axisB+")"
  printtocon, "% FlipAxis: Creating new stack " + name 
  ;; STOP
  pp=DataList_CreateElement(ptr, name)
  ;; make sure we will not get an overflow when adding data 
  (*pp).type=(*e).type
  CASE axisA of 
     "x": BEGIN
        CASE axisB of 
           "y": BEGIN
              (*pp).SzX=(*e).SzY
              (*pp).SzY=(*e).SzX
              (*pp).SzZ=(*e).SzZ
              (*pp).xsamp=(*e).ysamp
              (*pp).ysamp=(*e).xsamp
              (*pp).zsamp=(*e).zsamp
              transp=[1,0,2]
           END
           "z":  BEGIN
              (*pp).SzX=(*e).SzZ
              (*pp).SzZ=(*e).SzX
              (*pp).SzY=(*e).SzY
              (*pp).xsamp=(*e).zsamp
              (*pp).zsamp=(*e).xsamp
              (*pp).ysamp=(*e).ysamp
              transp=[2,1,0]
           END
        END
     END
     "y": BEGIN
        CASE axisB of 
           "x": BEGIN
              (*pp).SzX=(*e).SzY
              (*pp).SzY=(*e).SzX
              (*pp).SzZ=(*e).SzZ
              (*pp).xsamp=(*e).ysamp
              (*pp).ysamp=(*e).xsamp
              (*pp).zsamp=(*e).zsamp
              transp=[1,0,2]
           END
           "z":  BEGIN
              (*pp).SzY=(*e).SzZ
              (*pp).SzZ=(*e).SzY
              (*pp).SzX=(*e).SzX
              (*pp).ysamp=(*e).zsamp
              (*pp).zsamp=(*e).ysamp
              (*pp).xsamp=(*e).xsamp
              transp=[0,2,1]
           END
        END
     END
     "z": BEGIN 
        CASE axisB of 
           "x": BEGIN
              (*pp).SzX=(*e).SzZ
              (*pp).SzZ=(*e).SzX
              (*pp).SzY=(*e).SzY
              (*pp).xsamp=(*e).zsamp
              (*pp).zsamp=(*e).xsamp
              (*pp).ysamp=(*e).ysamp
              transp=[2,1,0]
           END
           "y":  BEGIN
              (*pp).SzY=(*e).SzZ
              (*pp).SzZ=(*e).SzY
              (*pp).SzX=(*e).SzX
              (*pp).ysamp=(*e).zsamp
              (*pp).zsamp=(*e).ysamp
              (*pp).xsamp=(*e).xsamp
              transp=[0,2,1]
           END
        END
     END
     ELSE: BEGIN
        printtocon, "% FlipAxis: invalid stack axis" 
        return
     END
  END
  (*pp).slice=0
  (*pp).zcoord=3
  (*pp).contrastmode="auto"
  ppd=(Data_GetEmptyArrayP(pp))
  IF NOT(PTR_VALID(ppd)) THEN BEGIN
     printtocon, "% FlipAxis: failed to create data array for the Radon transform images" 
     return
  END
  current=(*ptr).current
  (*current).name=name
  (*pp).data=ppd
  (*ppd)=Transpose((*(*e).data),transp)
  CreateWindow
  TVDisplay
  Update_XTabControl
END 


PRO FlipToTomatoCoordinates
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    Printtocon, "% FlipToTomatoCoordinates:    Fatal error "
    Printtocon, "%   Error status  - " + STRING(error_status)
    Printtocon, "%   Error message - " + !ERROR_STATE.MSG
    ErrMsg, !ERROR_STATE.MSG
    CATCH, /Cancel
    XConsole_PopState
    return
END
 ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     printtocon, "% FlipToTomatoCoordinates: Root pointer is invalid" 
     return
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% FlipToTomatoCoordinates: current stack pointer is invalid" 
     return
  END
  e=(*c).datap
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% FlipToTomatoCoordinates: current data pointer is invalid" 
     return
  END
  ;; if x has the smallest number of elements then it may be the right
  ;; orientation already
  IF ((*e).SzZ LT (*e).SzY) THEN BEGIN
     IF ((*e).SzZ LT (*e).SzX) THEN BEGIN
        axisA="z"
        axisB="y"
       END ELSE BEGIN
        axisA="x"
        axisB="z"
     END
  END ELSE BEGIN
     IF ((*e).SzY LT (*e).SzX) THEN BEGIN
        axisA="y"
        axisB="x"
       END ELSE BEGIN
        axisA="x"
        axisB="z"
     END
  END
result=1 ;; set return result to error
title='Coordinate System'
help_text=["Flip to tomato coordinate system", $
           "", $
           "tomato expects the tilt angles in the first coordinate along x", $
           "and the tilt axis parallel to z. Name the current dimensions ", $
           "for the tilt angles and the tilt axis. The procedure will ", $
           "transpose and invert axes accordingly, maintaining the handedness.", $
           "", $
           "Tilt Angle Coordinate: The dimension that holds the tilt angles.", $
           "Tilt Axis Coordinate : The dimension that is parallel to the tilt", $
           "                       axis.", $
           "" $
          ]
base=widget_base(Title=title, /Column, TAB_MODE = 1)
frame1 = WIDGET_BASE(base,/Column)
row11 = WIDGET_BASE(frame1,/Row)
text=Widget_Label(row11, VALUE='Tilt Angle Coordinate:')
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
row12 =  WIDGET_BASE(frame1, /ROW) 
text=Widget_Label(row12, VALUE='Tilt Axis Coordinate: ')
tilttogglebase = WIDGET_BASE(row12, /Row, /EXCLUSIVE)
direction_id=INTARR(3)
direction_id[0] = WIDGET_BUTTON(tilttogglebase, VALUE="x", UVALUE="stackx", /NO_RELEASE)
direction_id[1] = WIDGET_BUTTON(tilttogglebase, VALUE="y", UVALUE="stacky", /NO_RELEASE)
direction_id[2] = WIDGET_BUTTON(tilttogglebase, VALUE="z", UVALUE="stackz", /NO_RELEASE)
CASE axisB OF
"y": i=1
"z": i=2
ELSE: i=0
END
WIDGET_CONTROL, direction_id[i], /SET_BUTTON
c=WIDGET_BASE(base, /ROW)
apply = WIDGET_BUTTON(c, value='Accept', UVALUE='apply')
help = WIDGET_BUTTON(c, value='Help', UVALUE='help')
cancel = WIDGET_BUTTON(c, value='Cancel', UVALUE='cancel')

WIDGET_CONTROL, base, /REALIZE
 
quit=0
REPEAT BEGIN
  ev = WIDGET_Event(base)
  WIDGET_CONTROL, ev.id, GET_UVALUE = uv
  CASE uv OF
     'tiltx': BEGIN
        axisA="x"
        IF (axisB EQ "x") THEN BEGIN
          axisB="y" & WIDGET_CONTROL, direction_id[1], /SET_BUTTON
        END
     END
     'tilty': BEGIN
        axisA="y"
        IF (axisB EQ "y") THEN BEGIN
          axisB="x" & WIDGET_CONTROL, direction_id[0], /SET_BUTTON
        END
     END
     'tiltz': BEGIN
        axisA="z"
        IF (axisB EQ "z") THEN BEGIN
           axisB="x" & WIDGET_CONTROL, direction_id[0], /SET_BUTTON
        END
     END
     'stackx': BEGIN
        IF (axisA NE "x") THEN BEGIN
           axisB="x"
        END ELSE BEGIN
           CASE axisB OF
              "y": WIDGET_CONTROL, direction_id[1], /SET_BUTTON
              "z": WIDGET_CONTROL, direction_id[2], /SET_BUTTON
              ELSE: 
           END
        END 
     END
     'stacky': BEGIN
        IF (axisA NE "y") THEN BEGIN
           axisB="y"
        END ELSE BEGIN
           CASE axisB OF
              "x": WIDGET_CONTROL, direction_id[0], /SET_BUTTON
              "z": WIDGET_CONTROL, direction_id[2], /SET_BUTTON
              ELSE: 
           END
        END 
     END
     'stackz': BEGIN
        IF (axisA NE "z") THEN BEGIN
           axisB="z"
        END ELSE BEGIN
           CASE axisB OF
              "x": WIDGET_CONTROL, direction_id[0], /SET_BUTTON
              "y": WIDGET_CONTROL, direction_id[1], /SET_BUTTON
              ELSE: 
           END
        END 
     END
     'apply': BEGIN
        IF ((axisA NE axisB)) THEN BEGIN 
           result=0
           quit=1
        END ELSE BEGIN
           dummy=Dialog_Info(["Conflict in axis selection.","Please check."], /WARNING)
        END
        
     END
     'help': dummy=Dialog_Info(help_text)
     'cancel': BEGIN
        quit=1
        result=1
        WIDGET_CONTROL, base, /DESTROY
        return
     END
     ELSE: 
  END
ENDREP UNTIL (quit EQ 1)
WIDGET_CONTROL, base, /DESTROY

;;XConsole_PopState
transp=[0,1,2]
mirror=[0,0,0]
transs="(xyz)->(xyz)"
; Now change coordinates
CASE axisA of 
;; tilt axis should be
"x": BEGIN
   ;; tilt axis is x, we don't need to change it
   CASE axisB OF
      "y": BEGIN
             ;; stack axis is y, need to swap z and y, then invert y
         printtocon, "% FlipToTomatoCoordinates: Transforming (x,y,z)->(x,-z,y)." 
         transs="(xyz)->(x-zy)"
         transp=[0,2,1]
         mirror=[0,1,0]
      END
      "z": BEGIN
;; nothing to do
      END
      ELSE: BEGIN
         printtocon, "% FlipToTomatoCoordinates: Unexpected coordinate assignment." 
         return
      END
;; tilt axis is z, we don't need to change it
   END
END 
"y": BEGIN
   ;; tilt axis is x, we don't need to change it
   CASE axisB OF
      "x": BEGIN
         ;; tilt axis is y and stack axis is x, need to swap x and y,
         ;; then y and z
         printtocon, "% FlipToTomatoCoordinates: Transforming (x,y,z)->(y,z,x)." 
         transs="(xyz)->(yzx)"
         transp=[1,2,0]
         mirror=[0,0,0]
      END 
      "z": BEGIN
         ;; tilt axis is y and stack axis is z, need to swap x and y,
         ;; then mirror y
         printtocon, "% FlipToTomatoCoordinates: Transforming (x,y,z)->(y,-x,z)." 
         transs="(xyz)->(y-xz)"
         transp=[1,0,2]
         mirror=[0,1,0]
      END 
      ELSE: BEGIN
         printtocon, "% FlipToTomatoCoordinates: Unexpected coordinate assignment." 
         return
      END
;; tilt axis is z, we don't need to change it
   END
END
"z": BEGIN
   ;; tilt axis is z
   CASE axisB OF
      "x": BEGIN
         ;; tilt axis is z and stack axis is x, need to swap x and z,
         ;; then mirror y
         printtocon, "% FlipToTomatoCoordinates: Transforming (x,y,z)->(z,-y,x)." 
         transs="(xyz)->(z-yx)"
         transp=[2,1,0]
         mirror=[0,1,0]
      END 
      "y": BEGIN
         ;; tilt axis is z and stack axis is y, need to swap x and y,
         ;; then mirror y
         printtocon, "% FlipToTomatoCoordinates: Transforming (x,y,z)->(z,x,y)." 
         transs="(xyz)->(zxy)"
         transp=[2,0,1]
         mirror=[0,0,0]
      END 
      ELSE: BEGIN
         printtocon, "% FlipToTomatoCoordinates: Unexpected coordinate assignment." 
         return
      END
;; tilt axis is z, we don't need to change it
   END
END
ELSE:
END 
IF (ArrayDistance(transp, [0,1,2]) NE 0) THEN BEGIN
   printtocon, "% FlipToTomatoCoordinates: Transposing data stack."

name="CoordinateTransform("+(*e).id+","+transs+")"
pp=DataList_CreateElement(ptr, name)
;; make sure we will not get an overflow when adding data 
(*pp).type=(*e).type
CASE 1 of 
   (ArrayDistance(transp,[1,0,2]) EQ 0): BEGIN
      (*pp).SzX=(*e).SzY
      (*pp).SzY=(*e).SzX
      (*pp).SzZ=(*e).SzZ
      (*pp).xsamp=(*e).ysamp
      (*pp).ysamp=(*e).xsamp
      (*pp).zsamp=(*e).zsamp
   END
   (ArrayDistance(transp,[2,1,0]) EQ 0):  BEGIN
      (*pp).SzX=(*e).SzZ
      (*pp).SzZ=(*e).SzX
      (*pp).SzY=(*e).SzY
      (*pp).xsamp=(*e).zsamp
      (*pp).zsamp=(*e).xsamp
      (*pp).ysamp=(*e).ysamp
   END
   (ArrayDistance(transp,[1,0,2]) EQ 0): BEGIN
      (*pp).SzX=(*e).SzY
      (*pp).SzY=(*e).SzX
      (*pp).SzZ=(*e).SzZ
      (*pp).xsamp=(*e).ysamp
      (*pp).ysamp=(*e).xsamp
      (*pp).zsamp=(*e).zsamp
   END
   (ArrayDistance(transp,[0,2,1]) EQ 0):  BEGIN
      (*pp).SzY=(*e).SzZ
      (*pp).SzZ=(*e).SzY
      (*pp).SzX=(*e).SzX
      (*pp).ysamp=(*e).zsamp
      (*pp).zsamp=(*e).ysamp
      (*pp).xsamp=(*e).xsamp
   END
   (ArrayDistance(transp,[2,0,1]) EQ 0): BEGIN
      (*pp).SzX=(*e).SzZ
      (*pp).SzY=(*e).SzX
      (*pp).SzZ=(*e).SzY
      (*pp).xsamp=(*e).zsamp
      (*pp).ysamp=(*e).xsamp
      (*pp).zsamp=(*e).ysamp
   END
   (ArrayDistance(transp,[1,2,0]) EQ 0): BEGIN
      (*pp).SzX=(*e).SzY
      (*pp).SzY=(*e).SzZ
      (*pp).SzZ=(*e).SzX
      (*pp).xsamp=(*e).ysamp
      (*pp).ysamp=(*e).zsamp
      (*pp).zsamp=(*e).xsamp
   END
   (ArrayDistance(transp,[2,1,0]) EQ 0): BEGIN
      (*pp).SzX=(*e).SzZ
      (*pp).SzY=(*e).SzY
      (*pp).SzZ=(*e).SzX
      (*pp).xsamp=(*e).zsamp
      (*pp).ysamp=(*e).ysamp
      (*pp).zsamp=(*e).xsamp
   END
   ELSE: BEGIN
      printtocon, "% FlipToTomatoCoordinates: Invalid permutation vector." 
      return
   END
  END
  (*pp).slice=0
  (*pp).zcoord=3
  (*pp).contrastmode="auto"
  ppd=(Data_GetEmptyArrayP(pp))
  IF NOT(PTR_VALID(ppd)) THEN BEGIN
     printtocon, "% FlipToTomatoCoordinates: Failed to create data array." 
     return
  END
  current=(*ptr).current
  (*current).name=name
  (*pp).data=ppd
  (*ppd)=Transpose((*(*e).data),transp)
  IF (mirror[0] EQ 1) THEN BEGIN
     printtocon, "% FlipToTomatoCoordinates: Inverting x."
     (*ppd)=REVERSE((*ppd),1)
  END
  IF (mirror[1] EQ 1) THEN BEGIN
     printtocon, "% FlipToTomatoCoordinates: Inverting y."
     (*ppd)=REVERSE((*ppd),2)
  END
  IF (mirror[2] EQ 1) THEN BEGIN
     printtocon, "% FlipToTomatoCoordinates: Inverting z."
     (*ppd)=REVERSE((*ppd),3)
  END
  CreateWindow
  TVDisplay
END 
  Update_XTabControl
return
END  


PRO RevertTomatoCoordinates
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    Printtocon, "% RevertTomatoCoordinates:    Fatal error "
    Printtocon, "%   Error status  - " + STRING(error_status)
    Printtocon, "%   Error message - " + !ERROR_STATE.MSG
    ErrMsg, !ERROR_STATE.MSG
    CATCH, /Cancel
    XConsole_PopState
    return
END
 ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     printtocon, "% RevertTomatoCoordinates: Root pointer is invalid" 
     return
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% RevertTomatoCoordinates: current stack pointer is invalid" 
     return
  END
  e=(*c).datap
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% RevertTomatoCoordinates: current data pointer is invalid" 
     return
  END
 
transp=[1,2,0]
mirror=[0,0,0]
transs="(xyz)->(yzx)"
printtocon, "% RevertTomatoCoordinates: Transforming (x,y,z)->(y,z,x)." 
name="CoordinateTransform("+(*e).id+","+transs+")"
pp=DataList_CreateElement(ptr, name)
;; make sure we will not get an overflow when adding data 
(*pp).type=(*e).type
(*pp).SzX=(*e).SzY
(*pp).SzY=(*e).SzZ
(*pp).SzZ=(*e).SzX
(*pp).xsamp=(*e).ysamp
(*pp).ysamp=(*e).zsamp
(*pp).zsamp=(*e).xsamp
(*pp).slice=0
(*pp).zcoord=3
(*pp).contrastmode="auto"
ppd=(Data_GetEmptyArrayP(pp))
IF NOT(PTR_VALID(ppd)) THEN BEGIN
   printtocon, "% RevertTomatoCoordinates: Failed to create data array." 
   return
END
current=(*ptr).current
(*current).name=name
(*pp).data=ppd
(*ppd)=Transpose((*(*e).data),transp)
CreateWindow
TVDisplay  
Update_XTabControl
return
END    
