FUNCTION XScaleVolDialog, scale, TITLE=title, TEXT=text, Apply=apply, Cancel=cancel
;; scale is a 3-element vector for the scaling factors along x,y,z
  IF (GetDebugFlag()) THEN BEGIN
  END ELSE BEGIN
     CATCH, Error_status
     IF (Error_status NE 0) THEN BEGIN
        Print, "% XScaleVolDialog:    Fatal error "
        Print, "%   Error status  - " + STRING(error_status)
        Print, "%   Error message - " + !ERROR_STATE.MSG
        CATCH, /Cancel
        return, 0
     END
  END 
result=1 ;; set return result to error
IF NOT(Keyword_set(title)) THEN title='Volume Scaling Parameters'
IF NOT(Keyword_set(text)) THEN text='Scale factor x, y, z: '
IF NOT(Keyword_set(apply)) THEN apply=' Scale '
IF NOT(Keyword_set(cancel)) THEN cancel=' Cancel '
base=widget_base(Title=title, /Column)
frame1 = WIDGET_BASE(base,/Column)
row11 = WIDGET_BASE(frame1,/Row)
text=Widget_Label(row11, VALUE=text)
scale_labelx = CW_Field(row11, TITLE='', XSize=5, Value=MySTRING(scale[0]))
scale_labely = CW_Field(row11, TITLE='', XSize=5, Value=MySTRING(scale[1]))
scale_labelz = CW_Field(row11, TITLE='', XSize=5, Value=MySTRING(scale[2]))


c=WIDGET_BASE(base, /ROW)
apply = WIDGET_BUTTON(c, value=apply, UVALUE='apply')
cancel = WIDGET_BUTTON(c, value=cancel, UVALUE='cancel')

WIDGET_CONTROL, base, /REALIZE
 
quit=0
REPEAT BEGIN
  ev = WIDGET_Event(base)
  WIDGET_CONTROL, ev.id, GET_UVALUE = uv
  
  CASE uv OF
     'apply': BEGIN
        result=0
        quit=1
        TMP='' & WIDGET_Control, scale_labelx, GET_VALUE=TMP
        TMP=TMP(0) & IF (TMP NE '') THEN scale[0]=FLOAT(TMP)
        TMP='' & WIDGET_Control, scale_labely, GET_VALUE=TMP
        TMP=TMP(0) & IF (TMP NE '') THEN scale[1]=FLOAT(TMP)
        TMP='' & WIDGET_Control, scale_labelz, GET_VALUE=TMP
        TMP=TMP(0) & IF (TMP NE '') THEN scale[2]=FLOAT(TMP)
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


PRO ScaleVol, SCALE=scale
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     print, "% ScaleVol: Root pointer is invalid" 
     return
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     print, "% ScaleVol: current stack pointer is invalid" 
     return
  END
  e=(*c).datap
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     print, "% ScaleVol: current data pointer is invalid" 
     return
  END
  IF NOT(keyword_set(scale)) THEN BEGIN
     scale=FLTArr(3)
     scale=[1,0.5,0.5]
     IF (XScaleVolDialog(scale) GT 0) THEN BEGIN
        print, "% ScaleVol: cancelled" 
        return
     END
  END
  ;;Create a new stack
  name="Scaled("+(*e).id+",["+MyString(scale[0])+","+MyString(scale[1])+","+MyString(scale[2])+"])"
  pp=DataList_CreateElement(ptr, name)
  (*pp).type=(*e).type
  ;; correct scale factors
  Printtocon, "% ScaleVol"
  Printtocon, "     desired scaling (x,y,z): "+MyString(scale[0])+","+MyString(scale[1])+","+MyString(scale[2])
  truescale=scale
  scaleerr=[0.,0.,0.]
  fract=(*e).SzX*scale[0]
  intfract=ABS(ROUND(fract))
  truescale[0]=FLOAT(intfract)/FLOAT((*e).SzX)
  scaleerr[0]=100.*(FLOAT(truescale[0])-FLOAT(scale[0]))/FLOAT(scale[0])
  (*pp).SzX=intfract 
  fract=(*e).SzY*scale[1]
  intfract=ABS(ROUND(fract))
  truescale[1]=FLOAT(intfract)/FLOAT((*e).SzY)
  scaleerr[1]=100.*(FLOAT(truescale[1])-FLOAT(scale[1]))/FLOAT(scale[1])
  (*pp).SzY=intfract 
  fract=(*e).SzZ*scale[2]
  intfract=ABS(ROUND(fract))
  truescale[2]=FLOAT(intfract)/FLOAT((*e).SzZ)
  scaleerr[2]=100.*(FLOAT(truescale[2])-FLOAT(scale[2]))/FLOAT(scale[2])
  (*pp).SzZ=intfract
  Printtocon, "     true scaling (x,y,z)   : "+MyString(scale[0])+","+MyString(scale[1])+","+MyString(scale[2])
  Printtocon, "     error in % (x,y,z)     : "+MyString(scaleerr[0])+","+MyString(scaleerr[1])+","+MyString(scaleerr[2])
  ;; 
  (*pp).xsamp=(*e).xsamp/truescale[0] 
  (*pp).ysamp=(*e).ysamp/truescale[1] 
  (*pp).zsamp=(*e).zsamp/truescale[2] 
  ;;  

  ppd=(Data_GetEmptyArrayP(pp))
  IF NOT(PTR_VALID(ppd)) THEN BEGIN
     print, "% ScaleVol: failed to create data array for the projected images" 
     return
  END
  (*pp).data=ppd
  ;; Scale volume
  (*ppd)=CONGRID((*(*e).data), (*pp).SzX, (*pp).SzY, (*pp).SzZ, /CENTER,/MINUS_ONE)
  ;; current stack is *pp, display the projections on the screen  
  (*pp).slice=0
  (*pp).zcoord=2
  (*pp).contrastmode="auto"
  IF NOT(GetCommandLineMode()) THEN BEGIN
     CreateWindow
     TVDisplay
     Update_XTabControl
  END
END 



