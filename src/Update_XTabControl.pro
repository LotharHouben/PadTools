PRO Slider_Control, psliderstate, SET=set, REFRESH=refresh, AVGSET=avgset
IF (GetDebugFlag()) THEN BEGIN
  END ELSE BEGIN
     CATCH, Error_status
     IF (Error_status NE 0) THEN BEGIN
        Print, "% Slider_Control:    Fatal error "
        Print, "%   Error status  - " + STRING(error_status)
        Print, "%   Error message - " + !ERROR_STATE.MSG
        ;; ErrMsg, !ERROR_STATE.MSG
        CATCH, /Cancel
        return
     END
  END
  stackptr=PTR_NEW()
  ptr=GetRootP()
  IF (PTR_VALID(ptr)) THEN BEGIN
     c=(*ptr).current
     IF (PTR_VALID(c)) THEN BEGIN
        e=(*c).datap
        IF (PTR_VALID(e)) THEN BEGIN
           (*psliderstate).stackp=c
        END
     END
  END 
  IF PTR_VALID((*psliderstate).stackp) THEN BEGIN
     ;; auto-determine stack size
     datap=(*e).data
     IF (SIZE(*datap, /N_DIMENSIONS) GT 2) THEN BEGIN
        N=Size(*datap, /DIMENSIONS)
        (*psliderstate).min=0L
        (*psliderstate).max=LONG(N[(*e).zcoord-1]-1)
        (*psliderstate).active=1
        (*psliderstate).slice=(*E).slice
        (*psliderstate).width=(*e).slicewidth
        ;; preset slice
        IF (N_Elements(set) GT 0) THEN BEGIN
           IF ((set LE (*psliderstate).max) AND (set GE 0)) THEN BEGIN
              (*Psliderstate).slice=set
              (*e).slice=set
           END
        END
        ;; set avg width
        IF (N_Elements(avgset) GT 0) THEN BEGIN
           IF ((avgset LE (*psliderstate).max) AND (avgset GT 0)) THEN BEGIN
              (*psliderstate).Width=avgset
              (*e).slicewidth=avgset
           END ELSE  BEGIN
              (*psliderstate).width=1L ;; something is wrong, ignore averaging
              (*e).slicewidth=1L
           END
        END
        ;; check slice and averaging
        tmp=(*E).slice+((*psliderstate).width-1L)
        IF (tmp GT (*psliderstate).max) THEN BEGIN ;; right averaging is beyond range
           (*E).slice=(*psliderstate).max-(*psliderstate).width+1L
           (*psliderstate).slice=(*E).slice      
        END
       ;; we should have a valid pair of slice and right-hand side average      
       TVDisplay
     END  ELSE BEGIN
     (*psliderstate).active=0
     (*psliderstate).max=1
     (*psliderstate).min=0
     (*psliderstate).width=1L
     (*psliderstate).slice=0
  END
  END ELSE BEGIN
     (*psliderstate).active=0
     (*psliderstate).max=1
     (*psliderstate).min=0
     (*psliderstate).width=1L
     (*psliderstate).slice=0
  END
  WIDGET_CONTROL, (*psliderstate).id, Set_SLIDER_MIN=(*psliderstate).min, SET_SLIDER_MAX=(*psliderstate).max, SET_VALUE=(*psliderstate).slice, SENSITIVE=(*psliderstate).active
  IF ((*psliderstate).avgid GT 1L) THEN BEGIN
     WIDGET_CONTROL, (*psliderstate).avgid, Set_SLIDER_MIN=(*psliderstate).min, SET_SLIDER_MAX=(*psliderstate).max, SET_VALUE=(*psliderstate).width, SENSITIVE=(*psliderstate).active
  END 
END 



FUNCTION ImageInfo
  ptr=GetRootP()
  s=['no description available']
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     print, "% ImageInfo: Root pointer is invalid"
     return, s
  END
  p=(*ptr).current
  IF (NOT(PTR_VALID(p))) THEN BEGIN
     print, "% ImageInfo: current pointer is invalid"
     return, s
  END
 IF (PTR_VALID(p)) THEN BEGIN
    datap=(*p).datap
     IF (PTR_VALID(datap)) THEN BEGIN
        IF (GetContrastMode() EQ "auto") THEN BEGIN
           contraststr="automatic ("+(*datap).contrastsubmode +")"
        END ELSE BEGIN
           contraststr="fix cut-off ("+MyString(((*datap).contrast)[0])+' : '+MyString(((*datap).contrast)[1])+")"
        END
        IF (Obj_Valid((*datap).radongeometry)) THEN rspacegeom='initialized' ELSE rspacegeom='-'
         IF (Obj_Valid((*datap).extra)) THEN extraobj=TypeName((*datap).extra) ELSE extraobj='-'
        IF PTR_VALID((*datap).plink) THEN BEGIN 
           link=MyString((*(*datap).plink).name)
        END ELSE BEGIN
           link='none'
        END
        s=["name    : "+ (*p).name, $
           "type    : "+ type_spec((*datap).type), $
           "voltage : "+ MySTRING((*datap).voltage) +" V", $
           "size    : " + MySTRING((*datap).SzX) +" x "+ MySTRING((*datap).SzY) +" x "+ MySTRING((*datap).SzZ),  $
           "sampling: " + MySTRING((*datap).xsamp) + " x " + MySTRING((*datap).ysamp) + " x " + MySTRING((*datap).zsamp), $
           "units   : " + MySTRING((*datap).xunit) + " x " + MySTRING((*datap).yunit) + " x " + MySTRING((*datap).zunit), $
      "proj    : "+ MyString((*datap).zcoord), $
      "slice   : "+ MyString((*datap).slice), $
      "contrast: "+contraststr,  $
           ;;          "geometry: "+  rspacegeom, $
           "scalebar: "+MyString((*datap).DisplayScaleBar), $
           "extra   : "+  extraobj, $
      "link    : "+  link $
          ]  
     END
 END
 return, s
END

FUNCTION GraphInfo
  ptr=GetRootPXYGraph()
  s=['no description available']
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     print, "% GraphInfo: Graph Root pointer is invalid"
     return, s
  END
  p=(*ptr).current
  IF (NOT(PTR_VALID(p))) THEN BEGIN
     ;; print, "% GraphInfo: current graph pointer is invalid"
     return, s
  END
 IF (PTR_VALID(p)) THEN BEGIN
    datap=(*p).datap
     IF (PTR_VALID(datap)) THEN BEGIN
           s=["graph name     : " + (*p).name]  
        mygraph=(*datap).pobj
        IF OBJ_VALID(mygraph) THEN BEGIN
           s=[s,$
              "no of data sets: "+STRTRIM(STRING(mygraph->NSets()),2)]
        END
     END
 END
 return, s
END

FUNCTION TableInfo
  ptr=GetRootPTable()
  s=['no description available']
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     print, "% TableInfo: Table root pointer is invalid"
     return, s
  END
  p=(*ptr).current
  IF (NOT(PTR_VALID(p))) THEN BEGIN
     print, "% TableInfo: Current table pointer is invalid"
     return, s
  END
 IF (PTR_VALID(p)) THEN BEGIN
    datap=(*p).datap
     IF (PTR_VALID(datap)) THEN BEGIN
           s=["table name     : " + (*p).name]  
        mytable=(*datap).pobj
        IF OBJ_VALID(mytable) THEN BEGIN
           s=[s,$
              "no of data sets: "+STRTRIM(STRING(mygraph->NSets()),2)]
        END
     END
 END
 return, s
END


PRO Update_XTabControl
COMMON XTABCONTROLID, ilist, itext, glist, gilist, gtext, dlist, tlist, dtext, ttext, conbutton, coffbutton, slider_id, base, avgslider_id
;;IF (GetDebugFlag()) THEN BEGIN
   IF (1 EQ 1) THEN BEGIN
  END ELSE BEGIN
     CATCH, Error_status
     IF (Error_status NE 0) THEN BEGIN
        Print, "% Update_XTabControl: Error"
        Print, "%            " + !ERR_STRING
        (*winp).wdow = -1
        ErrMsg, !ERR_STRING
        CATCH, /Cancel
        return
     END
  END
  ;;
  IF (XREGISTERED('XTabControl')) THEN BEGIN
  ;;
  ;; first update the image list
  ;; 
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     print, "% XTabControl: Root pointer invalid"
     return
  END
  first=(*ptr).first
  current=(*ptr).current
  names=DataList_GetNames(ptr) ;; see DataStack.pro for details
  namesSize = size(names)    
  Selected=0
  ctr=0
  IF PTR_VALID(current) THEN BEGIN
     Selected=WHERE(names EQ (*current).name)
     While (N_ELEMENTS(Selected) GT 1) Do BEGIN
     print, "% Update_XTabControl: duplicate name found in image list"
     (*current).name=(*current).name+"["+MyString(ctr)+"]"
     ctr=ctr+1
     names=DataList_GetNames(ptr)
     Selected=WHERE(names EQ (*current).name)
  END 
  END
  IF (Selected EQ -1) THEN BEGIN 
   print, "% Update_XTabControl: current image pointer not found in image list"
   Selected=0
   (*ptr).current=(*ptr).first 
   current=(*ptr).current
  END                 

  WIDGET_CONTROL, ilist, SET_VALUE=names, SET_LIST_SELECT=Selected
  ;;
  ;; finally, update the info text boxes
  ;;
  WIDGET_CONTROL, itext, SET_VALUE=ImageInfo()
  ;; 
  ;; now update the graph list
  ptr=GetRootPXYGraph()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     ;; print, "% XTabControl: XY Graph Root pointer invalid"
     return
  END
  first=(*ptr).first
  current=(*ptr).current
  names=DataList_GetNames(ptr) ;; see DataStack.pro for details
  namesSize = size(names)    
  Selected=0
  ctr=0
  IF PTR_VALID(current) THEN BEGIN
     Selected=WHERE(names EQ (*current).name)
     While (N_ELEMENTS(Selected) GT 1) Do BEGIN
     print, "% Update_XTabControl: duplicate name found in image list"
     (*current).name=(*current).name+"["+MyString(ctr)+"]"
     ctr=ctr+1
     names=DataList_GetNames(ptr)
     Selected=WHERE(names EQ (*current).name)
  END 
  END
  IF (Selected EQ -1) THEN BEGIN 
   print, "% Update_XTabControl: current image pointer not found in image list"
   Selected=0
   (*ptr).current=(*ptr).first 
   current=(*ptr).current
  END                 

  WIDGET_CONTROL, dlist, SET_VALUE=names, SET_LIST_SELECT=Selected
  ;;
  ;; finally, update the info text boxes
  ;;
  WIDGET_CONTROL, dtext, SET_VALUE=GraphInfo()
  ;;
  ;; now update the table list
  ptr=GetRootPTable()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     ;; print, "% XTabControl: Table root pointer invalid"
     return
  END
  first=(*ptr).first
  current=(*ptr).current
  names=DataList_GetNames(ptr) ;; see DataStack.pro for details
  namesSize = size(names)    
  Selected=0
  ctr=0
  IF PTR_VALID(current) THEN BEGIN
     Selected=WHERE(names EQ (*current).name)
     While (N_ELEMENTS(Selected) GT 1) Do BEGIN
     print, "% Update_XTabControl: duplicate name found in image list"
     (*current).name=(*current).name+"["+MyString(ctr)+"]"
     ctr=ctr+1
     names=DataList_GetNames(ptr)
     Selected=WHERE(names EQ (*current).name)
  END 
  END
  IF (Selected EQ -1) THEN BEGIN 
   print, "% Update_XTabControl: current image pointer not found in image list"
   Selected=0
   (*ptr).current=(*ptr).first 
   current=(*ptr).current
  END                 

  WIDGET_CONTROL, tlist, SET_VALUE=names, SET_LIST_SELECT=Selected
  ;;
  ;; finally, update the info text boxes
  ;;
  WIDGET_CONTROL, dtext, SET_VALUE=GraphInfo()
  ;; and the widget slider
  WIDGET_CONTROL, slider_id, GET_UVALUE=psliderstate
  Slider_Control, psliderstate, /REFRESH
  ;;
END 
END
