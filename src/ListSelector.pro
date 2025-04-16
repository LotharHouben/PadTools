FUNCTION XListSelector, TITLE=title
 IF (GetDebugFlag()) THEN BEGIN
  END ELSE BEGIN
     CATCH, Error_status
     IF (Error_status NE 0) THEN BEGIN
        Print, "% XListSelector:    Fatal error "
        Print, "%   Error status  - " + STRING(error_status)
        Print, "%   Error message - " + !ERROR_STATE.MSG
        ErrMsg, !ERROR_STATE.MSG
        CATCH, /Cancel
        return, 0
     END
  END
  ;; ++++++++++++++++++++++++++++++++
  ;; first update the image list
  ;; and determine the selected image
  ;; ++++++++++++++++++++++++++++++++
  pselected=0
  IF NOT(keyword_set(title)) then title='Select Data Stack'
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     print, "% XListSelector: Root pointer invalid"
     return, 0
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
        print, "% XListSelector: duplicate name found in image list"
        (*current).name=(*current).name+"["+MyString(ctr)+"]"
        ctr=ctr+1
        names=DataList_GetNames(ptr)
        Selected=WHERE(names EQ (*current).name)
     END 
     pselected=current
  END
  IF (Selected EQ -1) THEN BEGIN 
   print, "% ListSelector: current image pointer not found in image list"
   (*ptr).current=(*ptr).first 
   current=(*ptr).current
   return, 0
 END 
  ;; ++++++++++++++++++++++++++++++++
  ;; create the widget
  ;; and determine the selected image
  ;; ++++++++++++++++++++++++++++++++
  XW=400
  base = WIDGET_BASE(TITLE="Data Stack Selector", /COLUMN, XSIZE=XW) 
  frame0=Widget_Base(base,/COLUMN)
  progresstext=WIDGET_LABEL(frame0, /ALIGN_LEFT, VALUE=title)
  listitems=names ;; list
  ilist = WIDGET_LIST(frame0, VALUE = listitems, UVALUE = 'LIST', YSIZE = 10)
  brow=WIDGET_BASE(base, /ROW)
  help = WIDGET_BUTTON(brow,   VALUE = ' Accept ', UVALUE = 'apply')
  quit = WIDGET_BUTTON(brow,   VALUE = ' Cancel ', UVALUE = 'quit')
  widpos=PlaceWidget(base, POSKEY="c")
  Widget_Control, base, XOFFSET=widpos[0], YOFFSET=widpos[1]+100, /REALIZE
  WIDGET_CONTROL, ilist, SET_LIST_SELECT=Selected
  quit = 0
  REPEAT BEGIN
     ev = WIDGET_Event(base)
     WIDGET_CONTROL, ev.id, GET_UVALUE = uv
       ;; print, uv
     CASE uv OF
        'LIST': BEGIN
                                ; The list has been touched.
                                ; The index of the list item touched is held in
                                ; 'event.index'... how convenient!
           ptr=GetRootP()
           x=(*ptr).first
           i=0
           WHILE (i LT ev.index) DO BEGIN
              x=(*x).next
              i=i+1
           END
           IF PTR_VALID(x) THEN BEGIN
              pselected=x
           END ELSE printToCon, "% XListSelector: index not found in stack list, current stack pointer not changed"
        END 
        'quit': BEGIN
           quit=1
           pselected=0 ;; return invalid pointer
        END 
        'apply': BEGIN
           quit=1
        END
        ELSE: 
     END 
  ENDREP  UNTIL (quit EQ 1)
  Widget_Control, base, /DESTROY
  return, pselected
END 

FUNCTION DataStackList
  
PrintToCon, "% DataStackList: Reading Data Stack List."
     ;; ++++++++++++++++++++++++++++++++
     ;; first update the image list
     ;; and determine the selected image
     ;; ++++++++++++++++++++++++++++++++
     pselected=0
     ptr=GetRootP()
     IF (NOT(PTR_VALID(ptr))) THEN BEGIN
        print, "% XListSelector: Root pointer invalid"
        return, 0
     END
     first=(*ptr).first
     current=(*ptr).current
     list=DataList_GetNames(ptr) ;; see DataStack.pro for details
     return, list
  END


FUNCTION XListChoice, LIST=list, TITLE=title, Multiple=multiple
;;
;; choose list items
;; returns -1 upon failure or cancellation
;; 
;;
 IF (GetDebugFlag()) THEN BEGIN
  END ELSE BEGIN
     CATCH, Error_status
     IF (Error_status NE 0) THEN BEGIN
        PrintToCon, "% XListChoice:    Fatal error "
        PrintToCon, "%   Error status  - " + STRING(error_status)
        PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
        ErrMsg, !ERROR_STATE.MSG
        CATCH, /Cancel
        return, -1
     END
  END
  ;; ++++++++++++++++++++++++++++++++
  ;; first update the image list
  ;; and determine the selected image
  ;; ++++++++++++++++++++++++++++++++
  selected=0
  IF NOT(keyword_set(title)) then title='Select List Elements'
  IF NOT(Keyword_set(list)) then BEGIN
     PrintToCon, "% XListChoice: No list, returning. "
     return, -1
  END
  ListSize = size(list)    
  ;; ++++++++++++++++++++++++++++++++
  ;; create the widget
  ;; and determine the selected image
  ;; ++++++++++++++++++++++++++++++++
  XW=350
  base = WIDGET_BASE(TITLE=title, /COLUMN, XSIZE=XW) 
  frame0=Widget_Base(base,/COLUMN)
  progresstext=WIDGET_LABEL(frame0, /ALIGN_LEFT, VALUE=title)
  listitems=list ;; list
  ilist = WIDGET_LIST(frame0, VALUE = listitems, UVALUE = 'LIST', YSIZE = 10, MULTIPLE=multiple, XSIZE=XW)
  brow=WIDGET_BASE(base, /ROW)
  help = WIDGET_BUTTON(brow,   VALUE = ' Accept ', UVALUE = 'apply')
  help = WIDGET_BUTTON(brow,   VALUE = ' All ', UVALUE = 'all')
  help = WIDGET_BUTTON(brow,   VALUE = ' Even ', UVALUE = 'even')
  help = WIDGET_BUTTON(brow,   VALUE = ' Invert ', UVALUE = 'rev') 
  quit = WIDGET_BUTTON(brow,   VALUE = ' Cancel ', UVALUE = 'quit')
  widpos=PlaceWidget(base, POSKEY="c")
  Widget_Control, base, XOFFSET=widpos[0], YOFFSET=widpos[1]+100, /REALIZE
  WIDGET_CONTROL, ilist, SET_LIST_SELECT=Selected
  quit = 0
  REPEAT BEGIN
     ev = WIDGET_Event(base)
     WIDGET_CONTROL, ev.id, GET_UVALUE = uv
       ;; print, uv
     CASE uv OF
        'LIST': BEGIN
                                ; The list has been touched.
                                ; The index of the list item touched is held in
                                ; 'event.index'... how convenient!
           ;; i=ev.index
 
        END
        'all': BEGIN
           ;; mark all
           Widget_Control, ilist, SET_LIST_SELECT=pselected
           pselected=LINDGEN(N_Elements(List))
           Widget_Control, ilist, SET_LIST_SELECT=pselected
        END
        'even': BEGIN
           tmp=LINDGEN(N_Elements(List))
           B=WHERE((tmp MOD 2) EQ 0)
           pselected=tmp(B)
           Widget_Control, ilist, SET_LIST_SELECT=pselected
        END
        'rev':BEGIN
           tmp=LINDGEN(N_Elements(List))
           pselected=Widget_Info(ilist, /LIST_SELECT) 
           FOR j=0,N_ELEMENTS(pselected)-1 DO BEGIN
              tmp(pselected[j])=-1
           END
           B=WHERE(tmp GE 0)
           pselected=tmp(B)
           Widget_Control, ilist, SET_LIST_SELECT=pselected
        END   
        'quit': BEGIN
           quit=1
           pselected=-1 ;; return invalid pointer
        END
        'apply': BEGIN
           quit=1
           pselected=Widget_Info(ilist, /LIST_SELECT) 
        END
        ELSE: 
     END 
     ENDREP UNTIL (quit EQ 1)
  Widget_Control, base, /DESTROY
  ;; STOP
  return, pselected
END
