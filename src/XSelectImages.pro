FUNCTION XSelectImages, p1, p2, TITLE=title, LLABEL=llabel, RLABEL=rlabel, XLIST=xlist, SELXLIST=selxlist, LABELXLIST=labelxlist, Help=hlp
;; select two images or groups 
;; p1: pointer to first image, group
;; p2: pointer to second image
;; llabel: selection box label for first image
;; rlabel: selection box label for second image
;; xlist: optional, list of dropdown menu items
;;        used for instance to select an operator 
;; selxlist: not used
;; labelxlist: label for optional droplist menu 
;;
;; p1=GetCurrentImage()
;; p2=GetCurrentImage()
;; title='Select two images/groups'
;; llabel='image A'
;; rlabel='image B'
;;
XConsole_PushState
XConsole_WaitingForInput
IF NOT(GetDebugFlag()) THEN BEGIN
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    PrintToCon, "% XselectImages:    Fatal error "
    PrintToCon, "%   Error status  - " + STRING(error_status)
    PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
    CATCH, /Cancel
    XConsole_PopState
    return, 0
 END
END
result=0
first=GetRootP()
IF PTR_VALID(first) THEN first=(*first).FIRST ELSE return, 0

IF PTR_VALID(first) THEN BEGIN
   
if not(keyword_set(title)) THEN title="Select Images"
if not(keyword_set(llabel)) THEN llabel="Image A"
if not(keyword_set(rlabel)) THEN rlabel="Image B"
if (keyword_set(xlist)) THEN BEGIN
  if not(keyword_set(selxlist)) THEN selxtralist=0
  if not(keyword_set(labelxlist)) THEN labelxlist="Options"
END

IF PTR_VALID(first) THEN BEGIN
 names=NGET_IMAGE_NAMELIST(first)
 current=GetCurrentP()
 IF PTR_VALID(current) THEN Selected=NGET_LISTPOS(first, current)

 base = WIDGET_BASE(TITLE=title, /COLUMN) ;; , DISPLAY_NAME=GetDisplayName())
 lists = WIDGET_BASE(base, /ROW)
 l1 = WIDGET_BASE(lists, /COLUMN)
 list_1_lab = WIDGET_LABEL(l1, VALUE=llabel)
 list_1 = WIDGET_LIST(l1, XSIZE=30, VALUE = names, UVALUE = 'list_1', YSIZE = 15)
 l2 = WIDGET_BASE(lists, /COLUMN)
 list_2_lab = WIDGET_LABEL(l2, VALUE=rlabel)
 list_2 = WIDGET_LIST(l2, XSIZE=30, VALUE = names, UVALUE = 'list_2', YSIZE = 15)

 if (keyword_set(xlist)) THEN BEGIN
   anlist=WIDGET_BASE(base, /ROW)
   dlistlabel=WIDGET_LABEL(anlist, VALUE=labelxlist)
   dlist=WIDGET_DROPLIST(anlist, VALUE=xlist, UVALUE = 'dlist', YSIZE = 50)
 END


 buttons = WIDGET_BASE(base, /ROW)
 if not(keyword_set(hlp)) THEN BEGIN
    buttons=CW_BGroup(buttons, /ROW, ['   Apply   ','   Cancel   '] , Button_UVALUE=['apply','cancel'])
 END ELSE BEGIN
    buttons=CW_BGroup(buttons, /ROW, [' Apply ',' Help ', ' Cancel   '] , Button_UVALUE=['apply','help','cancel'])
 END

 WIDGET_CONTROL, list_1, SET_LIST_SELECT=Selected
 WIDGET_CONTROL, list_2, SET_LIST_SELECT=Selected
;; widpos=PlaceWidget(base) ;; , POSKEY=WidgetPos("DialogWin"))
 WIDGET_CONTROL, base, /REALIZE
 quit=0
 REPEAT BEGIN

   ev = WIDGET_Event(base)
   uv=""
   if (keyword_set(xlist)) THEN BEGIN
     IF ((ev.id EQ dlist)) THEN BEGIN
       WIDGET_CONTROL, ev.id, GET_UVALUE = uv
       IF (uv EQ 'dlist') then BEGIN
         selxlist=ev.index
         GOTO, ENDOFLOOP
       END
     END
   END
   IF ((ev.id EQ list_1) OR (ev.id EQ list_2)) THEN BEGIN
      WIDGET_CONTROL, ev.id, GET_UVALUE = uv
      CASE uv OF
      'list_1': BEGIN
                 p1=NGET_LISTPTR(first, ev.index)
                 IF NOT(PTR_VALID(p1)) THEN p1=first
                 Selected=NGET_LISTPOS(first, p1)
                 WIDGET_CONTROL, list_1, SET_LIST_SELECT=Selected
                END
      'list_2': BEGIN
                 p2=NGET_LISTPTR(first, ev.index)
                 IF NOT(PTR_VALID(p2)) THEN p2=first
                 Selected=NGET_LISTPOS(first, p2)
                 WIDGET_CONTROL, list_2, SET_LIST_SELECT=Selected
                END
      ELSE:     BEGIN
                END
      ENDCASE
   ENDIF ELSE BEGIN
      CASE ev.value of
      'apply':  BEGIN
                 result=1
                 quit=1
                END
      'cancel': BEGIN
                 quit=1
                 result=0
              END
      'help': BEGIN
         msg = dialog_info(hlp, /Info)
      END
      ENDCASE
   ENDELSE
 ENDOFLOOP:
 ENDREP UNTIL (quit EQ 1)
 END

 Widget_Control, base, /DESTROY

END  ELSE BEGIN
 PrintToCon, "% XSelectImages: image list contains no data!"
END 
XConsole_PopState
 return, result
END



FUNCTION XSelectSingleImage, p1, TITLE=title, NONE=none
XConsole_PushState
XConsole_WaitingForInput
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    PrintToCon, "% XSelectSingleImage:    Fatal error "
    PrintToCon, "%   Error status  - " + STRING(error_status)
    PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
    CATCH, /Cancel
    XConsole_PopState
    return, 0
END

result = 0
first=GetRootP()
IF PTR_VALID(first) THEN first=(*first).FIRST ELSE return, 0
IF PTR_VALID(first) THEN BEGIN

if not(keyword_set(title)) THEN title="Select Image"

IF PTR_VALID(first) THEN BEGIN
 names=NGET_IMAGE_NAMELIST(first)
 IF keyword_set(none) THEN names=[names,"[none]"]
 current=GetCurrentP()
 IF PTR_VALID(current) THEN Selected=NGET_LISTPOS(first, current)
 base = WIDGET_BASE(TITLE=title, /COLUMN) ;; , DISPLAY_NAME=GetDisplayName())
 lists = WIDGET_BASE(base, /ROW)
 l1 = WIDGET_BASE(lists, /COLUMN)
;;  list_1_lab = WIDGET_LABEL(l1, VALUE='select image')
 list_1 = WIDGET_LIST(l1, XSIZE=30, VALUE = names, UVALUE = 'list_1', YSIZE = 5)


 buttons = WIDGET_BASE(base, /ROW)
 buttons=CW_BGroup(buttons, /ROW, ['   Apply   ','   Cancel   '] , Button_UVALUE=['apply','cancel'])

 WIDGET_CONTROL, list_1, SET_LIST_SELECT=Selected

 ;; widpos=PlaceWidget(base, POSKEY=WidgetPos("DialogWin"))
 WIDGET_CONTROL, base, /REALIZE
 
 quit=0 
 REPEAT BEGIN

   ev = WIDGET_Event(base)
   uv=""
   IF ((ev.id EQ list_1)) THEN BEGIN
      WIDGET_CONTROL, ev.id, GET_UVALUE = uv
      CASE uv OF
      'list_1': BEGIN
                 p1=NGET_LISTPTR(first, ev.index)
                 IF keyword_Set(none) THEN BEGIN
                 END ELSE BEGIN
                    IF NOT(PTR_VALID(p1)) THEN p1=first
                    Selected=NGET_LISTPOS(first, p1)
                    WIDGET_CONTROL, list_1, SET_LIST_SELECT=Selected
                 END
                END
      ELSE:     BEGIN
                END
      ENDCASE
   ENDIF ELSE BEGIN
      CASE ev.value of
      'apply':  BEGIN
                 result=1
                 quit=1
                END
      'cancel': BEGIN
                 quit=1
                 result=0
                END
      ENDCASE
   ENDELSE
 ENDOFLOOP:
 ENDREP UNTIL (quit EQ 1)
 END

 Widget_Control, base, /DESTROY

END ELSE BEGIN
 PrintToCon, "% XSelectSingleImage: image list contains no data!"
END
XConsole_PopState
 return, result

END


FUNCTION XSelectMultipleImages, p1, TITLE=title, NONE=none
COMMON IMAGELIST, first, current
XConsole_PushState
XConsole_WaitingForInput
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    PrintToCon, "% XSelectMultipleImages:    Fatal error "
    PrintToCon, "%   Error status  - " + STRING(error_status)
    PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
    CATCH, /Cancel
    XConsole_PopState
    return, 0
END

result = 0

first=GetRootP()
IF PTR_VALID(first) THEN first=(*first).FIRST ELSE  return, 0
IF PTR_VALID(first) THEN BEGIN

if not(keyword_set(title)) THEN title="Select Images"

IF PTR_VALID(first) THEN BEGIN
 names=NGET_IMAGE_NAMELIST(first)
IF keyword_set(none) THEN names=[names,"[none]"]
 IF PTR_VALID(current) THEN Selected=NGET_LISTPOS(first, current)

 base = WIDGET_BASE(TITLE=title, /COLUMN) ;; , DISPLAY_NAME=GetDisplayName())
 lists = WIDGET_BASE(base, /ROW)
 l1 = WIDGET_BASE(lists, /COLUMN)
;;  list_1_lab = WIDGET_LABEL(l1, VALUE='select image')
 list_1 = WIDGET_LIST(l1, XSIZE=60, VALUE = names, UVALUE = 'list_1', YSIZE = 20, /MULTIPLE)


 buttons = WIDGET_BASE(base, /ROW)
 buttons=CW_BGroup(buttons, /ROW, ['   Apply   ','   Cancel   '] , Button_UVALUE=['apply','cancel'])

 WIDGET_CONTROL, list_1, SET_LIST_SELECT=Selected

 ;; widpos=PlaceWidget(base, POSKEY=WidgetPos("DialogWin"))
 WIDGET_CONTROL, base, /REALIZE
 
 quit=0 
 REPEAT BEGIN

   ev = WIDGET_Event(base)
   uv=""
   IF ((ev.id EQ list_1)) THEN BEGIN
      WIDGET_CONTROL, ev.id, GET_UVALUE = uv
      CASE uv OF
      'list_1': BEGIN
                 p1=NGET_LISTPTR(first, ev.index)
                 ;; IF keyword_Set(none) THEN BEGIN
                 ;; END ELSE BEGIN
                 ;;   IF NOT(PTR_VALID(p1)) THEN p1=first
                 ;;   Selected=NGET_LISTPOS(first, p1)
                 ;;   WIDGET_CONTROL, list_1, SET_LIST_SELECT=Selected
                 ;;END
              END 
      ELSE:     BEGIN
                END
      ENDCASE
   ENDIF ELSE BEGIN
      CASE ev.value of
      'apply':  BEGIN
                 quit=1
                 Selected=Widget_Info(list_1, /LIST_SELECT)
                 IF (N_Elements(Selected) GE 1) THEN BEGIN
                    ret=PTRARR(N_Elements(Selected))
                    FOR i=1,N_Elements(Selected) DO BEGIN
                       ret[i-1]=NGET_LISTPTR(first,Selected(i-1))
                    END
                 END ELSE ret=[PTR_NEW()]
                END
      'cancel': BEGIN
                 quit=1
                 ret=[PTR_NEW()]
                END
      ENDCASE
   ENDELSE
 ENDOFLOOP:
 ENDREP UNTIL (quit EQ 1)
 END
 
 Widget_Control, base, /DESTROY

END ELSE BEGIN
 PrintToCon, "% XSelectMultipleImages: Image list contains no data!"
END
XConsole_PopState
 return, ret

END

FUNCTION XSelectImageData, p1, p2, TITLE=title, LLABEL=llabel, RLABEL=rlabel, XLIST=xlist, SELXLIST=selxlist, LABELXLIST=labelxlist
COMMON IMAGELIST, first, current
XConsole_PushState
XConsole_WaitingForInput
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    PrintToCon, "% XselectImages:    Fatal error "
    PrintToCon, "%   Error status  - " + STRING(error_status)
    PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
    CATCH, /Cancel
    XConsole_PopState
    return, 0
END
result=0
firstdata=GetRootP()
currentdata=GetCurrentP()	
IF (PTR_VALID(first) and PTR_VALID(firstdata)) THEN BEGIN

if not(keyword_set(title)) THEN title="Select Images"
if not(keyword_set(llabel)) THEN llabel="Image A"
if not(keyword_set(rlabel)) THEN rlabel="Image B"
if (keyword_set(xlist)) THEN BEGIN
  if not(keyword_set(selxlist)) THEN selxtralist=0
  if not(keyword_set(labelxlist)) THEN labelxlist="Options"
END

IF PTR_VALID(first) THEN BEGIN
 
 names=NGET_IMAGE_NAMELIST(first)
 datanames=NGET_IMAGE_NAMELIST(firstdata)
	
 IF PTR_VALID(current) THEN Selected=NGET_LISTPOS(first, current)
IF PTR_VALID(currentdata) THEN Selected=NGET_LISTPOS(firstdata, currentdata)

 base = WIDGET_BASE(TITLE=title, /COLUMN) ;; , DISPLAY_NAME=GetDisplayName())
 lists = WIDGET_BASE(base, /ROW)
 l1 = WIDGET_BASE(lists, /COLUMN)
 list_1_lab = WIDGET_LABEL(l1, VALUE=llabel)
 list_1 = WIDGET_LIST(l1, XSIZE=30, VALUE = names, UVALUE = 'list_1', YSIZE = 5)
 l2 = WIDGET_BASE(lists, /COLUMN)
 list_2_lab = WIDGET_LABEL(l2, VALUE=rlabel)
 list_2 = WIDGET_LIST(l2, XSIZE=30, VALUE = datanames, UVALUE = 'list_2', YSIZE = 5)

 if (keyword_set(xlist)) THEN BEGIN
   anlist=WIDGET_BASE(base, /ROW)
   dlistlabel=WIDGET_LABEL(anlist, VALUE=labelxlist)
   dlist=WIDGET_DROPLIST(anlist, VALUE=xlist, UVALUE = 'dlist', YSIZE = 50)
 END


 buttons = WIDGET_BASE(base, /ROW)
 buttons=CW_BGroup(buttons, /ROW, ['   Apply   ','   Cancel   '] , Button_UVALUE=['apply','cancel'])

 WIDGET_CONTROL, list_1, SET_LIST_SELECT=Selected
 WIDGET_CONTROL, list_2, SET_LIST_SELECT=Selected
widpos=PlaceWidget(base, POSKEY=WidgetPos("DialogWin"))
 WIDGET_CONTROL, base, XOFFSET=widpos[0], YOFFSET=widpos[1], /REALIZE
 quit=0
 REPEAT BEGIN

   ev = WIDGET_Event(base)
   uv=""
   if (keyword_set(xlist)) THEN BEGIN
     IF ((ev.id EQ dlist)) THEN BEGIN
       WIDGET_CONTROL, ev.id, GET_UVALUE = uv
       IF (uv EQ 'dlist') then BEGIN
         selxlist=ev.index
         GOTO, ENDOFLOOP
       END
     END
   END
   IF ((ev.id EQ list_1) OR (ev.id EQ list_2)) THEN BEGIN
      WIDGET_CONTROL, ev.id, GET_UVALUE = uv
      CASE uv OF
      'list_1': BEGIN
                 p1=NGET_LISTPTR(first, ev.index)
                 IF NOT(PTR_VALID(p1)) THEN p1=first
                 Selected=NGET_LISTPOS(first, p1)
                 WIDGET_CONTROL, list_1, SET_LIST_SELECT=Selected
                END
      'list_2': BEGIN
                 p2=NGET_LISTPTR(firstdata, ev.index)
                 IF NOT(PTR_VALID(p2)) THEN p2=firstdata
                 Selected=NGET_LISTPOS(firstdata, p2)
                 WIDGET_CONTROL, list_2, SET_LIST_SELECT=Selected
                END
      ELSE:     BEGIN
                END
      ENDCASE
   ENDIF ELSE BEGIN
      CASE ev.value of
      'apply':  BEGIN
                 result=1
                 quit=1
                END
      'cancel': BEGIN
                 quit=1
                 result=0
                END
      ENDCASE
   ENDELSE
 ENDOFLOOP:
 ENDREP UNTIL (quit EQ 1)
 END

 Widget_Control, base, /DESTROY

END  ELSE BEGIN
 PrintToCon, "% XSelectImages: image list contains no data!"
END 
XConsole_PopState
 return, result
END


FUNCTION XSelectMultipleImagesDropList, parr, label, TITLE=title, NONE=none
XConsole_PushState
XConsole_WaitingForInput
IF NOT(GetDebugFlag()) THEN BEGIN
   CATCH, Error_status
   IF (Error_status NE 0) THEN BEGIN
      PrintToCon, "% XSelectMultipleImages:    Fatal error "
      PrintToCon, "%   Error status  - " + STRING(error_status)
      PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
      CATCH, /Cancel
      XConsole_PopState
      return, 0
   END
END
NIm=N_Elements(label)
Parr=PTRARR(NIm)
result=0
first=GetRootP()
IF PTR_VALID(first) THEN BEGIN
   If not(keyword_set(title)) THEN title="Select Images"
   names=NGET_IMAGE_NAMELIST(first)
   if keyword_set(none) THEN BEGIN
      Nnames=N_Elements(names)
      tmp=STRARR(Nnames+1)
      tmp[0]="none"
      tmp[1:Nnames]=names
      names=tmp
   END
   pixperchar=8
   labelsizex=FIX(Max(strlen(label))*pixperchar)
   namesizex=FIX(Max(strlen(names))*pixperchar)
   current=GetCurrentP()
   IF PTR_VALID(current) THEN Selected=NGET_LISTPOS(first, current)
   
   base = WIDGET_BASE(TITLE=title, /COLUMN) ;; , DISPLAY_NAME=GetDisplayName())

   ;; drop list rows
   labelid=LONARR(NIm)
   droplistid=LONARR(NIm)
   FOR i=1,NIm DO BEGIN
      listrow = WIDGET_BASE(base, /ROW)
      labelid[i-1] = WIDGET_LABEL(listrow, VALUE=label[i-1], XSIZE=labelsizex)
      droplistid[i-1] = WIDGET_DROPLIST(listrow, XSIZE=namesizex, VALUE = names, /FLAT)
   END
   ;; button row
   buttons = WIDGET_BASE(base, /ROW)
   buttons=CW_BGroup(buttons, /ROW, ['   Apply   ','   Cancel   '] , Button_UVALUE=['apply','cancel'])
   ;; set initial drop list status
   FOR i=1,NIm DO BEGIN
      WIDGET_CONTROL, droplistid[i-1], SET_DROPLIST_SELECT=Selected
   END
   widpos=PlaceWidget(base, POSKEY=WidgetPos("DialogWin"))
   WIDGET_CONTROL, base, XOFFSET=widpos[0], YOFFSET=widpos[1], /REALIZE
   ;; start loop
   quit=0
   REPEAT BEGIN
      ev = WIDGET_Event(base)
      ;; check whether drop list was selected
      For i=1, Nim DO BEGIN
         IF (ev.id EQ droplistid[i-1]) THEN BEGIN 
            print, ev.index
            if keyword_set(none) THEN BEGIN
               IF (ev.index EQ 0) THEN  BEGIN
                  parr[i-1]=PTR_NEW()
                  print, "selected none"
               END ELSE BEGIN
                  p1=NGET_LISTPTR(first, ev.index-1)
                  IF PTR_VALID(p1) THEN BEGIN 
                     parr[i-1]=p1
                     Selected=NGET_LISTPOS(first, p1)+1
                     WIDGET_CONTROL, droplistid[i-1], SET_DROPLIST_SELECT=Selected
                     printtocon, "selected"+(*p1).comment
                  END
               END
            END ELSE BEGIN
               p1=NGET_LISTPTR(first, ev.index)
               IF PTR_VALID(p1) THEN BEGIN
                  parr[i-1]=p1
                  Selected=NGET_LISTPOS(first, p1)
                  WIDGET_CONTROL, droplistid[i-1], SET_DROPLIST_SELECT=Selected
               END
            END
         END
      END  
      
      IF (ev.id EQ buttons) THEN BEGIN
         CASE ev.value of
            'apply':  BEGIN
               result=1
               quit=1
            END
            'cancel': BEGIN
               quit=1
               result=0
               parr[*]=PTR_NEW()
            END
         ENDCASE
      END
     
   ENDREP UNTIL (quit EQ 1)
   Widget_Control, base, /DESTROY
END  ELSE BEGIN
   PrintToCon, "% XSelectImages: image list contains no data!"
END 
XConsole_PopState
return, result
END 
