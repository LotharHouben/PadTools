FUNCTION XMChoiceDialog, mclist, title, HORIZ=horiz, HELP=hlp,  TEXT=text, SELECTED=selected, INDEX=index
;;
;; Multiple Choice Dialog
;;
;; returns choice out of mclist, "" if user cancelled
;; if index is set, the then index in the array mclist is returned
;; (starting from 1!)
;; Example: 
;;
;;
;;       choices = ["chicken","beef","vegetarian"]
;;        title = "What would you like to have?"
;;        res = XMChoiceDialog(choices, title)
;;        IF (res LE 0) THEN BEGIN
;;           printtocon, "% : user aborted" & return
;;        END
;;        yourchoice=res
;;
;; 
XConsole_PushState
XConsole_WaitingForInput
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    PrintToCon, "% XMChoiceDialog:    Fatal error "
    PrintToCon, "%   Error status  - " + STRING(error_status)
    PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
    CATCH, /Cancel
    ErrMsg, !ERROR_STATE.MSG
    XConsole_PopState
    return, 0
END

result=1
N=SIZE(mclist, /N_ELEMENTS)
base=widget_base(Title=title, /Column)
IF keyword_set(text) THEN BEGIN
   FOR i=0,(N_Elements(text)-1) DO BEGIN
      l=Widget_Label(base, VALUE=text[i], FRAME=0, /ALIGN_LEFT)
   END
END
buttons = WIDGET_BASE(base,/Column)
IF NOT(keyword_set(horiz)) THEN togglebase = WIDGET_BASE(buttons, /Column, /EXCLUSIVE) ELSE togglebase = WIDGET_BASE(buttons, /Row, /EXCLUSIVE)
b=INTARR(N)
FOR i=1,N DO BEGIN
 s=STRING(i) 
 b(i-1) = WIDGET_BUTTON(togglebase, VALUE=mclist(i-1), UVALUE=s, /NO_RELEASE)
 
END

IF Keyword_set(selected) THEN BEGIN
   FOR i=1,N DO BEGIN
      IF (selected EQ mclist(i-1)) THEN BEGIN
         WIDGET_CONTROL, b(i-1), /SET_BUTTON 
         result=i
      END
   END
END

WIDGET_CONTROL, b(0), /SET_BUTTON

c=WIDGET_BASE(base, /ROW)
apply = WIDGET_BUTTON(c, value='Accept', UVALUE='apply')
IF keyword_set(hlp) THEN help = WIDGET_BUTTON(c, value='Help', UVALUE='help')
cancel = WIDGET_BUTTON(c, value='Cancel', UVALUE='cancel')


widpos=PlaceWidget(base, POSKEY="cc")
 WIDGET_CONTROL, base, XOFFSET=widpos[0], YOFFSET=widpos[1], /REALIZE
 
quit=0
REPEAT BEGIN
  ev = WIDGET_Event(base)
  WIDGET_CONTROL, ev.id, GET_UVALUE = uv
  
  FOR i=1,N DO BEGIN
     IF (uv EQ STRING(i)) THEN BEGIN
        IF keyword_set(index) THEN result=i ELSE result=mclist(i-1)
     END
  END

  
  CASE uv OF
      'apply': BEGIN
          quit=1
      END
      'help': BEGIN
         msg = dialog_message(hlp, /Information)
      END
      'cancel': BEGIN
          quit=1
          IF keyword_set(index) THEN result=0 ELSE result=""
      END
      ELSE: 
  END
ENDREP UNTIL (quit EQ 1)


WIDGET_CONTROL, base, /DESTROY
XConsole_PopState
return, result
END


FUNCTION XButtonMChoiceDialog, mclist, title, TEXT=text, VERT=vert
;;
;; Multiple Choice Dialog
;;
;; Example: 
;;
;;
;;       choices = ["chicken","beef","vegetarian"]
;;        title = "What would you like to have?"
;;        text = ["We offer", "today"]
;;        res=XButtonMChoiceDialog(mclist,title, TEXT=text)
;;        CASE res OF
;;          'chicken': print, "chicken chicken chicken"
;;          'beef': print, "moooh"
;;          'vegetarian': print, "veggie"
;;          ELSE:
;;        END
;; 
XConsole_PushState
XConsole_WaitingForInput
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    PrintToCon, "% XButtonMChoiceDialog:    Fatal error "
    PrintToCon, "%   Error status  - " + STRING(error_status)
    PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
    CATCH, /Cancel
    ErrMsg, !ERROR_STATE.MSG
    XConsole_PopState
    return, 0
END

result=1
N=SIZE(mclist, /N_ELEMENTS)
base=widget_base(Title=title, /Column)
buttons = WIDGET_BASE(base, /Column)
IF keyword_set(text) THEN BEGIN
   FOR i=0,(N_Elements(text)-1) DO BEGIN
      l=Widget_Label(buttons, VALUE=text[i], FRAME=0)
   END
END

IF NOT(keyword_set(vert)) THEN togglebase = WIDGET_BASE(buttons, /row) ELSE togglebase = WIDGET_BASE(buttons, /column)
b=INTARR(N)
FOR i=1,N DO BEGIN
 s=STRING(i) 
 b(i-1) = WIDGET_BUTTON(togglebase, VALUE=mclist(i-1), UVALUE=s)
END

widpos=PlaceWidget(base, POSKEY="cc")
 WIDGET_CONTROL, base, XOFFSET=widpos[0], YOFFSET=widpos[1], /REALIZE
 
result=0
REPEAT BEGIN
  ev = WIDGET_Event(base)
  WIDGET_CONTROL, ev.id, GET_UVALUE = uv
  
  FOR i=1,N DO BEGIN
      IF (uv EQ STRING(i)) THEN result=i
  END

  
ENDREP UNTIL (result GT 0)


WIDGET_CONTROL, base, /DESTROY
XConsole_PopState
return, mclist[result-1]
END





