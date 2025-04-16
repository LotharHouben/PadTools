;+
; NAME: XMultiButtonDialog
;
;
;
; PURPOSE: A simple dialog for a manifold choice answer 
;
;
; EXAMPLE: s=["Calculation result is a large number.","Do you want to proceed with the result", "or redo or cancel the calculation?"]
;          b=["Proceed","Redo","Cancel"]
;          i=XMultiButtonDialog(s,b)
;          print, "Answer is "+i
;
; MODIFICATION HISTORY:
;
;-



FUNCTION XMultiButtonDialog, s, buttonlabels, TITLE=title
XConsole_PushState
XConsole_WaitingForInput
Answer=0
if NOT(keyword_set(title)) Then title='Question'
if (N_Elements(buttonlabels) LT 1) THEN return, 0
base = WIDGET_BASE(/COLUMN, TITLE=title)
ysz=N_Elements(s)
text = WIDGET_LABEL(base, VALUE=" ", FRAME=0)
FOR i=0, ysz-1 DO BEGIN
   text = WIDGET_LABEL(base, VALUE=s[i], FRAME=0)
END
text = WIDGET_LABEL(base, VALUE=" ", FRAME=0)
b = CW_BGroup(base, /ROW , buttonlabels , Button_UVALUE=buttonlabels)
widpos=PlaceWidget(base, POSKEY="cc")
 WIDGET_CONTROL, base, XOFFSET=widpos[0], YOFFSET=widpos[1], /REALIZE
   do_it_again = 0
   REPEAT BEGIN
       ev = WIDGET_Event(base, /NoWait)
       IF (ev.id EQ b) THEN BEGIN
          Answer=ev.value
          do_it_again = NOT(do_it_again)
   END 
   ENDREP UNTIL do_it_again
   Widget_Control, base, /DESTROY
XConsole_PopState
return, Answer
END
