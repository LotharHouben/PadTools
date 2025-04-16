;+
; NAME: XSimpleDialog
;
;
;
; PURPOSE: A simple dialog for a twofold choice answer 
;
;
; EXAMPLE: s="  beef or chicken?"
;          i=XSimpleDialog(s, YES="beef", NO="chicken")
;          IF (i EQ 1) THEN print, "beef" ELSE print, "chicken"
;
; MODIFICATION HISTORY:
;
;-

FUNCTION XSimpleDialog, s, YES=yes, No=no, TITLE=title
COMMON YESNOBLOCK, Answer 
XConsole_PushState
XConsole_WaitingForInput
Answer=0
if NOT(keyword_set(no)) Then no='No'
if NOT(keyword_set(yes)) Then yes='Yes'
if NOT(keyword_set(title)) Then title='Question'
ysz=N_Elements(s)
base = WIDGET_BASE(/COLUMN, TITLE=title)
text = WIDGET_LABEL(base, VALUE=" ", FRAME=0)
FOR i=0, ysz-1 DO BEGIN
   text = WIDGET_LABEL(base, VALUE=s[i], FRAME=0)
END
text = WIDGET_LABEL(base, VALUE=" ", FRAME=0)
b = CW_BGroup(base, /ROW , [yes,no] , Button_UVALUE=[' Yes ',' No '])
widpos=PlaceWidget(base, POSKEY="cc")
 WIDGET_CONTROL, base, XOFFSET=widpos[0], YOFFSET=widpos[1], /REALIZE
   do_it_again = 0
   REPEAT BEGIN
       ev = WIDGET_Event(base, /NoWait)
       IF (ev.id EQ b) THEN BEGIN
       CASE ev.value OF
           ' Yes ' : BEGIN 
               Answer=1 
               do_it_again = NOT(do_it_again)
           END
           ' No ' : BEGIN 
               Answer=0 
               do_it_again = NOT(do_it_again)
           END
           'ELSE': BEGIN
           END
       ENDCASE
   END 
   ENDREP UNTIL do_it_again
   Widget_Control, base, /DESTROY
XConsole_PopState
return, Answer
END

