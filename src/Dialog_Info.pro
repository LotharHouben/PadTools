;
;+
; NAME:
; 
;	DIALOG
;
; PURPOSE:
; 
;       A popup widget dialog box to get user input. Like
;       WIDGET_MESSAGE
;
; CATEGORY:
; 
;	Widgets.
;
; CALLING SEQUENCE:
;
;	Result = DIALOG([TEXT])
;
; OPTIONAL INPUTS:
; 
;	TEXT - Array of strings.
;	
; KEYWORD PARAMETERS:
; 
;   There are 4 types of dialogs, each with unique behavior.  With
;   each default dialog type are associated buttons; these buttons can
;   be overridden with the BUTTONS keyword, except in the case of the
;
;       One of the following six keywords MUST be set:
;
;       ERROR - Display an error message; default BUTTONS =
;               [' Abort ',' Continue ']
;
;       WARNING - Display a warning message.  default BUTTONS = [' Ok ']
;
;       INFO - Display an informational message; 
;              default BUTTONS = [' Ok ']
; 
;       QUESTION - Ask a question.  default BUTTONS =
;                  [' Yes ',' No ']
;
;
;   TITLE - title of popup widget.
;	
; COMMON BLOCKS:
;
;
; OUTPUTS:
; 
; button label
;
; EXAMPLE:
; 
;       D = dialog_info(['Hello World!','do you want to press a button?'], /Question)
;
; MODIFICATION HISTORY:
; 
;       L. Houben, Sept. 2005
;


function dialog_info, text, error=error, warning=warning, info=info, question=question, help=help, title=title, poskey=poskey, BREAKBUTTON=breakbutton

; set the list and field widget id's to zero, in case
; they've already been defined from a previous instance of dialog.

selection=' Ok '
;;XConsole_PushState
;;XConsole_WaitingForInput
IF NOT(GetDebugFlag() EQ 1) THEN BEGIN
   CATCH, Error_status
   
   IF (Error_status NE 0) THEN BEGIN
      PrintToCon, "% DialogInfo:    Fatal error "
      PrintToCon, "%   Error status  - " + STRING(error_status)
      PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
      CATCH, /Cancel
      ErrMsg, !ERROR_STATE.MSG
      XConsole_PopState
      return, selection
   END
END
if keyword_set(title) eq 0 then BEGIN 
    title=' '
    if keyword_set(help) THEN title="Help"
    if keyword_set(error) THEN title="Error"
    if keyword_set(info) THEN title="Information"
    if keyword_set(question) THEN title="Question"
    if keyword_set(warning) THEN title="Warning"
END

; make widget base:
base=widget_base(title=title,/column) 
;; display text
NLines=(N_Elements(text)-1)
txt=widget_base(base,/column) 
IF NLines GT 25 THEN BEGIN
    logtext = WIDGET_TEXT(txt, /SCROLL, XSIZE=100, YSIZE=25, VALUE=text)
END ELSE BEGIN
    for i=0,NLines Do lab=Widget_Label(txt, VALUE=text(i),/ALIGN_LEFT) 
END
case 1 of
    keyword_set(error):begin
        buttons=[' Abort ',' Continue ']
    end
    keyword_set(warning):begin
        buttons=[' Ok ']
    end
    keyword_set(warning):begin
        buttons=[' Ok ']
    end
    keyword_set(info):begin
        buttons=[' Ok ']
    end
    keyword_set(question):begin
        buttons=[' Yes ',' No ']
    end
    ELSE: buttons=[' Ok ']
 endcase
IF keyword_set(breakbutton) THEN BEGIN
   buttons=[buttons, 'Break']
END
; make widget buttons:

bgroup=cw_bgroup(base,/row,buttons,uvalue='buttons',/return_name)
 widpos=PlaceWidget(base, POSKEY="cc") 
; realize widget:
 WIDGET_CONTROL, base, XOFFSET=widpos[0], YOFFSET=widpos[1],/REALIZE
do_it_again = 0
REPEAT BEGIN
    ev = WIDGET_Event(base)
    IF (ev.id EQ bgroup) THEN BEGIN
        selection=ev.value 
        do_it_again = NOT(do_it_again)
    END 
ENDREP UNTIL do_it_again
widget_control, base, /DESTROY
;; XConsole_PopState
return, selection
end

PRO TEST_Dialog_info
print, Dialog_Info(["Do you want to add another ROI?"], /BREAKBUTTON, /QUESTION)
END

