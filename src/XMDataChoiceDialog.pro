
FUNCTION XMDataChoiceField, s, c, Excl=excl, TEXT=text, TITLE=t, ACCEPT=accept, CANCEL=cancel, HELP=hlp, NOCANCEL=nocancel, XWIDTH=xwidth
;; s: list of structures for input field definition
;; s=list()
;; s.Add, {value:12,label:"inner radius (pix)",newrow:0B}
;; s.Add, {value:16,label:"outer radius (pix)",newrow:0B}
;;  s.Add, {value:0.2,label:"correlation lower bound (<=1)",newrow:1B}
;;  c=list()
;; c.Add, {value:1B,label:"subpixel accuracy",newrow:0B}
;; c.Add, {value:0B,label:"iterate",newrow:1B}
;;
;; excl is like c, but an exclusive radio button field
 
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    PrintToCon, "% XDataField:    Fatal error "
    PrintToCon, "%   Error status  - " + STRING(error_status)
    PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
    CATCH, /Cancel
    XConsole_PopState
    return, 0
 END  
XConsole_PushState
XConsole_WaitingForInput
result=1
if not(keyword_set(t)) then t=""
YOffs=5
base = Widget_Base(TITLE=t)
if NOT(keyword_set(xwidth)) THEN xwidth=10
if (keyword_set(text)) THEN BEGIN
   ysz=N_ELEMENTS(text)
   YOffs=ysz*25
   label = Widget_TEXT(base, XSize=30, YSIZE=ysz, VALUE = text, SENSITIVE=1)
END
input =  WIDGET_BASE(base, YOffset=YOffs, /COLUMN, XPAD=10)
IF (s.Count() GT 0) THEN BEGIN
input_field=LONARR(s.Count())
row=WIDGET_BASE(input, YOffset=YOffs, /ROW, XPAD=10)
For i=0,s.Count()-1 DO BEGIN
   if (s[i].newrow GT 0) THEN row=WIDGET_BASE(input, /ROW, XPAD=10)
   input_field[i]= CW_Field(row, TITLE=s[i].label, XSize=xwidth, Frame=0, Value=MySTRING(s[i].value))
END
END
IF (c.Count() GT 0) THEN BEGIN
checkbox_field=LONARR(c.Count())
row=WIDGET_BASE(input, YOffset=YOffs, /ROW, XPAD=10, /NonExclusive)
For i=0,c.Count()-1 DO BEGIN
   if (c[i].newrow GT 0) THEN row=WIDGET_BASE(input, /ROW, XPAD=10, /NonExclusive)
   checkbox_field[i] = Widget_Button(row, Value=c[i].label)
   if (c[i].value EQ 1B) THEN Widget_Control, checkbox_field[i], Set_Button=1
END

END  
IF keyword_set(excl) THEN BEGIN
IF (excl.Count() GT 0) THEN BEGIN
   exclcheckbox_field=LONARR(excl.Count())
   row=WIDGET_BASE(input, /ROW)
   IF (WHERE(Tag_names(excl[0]) EQ 'PRETEXT') EQ 1) THEN dummy=WIDGET_Label(row, VALUE='  '+excl[0].pretext)
tlb=WIDGET_BASE(row, YOffset=YOffs, /ROW, XPAD=10, /Exclusive)
For i=0,excl.Count()-1 DO BEGIN
   
   exclcheckbox_field[i] = Widget_Button(tlb, Value=excl[i].label)
   if (excl[i].value EQ 1B) THEN Widget_Control, exclcheckbox_field[i], Set_Button=1
END

END  
END

IF NOT(keyword_set(accept)) THEN accept="  Apply  "
IF NOT(keyword_set(cancel)) THEN cancel="  Cancel  "
row=WIDGET_BASE(input, /ROW, XPAD=10)
IF NOT(keyword_set(hlp)) THEN BEGIN
   IF keyword_set(nocancel) THEN BEGIN
      b = CW_BGroup(row, /ROW, YOffset=YOffs+45, [accept] , Button_UVALUE=['apply'])
   END ELSE BEGIN
      b = CW_BGroup(row, /ROW, YOffset=YOffs+45, [accept,cancel] , Button_UVALUE=['apply','cancel'])
   END
END ELSE BEGIN
   IF keyword_set(nocancel) THEN BEGIN
      b = CW_BGroup(row, /ROW, YOffset=YOffs+45, [accept,'  Help  '] , Button_UVALUE=['apply','help'])
   END ELSE BEGIN
      b = CW_BGroup(row, /ROW, YOffset=YOffs+45, [accept,cancel,'  Help  '] , Button_UVALUE=['apply','cancel','help'])
     END
END
widpos=PlaceWidget(base)
WIDGET_CONTROL, base, XOFFSET=widpos[0], YOFFSET=widpos[1], /REALIZE
do_it_again = 0
REPEAT BEGIN
   ev = WIDGET_Event(base)
   IF (ev.id EQ b) THEN BEGIN                ;; Button-Menu
      CASE ev.value OF
         'cancel': BEGIN
            do_it_again = NOT(do_it_again)
            result=0
         END
         'help': BEGIN
            msg = dialog_info(hlp, /Info)
         END
         'apply': BEGIN
            TMP=''
            IF (s.Count() GT 0) THEN BEGIN
            For i=0,s.Count()-1 DO BEGIN
               WIDGET_Control, input_field[i], GET_VALUE=TMP
               TMP=TMP(0)
               ;; type conversion
               type = SIZE(s[i].value, /TYPE)
               IF (TMP NE '') THEN BEGIN
                  cp=s[i]
                  cp.value=FIX(TMP, TYPE=type)
                  s[i]=cp
               ENDIF
            END
         END
            IF (c.Count() GT 0) THEN BEGIN
            For i=0,c.Count()-1 DO BEGIN
               cp=c[i]
               IF WIDGET_Info(checkbox_field[i], /BUTTON_SET) THEN cp.value=1B ELSE cp.value=0B
               c[i]=cp
            END
         END
            IF keyword_set(excl) THEN BEGIN
               IF (excl.Count() GT 0) THEN BEGIN
                  For i=0,excl.Count()-1 DO BEGIN
                     cp=excl[i]
                     IF WIDGET_Info(exclcheckbox_field[i], /BUTTON_SET) THEN cp.value=1B ELSE cp.value=0B
                     excl[i]=cp
                  END
               END
            END
            do_it_again = NOT(do_it_again)      
            result=1 
         END
      ENDCASE
   ENDIF 
ENDREP UNTIL do_it_again
Widget_Control, base, /DESTROY
XConsole_PopState
return, result
END   
