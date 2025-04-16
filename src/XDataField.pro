FUNCTION XDataField, x, s, Y=ypsilon, SY=sypsilon, Z=zett, SZ=szett, W=w, SW=sw, U=u, SU=su, TEXT=text, TITLE=t, ACCEPT=accept, CANCEL=cancel, HELP=hlp, NOCANCEL=nocancel
IF (GetDebugFlag()) THEN BEGIN
  END ELSE BEGIN
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    PrintToCon, "% XDataField:    Fatal error "
    PrintToCon, "%   Error status  - " + STRING(error_status)
    PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
    CATCH, /Cancel
    XConsole_PopState
    return, 0
 END
END
XConsole_PushState
XConsole_WaitingForInput
   result=1
   if not(keyword_set(t)) then t=""
   YOffs=5
   base = Widget_Base(TITLE=t)
   if (keyword_set(text)) THEN BEGIN
      ysz=N_ELEMENTS(text)
      YOffs=ysz*25
      label = Widget_TEXT(base, XSize=30, YSIZE=ysz, VALUE = text, SENSITIVE=1)
   END
   input =  WIDGET_BASE(base, YOffset=YOffs, /ROW, XPAD=10)
   x_label = CW_Field(input, TITLE=s, XSize=10, Frame=0, Value=MySTRING(x))
   if (keyword_set(sypsilon)) THEN BEGIN
      if not(keyword_set(ypsilon)) THEN ypsilon=0
      y_label=CW_Field(input, TITLE=sypsilon, XSize=10, Frame=0, Value=MySTRING(ypsilon))
   END
   if (keyword_set(szett)) THEN BEGIN
      if not(keyword_set(zett)) THEN zett=0
      z_label=CW_Field(input, TITLE=szett, XSize=10, Frame=0, Value=MySTRING(zett))
  END
  if (keyword_set(sw)) THEN BEGIN
      if not(keyword_set(w)) THEN w=0
      w_label=CW_Field(input, TITLE=sw, XSize=10, Frame=0, Value=MySTRING(w))
  END
  if (keyword_set(su)) THEN BEGIN
      if not(keyword_set(u)) THEN u=0
      u_label=CW_Field(input, TITLE=su, XSize=10, Frame=0, Value=MySTRING(u))
  END
  IF NOT(keyword_set(accept)) THEN accept="Apply"
  IF NOT(keyword_set(cancel)) THEN cancel="Cancel"

  IF NOT(keyword_set(hlp)) THEN BEGIN
     IF keyword_set(nocancel) THEN BEGIN
                b = CW_BGroup(base, /ROW, YOffset=YOffs+45, [accept] , Button_UVALUE=['apply'])
     END ELSE BEGIN
        b = CW_BGroup(base, /ROW, YOffset=YOffs+45, [accept,cancel] , Button_UVALUE=['apply','cancel'])
     END
  END ELSE BEGIN
     IF keyword_set(nocancel) THEN BEGIN
        b = CW_BGroup(base, /ROW, YOffset=YOffs+45, [accept,'Help'] , Button_UVALUE=['apply','help'])
     END ELSE BEGIN
        b = CW_BGroup(base, /ROW, YOffset=YOffs+45, [accept,cancel,'Help'] , Button_UVALUE=['apply','cancel','help'])
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
                  WIDGET_Control, x_label, GET_VALUE=TMP
                  TMP=TMP(0)
                  IF (TMP NE '') THEN BEGIN
	              x=(FLOAT(TMP))
                  ENDIF
                  if (keyword_set(sypsilon)) THEN BEGIN
                      TMP=''
                      WIDGET_Control, y_label, GET_VALUE=TMP
                      TMP=TMP(0)
                      IF (TMP NE '') THEN BEGIN
	               ypsilon=(FLOAT(TMP))
                      ENDIF
                  ENDIF   
                  if (keyword_set(szett)) THEN BEGIN
                      TMP=''
                      WIDGET_Control, z_label, GET_VALUE=TMP
                      TMP=TMP(0)
                      IF (TMP NE '') THEN BEGIN
	               zett=(FLOAT(TMP))
                      ENDIF
                   ENDIF
                  if (keyword_set(sw)) THEN BEGIN
                      TMP=''
                      WIDGET_Control, w_label, GET_VALUE=TMP
                      TMP=TMP(0)
                      IF (TMP NE '') THEN BEGIN
	               w=(FLOAT(TMP))
                      ENDIF
                   ENDIF
                  if (keyword_set(su)) THEN BEGIN
                      TMP=''
                      WIDGET_Control, u_label, GET_VALUE=TMP
                      TMP=TMP(0)
                      IF (TMP NE '') THEN BEGIN
	               u=(FLOAT(TMP))
                      ENDIF
                  ENDIF
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

FUNCTION XStringField, x, s, Y=ypsilon, SY=sypsilon, Z=zett, SZ=szett, TEXT=text, TITLE=t, HELP=hlp, ACCEPT=accept, CANCEL=cancel, RETURNSTRING=returnstring, ALLOWEMPTY=allowempty, XSIZE=xsize, YSize=ysize, ZSize=zsize
XConsole_PushState
XConsole_WaitingForInput
IF (GetDebugFlag()) THEN BEGIN
  END ELSE BEGIN
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    PrintToCon, "% XStringField:    Fatal error "
    PrintToCon, "%   Error status  - " + STRING(error_status)
    PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
    CATCH, /Cancel
    XConsole_PopState
    return, 0
END
END
  result=1
  if  not(keyword_set(xsize)) THEN xsize=10
  if  not(keyword_set(ysize)) THEN ysize=10
  if  not(keyword_set(zsize)) THEN zsize=10
   if not(keyword_set(t)) then t=""
   YOffs=5
   base = Widget_Base(TITLE=t)
   if (keyword_set(text)) THEN BEGIN
      ysz=N_ELEMENTS(text)
      YOffs=ysz*25
      label = Widget_TEXT(base, XSize=30, YSIZE=ysz, VALUE = text, SENSITIVE=0)
   END
   input =  WIDGET_BASE(base, YOffset=YOffs, /ROW, XPAD=10)
   x_label = CW_Field(input, TITLE=s, XSize=xsize, Frame=0, Value=MySTRING(x))
   if (keyword_set(sypsilon)) THEN BEGIN
      if not(keyword_set(ypsilon)) THEN yval='' ELSE yval=ypsilon
      y_label=CW_Field(input, TITLE=sypsilon, XSize=ysize, Frame=0, Value=MySTRING(yval))
   END
   if (keyword_set(szett)) THEN BEGIN
      if not(keyword_set(zett)) THEN zval='' ELSE zval=zett
      z_label=CW_Field(input, TITLE=szett, XSize=zsize, Frame=0, Value=MySTRING(zval))
   END
  IF NOT(keyword_set(accept)) THEN accept="Apply"
  IF NOT(keyword_set(cancel)) THEN cancel="Cancel"
   if (keyword_set(hlp)) THEN BEGIN
       b = CW_BGroup(base, /ROW, YOffset=YOffs+45, [accept,cancel,'Help'] , Button_UVALUE=['apply','cancel','help'])
   END ELSE BEGIN
       b = CW_BGroup(base, /ROW, YOffset=YOffs+45, [accept,cancel] , Button_UVALUE=['apply','cancel'])
   END
 widpos=PlaceWidget(base)
 WIDGET_CONTROL, base, XOFFSET=widpos[0], YOFFSET=widpos[1], /REALIZE
   do_it_again = 0
   REPEAT BEGIN
     ev = WIDGET_Event(base)
     IF (ev.id EQ b) THEN BEGIN                ;; Button-Menu
         CASE ev.value OF
           'help': BEGIN
                     msg = dialog_info(hlp, /Info)
           END
           'cancel': BEGIN
               do_it_again = NOT(do_it_again)
	       result=0
               IF keyword_set(returnstring) THEN BEGIN
                                  TMP=''
                  WIDGET_Control, x_label, GET_VALUE=TMP
                  TMP=TMP(0)
                  IF (TMP NE '') THEN BEGIN
	              x=TMP
                   END ELSE IF (Keyword_set(allowempty)) THEN x=TMP
                  if (keyword_set(sypsilon)) THEN BEGIN
                      TMP=''
                      WIDGET_Control, y_label, GET_VALUE=TMP
                      TMP=TMP(0)
                      
                      IF (TMP NE '') THEN BEGIN
	               ypsilon=TMP
                    END  ELSE IF (Keyword_set(allowempty)) THEN ypsilon=TMP
                  ENDIF   
                  if (keyword_set(szett)) THEN BEGIN
                      TMP=''
                      WIDGET_Control, z_label, GET_VALUE=TMP
                      TMP=TMP(0)
                      IF (TMP NE '') THEN BEGIN
	               zett=TMP
                    END  ELSE IF (Keyword_set(allowempty)) THEN zett=TMP
                  ENDIF
               END
           END
           'apply': BEGIN
                  TMP=''
                  WIDGET_Control, x_label, GET_VALUE=TMP
                  TMP=TMP(0)
                  IF (TMP NE '') THEN BEGIN
	              x=TMP
                   END  ELSE IF (Keyword_set(allowempty)) THEN x=TMP
                  if (keyword_set(sypsilon)) THEN BEGIN
                      TMP=''
                      WIDGET_Control, y_label, GET_VALUE=TMP
                      TMP=TMP(0)
                      IF (TMP NE '') THEN BEGIN
	               ypsilon=TMP
                    END  ELSE IF (Keyword_set(allowempty)) THEN ypsilon=TMP
                  ENDIF   
                  if (keyword_set(szett)) THEN BEGIN
                      TMP=''
                      WIDGET_Control, z_label, GET_VALUE=TMP
                      TMP=TMP(0)
                      IF (TMP NE '') THEN BEGIN
	               zett=TMP
                    END  ELSE IF (Keyword_set(allowempty)) THEN zett=TMP
                  ENDIF
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



















