Pro Close_XConsole
COMMON CONWINIDS, logtext, buttons, base, activity, curract
WIDGET_Control, base, /DESTROY
END

PRO PrintToXConsole, s
COMMON CONWINIDS, logtext, buttons, base, activity, curract
; WIDGET_Control, logtext, GET_VALUE=q
; q=[q,s]
 WIDGET_Control, logtext, SET_VALUE=s, /APPEND
END

PRO XConsole_WaitingForInput
COMMON CONWINIDS, logtext, buttons, base, activity, curract
IF NOT(XREGISTERED('XConsole')) THEN return
WIDGET_CONTROL, activity[2], /SET_BUTTON
curract=2
END

PRO XConsole_PushState
COMMON CONWINIDS, logtext, buttons, base, activity, curract
COMMON CONSTAT, oldact
IF NOT(XREGISTERED('XConsole')) THEN return
oldact=Push(oldact, curract)
;; print, "pushing curract=", curract
END

PRO XConsole_PopState
COMMON CONWINIDS, logtext, buttons, base, activity, curract
COMMON CONSTAT, oldact
IF NOT(XREGISTERED('XConsole')) THEN return
oldact=Pop(oldact, curract)
WIDGET_CONTROL, activity[curract], /SET_BUTTON
;; print, "popped curract=", curract
END


PRO XConsole_Busy
COMMON CONWINIDS, logtext, buttons, base, activity, curract
IF NOT(XREGISTERED('XConsole')) THEN return
WIDGET_CONTROL, activity[1], /SET_BUTTON
curract=1
END


PRO XConsole_Idle
COMMON CONWINIDS, logtext, buttons, base, activity, curract
IF NOT(XREGISTERED('XConsole')) THEN return
WIDGET_CONTROL, activity[0], /SET_BUTTON
curract=0
END

PRO XConsole_Save, f, AUTOSAVE=autosave, OVERWRITE=overwrite
COMMON CONWINIDS, logtext, buttons, base, activity, curract
IF (f NE "") THEN BEGIN
              fn="" & path="" & SplitPath, f, path, fn
              ;; if (path NE "") THEN workingdir=path 
              
              IF (FileIsWriteable(f, OVERWRITE=overwrite) EQ 1) THEN BEGIN
                 WIDGET_Control, logtext, GET_VALUE=s
                 OPENW, LogF, f,  /GET_LUN 
                 i=0
                 For i=0, (N_Elements(s)-1) DO BEGIN
                    PRINTF, LogF, s[i]
                 END
                 FREE_LUN, LogF
              END ELSE BEGIN
                 PrintToCon, "% XLogWin: Export cancelled" 
              END
              
              
           END  
END


PRO XConsole_event, ev
COMMON CONSTAT, oldact
COMMON CONWINIDS, logtext, buttons, base, activity, curract
IF (ev.id EQ buttons) THEN BEGIN  
    CASE ev.value OF
        'clear': BEGIN
            s=[""]
            WIDGET_Control, logtext, SET_VALUE=s
        END
        'close': BEGIN
            ;; delete status stack
            ClearStack, oldact, curract
            curract=0
            Widget_Control, ev.top, /DESTROY 
        END
        'save': BEGIN
           wdir=GetWorkingdir()
           f=''
           tlist=PTR_NEW(['Simple Text Format'])
           savedatatype=0
           result=rmd_pickfile(  $
                  filter_in=datafilter, $
                  filter_out=datafilter, $
                  path = wdir,          $
                  get_path = wdir,          $
                  title = "Save Data to File",          $
                  crgrp = savealldata,          $
                  cancelled = cancelled,        $
                  type = savedatatype,        $
                  ftypes=tlist, $
                  /save)
           ftype=(*tlist)[savedatatype]
           f=result[0]
           XConsole_Save, f
        END
        ELSE:
    ENDCASE
ENDIF
END


Pro XConsole
COMMON CONWINIDS, logtext, buttons, base, activity, curract
COMMON CONSTAT, oldact
   base = Widget_BASE(TITLE='Console', /COLUMN, GROUP_LEADER=WidgetID(0,/GETGROUPLEADERTOP))
   row1 =  WIDGET_BASE(base, /ROW) 
   logtext = WIDGET_TEXT(row1, /SCROLL, XSIZE=120, YSIZE=12, VALUE="")
   row2 = WIDGET_BASE(base, /ROW)
   buttons = CW_BGroup(row2, /ROW, [' Save ',' Clear ',' Close '] , Button_UVALUE=['save','clear','close'])
   togglebase = WIDGET_BASE(row2, /ROW ,/EXCLUSIVE)
   idle = WIDGET_BUTTON(togglebase, VALUE='idle', UVALUE='idle', /NO_RELEASE)
   busy = WIDGET_BUTTON(togglebase, VALUE='busy', UVALUE='busy', /NO_RELEASE)
   waiting = WIDGET_BUTTON(togglebase, VALUE='waiting for input', UVALUE='waiting', /NO_RELEASE)
   activity=[idle,busy,waiting]
   WIDGET_CONTROL, idle, /SET_BUTTON
   curract=0
   ;; create status stack
   oldact=CreateStack(curract)
   ;;
   widpos=PlaceWidget(base, POSKEY="lr")
   Widget_Control, base, XOFFSET=widpos[0], YOFFSET=widpos[1]-100, /REALIZE


   XManager, 'XConsole', base, /NO_BLOCK  
END



PRO PrintToCon, s
 ;; Check whether X log window exists 
 ;; print string to the window if it exists
 ;; otherwise to the shell  
  IF NOT(XREGISTERED('XConsole')) THEN BEGIN
     print, s
     return
  END
 PrintToXConsole, s
END



