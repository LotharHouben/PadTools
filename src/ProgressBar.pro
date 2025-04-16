;+
; NAME: ProgressBar
;
;
;
; PURPOSE: Provides routines for displaying a progress bar
;
;
;
; CATEGORY: graphical user interface
;
;
; EXAMPLE:  see end of file
;
; NOTES  More than one progress bar can be nested, e.g. in double loops!
; DEPENDENCIES: STACK.pro
;
; MODIFICATION HISTORY: 15.09.2004 first implementation
;                       22.04.2005 revision
;
;-

PRO SetProgressBarPrefs
COMMON PBAR, baseid, barid, labid, minval, maxval, barval, breakstate
baseid=-1
breakstate=0
 END



PRO InitProgressBar_event, ev
COMMON PBAR, baseid, barid, labid, minval, maxval, barval, breakstate
 WIDGET_CONTROL, ev.id, GET_UVALUE = uv
 printtocon, ev
  CASE uv OF
     'break': BEGIN
           WIDGET_CONTROL, ev.top, /DESTROY
           ;; division by zero
           breakstate=1
           printtocon, "break activated"
        END
     ELSE:
  END
END

Function ProgressBarCheckBreak
  COMMON PBAR, baseid, barid, labid, minval, maxval, barval, breakstate
  return, breakstate
END

PRO InitProgressBar, MINV=minv, MAXV=maxv, TEXT=text, BREAKHANDLER=breakhandler
COMMON PBAR, baseid, barid, labid, minval, maxval, barval, breakstate
;; barid: array of widget_draw ids
;;        each progress bar has an id  barid[0] ... barid[n-1] 
;; minv:  array of starting values 
;; maxv:  array of end values
;;
;; standard: a single percent bar 
IF NOT(Keyword_Set(minv)) THEN minval=[0] ELSE minval=minv
IF NOT(Keyword_Set(maxv)) THEN maxval=[100] ELSE maxval=maxv
;; how many bars?
NBars=N_Elements(minval)
;;
FOR i=0,(NBars-1) Do Begin
   IF (minval[i] GT maxval[i]) THEN BEGIN
      val=maxval[i]
      maxval[i]=minval[i]
      minval[i]=val
   END ELSE BEGIN
      IF (minval[i] EQ maxval[i]) THEN maxval[i]=minval[i]+100
   END
END

 IF Not(Keyword_Set(text)) THEN BEGIN
     text=STRARR(NBars)
     FOR i=0,(NBars-1) Do Begin
         text[i]="Progress (%)" 
     END
 END

 barid=LONARR(NBars)
 labid=LONARR(NBars)
 barval=minval

 IF (baseid GE 1) THEN BEGIN
     ;; is there already a progress bar? 
     ;; there can only be one!
     existing=Widget_Info(baseid, /VALID_ID)
     if (existing EQ 1) THEN Widget_Control, baseid, /DESTROY
     baseid = -1
 END
 baseid = Widget_Base(TITLE='Progress', /COLUMN)
 ysz=15 & xsz=200
;; frame0=Widget_Base(baseid, /COLUMN, /FRAME)
;; progresstext=WIDGET_LABEL(frame0, /ALIGN_CENTER, VALUE="Progress")
 frame=Widget_Base(baseid, /COLUMN)
 FOR i=0,(NBars-1) Do Begin
      labid[i]=WIDGET_LABEL(frame, /ALIGN_CENTER, VALUE=text[i])
      barid[i] = Widget_Draw(frame, XSize=xsz, YSize=ysz)
   END
 IF Keyword_set(breakhandler) THEN BEGIN
    frame2=Widget_Base(baseid, /COLUMN, /FRAME)
    break = WIDGET_BUTTON(frame2,   VALUE = '     Break     ', UVALUE = 'break')
 END
 widpos=PlaceWidget(baseid, POSKEY="uc") 
 Widget_Control, baseid,  XOFFSET=widpos[0], YOFFSET=widpos[1],/REALIZE
 ;; print, "creating progress bar ", pb.baseid
 IF Keyword_set(breakhandler) THEN BEGIN
    XMANAGER, 'InitProgressBar',  baseid,/NO_BLOCK
 END
END 

PRO ProgressBarSet, number, value, TEXT=text
COMMON PBAR, baseid, barid, labid, minval, maxval, barval, breakstate
COMMON PLOTCOLORS, backgr, foregr
IF (number LT N_Elements(barid)) THEN BEGIN
   IF (barid[number] GT 1) THEN BEGIN
       barval[number]=value
       thisWindow = !D.Window
       Widget_Control, barid[number], Get_Value=wid
       WSet, wid
       x1 = 0 & y1 = 0
       x2 = Fix((!D.X_SIZE)*(value-minval[number])/(maxval[number]-minval[number]))
       y2 = !D.Y_SIZE
       x3 = !D.X_SIZE
       Polyfill, [x1, x1, x3, x3, x1], [y1, y2, y2, y1, y1], /Device, Color=foregr
       Polyfill, [x1, x1, x2, x2, x1], [y1, y2, y2, y1, y1], /Device, Color=backgr
       IF Keyword_set(text) THEN BEGIN
           XYOUTS, FLOOR(x3/2), y1+4, COLOR=255, /DEVICE, STRCOMPRESS(text)
       END
       WSet, thisWindow
   END
END
END 

PRO ProgressBarStep, number, TEXT=text
COMMON PBAR, baseid, barid, labid, minval, maxval, barval, breakstate
IF (number LT N_Elements(barid)) THEN BEGIN
    value=barval[number]+1
    IF NOT(Keyword_Set(text)) THEN text=STRING(value)
    ProgressBarSet, number, value, TEXT=text
END
END

PRO ProgressBarText, number, text
COMMON PBAR, baseid, barid, labid, minval, maxval, barval, breakstate
IF (number LT N_Elements(barid)) THEN BEGIN
    WIDGET_CONTROL, labid[number], SET_VALUE=text
END
END 



PRO DestroyProgressBar
COMMON PBAR, baseid, barid, labid, minval, maxval, barval, breakstate
IF (baseid GE 1) THEN Widget_Control, baseid, /DESTROY
baseid=-1
    ;; print, "destroying progress bar ", pb.baseid
END





;;PRO TestProgressbar
;; InitPlotColors
;;  SetProgressBarPrefs
;;  minv=[-8,-15]
;;  maxv=[8,15]
;;  text=["outer loop index","inner loop index"]
;;  i=minv[0]
;;  InitProgressBar, MINV=minv, MAXV=maxv, TEXT=text
;;  WHILE (i LT maxv[0]) DO BEGIN
;;        ProgressBarStep, 0, TEXT=STRING(i)
;;        j=maxv[1]
;;        ProgressBarSet, 1, j
;;        WHILE (j GE minv[1]) DO BEGIN
;;            ProgressBarSet, 1, j, TEXT=STRING(j)
;;            Wait, 0.05
;;            j=j-1 
;;        END
;;        i=i+1
;;   END
;;   DestroyProgressBar
;;  END
