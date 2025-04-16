;+
; NAME: ProgressBar
;
;
;
; PURPOSE:  Progress bar class
;
;
;
; CATEGORY: graphical user interface
;
;
; EXAMPLE:  see end of file
;
; NOTES  More than one progress bar can be nested, e.g. in double loops!
; DEPENDENCIES: PlaceWidget.pro
;
; MODIFICATION HISTORY: 18.05.2014 - first implementation
;
;-



FUNCTION ProgressBar::Init, MINV=minv, MAXV=maxv, TEXT=text, TITLE=title, STATUS=Status, FRAMESTYLE=framestyle
;; barid: array of widget_draw ids
;; minv:  array of starting values 
;; maxv:  array of end values
 IF NOT(Keyword_Set(minv)) THEN self.minval=PTR_NEW([0L]) ELSE self.minval=PTR_NEW(LONG(minv))
 IF NOT(Keyword_Set(maxv)) THEN self.maxval=PTR_NEW([100L]) ELSE self.maxval=PTR_NEW(LONG(maxv))
 self.NBars=N_Elements(*(self.minval))
 ;; 
 FOR i=0,(self.NBars-1) Do Begin
     IF ((*(self.minval))[i] GT (*(self.maxval))[i]) THEN BEGIN
         val=(*(self.maxval))[i]
         (*(self.maxval))[i]=(*(self.minval))[i]
         (*(self.minval))[i]=val
     END ELSE BEGIN
         IF ((*(self.minval))[i] EQ (*(self.maxval))[i]) THEN (*(self.maxval))[i]=(*(self.minval))[i]+100
     END
 END
 ;;
 IF Not(Keyword_Set(text)) THEN BEGIN
    self.text=PTR_NEW(STRARR((self.NBars)))
    FOR i=0,(self.NBars-1) Do Begin
       (*(self.text))[i]="Progress (%)" 
    END
 END ELSE BEGIN
    self.text=PTR_NEW(text)
  END
 self.barid=PTR_NEW(LONARR(self.NBars))
 self.labid=PTR_NEW(LONARR(self.NBars))
 self.barval=PTR_NEW(*(self.minval))
 if NOT(keyword_set(title)) THEN title="Progress"
 self.baseid = Widget_Base(TITLE=title, /COLUMN)
;;  self.baseid = Widget_Base(TITLE='', /COLUMN, DISPLAY_NAME=GetDisplayName())
 ysz=15 & xsz=250
 ;; frame0=Widget_Base(self.baseid, /COLUMN)
 ;; progresstext=WIDGET_LABEL(frame0, /ALIGN_CENTER, VALUE="Progress")
 frame=Widget_Base(self.baseid, /COLUMN, FRAME=framestyle)
 FOR i=0,(self.NBars-1) Do Begin
      (*(self.labid))[i]=WIDGET_LABEL(frame, /ALIGN_CENTER, VALUE=(*(self.text))[i])
      (*(self.barid))[i] = Widget_Draw(frame, XSize=xsz, YSize=ysz)
   END
 IF keyword_set(status) THEN BEGIN
    frame2=Widget_Base(self.baseid, /COLUMN)
    void=WIDGET_LABEL(frame2, /ALIGN_Left, VALUE="")
    self.statusid=WIDGET_LABEL(frame2, FRAME=0, /ALIGN_Left, VALUE=status)
 END
 widpos=PlaceWidget(self.baseid, POSKEY="cc")    
 Widget_Control, self.baseid, XOFFSET=widpos[0], YOffset=widpos[1],  /REALIZE
 ;; set colors
 Device, Get_decomposed=decomposed
 maxrgb=!D.TABLE_SIZE
 black=0
 IF (decomposed EQ 1) THEN BEGIN
    ;; 24 bit RGB in three 8bit values
    white=((maxrgb-10)+(maxrgb-10)*maxrgb+(maxrgb-10)*maxrgb*maxrgb)
 END ELSE BEGIN
    ;; 8 bit RGB
    white=(maxrgb-10)  ;; actually a grey
 END
 self.backgr=white & self.foregr=black
 return, 1
END 

PRO ProgressBar::SetStatus, value
IF (self.statusid GT 0) THEN BEGIN
       Widget_Control, (self.statusid), Set_Value=value
    END 
END

PRO ProgressBar::ReInit, number, min, max, TEXT=text
IF (number LT self.NBars) THEN BEGIN
   (*(self.maxval))[number]=LONG(max)
   (*(self.minval))[number]=LONG(min)
   IF keyword_set(text) THEN BEGIN
      (*(self.text))[number]=text
      IF ((*(self.labid))[number] GT 1) THEN WIDGET_CONTROL, (*(self.labid))[number], SET_VALUE=text
   END
   value=LONG(min)
   IF ((*(self.barid))[number] GT 1) THEN BEGIN
       (*(self.barval))[number]=LONG(min)
       thisWindow = !D.Window
       Widget_Control, (*(self.barid))[number], Get_Value=wid
       WSet, wid
       x1 = 0 & y1 = 0
       x2 = Fix((!D.X_SIZE)*(value-(*(self.minval))[number])/((*(self.maxval))[number]-(*(self.minval))[number]))
       y2 = !D.Y_SIZE
       x3 = !D.X_SIZE
       Polyfill, [x1, x1, x3, x3, x1], [y1, y2, y2, y1, y1], /Device, Color=self.foregr
       Polyfill, [x1, x1, x2, x2, x1], [y1, y2, y2, y1, y1], /Device, Color=self.backgr
       IF Keyword_set(text) THEN BEGIN
           XYOUTS, FLOOR(x3/2), y1+4, COLOR=255, /DEVICE, STRCOMPRESS(text)
       END
       WSet, thisWindow
   END
END
END  


PRO ProgressBar::Set, number, value, TEXT=text
IF (number LT self.NBars) THEN BEGIN
   IF ((*(self.barid))[number] GT 1) THEN BEGIN
       (*(self.barval))[number]=value
       thisWindow = !D.Window
       Widget_Control, (*(self.barid))[number], Get_Value=wid
       WSet, wid
       x1 = 0 & y1 = 0
       x2 = Fix((!D.X_SIZE)*(value-(*(self.minval))[number])/((*(self.maxval))[number]-(*(self.minval))[number]))
       y2 = !D.Y_SIZE
       x3 = !D.X_SIZE
       Polyfill, [x1, x1, x3, x3, x1], [y1, y2, y2, y1, y1], /Device, Color=self.foregr
       Polyfill, [x1, x1, x2, x2, x1], [y1, y2, y2, y1, y1], /Device, Color=self.backgr
       IF Keyword_set(text) THEN BEGIN
           XYOUTS, FLOOR(x3/2), y1+4, COLOR=255, /DEVICE, STRCOMPRESS(text)
       END

       WSet, thisWindow
   END
END
END 

PRO ProgressBar::Step, number, TEXT=text
IF (number LT self.NBars) THEN BEGIN
    value=(*(self.barval))[number]+1
    IF NOT(Keyword_Set(text)) THEN text=STRING(value)
    self->Set, number, value, TEXT=text
END
END

PRO ProgressBar::Text, number, text
IF (number LT self.NBars) THEN BEGIN
    WIDGET_CONTROL, (*(self.labid))[number], SET_VALUE=text
END
END 


PRO ProgressBar::Cleanup
  ;; CATCH, Error_status
  ;; IF (Error_status NE 0) THEN BEGIN
  ;;    Print, "% ProgressBar::Cleanup: Fatal error message"
  ;;    Print, "%            " + !ERR_STRING
  ;;   CATCH, /Cancel‚
  ;;   return
  ;; END
  Print, "% ProgressBar::Cleanup: Cleaning up object information."
  IF PTR_VALID(self.minval) THEN PTR_FREE, self.minval
  IF PTR_VALID(self.maxval) THEN PTR_FREE, self.maxval
  IF PTR_VALID(self.text) THEN PTR_FREE, self.text
  IF PTR_VALID(self.barid) THEN PTR_FREE, self.barid
  IF PTR_VALID(self.labid) THEN PTR_FREE, self.labid
  IF PTR_VALID(self.barval) THEN PTR_FREE, self.barval
  ;; use WIDGET_INFO, otherwise the window kill button may lead to an
  ;; exception here
  if WIDGET_INFO(self.baseid, /VALID_ID) THEN WIDGET_CONTROL, self.baseid, /DESTROY
  Print, "% ProgressBar::Cleanup: Self-destruction."
  HELP, self
  obj_destroy, self
  HELP, self
  return
END



pro ProgressBar__define
 void={ProgressBar,name:'ProgressBar', $
       NBars:0B, $
       minval: PTR_NEW(), $
       maxval: PTR_NEW(), $
       text: PTR_NEW(), $
       barid: PTR_NEW(), $
       labid: PTR_NEW(), $
       barval: PTR_NEW(), $
       statusid: -1L, $
       baseid: -1L, $
       backgr: !P.BACKGROUND, $
       foregr: !P.COLOR, $
       break: 0}
 return
end


  PRO TestProgressbar2
  CATCH, Error_status
  IF (Error_status NE 0) THEN BEGIN
     Print, "% TestProgressBar: Fatal error message"
     Print, "%            " + !ERR_STRING
     ErrMsg, "%            " + !ERR_STRING
     CATCH, /Cancel
     return
  END
   minv=[-8,-15]
   maxv=[8,15]
   mybar=obj_new('ProgressBar', MINV=minv,MAXV=maxv,TEXT=["outer loop index","inner loop index"], STATUS="Processing negative data ...", FRAMESTYLE=2)
   i=minv[0]
   WHILE (i LE maxv[0]) DO BEGIN
       mybar->Set, 0, i, TEXT=STRING(i)
       if (i EQ 0) THEN mybar->SetStatus, "Processing positive data ..."
       j=maxv[1]
       mybar->Set, 1, j
       WHILE (j GE minv[1]) DO BEGIN
          mybar->Set, 1, j, TEXT=STRING(j)
          Wait, 0.05
          j=j-1 
       END
       i=i+1
    END
   minv=[0,1]
   maxv=[9,5]
   mybar->ReInit, 0, minv[0], maxv[0], Text="new outer loop"
   mybar->ReInit, 1, minv[1], maxv[1], Text="new inner loop"
   i=minv[0]
   WHILE (i LE maxv[0]) DO BEGIN
       mybar->Set, 0, i, TEXT=STRING(i)
       if (i EQ 0) THEN mybar->SetStatus, "Processing data ..."
       j=minv[1]
       mybar->Set, 1, j
       WHILE (j LE maxv[1]) DO BEGIN
          mybar->Set, 1, j, TEXT=STRING(j)
          Wait, 0.05
          j=j+1 
       END
       i=i+1
    END
   STOP
   obj_destroy, mybar
END
