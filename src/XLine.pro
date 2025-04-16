PRO PlotLine, x1, y1, x2, y2, wx, wy, COLOR=color
;;
;; for backwards compatibility
;;
   plcolor=25600L
   IF KEYWORD_SET(color) THEN plcolor=color
   plots, [x1,x2], [y1,y2],/device, COLOR=plcolor
   if ((ABS(FIX(wx)) GT 0) OR  (ABS(FIX(wy)) GT 0)) THEN BEGIN
    ; parallel line
    plots, [x1+FIX(wx),x2+FIX(wx)], [y1+FIX(wy),y2+FIX(wy)],/device, LINESTYLE=2, COLOR=plcolor 
    ; lines completing a box  
    plots, [(x2+x1)/2,(x2+x1)/2+FIX(wx)], [(y2+y1)/2,(y2+y1)/2+FIX(wy)],/device, LINESTYLE=2, COLOR=plcolor
    plots, [x1,x1+FIX(wx)], [y1,y1+FIX(wy)],/device, LINESTYLE=2, COLOR=plcolor
    plots, [x2,x2+FIX(wx)], [y2,y2+FIX(wy)],/device, LINESTYLE=2, COLOR=plcolor
   ENDIF
END


PRO OrthogonalComp, x1, y1, x2, y2, w, wx, wy, BIN=bin
;;
;; calculate the components wx and wy of the width vector w parallel to x and y
;;
 IF NOT(KEYWORD_SET(bin)) THEN bin=1.
 v=[FLOAT(y2-y1), - FLOAT(x2-x1)]
 v=(w/SQRT((v(0))^2+(v(1))^2))*v
 wx=v(0)*bin & wy=v(1)*bin
END 


FUNCTION CheckLineCoordinates, xr1, yr1, xr2, yr2, w, maxx, maxy, BIN=binning
;; do not change anything if the coordinates do not fit into the array dimensions
 ;; print, "% CheckLineCoordinates: x=0:",maxx
 ;; print, "% CheckLineCoordinates: y=0:",maxy
 IF NOT(KEYWORD_SET(binning)) THEN binning=1.
 wrx=0 & wry=0
 IF (w GT 0) THEN OrthogonalComp, xr1, yr1, xr2, yr2, w, wrx, wry
 A=binning*[xr1, xr2, (xr1+wrx), (xr2+wrx)]
 Min_=MIN(A, MAX=Max_)
;print, "Min_=", Min_, " Max_=", Max_
;STOP
;; print, "% CheckLineCoordinates: ", xr1,":",xr2,":", (xr1+wrx),":", (xr2+wrx)
 IF ((Min_ LT 0) OR (Max_ GT maxx))THEN return, 0
 A=binning*[yr1, yr2, (yr1+wry), (yr2+wry)]
;;  print, "% CheckLineCoordinates: ", yr1,":",yr2,":", (yr1+wry),":", (yr2+wry)
 Min_=MIN(A, MAX=Max_)
;print, "Min_=", Min_, " Max_=", Max_
;STOP
 IF ((Min_ LT 0) OR (Max_ GT maxy))THEN return, 0
 return, 1
END

Pro draw_line, x0, x1, y0, y1, s0, s1, W=w, COLOR=color, NOSPRITE=nosprite
;; draws a line in XOR mode between the two points defined by the
;; sprites s0 and s1
;;
;; plot the endpoints
;; 
;; screen coordinates!
;; 
IF NOT(Keyword_set(nosprite)) THEN BEGIN
   PutSprite, s0,  x0, y0
   PutSprite, s1,  x1, y1
END
;;
;; plot the line 
;; 
plcolor=25600L
IF KEYWORD_SET(color) THEN plcolor=color
 device, get_graphics=gmode   ; Entry mode.
 device, set_graphics=9       ; XOR mode.
 plots, [(*s0).x,(*s1).x], [(*s0).y,(*s1).y],/device, COLOR=plcolor
;; plot a parallel line in case the width is specified 
 IF (KEYWORD_SET(w) AND (w GT 0))THEN BEGIN
     wrx=0 & wry=0
     OrthogonalComp, (*s0).x, (*s0).y, (*s1).x, (*s1).y, w, wrx, wry
     plots, [(*s0).x,((*s0).x+wrx)], [(*s0).y,((*s0).y+wry)],/device, COLOR=plcolor
     plots, [(*s1).x,((*s1).x+wrx)], [(*s1).y,((*s1).y+wry)],/device, COLOR=plcolor
     plots, [((*s0).x+wrx),((*s1).x+wrx)], [((*s0).y+wry),((*s1).y+wry)],/device, COLOR=plcolor
 END
 device, set_graphics=gmode
;; 
END

PRO remove_line, s0, s1, W=w, COLOR=color, NOSPRITE=nosprite
;; removes a line in XOR mode between the two points defined by the
;; sprites s0 and s1
;;
;; plot the line 
;; 
 plcolor=25600L
 IF KEYWORD_SET(color) THEN plcolor=color
 device, get_graphics=gmode   ; Entry mode.
 device, set_graphics=9       ; XOR mode.
 plots, [(*s0).x,(*s1).x], [(*s0).y,(*s1).y],/device, COLOR=plcolor
 IF KEYWORD_SET(w) THEN BEGIN
     wrx=0 & wry=0
     OrthogonalComp, (*s0).x, (*s0).y, (*s1).x, (*s1).y, w, wrx, wry
     plots, [(*s0).x,((*s0).x+wrx)], [(*s0).y,((*s0).y+wry)],/device, COLOR=plcolor
     plots, [(*s1).x,((*s1).x+wrx)], [(*s1).y,((*s1).y+wry)],/device, COLOR=plcolor
     plots, [((*s0).x+wrx),((*s1).x+wrx)], [((*s0).y+wry),((*s1).y+wry)],/device, COLOR=plcolor
 END
 device, set_graphics=gmode
;;
;; plot the endpoints
;; 
IF NOT(Keyword_set(nosprite)) THEN BEGIN
   RestoreSprite, s0
   RestoreSprite, s1
END
;; 
END

PRO Update_LineInfo, x1_label, x1, x2_label, x2, y1_label, y1, y2_label, y2, w_label, w, dist_label, BIN=binning
WIDGET_Control, x1_label, SET_VALUE=MySTRING(x1*binning)
WIDGET_Control, x2_label, SET_VALUE=MySTRING(x2*binning)
WIDGET_Control, y1_label, SET_VALUE=MySTRING(y1*binning)
WIDGET_Control, y2_label, SET_VALUE=MySTRING(y2*binning)
IF (w_label GT 0) THEN WIDGET_Control, w_label, SET_VALUE=MySTRING((w*binning))
dist = SQRT(FLOAT((x1-x2))*FLOAT(x1-x2)+FLOAT(y1-y2)*FLOAT(y1-y2))
angle = ATAN((y2-y1),(x2-x1))*180/!DPI
WIDGET_Control, dist_label, SET_VALUE="length: "+MySTRING(dist*binning)+", angle: " +MySTRING(angle)
END


FUNCTION XLINE, xi1, yi1, xi2, yi2, wx, wy, bin=binning, TITLE=title, SEQUENCE=sequence, SEQSTEP=seqstep, SEQAVG=seqavg, WINDOWNR=windownr, NOSPRITE=nosprite
COMMON MOUSEINTERACTION, SlowComputer
COMMON PLOTOBJECTS, ByDefaultSticky
   ;;
   XConsole_PushState
   XConsole_WaitingForInput

   CATCH, Error_status
   IF (Error_status NE 0) THEN BEGIN
       PrintToCon, "% XLine:    Fatal error "
       PrintToCon, "%   Error status  - " + STRING(error_status)
       PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
       CATCH, /Cancel
       XConsole_PopState
       return, 0
   END

   result=0
   plcolor=100L
   ;;
   if (not (keyword_set(binning))) THEN binning=1 
   if (not (keyword_set(width))) THEN width=0.0 
   if (not (keyword_set(title))) THEN title="Line" 
   if (not (keyword_set(seqstep))) THEN seqstep=1 
   if (not (keyword_set(seqavg))) THEN seqavg=1 
   ;;
   ;; is there a window
   ;;
   IF (!D.WINDOW LT 0) THEN BEGIN
       printToCon, "% XLine: invalid window id"
       XConsole_PopState
       return, result
   END
   ;;
   ;;  read the window dimensions
   ;;
   wsizex=!D.X_VSIZE & wsizey=!D.Y_VSIZE
   maxx=wsizex*binning-1 & maxy=wsizey*binning-1
   ;;
   if (not (keyword_set(binning))) THEN binning=1 
   if (not (keyword_set(width))) THEN width=0.0 

   x1 = xi1/binning & y1 = yi1/binning & x2 = xi2/binning & y2 = yi2/binning 
   w= SQRT(wx^2+wy^2)/binning ;; width
   dist = SQRT((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2)) ;; length
   angle = ATAN((y2-y1),(x2-x1))*180/!DPI ;; angle with horizontal line
   ;;
   ;; the widget
   ;;
   base = Widget_Base(TITLE=title, /COLUMN) ;;, DISPLAY_NAME=GetDisplayName())
   
   ;; Create a row base to hold the input fields
   row1 =  WIDGET_BASE(base, /ROW, XPAD=5)
   x1_label = CW_Field(row1, TITLE='x1', XSize=5, Value=MySTRING(x1*binning))
   x2_label = CW_Field(row1, TITLE='x2', XSize=5, Value=MySTRING(x2*binning))
   y1_label = CW_Field(row1, TITLE='y1', XSize=5, Value=MySTRING(y1*binning))
   y2_label = CW_Field(row1, TITLE='y2', XSize=5, Value=MySTRING(y2*binning))
   w_label  = CW_Field(row1, TITLE='w',  XSize=5, Value=MySTRING(w*binning))
   
   ;; Create a row base to hold the sequence step and sequence average input fields
   IF KEYWORD_SET(sequence) THEN BEGIN
       row1b=WIDGET_BASE(base, /ROW, XPAD=5)
       seqavg_label = CW_Field(row1b, TITLE='sequence: avg.', XSize=5, Value=MySTRING(seqavg))
       seqstep_label = CW_Field(row1b, TITLE='step', XSize=5, Value=MySTRING(seqstep))
   END

   ;; Create a row base to hold the length and angle labels
   row2 = WIDGET_BASE(base, /ROW, XPAD=5)
   dist_label= WIDGET_Label(row2, /Dynamic_Resize, Value="l: "+MySTRING(dist*binning)+", angle: " +MySTRING(angle))

   ;; 
   row3 =  WIDGET_BASE(base, /ROW)
   sticktext=Widget_Label(row3, VALUE='Keep marker')
   togglebase = WIDGET_BASE(row3, /ROW, /EXCLUSIVE)
   stick_y = WIDGET_BUTTON(togglebase, VALUE='y', UVALUE='stick_y', /NO_RELEASE)
   stick_n = WIDGET_BUTTON(togglebase, VALUE='n', UVALUE='stick_n', /NO_RELEASE)

   ;; a fourth row for the buttons
   row4 = WIDGET_BASE(base, /ROW)
   b = CW_BGroup(row4, /ROW , ['  Accept  ','  Apply  ','  Help  ','  Cancel  '] , Button_UVALUE=['close','edit','help','cancel'])
   widpos=PlaceWidget(base, POSKEY=WidgetPos("ActiveDialogWin"))    
   Widget_Control, base, XOFFSET=widpos[0], YOffset=widpos[1], /REALIZE


   ;; now define the sprites for the endpoints
   pixsize=6
   p1=CreateSprite(pixsize, COLOR=255, CHANNEL=2) ;; green - start
   p2=CreateSprite(pixsize, COLOR=255, CHANNEL=1) ;; red   - end
   ;;
   draw_line, x1, x2, y1, y2, p1, p2, W=w, COLOR=plcolor, NOSPRITE=nosprite
   ;;
   sticky=ByDefaultSticky
   If (sticky EQ 1) THEN WIDGET_CONTROL, stick_y, /SET_BUTTON $
   ELSE WIDGET_CONTROL, stick_n, /SET_BUTTON
   ;;
   do_it_again = 0 
   ;;
   ;; bring current window to top
   IF (keyword_set(windownr)) THEN BEGIN
      WSHOW, windownr
   END ELSE BEGIN
      WSHOW, !D.WINDOW
   END
   ;;
   REPEAT BEGIN
      WAIT, 0.02
       cursor, ix, iy, 1, /dev, /NOWAIT
       IF !mouse.button eq 1 THEN BEGIN
           IF (InSprite(p1,ix,iy,tol=2)) THEN BEGIN 
               ;;
               ;; change position of p0 while mouse button is pressed
               ;;
               ;; wait until button up operation is detected
               ;; on slow computers
               ;;
               IF SlowComputer THEN cursor, ix, iy, 4, /dev
               ;;
               ;; test new coordinates
               ;;
               tx1=ix & ty1=iy
               ;;
               IF CheckLineCoordinates(tx1, ty1, x2, y2, w, maxx, maxy, BIN=binning) THEN BEGIN
                   remove_line, p1, p2,  W=w, COLOR=plcolor, NOSPRITE=nosprite
                   x1=ix & y1=iy
                   draw_line, x1, x2, y1, y2, p1, p2, W=w, COLOR=plcolor, NOSPRITE=nosprite
                   Update_LineInfo, x1_label, x1, x2_label, x2, y1_label, y1, y2_label, y2, w_label, w, dist_label, BIN=binning
               ENDIF ELSE printToCon, "% XLine: coordinates out of bounds!"                   
           ENDIF  
           IF (InSprite(p2,ix,iy,tol=2)) THEN BEGIN 
               ;;
               ;; change position of p1 while mouse button is pressed
               ;;
               ;; wait until button up operation is detected
               ;;
               IF SlowComputer THEN cursor, ix, iy, 4, /dev
               ;;
               tx2=ix & ty2=iy
               IF CheckLineCoordinates(x1, y1, tx2, ty2, w, maxx, maxy, BIN=binning) THEN BEGIN

                   remove_line, p1, p2, W=w, COLOR=plcolor, NOSPRITE=nosprite
                   x2=ix & y2=iy
                   draw_line, x1, x2, y1, y2, p1, p2 , W=w, COLOR=plcolor, NOSPRITE=nosprite
                   Update_LineInfo, x1_label, x1, x2_label, x2, y1_label, y1, y2_label, y2, w_label, w, dist_label, BIN=binning
               ENDIF ELSE printToCon, "% XLine: coordinates out of bounds!"                              
           ENDIF
       END 
       IF (!mouse.button eq 2) THEN BEGIN
           IF (InSprite(p1,ix,iy,tol=2)) THEN BEGIN ;; move position of p1 and p2
               IF SlowComputer THEN cursor, ix, iy, 4, /dev
               dx=ix-(*p1).x & dy=iy-(*p1).y
               tx1=x1+dx & tx2=x2+dx & ty1=y1+dy & ty2=y2+dy
               IF CheckLineCoordinates(tx1, ty1, x2, y2, w, maxx, maxy, BIN=binning) THEN BEGIN
                   remove_line, p1, p2, W=w, COLOR=plcolor, NOSPRITE=nosprite
                   x1=tx1 & x2=tx2 & y1=ty1 & y2=ty2
                   draw_line, x1, x2, y1, y2, p1, p2, W=w, COLOR=plcolor, NOSPRITE=nosprite
                   Update_LineInfo, x1_label, x1, x2_label, x2, y1_label, y1, y2_label, y2, w_label, w, dist_label, BIN=binning
               ENDIF ELSE printToCon, "% XLine: coordinates out of bounds!"                   
           ENDIF  
       END 
       IF (!mouse.button EQ 4) THEN  BEGIN
           do_it_again = NOT(do_it_again)
           result=1
       END

       ev = WIDGET_Event(base, /NoWait)
       
       if ((ev.id EQ stick_n) or (ev.id EQ stick_y)) THEN BEGIN
           uv=""
           WIDGET_CONTROL, ev.id, GET_UVALUE = uv
           CASE uv OF
               'stick_y': sticky=1
               'stick_n': sticky=0
               ELSE:
           ENDCASE
       END        
       
       IF (ev.id EQ b) THEN BEGIN                ;; Button-Menu
           CASE ev.value OF
               'help': BEGIN
                   msg = Dialog_Info( $ 
                        ["Define a line in the current window",$  
                         " ", $
                         "* Mouse operations:",$
                         "  LEFT button   - move start (green) or end point (red)",$
                         "  MIDDLE button - move the line (only start point)",$
                         "  RIGHT button  - accept the current settings", $
                         "                  (same as pressing ACCEPT)", $
                         "* You can also directly input coordinates into the",$
                         "  data fields followed by pressing the APPLY button", $
                         "* Use the ACCEPT button to continue processing, e.g.",$
                         "  in order to perform a line scan.", $
                         "* Use the CANCEL button to leave the dialog without",$
                         "  further action.", $
                         "  "], $
                                         /INFO)
               END
               'edit': BEGIN

                   ;; get values from input fields
                   TMP='' & WIDGET_Control, x1_label, GET_VALUE=TMP
                   TMP=TMP(0) & IF (TMP NE '') THEN tx1=FIX(FLOAT(TMP))
                   ;;
                   TMP='' & WIDGET_Control, x2_label, GET_VALUE=TMP
                   TMP=TMP(0) & IF (TMP NE '') THEN tx2=FIX(FLOAT(TMP))
                   ;;
                   TMP='' & WIDGET_Control, y1_label, GET_VALUE=TMP
                   TMP=TMP(0) & IF (TMP NE '') THEN ty1=FIX(FLOAT(TMP))
                   ;;
                   TMP='' & WIDGET_Control, y2_label, GET_VALUE=TMP
                   TMP=TMP(0) & IF (TMP NE '') THEN ty2=FIX(FLOAT(TMP))
                   TMP='' & WIDGET_Control, w_label, GET_VALUE=TMP
                   TMP=TMP(0)
                   IF (TMP NE '') THEN tw=((ABS(ROUND(FLOAT(TMP)))))
                   IF CheckLineCoordinates(tx1, ty1, tx2, ty2, tw, maxx, maxy) THEN BEGIN
                       remove_line, p1, p2,  W=w, COLOR=plcolor, NOSPRITE=nosprite
                       x1=FIX(tx1/binning) & x2=FIX(tx2/binning) & y1=FIX(ty1/binning) & y2=FIX(ty2/binning) & w=FIX(tw/binning)
                       
                       draw_line, x1, x2, y1, y2, p1, p2, W=w, COLOR=plcolor, NOSPRITE=nosprite
                   END ELSE printToCon, "% XLine: coordinates out of bounds!"                   
                   Update_LineInfo, x1_label, tx1, x2_label, tx2, y1_label, ty1, y2_label, ty2, w_label, tw, dist_label, BIN=1
                   
                   IF KEYWORD_SET(sequence) THEN BEGIN
                       TMP='' & WIDGET_Control, seqavg_label, GET_VALUE=TMP
                       TMP=TMP(0) & IF (TMP NE '') THEN seqavg=FIX(FLOAT(TMP))
                       TMP='' & WIDGET_Control, seqstep_label, GET_VALUE=TMP
                       TMP=TMP(0) & IF (TMP NE '') THEN seqstep=FIX(FLOAT(TMP))
                   END 
               END 
               'cancel':BEGIN
                   do_it_again = NOT(do_it_again)
                   result=0
               END  
               'close': BEGIN

                   ;; get values from input fields
                   TMP='' & WIDGET_Control, x1_label, GET_VALUE=TMP
                   TMP=TMP(0) & IF (TMP NE '') THEN tx1=FIX(FLOAT(TMP))
                   ;;
                   TMP='' & WIDGET_Control, x2_label, GET_VALUE=TMP
                   TMP=TMP(0) & IF (TMP NE '') THEN tx2=FIX(FLOAT(TMP))
                   ;;
                   TMP='' & WIDGET_Control, y1_label, GET_VALUE=TMP
                   TMP=TMP(0) & IF (TMP NE '') THEN ty1=FIX(FLOAT(TMP))
                   ;;
                   TMP='' & WIDGET_Control, y2_label, GET_VALUE=TMP
                   TMP=TMP(0) & IF (TMP NE '') THEN ty2=FIX(FLOAT(TMP))
                   TMP='' & WIDGET_Control, w_label, GET_VALUE=TMP
                   TMP=TMP(0)
  ;;                 IF (TMP NE '') THEN tw=((ABS(ROUND(FLOAT(TMP))))/binning)
                   IF (TMP NE '') THEN tw=((ABS(ROUND(FLOAT(TMP)))))
                   IF CheckLineCoordinates(tx1, ty1, tx2, ty2, tw, maxx, maxy) THEN BEGIN
                       remove_line, p1, p2,  W=w, COLOR=plcolor, NOSPRITE=nosprite
                       x1=FIX(tx1/binning) & x2=FIX(tx2/binning) & y1=FIX(ty1/binning) & y2=FIX(ty2/binning) & w=FIX(tw/binning)
                       draw_line, x1, x2, y1, y2, p1, p2, W=w, COLOR=plcolor, NOSPRITE=nosprite 
                       do_it_again = NOT(do_it_again)
                       result=1
                   END ELSE printToCon, "% XLine: coordinates out of bounds!"                   
                   
                   IF KEYWORD_SET(sequence) THEN BEGIN
                       TMP='' & WIDGET_Control, seqavg_label, GET_VALUE=TMP
                       TMP=TMP(0) & IF (TMP NE '') THEN seqavg=FIX(FLOAT(TMP))
                       TMP='' & WIDGET_Control, seqstep_label, GET_VALUE=TMP
                       TMP=TMP(0) & IF (TMP NE '') THEN seqstep=FIX(FLOAT(TMP))
                   END 

                END
           endcase
       ENDIF 
   ENDREP  UNTIL do_it_again

   if (sticky EQ 0) THEN BEGIN
       ;; remove line from screen
       remove_line, p1, p2, W=w, COLOR=plcolor, NOSPRITE=nosprite
   END    


   Widget_Control, base, /DESTROY
   device, cursor_standard=2
   xi1 = x1*binning & yi1 = y1*binning & xi2 = x2*binning & yi2 = y2*binning
   w=w*binning

   ;; calculate orthogonal components of width vector
   IF (w GT 0) THEN OrthogonalComp, xi1, yi1, xi2, yi2, w, wx, wy, BIN=1 $
   ELSE BEGIN
       wx=0 & wy=0
   END
   ;;
   DeleteSprite, p1
   DeleteSprite, p2
   ;;
   If (result GT 0) THEN BEGIN
       dist = SQRT((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2))
       angle = ATAN((y2-y1),(x2-x1))*180/!DPI
       printTocon, "% XLine: (x1, y1, x2, y2) = (" + MyString(x1)+", "+MyString(y1)+", "+ MyString(x2)+", "+MyString(y2)+")"
       printTocon, "%                 (w, wx, wy) = ("+ MyString(w)+", "+MyString(wx)+", "+MyString(wy)+")"
       printTocon, "%                    (l, angle) = ("+MyString(dist)+", "+MyString(angle)+")"
       IF KEYWORD_SET(sequence) THEN BEGIN
       printToCon, "%          (seq.-avg, step) = ("+MyString(dist)+", "+MyString(angle)+")"
       END
   END

   XConsole_PopState

   return, result
END


;; PRO TestLine
;; p=GetCurrentImage()
;; X0 = FIX((*p).SzX/4)
;; Y0 = FIX((*p).SzY/2)
;; X1 = FIX(3*(*p).SzX/4)
;; Y1 = FIX((*p).SzY/2)
;; width = 0.0
;; wx=0.0 & wy =0.0
;; tmp=XLine(X0, Y0, X1, Y1, wx, wy, bin=(*p).bin)
;; END
