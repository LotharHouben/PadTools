PRO CheckBoxCoord, x1, x2, y1, y2, xr1, xr2, yr1, yr2, b, F, Sq, MOUSE=mouse, MAXX=maxx, MAXY=maxy
  if keyword_set(mouse) THEN BEGIN
     ;; take coordinates from screen and multiply them to get the
     ;; real coordinates
     xr1=x1*b & xr2=x2*b
     yr1=y1*b & yr2=y2*b
  END
;; check for xr2 <= xr1
  IF (xr2 LT xr1) THEN BEGIN
     tmp=xr1
     xr1=xr2
     xr2=tmp
  END
;; check for yr2 <= yr1
  IF (yr2 LT yr1) THEN BEGIN
     tmp=yr1
     yr1=yr2
     yr2=tmp
  END
;; calculate size first
  DX=ABS(xr2 - xr1) + 1
  DY=ABS(yr2 - yr1) + 1 
;; square size?
  if KEYWORD_SET(maxx) THEN BEGIN
     IF (DX GT maxx) THEN DX=maxx
  END
  if KEYWORD_SET(maxy) THEN BEGIN
     IF (DY GT maxy) THEN DY=maxy
  END
  IF (Sq GT 0) THEN BEGIN
     ;; square size: try to adapt y size to x size
     if KEYWORD_SET(maxy) THEN BEGIN
        IF (DX LE maxy) THEN BEGIN 
           DY=DX
           Y0=CEIL((yr2+yr1)*0.5)
           yr1=Y0-FIX(DY*0.5) & yr2=yr1+DY-1
        END ELSE BEGIN
           DX=DY
           X0=CEIL((xr2+xr1)*0.5)   
           xr1=X0-FIX(DX*0.5) & xr2=xr1+DX-1
        END
     END 
  END 
       ; correct to size that is a product of prime factors max N 
  ;;  print, "F=", F, " Sq=", Sq, " DX=", DX, "closest=", closestprimefactor(F,DX)
  IF (F GT 1) THEN BEGIN
        
     DX=closestprimefactor(F,DX, /MINDIST)
     ;; check size, must not be bigger than array size
     IF keyword_set(MaxX) THEN BEGIN
        WHILE (DX GT (MaxX+1)) DO DX=closestprimefactor(F,DX-1, /MINDIST)
     END
     DY=closestprimefactor(F,DY, /MINDIST)
     IF keyword_set(MaxY) THEN BEGIN
        WHILE (DY GT (MaxY+1)) DO DY=closestprimefactor(F,DY-1, /MINDIST)
     END
        
        
        ;; recalculate corrected image coordinates
        X0=FLOOR((xr2+xr1)*0.5)   
        Y0=FLOOR((yr2+yr1)*0.5)
        xr1=X0-FIX(DX*0.5) & xr2=xr1+DX-1
        yr1=Y0-FIX(DY*0.5) & yr2=yr1+DY-1
        
     END
;;
;; box fits but may be outside the field of view
     IF (xr1 LT 0) THEN BEGIN
        xr2=xr2-xr1 & xr1=0
     END 
     IF (xr2 GE maxx) THEN BEGIN
        xr2=maxx-1 & xr1=xr2-DX+1
     END
     IF (yr1 LT 0) THEN BEGIN
        yr2=yr2-yr1 & yr1=0
     END 
     IF (yr2 GE maxy) THEN BEGIN
        yr2=maxy-1 &  yr1=yr2-DY+1
     END
;;
;; recalculate display coordinates
     x1=FIX(xr1/b) & x2=FIX(xr2/b)
     y1=FIX(yr1/b) & y2=FIX(yr2/b)
;;     x1=FLOOR(xr1/b) & x2=CEIL(xr2/b)
;;     y1=FLOOR(yr1/b) & y2=CEIL(yr2/b)

  END  

PRO plot_Box, x1, x2, y1, y2, COLOR=color, NOCROSS=nocross
COMMON BoxDevCoord, boxdata, crossdata
plcolor=25600L
IF KEYWORD_SET(color) THEN plcolor=color
   x=FIX((x1+x2)/2) & y=FIX((y1+y2)/2)
   IF NOT(KEYWORD_SET(nocross)) THEN plot_cross, x, y, COLOR=plcolor
   plots, [x1,x1], [y2,y1],/device, COLOR=plcolor
   plots, [x2,x2], [y2,y1],/device, COLOR=plcolor
   plots, [x1,x2], [y1,y1],/device, COLOR=plcolor
   plots, [x1,x2], [y2,y2],/device, COLOR=plcolor
   return
END

PRO StaticXBox, x1, x2, y1, y2, COLOR=color, NOCROSS=nocross
  IF KEYWORD_SET(color) THEN plcolor=color
  x=FIX((x1+x2)/2) & y=FIX((y1+y2)/2)
  device, get_graphics=gmode0     ; Entry mode.
  device, set_graphics=oplotmode  ; XOR mode.
  IF NOT(KEYWORD_SET(nocross)) THEN plot_cross, x, y, COLOR=plcolor
  plots, [x1,x1], [y2,y1],/device, COLOR=plcolor
  plots, [x2,x2], [y2,y1],/device, COLOR=plcolor
  plots, [x1,x2], [y1,y1],/device, COLOR=plcolor
  plots, [x1,x2], [y2,y2],/device, COLOR=plcolor
  device, set_graphics=gmode0   ; Restore mode.
END

FUNCTION XBox, xi1, yi1, xi2, yi2, bin=binning, SX=MaxX, SY=MaxY, TITLE=title, INTPOW2=intpow2, SQUARE=square, FIXX=fixx, FIXY=fixy, HELP=hlp, NOMOVE=nomove, MULTIPLE=multiple
;; 
;; interactively plot a box on the screen
;; xi1, yi1, xi1, yi2 are real image coordinates
;; bin is the binning factor for the TV display window
;; MaxX and MaxY are the maximum image indices, 
;; i.e. (number of elements -1)
;;
;; the xr* are image coordinates, x* is a display coordinate 
;;
;; multiple: ste to an empty list if you want to collect multiple rois
COMMON MOUSEBINDINGS, mm_move, mm_resize, mm_break
;;
XConsole_PushState
XConsole_WaitingForInput
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    PrintToCon, "% XBox:    Fatal error "
    PrintToCon, "%   Error status  - " + STRING(error_status)
    PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
    CATCH, /Cancel
    XConsole_PopState
    return, 0
END

result=0                        ; return false by default
oplotmode=9
plcolor=100L
print, "% XBox: current window index is ", !D.Window
   ;;
   ;; is there a window
   ;;
   IF (!D.WINDOW LT 0) THEN BEGIN
       printToCon, "% XBox: invalid window id"
       XConsole_PopState
       return, result
   END
   ;;
   ;; read the window dimensions
   ;;
   wsizex=!D.X_VSIZE & wsizey=!D.Y_VSIZE
   ;;
   if (not (keyword_set(binning))) THEN binning=1 
   x1 = xi1/binning
   y1 = yi1/binning
   ;;
   FixSq = 0
   if ((keyword_set(square))) THEN FixSq = 1
   MaxRadix = 0
   if ((keyword_set(intpow2))) THEN MaxRadix = 2
   ;;
   IF NOT(keyword_set(hlp)) THEN hlp=["Define a rectangular box in the current window.", $
                "Mouse operations:  left button - move centre", $
                "                   middle button - resize by picking the lower left edge", $ 
                "                   right button - accept the current settings and leave", $
                "You can also edit the edge positions and press on 'apply'"]
   ;; 
   ;; the maximum array indices MaxX, MaxY
   ;;
   if (not (keyword_set(binning))) THEN binning=1 
   if (not (keyword_set(MaxX))) THEN MaxX=wsizex*binning-1
   if (not (keyword_set(MaxY))) THEN MaxY=wsizey*binning-1
   ;;
   ;; xr* are real world coordinates
   ;; xi* are temporary storage real world coordinates
   ;; x*  ere display coordinates
   ;; 
   xr1 = xi1 & x1 = xi1/binning
   xr2 = xi2 & x2 = xi2/binning
   yr1 = yi1 & y1 = yi1/binning
   yr2 = yi2 & y2 = yi2/binning
   ;;
   ;; check bounds and fix to integer power of 2 size if desired
   ;;
   print, "% XBox: (x1,x2,y1,y2)=(", MyString(x1), ",", MyString(x2), ",", MyString(y1), ",", MyString(y2), ")" 
   print, "% XBox: (xr1,xr2,yr1,yr2)=(", MyString(xr1), ",", MyString(xr2), ",", MyString(yr1), ",", MyString(yr2), ")" 
   CheckBoxCoord, x1, x2, y1, y2, xr1, xr2, yr1, yr2, binning, MaxRadix, FixSq, MAXX=MaxX, MAXY=MaxY
   print, "% XBox: (x1,x2,y1,y2)=(", MyString(x1), ",", MyString(x2), ",", MyString(y1), ",", MyString(y2), ")" 
   print, "% XBox: (xr1,xr2,yr1,yr2)=(", MyString(xr1), ",", MyString(xr2), ",", MyString(yr1), ",", MyString(yr2), ")"
   ;;
   ;; create the widget window with the fancy buttons
   ;;
   if (not (keyword_set(title))) THEN title="Rectangle Dialog" 
   base = Widget_Base(TITLE=title, /COLUMN) ;;, DISPLAY_NAME=GetDisplayName())
                                ;Create a row base to hold the input
                                ;fields for the four coordinate values
   buffer_row= WIDGET_BASE(base, /ROW)
   label=Widget_LABEL(buffer_row, VALUE="")
   row1 =  WIDGET_BASE(base, /ROW, XPAD=10)
   x1_label = CW_Field(row1, TITLE='x1,x2:', XSize=5, Value=MySTRING(xr1))
   x2_label = CW_Field(row1, TITLE='', XSize=5, Value=MySTRING(xr2))
   y1_label = CW_Field(row1, TITLE='y1,y2:', XSize=5, Value=MySTRING(yr1))
   y2_label = CW_Field(row1, TITLE='', XSize=5, Value=MySTRING(yr2))
                                ; create a second row to hold the
                                ; label fields for the width and the height
   row2=Widget_Base(base, /ROW)
   my_label  = WIDGET_Label(row2, VALUE=' Width, height: ')
   z_label = WIDGET_Label(row2, /Dynamic_Resize, Value=MySTRING(ABS(xr2-xr1+1))+','+MySTRING(ABS(yr2-yr1+1)))
   
   buffer_row= WIDGET_BASE(base, /ROW)
   label=Widget_LABEL(buffer_row, VALUE="")
                                ; create another row to hold the
                                ; single step  shift buttons


                               ; create another row for the sticky flag
                                ; create another row for the fixIntPow flag
   row5 =  WIDGET_BASE(base, /ROW, XPAD=10)

   label=Widget_LABEL(row5,VALUE="Maximum prime factor:    ")
   pfactor=["Arbitrary","Two","Three","Five","Seven"]
   dbox = WIDGET_Droplist(row5, VALUE=pfactor, UVALUE="dbox", /FLAT)
 ;; input boxes for the value and the 
   if ((keyword_set(intpow2))) THEN WIDGET_CONTROL, dbox, SET_Droplist_SELECT=1 ELSE  WIDGET_CONTROL, dbox, SET_Droplist_SELECT=0


   
   row5b =  WIDGET_BASE(base, /ROW, XPAD=10)

   IntPowtext=Widget_Label(row5b, VALUE='Square size:              ')
   toggleSQ = WIDGET_BASE(row5b, /ROW, /EXCLUSIVE)
   SQ_y = WIDGET_BUTTON(toggleSQ, VALUE='Yes', UVALUE='SQ_y', /NO_RELEASE, TOOLTIP="adjust to square size")
   SQ_n = WIDGET_BUTTON(toggleSQ, VALUE='No', UVALUE='SQ_n', /NO_RELEASE, TOOLTIP="freely adjustable size")

   buffer_row= WIDGET_BASE(base, /ROW)
   label=Widget_LABEL(buffer_row, VALUE="")

   mvbuttons = WIDGET_BASE(base, /ROW, XPAD=10)
   label=Widget_LABEL(mvbuttons, VALUE="Position: ")
   mvb = CW_BGroup(mvbuttons, /ROW , ['   ^   ','   v   ','   <   ','   >   '] , Button_UVALUE=['up', 'down','left','right'])

   mvcbuttons = WIDGET_BASE(base, /ROW, XPAD=10)
   label=Widget_LABEL(mvcbuttons, VALUE="Size:     ")
   mvc = CW_BGroup(mvcbuttons, /ROW , [' < w > ',' > w < ',' < h > ',' > h < '] , Button_UVALUE=['wplus','wminus','hplus','hminus'])
   buffer_row= WIDGET_BASE(base, /ROW)
   label=Widget_LABEL(buffer_row, VALUE="")

   ;; row4 =  WIDGET_BASE(base, /ROW, XPAD=10)

  ;;  sticktext=Widget_Label(row4, VALUE='Remove marker box when closing   ')
  ;; togglebase = WIDGET_BASE(row4, /ROW, /EXCLUSIVE)
  ;; stick_y = WIDGET_BUTTON(togglebase, VALUE='yes', UVALUE='stick_y', /NO_RELEASE, TOOLTIP="keep marker rectangle after leaving the dialog")
  ;;  stick_n = WIDGET_BUTTON(togglebase, VALUE='no', UVALUE='stick_n', /NO_RELEASE, TOOLTIP="remove marker rectangle after leaving the dialog")

                                ; create another row to hold the buttons
   row3 =  WIDGET_BASE(base, /ROW, XPAD=10)
   IF keyword_set(multiple) THEN BEGIN
      
      b = CW_BGroup(row3, /ROW , ['  Finish  ','  Add ROI  ','  Apply Sizes  ','   Help   ','  Cancel  '] , Button_UVALUE=['accept','add','apply','help','cancel'])
   END ELSE BEGIN
      
      b = CW_BGroup(row3, /ROW , ['  Accept  ','  Apply Sizes  ','   Help   ','  Cancel  '] , Button_UVALUE=['accept','apply','help','cancel'])
   END
   ;; bring up the widget



  widpos=PlaceWidget(base, POSKEY="ur")
 WIDGET_CONTROL, base, XOFFSET=widpos[0], YOFFSET=widpos[1], /REALIZE

   ;;
   ;;
   ;;
   device, get_graphics_function=gmode0  ; store the current graphics mode.
   device, set_graphics_function=oplotmode       ; XOR mode.
   plot_Box, x1, x2, y1, y2, COLOR=plcolor
   x0= FIX((x1+x2)/2)
   y0= FIX((y1+y2)/2)
   do_it_again = 0 & sticky=1 & 
   
;;    WIDGET_CONTROL, stick_y, /SET_BUTTON

   IF (KEYWORD_SET(square)) THEN WIDGET_CONTROL, SQ_y, /SET_BUTTON ELSE WIDGET_CONTROL, SQ_n, /SET_BUTTON

   ;; bring current window to top
   WSHOW, !D.WINDOW
   
   REPEAT BEGIN
      ;; get new cursor pos
      cursor, ix, iy, 1, /dev, /NOWAIT
      ;; now check whether the coordinates ix, iy are inside the
      ;; window, workaround for a *silly* Windows event bug 
      ;; 
      IF (((ix GE 0) OR (iy GE 0)) AND (ix LT wsizex) AND (iy LT wsizey)) THEN BEGIN
          IF (!mouse.button eq mm_move) THEN BEGIN
              ;; move box 
              IF ((ix NE x0) AND (iy NE y0)) THEN BEGIN
                  ;; overplot old box
                  plot_Box, x1, x2, y1, y2, COLOR=plcolor
                  device, cursor_standard=45
                  x0= FIX((x1+x2)/2) ;; old centre x
                  if NOT(keyword_set(fixx)) THEN BEGIN
                      dx=ix-x0  ;; shift in x in window coordinates
                      x0=ix  ;; new center coordinate
                      xr1+=(binning*dx) 
                      xr2+=(binning*dx) 
                  END
                  y0= FIX((y1+y2)/2) ;; old center y
                  if NOT(keyword_set(fixy)) THEN BEGIN
                      dy=iy-y0
                      y0=iy
                      yr1+=(binning*dy) 
                      yr2+=(binning*dy)
                  END
;;                   print, "dx,dy=", dx, dy
                  TVCRS, x0, y0
                  CheckBoxCoord, x1, x2, y1, y2, xr1, xr2, yr1, yr2, binning, MaxRadix, FixSq, MAXX=MaxX, MAXY=MaxY
                  plot_Box, x1, x2, y1, y2, COLOR=plcolor
                  WIDGET_Control, x1_label, SET_VALUE=MySTRING(xr1)
                  WIDGET_Control, x2_label, SET_VALUE=MySTRING(xr2)
                  WIDGET_Control, y1_label, SET_VALUE=MySTRING(yr1)
                  WIDGET_Control, y2_label, SET_VALUE=MySTRING(yr2)
                  WIDGET_Control, z_label, SET_VALUE=MySTRING(ABS(xr2-xr1+1))+','+MySTRING(ABS(yr2-yr1+1))
              ENDIF 
          ENDIF
          IF (!mouse.button eq mm_resize) THEN BEGIN
              ;; change size !
              IF ((ix NE x1) AND (iy NE y1)) THEN BEGIN
                  ;; overplot old box
                  plot_Box, x1, x2, y1, y2, COLOR=plcolor
                  device, cursor_standard=42
                  ;; new edge coordinates
                  if NOT(keyword_set(fixx)) then BEGIN
                      x0= FIX((x1+x2)/2)
                      x1=ix
                      x2=2*x0-x1
                  END
                  if NOT(keyword_set(fixy)) then BEGIN
                      y0= FIX((y1+y2)/2)
                      y1=iy
                      y2=2*y0-y1 
                  END
                  CheckBoxCoord,x1,x2,y1,y2,xr1,xr2,yr1,yr2,binning,MaxRadix, FixSq, /MOUSE, MAXX=MaxX, MAXY=MaxY
                  TVCRS, x1, y1
                  plot_Box, x1, x2, y1, y2, COLOR=plcolor
                  xr1=x1*binning & xr2=x2*binning
                  yr1=y1*binning & yr2=y2*binning 
                  WIDGET_Control, x1_label, SET_VALUE=MySTRING(xr1)
                  WIDGET_Control, x2_label, SET_VALUE=MySTRING(xr2)
                  WIDGET_Control, y1_label, SET_VALUE=MySTRING(yr1)
                  WIDGET_Control, y2_label, SET_VALUE=MySTRING(yr2)
                  WIDGET_Control, z_label, SET_VALUE=MySTRING(ABS(xr2-xr1+1))+','+MySTRING(ABS(yr2-yr1+1))
              ENDIF 
          ENDIF
          IF (!mouse.button EQ mm_break) THEN  BEGIN
              result=1 & do_it_again = NOT(do_it_again)
          END
      END

      ev = WIDGET_Event(base, /NoWait)
      
;;       if ((ev.id EQ stick_n) or (ev.id EQ stick_y)) THEN BEGIN
;;         uv=""
;;         WIDGET_CONTROL, ev.id, GET_UVALUE = uv
;;         CASE uv OF
;;         'stick_y': sticky=1
;;         'stick_n': sticky=0
;;         ELSE:
;;         ENDCASE
;;      END

      if ((ev.id EQ dbox) or (ev.id EQ SQ_n) or (ev.id EQ SQ_y)) THEN BEGIN
         uv=""
         WIDGET_CONTROL, ev.id, GET_UVALUE = uv
         CASE uv OF
            'dbox': BEGIN
               s=WIDGET_INFO(ev.id, /DROPLIST_SELECT)
               radix=pfactor[s]
               CASE s OF
                  0: Maxradix=0
                  1: MaxRadix=2
                  2: MaxRadix=3
                  3: MaxRadix=5
                  4: MaxRadix=7
                   ELSE:
               END
            END 
            'SQ_y': FixSq=1
            'SQ_n': FixSq=0
            ELSE: 
         END
         plot_Box, x1, x2, y1, y2, COLOR=plcolor
         device, cursor_standard=42
         TMP='' & WIDGET_Control, x1_label, GET_VALUE=TMP
         TMP=TMP(0) & IF (TMP NE '') THEN xr1=FIX(FLOAT(TMP))
         TMP='' & WIDGET_Control, x2_label, GET_VALUE=TMP
         TMP=TMP(0) & IF (TMP NE '') THEN xr2=FIX(FLOAT(TMP))
         TMP='' & WIDGET_Control, y1_label, GET_VALUE=TMP
         TMP=TMP(0) & IF (TMP NE '') THEN yr1=FIX(FLOAT(TMP))
         TMP='' & WIDGET_Control, y2_label, GET_VALUE=TMP
         TMP=TMP(0) & IF (TMP NE '') THEN yr2=FIX(FLOAT(TMP))
         if keyword_set(fixx) then BEGIN
            xr1=x1 & xr2=x2
         END
         if keyword_set(fixy) then BEGIN
            yr1=y1 & yr2=y2
         END
         CheckBoxCoord,x1,x2,y1,y2,xr1,xr2,yr1,yr2,binning,MaxRadix, FixSq, MAXX=MaxX, MAXY=MaxY
         ;; TVCRS, x1, y1
         plot_Box, x1, x2, y1, y2, COLOR=plcolor
         WIDGET_Control, x1_label, SET_VALUE=MySTRING(xr1)
         WIDGET_Control, x2_label, SET_VALUE=MySTRING(xr2)
         WIDGET_Control, y1_label, SET_VALUE=MySTRING(yr1)
         WIDGET_Control, y2_label, SET_VALUE=MySTRING(yr2)
         WIDGET_Control, z_label, SET_VALUE=MySTRING(ABS(xr2-xr1+1))+','+MySTRING(ABS(yr2-yr1+1))
      END


      IF ((ev.id EQ b) or (ev.id EQ mvb) or (ev.id EQ mvc)) THEN BEGIN                ;; Button-Menu
         CASE ev.value OF
            'help': BEGIN
               msg = dialog_info(hlp, /HELP)
            END
            'accept': BEGIN
                    TMP='' & WIDGET_Control, x1_label, GET_VALUE=TMP
                    TMP=TMP(0) & IF (TMP NE '') THEN xr1=FIX(FLOAT(TMP))
                    TMP='' & WIDGET_Control, x2_label, GET_VALUE=TMP
                    TMP=TMP(0) & IF (TMP NE '') THEN xr2=FIX(FLOAT(TMP))
                    TMP='' & WIDGET_Control, y1_label, GET_VALUE=TMP
                    TMP=TMP(0) & IF (TMP NE '') THEN yr1=FIX(FLOAT(TMP))
                    TMP='' & WIDGET_Control, y2_label, GET_VALUE=TMP
                    TMP=TMP(0) & IF (TMP NE '') THEN yr2=FIX(FLOAT(TMP))
                    if keyword_set(fixx) then BEGIN
                        xr1=x1 & xr2=x2
                    END
                    if keyword_set(fixy) then BEGIN
                        yr1=y1 & yr2=y2
                     END
                    print, "% XBox: (x1,x2,y1,y2)=(", MyString(x1), ",", MyString(x2), ",", MyString(y1), ",", MyString(y2), ")" 
                    print, "% XBox: (xr1,xr2,yr1,yr2)=(", MyString(xr1), ",", MyString(xr2), ",", MyString(yr1), ",", MyString(yr2), ")" 
                    CheckBoxCoord,x1,x2,y1,y2,xr1,xr2,yr1,yr2,binning,MaxRadix, FixSq, MAXX=MaxX, MAXY=MaxY
                    print, "% XBox: (x1,x2,y1,y2)=(", MyString(x1), ",", MyString(x2), ",", MyString(y1), ",", MyString(y2), ")" 
                    print, "% XBox: (xr1,xr2,yr1,yr2)=(", MyString(xr1), ",", MyString(xr2), ",", MyString(yr1), ",", MyString(yr2), ")" 
               do_it_again = NOT(do_it_again)
               result=1
            END
            'cancel': BEGIN
               do_it_again = NOT(do_it_again)
            END
            'apply': BEGIN
	            plot_Box, x1, x2, y1, y2, COLOR=plcolor
        	    device, cursor_standard=42
                    TMP='' & WIDGET_Control, x1_label, GET_VALUE=TMP
                    TMP=TMP(0) & IF (TMP NE '') THEN xr1=FIX(FLOAT(TMP))
                    TMP='' & WIDGET_Control, x2_label, GET_VALUE=TMP
                    TMP=TMP(0) & IF (TMP NE '') THEN xr2=FIX(FLOAT(TMP))
                    TMP='' & WIDGET_Control, y1_label, GET_VALUE=TMP
                    TMP=TMP(0) & IF (TMP NE '') THEN yr1=FIX(FLOAT(TMP))
                    TMP='' & WIDGET_Control, y2_label, GET_VALUE=TMP
                    TMP=TMP(0) & IF (TMP NE '') THEN yr2=FIX(FLOAT(TMP))
                    if keyword_set(fixx) then BEGIN
                        xr1=x1 & xr2=x2
                    END
                    if keyword_set(fixy) then BEGIN
                        yr1=y1 & yr2=y2
                    END
                    CheckBoxCoord,x1,x2,y1,y2,xr1,xr2,yr1,yr2,binning,MaxRadix, FixSq, MAXX=MaxX, MAXY=MaxY
                    ;; TVCRS, x1, y1
            	    plot_Box, x1, x2, y1, y2, COLOR=plcolor
                    WIDGET_Control, x1_label, SET_VALUE=MySTRING(xr1)
                    WIDGET_Control, x2_label, SET_VALUE=MySTRING(xr2)
                    WIDGET_Control, y1_label, SET_VALUE=MySTRING(yr1)
                    WIDGET_Control, y2_label, SET_VALUE=MySTRING(yr2)
                    WIDGET_Control, z_label, SET_VALUE=MySTRING(ABS(xr2-xr1+1))+','+MySTRING(ABS(yr2-yr1+1))
                  END
	'add': BEGIN
	            plot_Box, x1, x2, y1, y2, COLOR=plcolor
        	    device, cursor_standard=42
                    TMP='' & WIDGET_Control, x1_label, GET_VALUE=TMP
                    TMP=TMP(0) & IF (TMP NE '') THEN xr1=FIX(FLOAT(TMP))
                    TMP='' & WIDGET_Control, x2_label, GET_VALUE=TMP
                    TMP=TMP(0) & IF (TMP NE '') THEN xr2=FIX(FLOAT(TMP))
                    TMP='' & WIDGET_Control, y1_label, GET_VALUE=TMP
                    TMP=TMP(0) & IF (TMP NE '') THEN yr1=FIX(FLOAT(TMP))
                    TMP='' & WIDGET_Control, y2_label, GET_VALUE=TMP
                    TMP=TMP(0) & IF (TMP NE '') THEN yr2=FIX(FLOAT(TMP))
                    if keyword_set(fixx) then BEGIN
                        xr1=x1 & xr2=x2
                    END
                    if keyword_set(fixy) then BEGIN
                        yr1=y1 & yr2=y2
                    END
                    CheckBoxCoord,x1,x2,y1,y2,xr1,xr2,yr1,yr2,binning,MaxRadix, FixSq, MAXX=MaxX, MAXY=MaxY
                    ;; TVCRS, x1, y1
                    plot_Box, x1, x2, y1, y2, COLOR=plcolor
                    plot_Box, x1, x2, y1, y2, COLOR=plcolor
                    WIDGET_Control, x1_label, SET_VALUE=MySTRING(xr1)
                    WIDGET_Control, x2_label, SET_VALUE=MySTRING(xr2)
                    WIDGET_Control, y1_label, SET_VALUE=MySTRING(yr1)
                    WIDGET_Control, y2_label, SET_VALUE=MySTRING(yr2)
                    WIDGET_Control, z_label, SET_VALUE=MySTRING(ABS(xr2-xr1+1))+','+MySTRING(ABS(yr2-yr1+1))
                    if keyword_set(multiple) then multiple.add, [xr1,xr2,yr1,yr2]
                  END
	    ELSE: BEGIN
	            plot_Box, x1, x2, y1, y2, COLOR=plcolor
        	    device, cursor_standard=42
	            ;; create new coordinates
                    dx=0 & dy=0 & dw=0 & dh=0
	            CASE ev.value OF 
        	      'up': dy=1
                      'down': dy=-1
                      'left': dx=-1
                      'right': dx=1
                      'wplus': BEGIN
                         dw=1
                         IF (MaxRadix GT 1) THEN BEGIN
                            tmp=closestprimefactor(MaxRadix, xr2-xr1+2, /LARGER)
                            dw=tmp-(xr2-xr1+1)
                         END
                      END
                      'wminus': BEGIN
                         dw=-1
                         IF (MaxRadix GT 1) THEN BEGIN
                            tmp=closestprimefactor(MaxRadix, xr2-xr1)
                            dw=tmp-(xr2-xr1+1)
                         END
                      END
                      'hplus': BEGIN
                         dh=1
                         IF (MaxRadix GT 1) THEN BEGIN
                            tmp=closestprimefactor(MaxRadix, yr2-yr1+2, /LARGER)
                            dh=tmp-(yr2-yr1+1)
                         END
                      END
                      'hminus': BEGIN
                         dh=-1
                         IF (MaxRadix GT 1) THEN BEGIN
                            tmp=closestprimefactor(MaxRadix, yr2-yr1)
                            dh=tmp-(yr2-yr1+1)
                         END
                      END
                      ELSE:
                    ENDCASE
                    if keyword_set(fixx) then BEGIN
                        dx=0 & dw=0
                    END
                    if keyword_set(fixy) then BEGIN
                        dy=0 & dh=0
                    END
 	            xr1=xr1+dx & xr2=xr2+dx+dw
	            yr1=yr1+dy & yr2=yr2+dy+dh
                    CheckBoxCoord,x1,x2,y1,y2,xr1,xr2,yr1,yr2,binning,MaxRadix, FixSq, MAXX=MaxX, MAXY=MaxY
                    ;; TVCRS, x1, y1
            	    plot_Box, x1, x2, y1, y2, COLOR=plcolor
                    WIDGET_Control, x1_label, SET_VALUE=MySTRING(xr1)
                    WIDGET_Control, x2_label, SET_VALUE=MySTRING(xr2)
                    WIDGET_Control, y1_label, SET_VALUE=MySTRING(yr1)
                    WIDGET_Control, y2_label, SET_VALUE=MySTRING(yr2)
                    WIDGET_Control, z_label, SET_VALUE=MySTRING(ABS(xr2-xr1+1))+','+MySTRING(ABS(yr2-yr1+1))
                 END 
         endcase
      ENDIF
      MyWAIT, GetWaitTime()
   ENDREP UNTIL do_it_again
   
   ;; empty buffered mouse button clicks
   ClearMouseButton, mm_break

;;   if (sticky EQ 0) THEN BEGIN
   ;; remove box from screen
;;     plot_Box, x1, x2, y1, y2, COLOR=plcolor
;;   END   
   ;; CheckBoxCoord, x1, x2, y1, y2, xr1, xr2, yr1, yr2, binning, MaxRadix, FixSq, /MOUSE, MAXX=MaxX, MAXY=MaxY
   ;; plot_Box, x1, x2, y1, y2, COLOR=plcolor
   
   Widget_Control, base, /DESTROY
   device, set_graphics_function=gmode0  ; Restore mode.
   device, cursor_standard=2

   IF (xr1 GT xr2) THEN BEGIN
      tmp = xr1 &  xr1=xr2 &  xr2=tmp
   END

   IF (yr1 GT yr2) THEN BEGIN
      tmp = yr1 &  yr1=yr2 &  yr2=tmp
   END
                      
   print, "% XBox: (x1,x2,y1,y2)=(", MyString(x1), ",", MyString(x2), ",", MyString(y1), ",", MyString(y2), ")" 
   print, "% XBox: (xr1,xr2,yr1,yr2)=(", MyString(xr1), ",", MyString(xr2), ",", MyString(yr1), ",", MyString(yr2), ")"   
   xi1 = xr1
   xi2 = xr2
   yi1 = yr1
   yi2 = yr2
   print, "% XBox: (x1:x2,y1:y2)=(", MyString(xi1), ",", MyString(xi2), ",", MyString(yi1), ",", MyString(yi2), ")"
   ;; print, result

   XConsole_PopState
   return, result
END


Function GetROI, p, proj, roi, SLICE=slice, INTPOW2=intpow2, SQUARE=square, TITLE=title
  IF (GetDebugFlag()) THEN BEGIN
  END ELSE BEGIN
     CATCH, Error_status
     IF (Error_status NE 0) THEN BEGIN
        Print, "% GetROI:    Fatal error "
        Print, "%   Error status  - " + STRING(error_status)
        Print, "%   Error message - " + !ERROR_STATE.MSG
        ErrMsg, !ERROR_STATE.MSG
        CATCH, /Cancel
        return, 0
     END
  END
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     printtocon, "% GetROI: Root pointer is invalid" 
     return, 0
  END
  IF PTR_VALID(p) THEN BEGIN
     c=(*ptr).first
     match=0
     WHILE ((match EQ 0) AND (PTR_VALID(c))) DO BEGIN
        IF (c EQ p) then match=1 ELSE c=(*c).next
     END
     IF (match EQ 0) THEN BEGIN
        printtocon, "% GetROI: stack pointer is invalid" 
        return, 0
     END
     e=(*p).datap
     IF (NOT(PTR_VALID(e))) THEN BEGIN
        printtocon, "% GetROI: data pointer is invalid" 
        return, 0
     END
     (*ptr).current=c
     (*e).zcoord=proj
     binning=(*e).binning
     CASE proj OF
        1: BEGIN ;; yz
           dimx=(*e).SzY & dimy=(*e).SzZ
           IF Keyword_Set(slice) THEN (*e).slice=slice ELSE (*e).slice=FIX((*e).SzX/2)
           xr1=FIX((*e).SzY*1/4) & xr2=FIX((*e).SzY*3/4) & yr1=FIX((*e).SzZ*1/4) & yr2=FIX((*e).SzZ*3/4)
        END
        2: BEGIN ;; xz
           dimx=(*e).SzX & dimy=(*e).SzZ
           IF Keyword_Set(slice) THEN (*e).slice=slice ELSE (*e).slice=FIX((*e).SzY/2)
           xr1=FIX((*e).SzX*1/4) & xr2=FIX((*e).SzX*3/4) & yr1=FIX((*e).SzZ*1/4) & yr2=FIX((*e).SzZ*3/4)
        END
        3: BEGIN ;; xy
           dimx=(*e).SzX & dimy=(*e).SzY
           IF Keyword_Set(slice) THEN (*e).slice=slice ELSE (*e).slice=FIX((*e).SzZ/2)
           xr1=FIX((*e).SzX*1/4) & xr2=FIX((*e).SzX*3/4) & yr1=FIX((*e).SzY*1/4) & yr2=FIX((*e).SzY*3/4)
        END
        ELSE: BEGIN
           printtocon, "% GetROI: invalid projection" 
           return, 0
        END
     END
     CreateWindow
     TVDisplay
     Update_XTabControlInfoField
     result=XBox(xr1, yr1, xr2, yr2, bin=binning, SX=dimx, SY=dimy, INTPOW2=intpow2, SQUARE=square, TITLE=title)
     IF (result GT 0) THEN roi=[xr1,xr2,yr1,yr2]
     return, result 
  END
END


Function GetMultipleROI, p, proj, mask, SLICE=slice, INTPOW2=intpow2, SQUARE=square, ROIcoord=roicoord, TITLE=title
  IF (GetDebugFlag()) THEN BEGIN
  END ELSE BEGIN
     CATCH, Error_status
     IF (Error_status NE 0) THEN BEGIN
        Print, "% GetMultipleROI:    Fatal error "
        Print, "%   Error status  - " + STRING(error_status)
        Print, "%   Error message - " + !ERROR_STATE.MSG
        ErrMsg, !ERROR_STATE.MSG
        CATCH, /Cancel
        return, 0
     END
  END
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     printtocon, "% GetMultipleROI: Root pointer is invalid" 
     return, 0
  END
  IF PTR_VALID(p) THEN BEGIN
     c=(*ptr).first
     match=0
     WHILE ((match EQ 0) AND (PTR_VALID(c))) DO BEGIN
        IF (c EQ p) then match=1 ELSE c=(*c).next
     END
     IF (match EQ 0) THEN BEGIN
        printtocon, "% GetMultipleROI: stack pointer is invalid" 
        return, 0
     END
     e=(*p).datap
     IF (NOT(PTR_VALID(e))) THEN BEGIN
        printtocon, "% GetMultipleROI: data pointer is invalid" 
        return, 0
     END
     (*ptr).current=c
     (*e).zcoord=proj
     binning=(*e).binning
     CASE proj OF
        1: BEGIN ;; yz
           dimx=(*e).SzY & dimy=(*e).SzZ
           IF Keyword_Set(slice) THEN (*e).slice=slice ELSE (*e).slice=FIX((*e).SzX/2)
           xr1=FIX((*e).SzY*1/4) & xr2=FIX((*e).SzY*3/4) & yr1=FIX((*e).SzZ*1/4) & yr2=FIX((*e).SzZ*3/4)
        END
        2: BEGIN ;; xz
           dimx=(*e).SzX & dimy=(*e).SzZ
           IF Keyword_Set(slice) THEN (*e).slice=slice ELSE (*e).slice=FIX((*e).SzY/2)
           xr1=FIX((*e).SzX*1/4) & xr2=FIX((*e).SzX*3/4) & yr1=FIX((*e).SzZ*1/4) & yr2=FIX((*e).SzZ*3/4)
        END
        3: BEGIN ;; xy
           dimx=(*e).SzX & dimy=(*e).SzY
           IF Keyword_Set(slice) THEN (*e).slice=slice ELSE (*e).slice=FIX((*e).SzZ/2)
           xr1=FIX((*e).SzX*1/4) & xr2=FIX((*e).SzX*3/4) & yr1=FIX((*e).SzY*1/4) & yr2=FIX((*e).SzY*3/4)
        END
        ELSE: BEGIN
           printtocon, "% GetMultipleROI: invalid projection" 
           return, 0
        END
     END
     CreateWindow
     TVDisplay
     Update_XTabControlInfoField
     ;;
     ;; loop over Box Selection
     ;;
     select=' Yes '
     ;; create  a mask
     B=BYTARR(dimx,dimy)
     ;; 
     result=0
     s=PTR_NEW() & nroi=0
     ;;
     WHILE (select EQ ' Yes ') DO BEGIN
        result=XBox(xr1, yr1, xr2, yr2, bin=binning, SX=dimx, SY=dimy, TITLE="Multiple ROI Selection", INTPOW2=intpow2, SQUARE=square)
        IF (result GT 0) THEN BEGIN
;; add roi to region, set mask values to 1 
           B(xr1:xr2,yr1:yr2)=1
           IF keyword_Set(roicoord) THEN BEGIN
              ;; store roi coordinates in a stack
              IF PTR_VALID(s) THEN BEGIN
                 s=Push(s,[xr1,xr2,yr1,yr2])
                 nroi=nroi+1
              END ELSE BEGIN
                 s=CreateStack([xr1,xr2,yr1,yr2])
                 nroi=1
              END
           END
           select=Dialog_Info(["Do you want to add another ROI?"], /BREAKBUTTON, /QUESTION)
           IF (select EQ 'Break') THEN return, -1
        END
     END
     ;; 
     ;; Window, 1 & TVSCL, B
     IF (result GT 0) THEN mask=WHERE(B EQ 1)
     printtocon, "% GetMultipleROI: "+MySTRING(nroi)+" ROIs Selected"  
     IF keyword_Set(roicoord) THEN BEGIN
        roicoord=INTARR(4,nroi)
        roiind=0
        ;; store roi coordinates in a stack
        While PTR_VALID(s) Do BEGIN
           s=Pop(s,tmp)
           printtocon, "% GetMultipleROI: ROI #"+MySTRING(roiind)+" - ("+MySTRING(tmp[0])+","+MySTRING(tmp[1])+ ","+MySTRING(tmp[2])+","+MySTRING(tmp[3])+")"
           roicoord[*,roiind]=tmp 
           roiind=roiind+1 
        END
     END
     return, result 
  END
END



PRO ROISelection, ROI=roi
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     printtocon, "% ROISelection: Root pointer is invalid" 
     return
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% ROISelection: current stack pointer is invalid" 
     return
  END
  pp=(*c).datap
  IF (NOT(PTR_VALID(pp))) THEN BEGIN
     printtocon, "% GridSearchTiltAxisShift: current data pointer is invalid" 
     return
  END
  IF NOT(keyword_set(roi)) THEN BEGIN
     CASE (*pp).zcoord OF 
        1: roi=[(FIX((*pp).SzY*0.25)),(FIX((*pp).SzY*0.75)),(FIX((*pp).SzZ*0.25)),(FIX((*pp).SzZ*0.75))] 
        2: roi=[(FIX((*pp).SzX*0.25)),(FIX((*pp).SzX*0.75)),(FIX((*pp).SzZ*0.25)),(FIX((*pp).SzZ*0.75))]
        3: roi=[(FIX((*pp).SzX*0.25)),(FIX((*pp).SzX*0.75)),(FIX((*pp).SzY*0.25)),(FIX((*pp).SzY*0.75))]
        ELSE:  BEGIN
           printtocon, "% ROIselection: wrong projection - "+MyString((*pp).zcoord) 
           return
        END
     END 
  END  
  IF (GetROI(c,(*pp).zcoord,roi,SLICE=(*pp).slice) GT 0) THEN BEGIN
  END ELSE BEGIN
     printtocon, "% ROISelection: cancelled." 
     return
  END
END

FUNCTION XSliceBox, SX=MaxX, SY=MaxY, TITLE=title, INTPOW2=intpow2, SQUARE=square, FIXX=fixx, FIXY=fixy, SLICE=slice
;; xi1, yi1, xi2, yi2, bin=binning, SX=MaxX, SY=MaxY, TITLE=title, INTPOW2=intpow2, SQUARE=square, FIXX=fixx, FIXY=fixy, SLICE=slice
;; 
;; interactively plot a box on the screen
;; xi1, yi1, xi1, yi2 are real image coordinates
;; bin is the binning factor for the TV display window
;; MaxX and MaxY are the maximum image indices, 
;; i.e. (number of elements -1)
;;
;; the xr* are image coordinates, x* is a display coordinate 
;;
COMMON MOUSEBINDINGS, mm_move, mm_resize, mm_break
;;
  IF (GetDebugFlag()) THEN BEGIN
  END ELSE BEGIN
     CATCH, Error_status
     IF (Error_status NE 0) THEN BEGIN
        PrintToCon, "% XSliceBox:    Fatal error "
        PrintToCon, "%   Error status  - " + STRING(error_status)
        PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
        CATCH, /Cancel
        return, 0
     END
  END
  ptr=GetRootP()
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; data pointer
;; e = pointer to the tilt series list entry
;; (*e).data = pointer to the data array
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     printtocon, "% XSliceBox: Root pointer is invalid" 
     return, 0
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% XSliceBox: current stack pointer is invalid" 
     return, 0
  END
  e=(*c).datap
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% XSliceBox: current data pointer is invalid" 
     return, 0
  END
  ;; set initial coordinates
  CASE (*e).zcoord OF 
     1: BEGIN
        maxslice=(*e).SzX-1
        xi1=FIX(0.25*(*e).SzY) & xi2=FIX(0.75*(*e).SzY)
        yi1=FIX(0.25*(*e).SzZ) & yi2=FIX(0.75*(*e).SzZ)
     END
     2: BEGIN
        maxslice=(*e).SzY-1
        xi1=FIX(0.25*(*e).SzX) & xi2=FIX(0.75*(*e).SzX)
        yi1=FIX(0.25*(*e).SzZ) & yi2=FIX(0.75*(*e).SzZ)
     END
     3: BEGIN
        maxslice=(*e).SzZ-1
        xi1=FIX(0.25*(*e).SzX) & xi2=FIX(0.75*(*e).SzX)
        yi1=FIX(0.25*(*e).SzY) & yi2=FIX(0.75*(*e).SzY)
     END
     ELSE: BEGIN
        printtocon, "% XSliceBox: Current projection is invalid."
     END
  END 

  result=0                      ; return false by default
  oplotmode=9
  plcolor=100L

  If not(keyword_set(slice)) THEN slice=(*e).slice
   ;;
   ;; is there a window
   ;;
   IF (!D.WINDOW LT 0) THEN BEGIN
       printToCon, "% XBox: invalid window id"
       return, result
   END
   ;;
   ;; read the window dimensions
   ;;
   wsizex=!D.X_VSIZE & wsizey=!D.Y_VSIZE
   ;;
   binning=(*e).binning 
   x1 = xi1/binning
   y1 = yi1/binning
   ;;
   FixSq = 0
   if ((keyword_set(square))) THEN FixSq = 1
   FixIntPow2 = 0
   if ((keyword_set(intpow2))) THEN FixIntPow2 = 1
   ;;
   help_text = ["* Interactively define a rectangular box in the current window.", $
                "* mouse operations:","  left button - move centre", $
                "  middle button - resize by picking the lower left edge", $ 
                "  right button - accept the current settings and leave", $
                "* you can also edit the edge positions and press on 'apply'"]
   ;; 
   ;; the maximum array indices MaxX, MaxY
   ;;
   if (not (keyword_set(binning))) THEN binning=1 
   if (not (keyword_set(MaxX))) THEN MaxX=wsizex*binning-1
   if (not (keyword_set(MaxY))) THEN MaxY=wsizey*binning-1
   ;;
   ;; xr* are real world coordinates
   ;; xi* are temporary storage real world coordinates
   ;; x*  ere display coordinates
   ;; 
   xr1 = xi1 & x1 = xi1/binning
   xr2 = xi2 & x2 = xi2/binning
   yr1 = yi1 & y1 = yi1/binning
   yr2 = yi2 & y2 = yi2/binning
   ;;
   ;; check bounds and fix to integer power of 2 size if desired
   ;; 
   CheckBoxCoord, x1, x2, y1, y2, xr1, xr2, yr1, yr2, binning, FixIntPow2, FixSq, MAXX=MaxX, MAXY=MaxY
   ;;
   ;; create the widget window with the fancy buttons
   ;;
   if (not (keyword_set(title))) THEN title="Rectangle Dialog" 
   base = Widget_Base(TITLE=title, /COLUMN)
   row0 = WIDGET_BASE(base, /ROW, XPAD=10, Frame=0)
   slice_label = CW_Field(row0, TITLE='Slice', XSize=5, Value=MySTRING(slice))
   sliceb = CW_BGroup(row0, /ROW , [' previous ',' next '] , Button_UVALUE=['sliceminus', 'sliceplus'])
                                ;Create a row base to hold the input
                                ;fields for the four coordinate values
   row1 =  WIDGET_BASE(base, /ROW, XPAD=10, Frame=0)
   x1_label = CW_Field(row1, TITLE='X1', XSize=5, Value=MySTRING(xr1))
   x2_label = CW_Field(row1, TITLE='X2', XSize=5, Value=MySTRING(xr2))
   y1_label = CW_Field(row1, TITLE='Y1', XSize=5, Value=MySTRING(yr1))
   y2_label = CW_Field(row1, TITLE='Y2', XSize=5, Value=MySTRING(yr2))
                                ; create a second row to hold the
                                ; label fields for the width and the height

   mvbuttons = WIDGET_BASE(base, /ROW)
   mvb = CW_BGroup(mvbuttons, /ROW , [' ^ ',' v ',' < ',' > ',' < w > ',' > w < ',' < h > ',' > h < '] , Button_UVALUE=['up', 'down','left','right','wplus','wminus','hplus','hminus'])

   row2=Widget_Base(base, /ROW)
   my_label  = WIDGET_Label(row2, VALUE=' Current size: ')
   z_label = WIDGET_Label(row2, /Dynamic_Resize, Value=MySTRING(ABS(xr2-xr1+1))+' x '+MySTRING(ABS(yr2-yr1+1)))

                                ; create another row for the sticky flag
;;   row4 =  WIDGET_BASE(base, /ROW, XPAD=10)

;;    sticktext=Widget_Label(row4, VALUE='remove marker box when closing   ')
;;   togglebase = WIDGET_BASE(row4, /ROW, /EXCLUSIVE)
;;   stick_y = WIDGET_BUTTON(togglebase, VALUE='yes', UVALUE='stick_y', /NO_RELEASE, TOOLTIP="keep marker rectangle after leaving the dialog")
;;   stick_n = WIDGET_BUTTON(togglebase, VALUE='no', UVALUE='stick_n', /NO_RELEASE, TOOLTIP="remove marker rectangle after leaving the dialog")
                                ; create another row for the fixIntPow flag
   row5 =  WIDGET_BASE(base, /ROW, XPAD=10)

   IntPowtext=Widget_Label(row5, VALUE='fix widths to integer power of 2 ')
   toggleIP = WIDGET_BASE(row5, /ROW, /EXCLUSIVE)
   IP_y = WIDGET_BUTTON(toggleIP, VALUE='yes', UVALUE='IP_y', /NO_RELEASE, TOOLTIP="adjust size to 2^k x 2^m")
   IP_n = WIDGET_BUTTON(toggleIP, VALUE='no', UVALUE='IP_n', /NO_RELEASE, TOOLTIP="freely adjustable size")
   row5b =  WIDGET_BASE(base, /ROW, XPAD=10)

   IntPowtext=Widget_Label(row5b, VALUE='fix to square width             ')
   toggleSQ = WIDGET_BASE(row5b, /ROW, /EXCLUSIVE)
   SQ_y = WIDGET_BUTTON(toggleSQ, VALUE='yes', UVALUE='SQ_y', /NO_RELEASE, TOOLTIP="adjust to square size")
   SQ_n = WIDGET_BUTTON(toggleSQ, VALUE='no', UVALUE='SQ_n', /NO_RELEASE, TOOLTIP="adjustable size")

                                ; create another row to hold the
                                ; single step  shift buttons
                                ; create another row to hold the buttons
   row3a =  WIDGET_BASE(base, /ROW, XPAD=10)
   ba = CW_BGroup(row3a, /ROW , ['  Add to list  ','  Copy to previous  ', '  Copy to following  '] , Button_UVALUE=['add','cprev','cnext'])

   row3 =  WIDGET_BASE(base, /ROW, XPAD=10)
   b = CW_BGroup(row3, /ROW , ['  Accept  ','  Apply  ','   Help   ','  Cancel  '] , Button_UVALUE=['close','apply','help','cancel'])
   ;; bring up the widget

  widpos=PlaceWidget(base, POSKEY="cr")
 WIDGET_CONTROL, base, XOFFSET=widpos[0], YOFFSET=widpos[1], /REALIZE

   ;;
   ;;
   ;;
   device, get_graphics_function=gmode0  ; store the current graphics mode.
   device, set_graphics_function=oplotmode       ; XOR mode.
   plot_Box, x1, x2, y1, y2, COLOR=plcolor
   x0= FIX((x1+x2)/2)
   y0= FIX((y1+y2)/2)
   do_it_again = 0 ;; & sticky=1
   
   ;; WIDGET_CONTROL, stick_y, /SET_BUTTON
   IF (KEYWORD_SET(intpow2)) THEN WIDGET_CONTROL, IP_y, /SET_BUTTON ELSE WIDGET_CONTROL, IP_n, /SET_BUTTON
   IF (KEYWORD_SET(square)) THEN WIDGET_CONTROL, SQ_y, /SET_BUTTON ELSE WIDGET_CONTROL, SQ_n, /SET_BUTTON

   ;; bring current window to top
   WSHOW, !D.WINDOW
   ;; 
   result=INTARR((maxslice+1),5) ;; result[n,0]=-1 or slice -> slice n has no or a valid box entry 
   ;; result[n,1:4]=X1,X2,Y1,Y2 -> box for slice n
   result[*,0]=-1
   REPEAT BEGIN
      ;; get new cursor pos
      cursor, ix, iy, 1, /dev, /NOWAIT
      ;; now check whether the coordinates ix, iy are inside the
      ;; window, workaround for a *silly* Windows event bug 
      ;; 
      IF (((ix GE 0) OR (iy GE 0)) AND (ix LT wsizex) AND (iy LT wsizey)) THEN BEGIN
          IF (!mouse.button eq mm_move) THEN BEGIN
              ;; move box 
              IF ((ix NE x0) AND (iy NE y0)) THEN BEGIN
                  ;; overplot old box
                  plot_Box, x1, x2, y1, y2, COLOR=plcolor
                  device, cursor_standard=45
                  x0= FIX((x1+x2)/2) ;; old centre x
                  if NOT(keyword_set(fixx)) THEN BEGIN
                      dx=x1-x0
                      x0=ix
                      x1=x0+dx 
                      x2=x0-dx
                  END
                  y0= FIX((y1+y2)/2) ;; old center y
                  if NOT(keyword_set(fixy)) THEN BEGIN
                      dy=y1-y0
                      y0=iy
                      y1=y0+dy 
                      y2=y0-dy
                  END
                  TVCRS, x0, y0
                  CheckBoxCoord, x1, x2, y1, y2, xr1, xr2, yr1, yr2, binning, FixIntPow2, FixSq, /MOUSE, MAXX=MaxX, MAXY=MaxY
                  plot_Box, x1, x2, y1, y2, COLOR=plcolor
                  WIDGET_Control, x1_label, SET_VALUE=MySTRING(xr1)
                  WIDGET_Control, x2_label, SET_VALUE=MySTRING(xr2)
                  WIDGET_Control, y1_label, SET_VALUE=MySTRING(yr1)
                  WIDGET_Control, y2_label, SET_VALUE=MySTRING(yr2)
                  WIDGET_Control, z_label, SET_VALUE=MySTRING(ABS(xr2-xr1+1))+' x '+MySTRING(ABS(yr2-yr1+1))
              ENDIF 
          ENDIF
          IF (!mouse.button eq mm_resize) THEN BEGIN
              ;; change size !
              IF ((ix NE x1) AND (iy NE y1)) THEN BEGIN
                  ;; overplot old box
                  plot_Box, x1, x2, y1, y2, COLOR=plcolor
                  device, cursor_standard=42
                  ;; new edge coordinates
                  if NOT(keyword_set(fixx)) then BEGIN
                      x0= FIX((x1+x2)/2)
                      x1=ix
                      x2=2*x0-x1
                  END
                  if NOT(keyword_set(fixy)) then BEGIN
                      y0= FIX((y1+y2)/2)
                      y1=iy
                      y2=2*y0-y1 
                  END
                  CheckBoxCoord,x1,x2,y1,y2,xr1,xr2,yr1,yr2,binning,FixIntPow2, FixSq, /MOUSE, MAXX=MaxX, MAXY=MaxY
                  TVCRS, x1, y1
                  plot_Box, x1, x2, y1, y2, COLOR=plcolor
                  xr1=x1*binning & xr2=x2*binning
                  yr1=y1*binning & yr2=y2*binning 
                  WIDGET_Control, x1_label, SET_VALUE=MySTRING(xr1)
                  WIDGET_Control, x2_label, SET_VALUE=MySTRING(xr2)
                  WIDGET_Control, y1_label, SET_VALUE=MySTRING(yr1)
                  WIDGET_Control, y2_label, SET_VALUE=MySTRING(yr2)
                  WIDGET_Control, z_label, SET_VALUE=MySTRING(ABS(xr2-xr1+1))+' x '+MySTRING(ABS(yr2-yr1+1))
              ENDIF 
              ClearMouseButton, mm_resize
          ENDIF
          IF (!mouse.button EQ mm_break) THEN  BEGIN
             ;; right mouse button : record region and proceed with
             ;; next slice
	            plot_Box, x1, x2, y1, y2, COLOR=plcolor
        	    device, cursor_standard=42
                    TMP='' & WIDGET_Control, x1_label, GET_VALUE=TMP
                    TMP=TMP(0) & IF (TMP NE '') THEN xr1=FIX(FLOAT(TMP))
                    TMP='' & WIDGET_Control, x2_label, GET_VALUE=TMP
                    TMP=TMP(0) & IF (TMP NE '') THEN xr2=FIX(FLOAT(TMP))
                    TMP='' & WIDGET_Control, y1_label, GET_VALUE=TMP
                    TMP=TMP(0) & IF (TMP NE '') THEN yr1=FIX(FLOAT(TMP))
                    TMP='' & WIDGET_Control, y2_label, GET_VALUE=TMP
                    TMP=TMP(0) & IF (TMP NE '') THEN yr2=FIX(FLOAT(TMP))
                    if keyword_set(fixx) then BEGIN
                        xr1=x1 & xr2=x2
                    END
                    if keyword_set(fixy) then BEGIN
                        yr1=y1 & yr2=y2
                    END
                    CheckBoxCoord,x1,x2,y1,y2,xr1,xr2,yr1,yr2,binning,FixIntPow2, FixSq, MAXX=MaxX, MAXY=MaxY
                    TVCRS, x1, y1
            	    plot_Box, x1, x2, y1, y2, COLOR=plcolor
                    WIDGET_Control, x1_label, SET_VALUE=MySTRING(xr1)
                    WIDGET_Control, x2_label, SET_VALUE=MySTRING(xr2)
                    WIDGET_Control, y1_label, SET_VALUE=MySTRING(yr1)
                    WIDGET_Control, y2_label, SET_VALUE=MySTRING(yr2)
                    WIDGET_Control, z_label, SET_VALUE=MySTRING(ABS(xr2-xr1+1))+' x '+MySTRING(ABS(yr2-yr1+1))
                    IF (xr1 GT xr2) THEN BEGIN
                       tmp = xr1 &  xr1=xr2 &  xr2=tmp
                    END
                    
                    IF (yr1 GT yr2) THEN BEGIN
                       tmp = yr1 &  yr1=yr2 &  yr2=tmp
                    END
   
                    xi1 = FIX(xr1)
                    xi2 = FIX(xr2)
                    yi1 = FIX(yr1)
                    yi2 = FIX(yr2)
                    printtocon, "% XSliceBox: (n;x1,x2,y1,y2)=(" + MyString(slice) + ";" + MyString(xi1) + "," + MyString(xi2) + "," + MyString(yi1) + "," + MyString(yi2) + ")"
                    result[slice,0:4]=[slice,xi1,xi2,yi1,yi2]
                    IF (slice LT maxslice) THEN BEGIN
                       slice=slice+1
                       (*e).slice=slice
                       device, set_graphics_function=gmode0 ; Restore mode.
                       device, cursor_standard=2
                       TVDisplay
                       Update_XTabControl
                       device, set_graphics_function=oplotmode ; xor mode.
                       plot_Box, x1, x2, y1, y2, COLOR=plcolor
                       WIDGET_Control, slice_label, SET_VALUE=MySTRING(slice)
                    END
                    !mouse.button=-1
                    ;; flush all mouse events                   
                    ClearMouseButton, mm_break, DELAY=0.2
                 END 
      END

      ev = WIDGET_Event(base, /NoWait)
      
      ;;if ((ev.id EQ stick_n) or (ev.id EQ stick_y)) THEN BEGIN
      ;;   uv=""
      ;;   WIDGET_CONTROL, ev.id, GET_UVALUE = uv
      ;;   CASE uv OF
      ;;   'stick_y': sticky=1
      ;;   'stick_n': sticky=0
      ;;   ELSE:
      ;;   ENDCASE
      ;;END

      if ((ev.id EQ IP_n) or (ev.id EQ IP_y) or (ev.id EQ SQ_n) or (ev.id EQ SQ_y)) THEN BEGIN
         uv=""
         WIDGET_CONTROL, ev.id, GET_UVALUE = uv
         CASE uv OF
         'IP_y': FixIntPow2=1
         'IP_n': FixIntPow2=0
         'SQ_y': FixSq=1
         'SQ_n': FixSq=0
         ELSE:
         ENDCASE
      END


      IF ((ev.id EQ b) or (ev.id EQ mvb) or (ev.id EQ sliceb) or (ev.id EQ ba)) THEN BEGIN                ;; Button-Menu
         CASE ev.value OF
            'help': BEGIN
               msg = dialog_info(help_text, /HELP)
            END
            'close': BEGIN
               do_it_again = NOT(do_it_again)
               B=WHERE(result[*,0] GE 0)
               tmp=result[B,*]
               result=tmp
            END
            'cancel': BEGIN
               do_it_again = NOT(do_it_again)
               result=0
            END
            'add': BEGIN
	            plot_Box, x1, x2, y1, y2, COLOR=plcolor
        	    device, cursor_standard=42
                    TMP='' & WIDGET_Control, x1_label, GET_VALUE=TMP
                    TMP=TMP(0) & IF (TMP NE '') THEN xr1=FIX(FLOAT(TMP))
                    TMP='' & WIDGET_Control, x2_label, GET_VALUE=TMP
                    TMP=TMP(0) & IF (TMP NE '') THEN xr2=FIX(FLOAT(TMP))
                    TMP='' & WIDGET_Control, y1_label, GET_VALUE=TMP
                    TMP=TMP(0) & IF (TMP NE '') THEN yr1=FIX(FLOAT(TMP))
                    TMP='' & WIDGET_Control, y2_label, GET_VALUE=TMP
                    TMP=TMP(0) & IF (TMP NE '') THEN yr2=FIX(FLOAT(TMP))
                    if keyword_set(fixx) then BEGIN
                        xr1=x1 & xr2=x2
                    END
                    if keyword_set(fixy) then BEGIN
                        yr1=y1 & yr2=y2
                    END
                    CheckBoxCoord,x1,x2,y1,y2,xr1,xr2,yr1,yr2,binning,FixIntPow2, FixSq, MAXX=MaxX, MAXY=MaxY
                    TVCRS, x1, y1
            	    plot_Box, x1, x2, y1, y2, COLOR=plcolor
                    WIDGET_Control, x1_label, SET_VALUE=MySTRING(xr1)
                    WIDGET_Control, x2_label, SET_VALUE=MySTRING(xr2)
                    WIDGET_Control, y1_label, SET_VALUE=MySTRING(yr1)
                    WIDGET_Control, y2_label, SET_VALUE=MySTRING(yr2)
                    WIDGET_Control, z_label, SET_VALUE=MySTRING(ABS(xr2-xr1+1))+' x '+MySTRING(ABS(yr2-yr1+1))
                    IF (xr1 GT xr2) THEN BEGIN
                       tmp = xr1 &  xr1=xr2 &  xr2=tmp
                    END
                    
                    IF (yr1 GT yr2) THEN BEGIN
                       tmp = yr1 &  yr1=yr2 &  yr2=tmp
                    END
   
                    xi1 = FIX(xr1)
                    xi2 = FIX(xr2)
                    yi1 = FIX(yr1)
                    yi2 = FIX(yr2)
                    printtocon, "% XSliceBox: (n;x1,x2,y1,y2)=(" + MyString(slice) + ";" + MyString(xi1) + "," + MyString(xi2) + "," + MyString(yi1) + "," + MyString(yi2) + ")"
                    result[slice,0:4]=[slice,xi1,xi2,yi1,yi2]
                 END
            'cnext': BEGIN
	            plot_Box, x1, x2, y1, y2, COLOR=plcolor
        	    device, cursor_standard=42
                    TMP='' & WIDGET_Control, x1_label, GET_VALUE=TMP
                    TMP=TMP(0) & IF (TMP NE '') THEN xr1=FIX(FLOAT(TMP))
                    TMP='' & WIDGET_Control, x2_label, GET_VALUE=TMP
                    TMP=TMP(0) & IF (TMP NE '') THEN xr2=FIX(FLOAT(TMP))
                    TMP='' & WIDGET_Control, y1_label, GET_VALUE=TMP
                    TMP=TMP(0) & IF (TMP NE '') THEN yr1=FIX(FLOAT(TMP))
                    TMP='' & WIDGET_Control, y2_label, GET_VALUE=TMP
                    TMP=TMP(0) & IF (TMP NE '') THEN yr2=FIX(FLOAT(TMP))
                    if keyword_set(fixx) then BEGIN
                        xr1=x1 & xr2=x2
                    END
                    if keyword_set(fixy) then BEGIN
                        yr1=y1 & yr2=y2
                    END
                    CheckBoxCoord,x1,x2,y1,y2,xr1,xr2,yr1,yr2,binning,FixIntPow2, FixSq, MAXX=MaxX, MAXY=MaxY
                    TVCRS, x1, y1
            	    plot_Box, x1, x2, y1, y2, COLOR=plcolor
                    WIDGET_Control, x1_label, SET_VALUE=MySTRING(xr1)
                    WIDGET_Control, x2_label, SET_VALUE=MySTRING(xr2)
                    WIDGET_Control, y1_label, SET_VALUE=MySTRING(yr1)
                    WIDGET_Control, y2_label, SET_VALUE=MySTRING(yr2)
                    WIDGET_Control, z_label, SET_VALUE=MySTRING(ABS(xr2-xr1+1))+' x '+MySTRING(ABS(yr2-yr1+1))
                    IF (xr1 GT xr2) THEN BEGIN
                       tmp = xr1 &  xr1=xr2 &  xr2=tmp
                    END
                    
                    IF (yr1 GT yr2) THEN BEGIN
                       tmp = yr1 &  yr1=yr2 &  yr2=tmp
                    END
   
                    xi1 = FIX(xr1)
                    xi2 = FIX(xr2)
                    yi1 = FIX(yr1)
                    yi2 = FIX(yr2)
                    printtocon, "% XSliceBox: (n;x1,x2,y1,y2)=(" + MyString(slice) +":"+ MyString(maxslice-1)+ ";" + MyString(xi1) + "," + MyString(xi2) + "," + MyString(yi1) + "," + MyString(yi2) + ")"
                    for ii=slice,maxslice Do BEGIN

                    result[ii,0:4]=[ii,xi1,xi2,yi1,yi2]
                 END
                 END
            'cprev': BEGIN
	            plot_Box, x1, x2, y1, y2, COLOR=plcolor
        	    device, cursor_standard=42
                    TMP='' & WIDGET_Control, x1_label, GET_VALUE=TMP
                    TMP=TMP(0) & IF (TMP NE '') THEN xr1=FIX(FLOAT(TMP))
                    TMP='' & WIDGET_Control, x2_label, GET_VALUE=TMP
                    TMP=TMP(0) & IF (TMP NE '') THEN xr2=FIX(FLOAT(TMP))
                    TMP='' & WIDGET_Control, y1_label, GET_VALUE=TMP
                    TMP=TMP(0) & IF (TMP NE '') THEN yr1=FIX(FLOAT(TMP))
                    TMP='' & WIDGET_Control, y2_label, GET_VALUE=TMP
                    TMP=TMP(0) & IF (TMP NE '') THEN yr2=FIX(FLOAT(TMP))
                    if keyword_set(fixx) then BEGIN
                        xr1=x1 & xr2=x2
                    END
                    if keyword_set(fixy) then BEGIN
                        yr1=y1 & yr2=y2
                    END
                    CheckBoxCoord,x1,x2,y1,y2,xr1,xr2,yr1,yr2,binning,FixIntPow2, FixSq, MAXX=MaxX, MAXY=MaxY
                    TVCRS, x1, y1
            	    plot_Box, x1, x2, y1, y2, COLOR=plcolor
                    WIDGET_Control, x1_label, SET_VALUE=MySTRING(xr1)
                    WIDGET_Control, x2_label, SET_VALUE=MySTRING(xr2)
                    WIDGET_Control, y1_label, SET_VALUE=MySTRING(yr1)
                    WIDGET_Control, y2_label, SET_VALUE=MySTRING(yr2)
                    WIDGET_Control, z_label, SET_VALUE=MySTRING(ABS(xr2-xr1+1))+' x '+MySTRING(ABS(yr2-yr1+1))
                    IF (xr1 GT xr2) THEN BEGIN
                       tmp = xr1 &  xr1=xr2 &  xr2=tmp
                    END
                    
                    IF (yr1 GT yr2) THEN BEGIN
                       tmp = yr1 &  yr1=yr2 &  yr2=tmp
                    END
   
                    xi1 = FIX(xr1)
                    xi2 = FIX(xr2)
                    yi1 = FIX(yr1)
                    yi2 = FIX(yr2)
                    printtocon, "% XSliceBox: (n;x1,x2,y1,y2)=(0:"+ MyString(slice)+ ";" + MyString(xi1) + "," + MyString(xi2) + "," + MyString(yi1) + "," + MyString(yi2) + ")"
                    for ii=0,slice Do BEGIN

                    result[ii,0:4]=[ii,xi1,xi2,yi1,yi2]
                 END
                 END
            'apply': BEGIN
	            plot_Box, x1, x2, y1, y2, COLOR=plcolor
        	    device, cursor_standard=42
                    TMP='' & WIDGET_Control, x1_label, GET_VALUE=TMP
                    TMP=TMP(0) & IF (TMP NE '') THEN xr1=FIX(FLOAT(TMP))
                    TMP='' & WIDGET_Control, x2_label, GET_VALUE=TMP
                    TMP=TMP(0) & IF (TMP NE '') THEN xr2=FIX(FLOAT(TMP))
                    TMP='' & WIDGET_Control, y1_label, GET_VALUE=TMP
                    TMP=TMP(0) & IF (TMP NE '') THEN yr1=FIX(FLOAT(TMP))
                    TMP='' & WIDGET_Control, y2_label, GET_VALUE=TMP
                    TMP=TMP(0) & IF (TMP NE '') THEN yr2=FIX(FLOAT(TMP))
                    if keyword_set(fixx) then BEGIN
                        xr1=x1 & xr2=x2
                    END
                    if keyword_set(fixy) then BEGIN
                        yr1=y1 & yr2=y2
                    END
                    CheckBoxCoord,x1,x2,y1,y2,xr1,xr2,yr1,yr2,binning,FixIntPow2, FixSq, MAXX=MaxX, MAXY=MaxY
                    TVCRS, x1, y1
            	    plot_Box, x1, x2, y1, y2, COLOR=plcolor
                    WIDGET_Control, x1_label, SET_VALUE=MySTRING(xr1)
                    WIDGET_Control, x2_label, SET_VALUE=MySTRING(xr2)
                    WIDGET_Control, y1_label, SET_VALUE=MySTRING(yr1)
                    WIDGET_Control, y2_label, SET_VALUE=MySTRING(yr2)
                    WIDGET_Control, z_label, SET_VALUE=MySTRING(ABS(xr2-xr1+1))+' x '+MySTRING(ABS(yr2-yr1+1))
                    IF (xr1 GT xr2) THEN BEGIN
                       tmp = xr1 &  xr1=xr2 &  xr2=tmp
                    END
                    
                    IF (yr1 GT yr2) THEN BEGIN
                       tmp = yr1 &  yr1=yr2 &  yr2=tmp
                    END
   
                    xi1 = FIX(xr1)
                    xi2 = FIX(xr2)
                    yi1 = FIX(yr1)
                    yi2 = FIX(yr2)
                  END
            'sliceminus': BEGIN
               ;; Display previous slice
               IF (slice GT 0) THEN BEGIN
                  slice=slice-1
                  (*e).slice=slice
                  device, set_graphics_function=gmode0 ; Restore mode.
                  device, cursor_standard=2
                  TVDisplay
                  Update_XTabControl
                  device, set_graphics_function=oplotmode ; xor mode.
                  ;; check whether box is already defined
                  IF NOT((result[slice,1] EQ 0) AND (result[slice,2] EQ 0) AND (result[slice,3] EQ 0) AND (result[slice,4] EQ 0)) THEN BEGIN
                     x1=FIX(FLOAT(result[slice,1])/binning) & x2=FIX(FLOAT(result[slice,2])/binning)
                     y1=FIX(FLOAT(result[slice,3])/binning) & y2=FIX(FLOAT(result[slice,4])/binning)
                  END
                  plot_Box, x1, x2, y1, y2, COLOR=plcolor
                  WIDGET_Control, slice_label, SET_VALUE=MySTRING(slice)
               END
            END
            'sliceplus': BEGIN
               IF (slice LT maxslice) THEN BEGIN
                  slice=slice+1
                  (*e).slice=slice
                  device, set_graphics_function=gmode0 ; Restore mode.
                  device, cursor_standard=2
                  TVDisplay
                  Update_XTabControl
                  device, set_graphics_function=oplotmode ; xor mode.
                  ;; check whether box is already defined
                  IF NOT((result[slice,1] EQ 0) AND (result[slice,2] EQ 0) AND (result[slice,3] EQ 0) AND (result[slice,4] EQ 0)) THEN BEGIN
                     x1=FIX(FLOAT(result[slice,1])/binning) & x2=FIX(FLOAT(result[slice,2])/binning)
                     y1=FIX(FLOAT(result[slice,3])/binning) & y2=FIX(FLOAT(result[slice,4])/binning)
                  END
                  ;;
                  plot_Box, x1, x2, y1, y2, COLOR=plcolor
                  WIDGET_Control, slice_label, SET_VALUE=MySTRING(slice)
               END
            END
	    ELSE: BEGIN
	            plot_Box, x1, x2, y1, y2, COLOR=plcolor
        	    device, cursor_standard=42
	            ;; create new coordinates
                    dx=0 & dy=0 & dw=0 & dh=0
	            CASE ev.value OF 
        	      'up': dy=1
                      'down': dy=-1
                      'left': dx=-1
                      'right': dx=1
                      'wplus': dw=1
                      'wminus': dw=-1
                      'hplus': dh=1
                      'hminus': dh=-1
                      ELSE:
                    ENDCASE
                    if keyword_set(fixx) then BEGIN
                        dx=0 & dw=0
                    END
                    if keyword_set(fixy) then BEGIN
                        dy=0 & dh=0
                    END
 	            xr1=xr1+dx & xr2=xr2+dx+dw
	            yr1=yr1+dy & yr2=yr2+dy+dh
                    CheckBoxCoord,x1,x2,y1,y2,xr1,xr2,yr1,yr2,binning,FixIntPow2, FixSq, MAXX=MaxX, MAXY=MaxY
                    ;; TVCRS, x1, y1
            	    plot_Box, x1, x2, y1, y2, COLOR=plcolor
                    WIDGET_Control, x1_label, SET_VALUE=MySTRING(xr1)
                    WIDGET_Control, x2_label, SET_VALUE=MySTRING(xr2)
                    WIDGET_Control, y1_label, SET_VALUE=MySTRING(yr1)
                    WIDGET_Control, y2_label, SET_VALUE=MySTRING(yr2)
                    WIDGET_Control, z_label, SET_VALUE=MySTRING(ABS(xr2-xr1+1))+' x '+MySTRING(ABS(yr2-yr1+1))
	          END
         endcase
      ENDIF
   ENDREP UNTIL do_it_again
   
   ;; empty buffered mouse button clicks
   ClearMouseButton, mm_break

   ;;if (sticky EQ 0) THEN BEGIN
   ;; remove box from screen
   ;;  plot_Box, x1, x2, y1, y2, COLOR=plcolor
   ;;END   

   
   Widget_Control, base, /DESTROY
   device, set_graphics_function=gmode0  ; Restore mode.
   device, cursor_standard=2

  
   ;; print, result

   return, result
END


PRO GetListOfROI
  
END
