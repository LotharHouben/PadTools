
FUNCTION XActionBox, viewerp,  xi1, yi1, xi2, yi2, bin=binning, SX=MaxX, SY=MaxY, TITLE=title, INTPOW2=intpow2, SQUARE=square, FIXX=fixx, FIXY=fixy, HELP=hlp, NOMOVE=nomove, ACTIONFUNC=actionfunc, FUNCARGS=funcargs, AUTOFILESAVE=autofilesave, LOG=log, SCALEBAR=scalebar, ROTEMPAD=rotempad
;; 
;; interactively plot a box on the screen
;; xi1, yi1, xi1, yi2 are real image coordinates
;; bin is the binning factor for the TV display window
;; MaxX and MaxY are the maximum image indices, 
;; i.e. (number of elements -1)
;;
;; the xr* are image coordinates, x* is a display coordinate
;;   
;;
;; viewerp: Pointer to the image that will be used for the inteactive
;; ROI selection
;; ACTIONFUNC: Function to be called whenever the action button is
;;             pressed
;;             The function ACTIONFUNC should follow the template
;;             FUNCTION ACTIONFUNC, FUNCARG=funcarg, ROI=roi
;;             roi will be passed by this function  
;; FUNCARGS:  A hash of function arguments, to be transferred by the
;;           calling routine
;;           
;;  
COMMON MOUSEBINDINGS, mm_move, mm_resize, mm_break
;;


IF NOT(GetDebugFlag()) THEN BEGIN 
   CATCH, Error_status
   IF (Error_status NE 0) THEN BEGIN
      PrintToCon, "% XActionBox:    Fatal error "
      PrintToCon, "%   Error status  - " + STRING(error_status)
      PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
      CATCH, /Cancel
      XConsole_PopState
      return, 0
   END
END

;; check that viewerp is set
IF NOT(PTR_VALID(viewerp)) THEN BEGIN
   PrintToCon, "% XActionBox:    Fatal error, View image image pointer is not valid."
   return, 0
END

datap=(*viewerp).datap
If NOT(PTR_VALID(datap)) THEN BEGIN
   PrintToCon, "% XActionBox:    Fatal error, View image data pointer is not valid."
   return, 0
END
;;
XConsole_PushState
XConsole_WaitingForInput
result=0                        ; return false by default
oplotmode=9
plcolor=100L

WSET, (*datap).Window
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
   print, "% XBox: Display (x1,x2,y1,y2)=(", MyString(x1), ",", MyString(x2), ",", MyString(y1), ",", MyString(y2), ")" 
   print, "% XBox: Data (xr1,xr2,yr1,yr2)=(", MyString(xr1), ",", MyString(xr2), ",", MyString(yr1), ",", MyString(yr2), ")" 
   CheckBoxCoord, x1, x2, y1, y2, xr1, xr2, yr1, yr2, binning, MaxRadix, FixSq, MAXX=MaxX, MAXY=MaxY
   print, "% XBox: Display (x1,x2,y1,y2)=(", MyString(x1), ",", MyString(x2), ",", MyString(y1), ",", MyString(y2), ")" 
   print, "% XBox: Data (xr1,xr2,yr1,yr2)=(", MyString(xr1), ",", MyString(xr2), ",", MyString(yr1), ",", MyString(yr2), ")"
   ;;
   ;; create the widget window with the fancy buttons
   ;;
   if (not (keyword_set(title))) THEN title="Rectangle Dialog" 
   base = Widget_Base(TITLE=title, /COLUMN)
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
;; checkboxes
   
   checkbxs =  WIDGET_BASE(base, /ROW, XPAD=10, /NONEXCLUSIVE)

  
   SQ_y = WIDGET_BUTTON(checkbxs, VALUE='Square Size', UVALUE='SQ_y', TOOLTIP="adjust to square size")
   if FixSq then Widget_Control, SQ_y, Set_Button=1 ELSE  Widget_Control, SQ_y, Set_Button=0

;; live update checkbox
   liveupdate=1
   LU_y = WIDGET_BUTTON(checkbxs, VALUE='Live Update', UVALUE='LU_y', TOOLTIP="live update")
   if liveupdate then Widget_Control, LU_y, Set_Button=1 ELSE  Widget_Control, LU_y, Set_Button=0
;; autofilesave checkbox
   IF NOT keyword_set(autofilesave) THEN autofilesave=0 ELSE autofilesave=1
   AS_y = WIDGET_BUTTON(checkbxs, VALUE='Autosave', UVALUE='AS_y', TOOLTIP="autosave to file")
   if autofilesave then Widget_Control, AS_y, Set_Button=1 ELSE  Widget_Control, AS_y, Set_Button=0
;; log scaling checkbox
   IF NOT(keyword_set(log)) THEN log=0 ELSE log=1
   LOG_y = WIDGET_BUTTON(checkbxs, VALUE='Log Scale', UVALUE='LOG_y', TOOLTIP="display log data")

   if log then Widget_Control, LOG_y, Set_Button=1 ELSE  Widget_Control, LOG_y, Set_Button=0
checkbxs2 =  WIDGET_BASE(base, /ROW, XPAD=10, /NONEXCLUSIVE)
   ;; scale bar checkbox
   IF NOT(keyword_set(scalebar)) THEN scalebar=0 ELSE scalebar=1
   SB_y = WIDGET_BUTTON(checkbxs2, VALUE='Scale Bar', UVALUE='SB_y', TOOLTIP="display scale bar")

   if scalebar then Widget_Control, SB_y, Set_Button=1 ELSE  Widget_Control, SB_y, Set_Button=0
 
   ;; rotate diffraction checkbox
   IF NOT(keyword_set(rotempad)) THEN rotempad=0 ELSE rotempad=1
   RE_y = WIDGET_BUTTON(checkbxs2, VALUE='Compensate Rotation', UVALUE='RE_y', TOOLTIP="compensate for diffraction rotation")

   if rotempad then Widget_Control, RE_y, Set_Button=1 ELSE  Widget_Control, RE_y, Set_Button=0

;;; some space 


   
   buffer_row= WIDGET_BASE(base, /ROW)
   label=Widget_LABEL(buffer_row, VALUE="")
;; pixel shift buttons
   mvbuttons = WIDGET_BASE(base, /ROW, XPAD=10)
   label=Widget_LABEL(mvbuttons, VALUE="Position: ")
   mvb = CW_BGroup(mvbuttons, /ROW , ['   ^   ','   v   ','   <   ','   >   '] , Button_UVALUE=['up', 'down','left','right'])
;; scaling buttons
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
   b = CW_BGroup(row3, /ROW , ['  Store  ','  Update Preview  ','   Help   ','  Cancel  '] , Button_UVALUE=['accept','apply','help','cancel'])
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


   ;; bring current window to top
   WSHOW, !D.WINDOW
   ;; store old position, will be updated in the llop to track a
   ;; change
   eps=1E-6
   posold=FIX([xr1, yr1, xr2, yr2])

   h=CurrentlyManagedWidgets()
   
   Repeat BEGIN
      

      ;; get new cursor pos
      cursor, ix, iy, 1, /dev, /NOWAIT
      ;; now check whether the coordinates ix, iy are inside the
      ;; window, workaround for a *silly* Windows event bug 
      ;; 
      IF (((ix GE 0) OR (iy GE 0)) AND (ix LT wsizex) AND (iy LT wsizey)) THEN BEGIN
          IF ((!mouse.button eq mm_move) OR (!mouse.button eq 4)) THEN BEGIN
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
             ;; result=1 & do_it_again = NOT(do_it_again)
             ;; map to apply button
             ;; the following code is a simple copy of the code below
             ;; under 'apply'
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

       if ((ev.id EQ dbox) or (ev.id EQ LU_y) or (ev.id EQ SQ_y) or (ev.id EQ AS_y) or (ev.id EQ LOG_y) or (ev.id EQ SB_y) or (ev.id EQ RE_y)) THEN BEGIN
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
            'RE_y': IF WIDGET_Info(RE_y, /BUTTON_SET) THEN rotempad=1 ELSE rotempad=0
            'SQ_y': IF WIDGET_Info(SQ_y, /BUTTON_SET) THEN FixSq=1 ELSE FixSq=0
            'SB_y': IF WIDGET_Info(SB_y, /BUTTON_SET) THEN scalebar=1 ELSE scalebar=0
            'LU_y': IF WIDGET_Info(LU_y, /BUTTON_SET) THEN liveupdate=1 ELSE liveupdate=0
            'AS_y': BEGIN
               IF WIDGET_Info(AS_y, /BUTTON_SET) THEN autofilesave=1 else autofilesave=0
            END
            'LOG_y': BEGIN
               IF WIDGET_Info(LOG_y, /BUTTON_SET) THEN log=1 ELSE log=0
            END
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
;; now call the action function!
                    ;; the action function may have redefined the
                    ;; current image pointer, so we have to reset the
                    ;; focus window
                    device, set_graphics_function=gmode0  ; Restore mode.
                    device, cursor_standard=2
                    ;; print, "call f - 413 " & print,
                    ;;                  [xr1,yr1,xr2,yr2]`
                    ;;
                    f=call_function(actionfunc, FUNCARGS=funcargs, ROI=[xr1,yr1,xr2,yr2], /STORE, AUTOFILESAVE=autofilesave, LOG=log, SCALEBAR=scalebar, ROTEMPAD=rotempad)
                    WSET, (*datap).Window
                    device, set_graphics_function=oplotmode ; XOR mode.
                    

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
;; now call the action function!
                    ;; the action function may have redefined the
                    ;; current image pointer, so we have to reset the
                    ;; focus window
                    device, set_graphics_function=gmode0  ; Restore mode.
                    device, cursor_standard=2
                    ;; print, "call f - 454 " & print, [xr1,yr1,xr2,yr2]
                    f=call_function(actionfunc, FUNCARGS=funcargs, ROI=[xr1,yr1,xr2,yr2], LOG=log, SCALEBAR=scalebar, ROTEMPAD=rotempad)
                    WSET, (*datap).Window
                    device, set_graphics_function=oplotmode ; XOR mode.
                    
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
      pos=FIX([xr1, yr1, xr2, yr2])
      ;; is the position new?
      IF (Total(ABS(pos-posold)) GT eps) THEN BEGIN
         ;; position changed
         posold=pos
         if (liveupdate EQ 1) THEN BEGIN
            ;; now call the action function!
            ;; the action function may have redefined the
            ;; current image pointer, so we have to reset the
            ;; focus window
            ;; print, "here"
            device, set_graphics_function=gmode0 ; Restore mode.
            device, cursor_standard=2
            ;; print, "call f - 534 " & print, pos
            f=call_function(actionfunc, FUNCARGS=funcargs, ROI=pos, LOG=log, SCALEBAR=scalebar, ROTEMPAD=rotempad)
            WSET, (*datap).Window
            device, set_graphics_function=oplotmode ; XOR mode.
         END
      END
      If h.HASKEY("XTabContrastDialog") THEN BEGIN
         device, set_graphics_function=gmode0 ; Restore mode.
         device, cursor_standard=2
         ev = WIDGET_Event(h["XTabContrastDialog"], /NoWait)
         IF (ev.id EQ h["XTabContrastDialog"]) THEN Begin
              STOP
             call_procedure, "XTabContrastDialog_event", ev
            
          END
          WSET, (*datap).Window
          device, set_graphics_function=oplotmode ; XOR mode.
      END

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

