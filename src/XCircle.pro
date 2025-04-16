PRO CheckCircleCoord, x1, y1, r, xr1, yr1, rr, b, MAXX=maxx, MAXY=maxy, MOUSE=mouse
if keyword_set(mouse) THEN BEGIN
    ;; take coordinates from screen and multiply them to get the
    ;; real world coordinates
    xr1=x1*b & yr1=y1*b & rr=r*b
END
;; calculate size first
D=ABS(2*rr) + 1
;; print, rr
;;
IF (keyword_set(maxx) AND keyword_SET(maxy)) THEN BEGIN
   IF (maxx GT maxy) THEN minsz=maxy ELSE minsz=maxx
   if (D GT (minsz+1)) THEN BEGIN
      ;; circle does not fit
      print, "circle does not fit"
      D=minsz+1 & rr=FLOOR(0.5*D) 
   END 
;; circle now fits but may be outside the field of view
   IF (xr1 LT rr) THEN BEGIN
      xr1=rr
   END ELSE BEGIN
      IF ((xr1+rr) GT maxx) THEN BEGIN
         xr1=maxx-rr 
      END
   END
   IF (yr1 LT rr) THEN BEGIN
      yr1=rr
   END ELSE BEGIN
      IF ((yr1+rr) GT maxy) THEN BEGIN
         yr1=maxy-rr 
      END
   END
END
;;
;; recalculate display coordinates
x1=FIX(xr1/b) & y1=FIX(yr1/b) & r=FIX(rr/b)
END




function makex,xlo,xhi,xst, help=hlp
 
   if (n_params(0) lt 3) or keyword_set(hlp) then begin
      print,' Make an array with specified start, end and step values.' 
      print,' x = makex(first, last, step)' 
      print,'   first, last = array start and end values.     in'
      print,'   step = step size between values.              in'
      print,'   x = resulting array.                          out' 
      return, -1
   endif
   return, xlo+xst*findgen(1+ long( (xhi-xlo)/xst) )
END


FUNCTION XCIRCLE, xi, yi, ri, sticky=sticky, help=hlp, bin=binning, add_circles=acirc, TITLE=title, CONT=cont, SCALE=scale, BUTTONTITLES=buttontitles, ALLOWOVERSIZE=allowoversize
;; xr, yr = Zentrum des Kreises in Bildkoordinaten
;; rr     = Radius des Kreises in Bildkoordinaten
;; x,y    = Zentrum des Kreises in Bildschrimkoordinaten
;;   r    = Radius des Kreises in Bildschirmkoordinaten
;;
COMMON MOUSEBINDINGS, mm_move, mm_resize, mm_break

   XConsole_PushState
   XConsole_WaitingForInput
   IF (GetDebugFlag()) THEN BEGIN
   END ELSE BEGIN
      CATCH, Error_status
      IF (Error_status NE 0) THEN BEGIN
         PrintToCon, "% XCircle:    Fatal error "
         PrintToCon, "%   Error status  - " + STRING(error_status)
         PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
         CATCH, /Cancel
         XConsole_PopState
         return, 0
      END
   END
   result=0                     ; return false by default
   oplotmode=9
   plcolor=100L
   ;;
   ;; image=tvrd()
   ;;
   ;; bring the window to front and read the window dimensions
      ;; bring current window to top
   viewerp=GetCurrentP()
;; check that viewerp is set
   IF NOT(PTR_VALID(viewerp)) THEN BEGIN
      PrintToCon, "% XCircle:    Fatal error, View image image pointer is not valid."
      return, 0
   END   
   datap=(*viewerp).datap
   If NOT(PTR_VALID(datap)) THEN BEGIN
      PrintToCon, "% XCircle:    Fatal error, View image data pointer is not valid."
      return, 0
   END
   WSET, (*datap).Window
   Wshow, (*datap).Window
   ;;
   wsizex=!D.X_VSIZE & wsizey=!D.Y_VSIZE
   ;;
   IF keyword_set(hlp) THEN BEGIN
      help_text=hlp
   END ELSE BEGIN
   IF keyword_set(cont) THEN BEGIN
   help_text = ["  Mark circles in the current window using your mouse.", " ", $
                "* mouse operations:","  left button - keep pressed and drag to move or click to position the circle", $
                "  middle button - keep pressed and drag or click to change the radius", $
                "  right button - print the current circle parameters in the log window", $
                "* button operations: ", $
                "  apply - you can edit the position and radius data and then press on 'apply'", $
                "  accept - print the current circle parameters in the log window", $
                "  cancel - leave the dialog"]   

   END ELSE BEGIN
  help_text = ["  Mark a circle in the current window using your mouse.", " ", $
                "* mouse operations:","  left button - ekep pressed and drag to move or click to position the circle", $
                "  middle button - keep pressed and drag or click to change the radius", $
                "  right button - leave the dialog and return the current circle parameters", $
                "* button operations: ", $
               "  apply - you can edit the position and radius data and then press on 'apply'", $
                "  accept - leave the dialog and return the current circle parameters", $
                "  cancel - leave the dialog"]    
END
END  
   
   if (not (keyword_set(binning))) THEN binning=1 
   x = xi/binning & xr=xi
   y = yi/binning & yr=yi
   r = ri/binning & rr=ri
   IF keyword_set(allowoversize) THEN BEGIN
      maxx=0 & maxy=0
   END ELSE BEGIN
      maxx=wsizex*binning-1 & maxy=wsizey*binning-1
   END
   CheckCircleCoord, x, y, r, xr, yr, rr, binning, MAXX=maxx, MAXY=maxy
   ;;
   IF NOT(keyword_set(title)) Then title="Circle Dialog"
   base = Widget_Base(TITLE=title, /COLUMN) ;;, DISPLAY_NAME=GetDisplayName())
   ;;
   input =  WIDGET_BASE(base, /ROW, XPAD=10) 
   x_label = CW_Field(input, TITLE='x', XSize=5, Value=MySTRING(xr))
   y_label = CW_Field(input, TITLE='y', XSize=5, Value=MySTRING(yr))
   r_label = CW_Field(input, TITLE='r', XSize=5, Value=MySTRING(rr))
   ;;
   ; create another row for the sticky flag
   row4 =  WIDGET_BASE(base, /ROW, XPAD=10)
   sticktext=Widget_Label(row4, VALUE='Keep marker')
   togglebase = WIDGET_BASE(row4, /ROW, /EXCLUSIVE)
   stick_y = WIDGET_BUTTON(togglebase, VALUE='yes', UVALUE='stick_y', /NO_RELEASE, TOOLTIP="keep marker circle after leaving the dialog")
   stick_n = WIDGET_BUTTON(togglebase, VALUE='no', UVALUE='stick_n', /NO_RELEASE, TOOLTIP="remove marker circle after leaving the dialog")  
   ;;
   mvbuttons = WIDGET_BASE(base, /ROW)
   mvb = CW_BGroup(mvbuttons, /ROW , [' ^ ',' v ',' < ',' > ','o-O','O-o'] , Button_UVALUE=['up', 'down','left','right','larger','smaller'])

   buttons = WIDGET_BASE(base, /ROW)
   IF keyword_set(buttontitles) THEN BEGIN
      IF (N_elements(buttontitles) LT 4) THEN BEGIN
         printtocon, "% XCircle: Button titles are incorrect. Using default titles"
         buttontitles=0
      END
   END
   IF NOT(keyword_set(buttontitles)) THEN buttontitles=['Accept','Apply','Help','Cancel']
   b = CW_BGroup(buttons, /ROW , buttontitles, Button_UVALUE=['accept', 'apply', 'help', 'cancel'])

   IF NOT(keyword_set(sticky)) THEN sticky=0 
   IF (sticky EQ 0) THEN WIDGET_CONTROL, stick_n, /SET_BUTTON ELSE  WIDGET_CONTROL, stick_y, /SET_BUTTON

 WIDGET_CONTROL, base, /REALIZE
   ;; 
   ;; unit radius x and y circle coordinates cosa and sina
   ;; 
   a = makex(0, 360, 4)/!radeg
   cosa = cos(a)
   sina = sin(a)
   xx = x + r*cosa
   yy = y + r*sina
   device, get_graphics=gmode0  ; Entry mode.
   device, set_graphics=oplotmode ; XOR mode.
   WSET, (*datap).Window
   plots,xx,yy,/device,COLOR=plcolor          ; plot the circle
   plot_cross, x, y, COLOR=plcolor             ; plot the crosshair cursor in the centre
   ;; read window content
   ;;Amap=
   ;;Adat=
   if ((keyword_set(acirc))) THEN BEGIN
     ; plot additional concentric circles
     FOR i=0, N_ELEMENTS(acirc)-1 DO BEGIN
       StaticXCIRCLE, x, y, r*acirc[i],COLOR=plcolor  ; plot circles with respective radii
     ENDFOR
   END
   IF NOT(keyword_set(scale)) THEN scale=1.

   ;;
   ;; start the event loop
   ;; 
   do_it_again = 0
   ;;   

   str = ["% MCircle - ix  iy  ir  x  y  r"]
   PrintToCon, str
;; 
   h=CurrentlyManagedWidgets()
   ;; 
   REPEAT BEGIN
      cursor, ix, iy, 1, /dev, /NOWAIT   ; read cursor position
      ;; now check whether the coordinates ix, iy are inside the
      ;; window, workaround for a *silly* Windows event handling
      ;; 
      IF (((ix GE 0) OR (iy GE 0)) AND (ix LT wsizex) AND (iy LT wsizey)) THEN BEGIN
     WSET, (*datap).Window 
      IF (ix NE x) OR (iy NE y) THEN BEGIN  ; did it change?
         IF (!mouse.button eq mm_move) THEN BEGIN   ; middle mouse button pressed
            device, cursor_standard=45      ; change cursor style
            TVCRS, 0                        ; hide cursor
            plots,xx,yy,/device,COLOR=plcolor             ; overplot circle in XOR mode
            plot_cross, x, y, COLOR=plcolor                
            if ((keyword_set(acirc))) THEN BEGIN
     	      FOR i=0, N_ELEMENTS(acirc)-1 DO BEGIN
       	        StaticXCIRCLE, x, y, r*acirc[i],COLOR=plcolor ; overplot other circles
     	      ENDFOR
          END
          ;;
          ;; use cursor coordinates to get new circle centre
          ;; 
          CheckCircleCoord, ix, iy, r, xr, yr, rr, binning, MAXX=maxx, MAXY=maxy, /MOUSE
          ;;
          ;; plot circle
          xx = ix + r*cosa      ; new circle coordinates 
          yy = iy + r*sina
          plots,xx,yy,/device,COLOR=plcolor
          plot_cross, ix, iy, COLOR=plcolor
          ;;
          if ((keyword_set(acirc))) THEN BEGIN
              FOR i=0, N_ELEMENTS(acirc)-1 DO BEGIN
                  StaticXCIRCLE, ix, iy, r*acirc[i],COLOR=plcolor
              ENDFOR
          END
          TVCRS, ix, iy         ; position cursor
          TVCRS, 1              ; show cursor
          x = ix  & y =  iy     ; new circle center
          WIDGET_Control, x_label, SET_VALUE=MySTRING(xr) 
          WIDGET_Control, y_label, SET_VALUE=MySTRING(yr)
         ENDIF 
         IF ((!mouse.button eq mm_resize) AND ((iy NE y) OR (ix NE x))) THEN BEGIN 
            device, cursor_standard=42
            TVCRS, 0            ;; hide cursor
            plots,xx,yy,/device,COLOR=plcolor
            plot_cross, x, y, COLOR=plcolor
            if ((keyword_set(acirc))) THEN BEGIN
     	      FOR i=0, N_ELEMENTS(acirc)-1 DO BEGIN
       	        StaticXCIRCLE, x, y, r*acirc[i],COLOR=plcolor
     	      ENDFOR
            END
            r = FIX(sqrt((iy-y)*(iy-y)+(ix-x)*(ix-x))) ; new radius
            ix=x & iy=y ;; only change radius but not the position 
            ;;
            ;; use cursor coordinates to get new circle centre
            ;; 
            CheckCircleCoord, ix, iy, r, xr, yr, rr, binning, MAXX=maxx, MAXY=maxy, /MOUSE
            ;;
            xx = ix + r*cosa
            yy = iy + r*sina
            plots,xx,yy,/device,COLOR=plcolor
            plot_cross, ix, iy, COLOR=plcolor
            if ((keyword_set(acirc))) THEN BEGIN
     	      FOR i=0, N_ELEMENTS(acirc)-1 DO BEGIN
       	        StaticXCIRCLE, x, y, r*acirc[i],COLOR=plcolor
     	      ENDFOR
            END
            ;; TVCRS, ix, iy
            x=ix & y=iy
            TVCRS, 1
            WIDGET_Control, x_label, SET_VALUE=MySTRING(xr) 
            WIDGET_Control, y_label, SET_VALUE=MySTRING(yr)
            WIDGET_Control, r_label, SET_VALUE=MySTRING(rr)
         ENDIF 
      ENDIF
      IF (!mouse.button EQ mm_break) THEN  BEGIN
         IF keyword_set(cont) THEN BEGIN
            ;; report coordinates to log windoew and continue
             ; Read new data from Widgets
            ;; 1
            ;; IF ((ix NE x) OR (iy NE y)) THEN BEGIN
            IF (1) THEN BEGIN
               TMP='' & WIDGET_Control, x_label, GET_VALUE=TMP
               TMP=TMP(0) & IF (TMP NE '') THEN xr=FLOAT(TMP)
               TMP='' & WIDGET_Control, y_label, GET_VALUE=TMP
               TMP=TMP(0) & IF (TMP NE '') THEN yr=FLOAT(TMP)
               TMP='' & WIDGET_Control, r_label, GET_VALUE=TMP
               TMP=TMP(0) & IF (TMP NE '') THEN rr=FLOAT(TMP)
               Printtocon, " "+MyString(x*binning)+" "+MyString(y*binning)+" "+MyString(r*binning)+" "+MyString(x*scale*binning)+" "+MyString(y*scale*binning)+" "+MyString(r*scale*binning)
               ;;
               IF (sticky EQ 1) THEN BEGIN
                  WSET, (*datap).Window
                  plots,xx,yy,/device,COLOR=plcolor
                  plot_cross, x, y, COLOR=plcolor
                  if ((keyword_set(acirc))) THEN BEGIN
                     FOR i=0, N_ELEMENTS(acirc)-1 DO BEGIN
                        StaticXCIRCLE, x, y, r*acirc[i],COLOR=plcolor
                     ENDFOR
                  END
               ENDIF
               ClearMouseButton, mm_break
            END  
         END ELSE BEGIN
            result=1 & do_it_again = NOT(do_it_again)
         END
      END
  END 
  
  ;; now check for widget or keyboard events

  ev = WIDGET_Event(base, /NoWait)
 
  if (ev.id NE 0) THEN BEGIN

      ;; first: check for the hot keys
      Widget_Control, ev.top, Get_UValue=info
      thisEvent = Tag_Names(ev, /Structure_Name)
      CASE thisEvent OF
          'WIDGET_TEXT_CH': BEGIN
                                ; Print the character in the window.
              text = 'Character: ('+ StrTrim(event.ch,2) + ')'
              print, text
          END
          ELSE:
      ENDCASE 
          
      if ((ev.id EQ stick_y) or (ev.id EQ stick_n)) THEN BEGIN
         uv=""
         WIDGET_CONTROL, ev.id, GET_UVALUE = uv
         CASE uv OF
         'stick_y': BEGIN
               sticky=1 
              END
         'stick_n': BEGIN 
               sticky=0 
              END
         ELSE:
         ENDCASE
      END  



      IF ((ev.id EQ b) OR (ev.id EQ mvb))  THEN BEGIN                ;; Button-Menu
         CASE ev.value OF
            'help': BEGIN
               msg = dialog_info(help_text, /HELP)
            END
            'accept': BEGIN
               IF keyword_set(cont) THEN BEGIN
                  ;; report coordinates to log windoew and continue
                                ; Read new data from Widgets
                  IF (ix NE x) OR (iy NE y) THEN BEGIN
                     TMP='' & WIDGET_Control, x_label, GET_VALUE=TMP
               TMP=TMP(0) & IF (TMP NE '') THEN xr=FLOAT(TMP)
               TMP='' & WIDGET_Control, y_label, GET_VALUE=TMP
               TMP=TMP(0) & IF (TMP NE '') THEN yr=FLOAT(TMP)
               TMP='' & WIDGET_Control, r_label, GET_VALUE=TMP
               TMP=TMP(0) & IF (TMP NE '') THEN rr=FLOAT(TMP)
               Printtocon, " "+MyString(x*binning)+" "+MyString(y*binning)+" "+MyString(r*binning)+" "+MyString(x*scale*binning)+" "+MyString(y*scale*binning)+" "+MyString(r*scale*binning)
                     ;;
               IF (sticky EQ 0) THEN BEGIN
                  WSET, (*datap).Window
                        plots,xx,yy,/device,COLOR=plcolor
                        plot_cross, x, y, COLOR=plcolor
                        if ((keyword_set(acirc))) THEN BEGIN
                           FOR i=0, N_ELEMENTS(acirc)-1 DO BEGIN
                              StaticXCIRCLE, x, y, r*acirc[i],COLOR=plcolor
                           ENDFOR
                        END
                     ENDIF
                  END 
                  
               END ELSE BEGIN
                                ; Read new data from Widgets
                  TMP='' & WIDGET_Control, x_label, GET_VALUE=TMP
                  TMP=TMP(0) & IF (TMP NE '') THEN xr=FLOAT(TMP)
                  TMP='' & WIDGET_Control, y_label, GET_VALUE=TMP
                  TMP=TMP(0) & IF (TMP NE '') THEN yr=FLOAT(TMP)
                  TMP='' & WIDGET_Control, r_label, GET_VALUE=TMP
                  TMP=TMP(0) & IF (TMP NE '') THEN rr=FLOAT(TMP)
                  CheckCircleCoord, ix, iy, r, xr, yr, rr, binning, MAXX=maxx, MAXY=maxy
                  result=1 & do_it_again = NOT(do_it_again)
               END
            END
            'cancel': BEGIN
               do_it_again = NOT(do_it_again)
            END
            'apply':BEGIN
               ;;device, cursor_standard=45      ; change cursor style
               ;;TVCRS, 0                        ; hide cursor
               WSET, (*datap).Window
               plots,xx,yy,/device,COLOR=plcolor             ; overplot circle in XOR mode
               plot_cross, x, y, COLOR=plcolor
               if ((keyword_set(acirc))) THEN BEGIN
     	         FOR i=0, N_ELEMENTS(acirc)-1 DO BEGIN
       	         StaticXCIRCLE, x, y, r*acirc[i],COLOR=plcolor ; overplot other circles
     	         ENDFOR
               END
               ; Read new data from Widgets
               TMP='' & WIDGET_Control, x_label, GET_VALUE=TMP
               TMP=TMP(0) & IF (TMP NE '') THEN xr=FLOAT(TMP)
               TMP='' & WIDGET_Control, y_label, GET_VALUE=TMP
               TMP=TMP(0) & IF (TMP NE '') THEN yr=FLOAT(TMP)
               TMP='' & WIDGET_Control, r_label, GET_VALUE=TMP
               TMP=TMP(0) & IF (TMP NE '') THEN rr=FLOAT(TMP)
               CheckCircleCoord, ix, iy, r, xr, yr, rr, binning, MAXX=maxx, MAXY=maxy
               xx = ix + r*cosa                ; new circle coordinates 
               yy = iy + r*sina
               plots,xx,yy,/device,COLOR=plcolor
               plot_cross, ix, iy, COLOR=plcolor
               if ((keyword_set(acirc))) THEN BEGIN
     	         FOR i=0, N_ELEMENTS(acirc)-1 DO BEGIN
       	           StaticXCIRCLE, ix, iy, r*acirc[i],COLOR=plcolor
     	         ENDFOR
               END
               x = ix & y = iy
               WIDGET_Control, x_label, SET_VALUE=MySTRING(xr) 
               WIDGET_Control, y_label, SET_VALUE=MySTRING(yr)
               WIDGET_Control, r_label, SET_VALUE=MySTRING(rr)
           END

            ELSE:BEGIN
               ;;device, cursor_standard=45      ; change cursor style
               ;;TVCRS, 0                        ; hide cursor
               WSET, (*datap).Window
               plots,xx,yy,/device,COLOR=plcolor             ; overplot circle in XOR mode
               plot_cross, x, y, COLOR=plcolor
               if ((keyword_set(acirc))) THEN BEGIN
     	         FOR i=0, N_ELEMENTS(acirc)-1 DO BEGIN
       	         StaticXCIRCLE, x, y, r*acirc[i],COLOR=plcolor ; overplot other circles
     	         ENDFOR
               END
               ;; create new coordinates
               dxr=0 & dyr=0 & dr=0
               CASE ev.value OF 
                'up': dyr=1
                'down': dyr=-1
                'left': dxr=-1
                'right': dxr=1
                'larger': dr=1
                'smaller': IF (rr GT 1) THEN dr=-1
                ELSE:
               ENDCASE
               xr=xr+dxr & yr=yr+dyr & rr=rr+dr
               CheckCircleCoord, ix, iy, r, xr, yr, rr, binning, MAXX=maxx, MAXY=maxy
               xx = ix + r*cosa                ; new circle coordinates 
               yy = iy + r*sina
               plots,xx,yy,/device,COLOR=plcolor
               plot_cross, ix, iy, COLOR=plcolor
               if ((keyword_set(acirc))) THEN BEGIN
     	         FOR i=0, N_ELEMENTS(acirc)-1 DO BEGIN
       	           StaticXCIRCLE, ix, iy, r*acirc[i],COLOR=plcolor
     	         ENDFOR
               END
               x = ix &  y =  iy
               WIDGET_Control, x_label, SET_VALUE=MySTRING(xr) 
               WIDGET_Control, y_label, SET_VALUE=MySTRING(yr)
               WIDGET_Control, r_label, SET_VALUE=MySTRING(rr)
	    END

         endcase
     ENDIF
   END

  If h.HASKEY("XTabContrastDialog") THEN BEGIN
          WSET, (*datap).Window
      plots,xx,yy,/device,COLOR=plcolor
      plot_cross, x, y, COLOR=plcolor
         device, set_graphics_function=gmode0 ; Restore mode.
         device, cursor_standard=2
         ev = WIDGET_Event(h["XTabContrastDialog"], /NoWait)
          WSET, (*datap).Window
          device, set_graphics_function=oplotmode ; XOR mode.
      plots,xx,yy,/device,COLOR=plcolor
      plot_cross, x, y, COLOR=plcolor
      END
  
  MyWAIT, GetWaitTime()
  
   ENDREP UNTIL do_it_again

   ;; empty buffered mouse button clicks
   ClearMouseButton, mm_break
   IF Not(keyword_set(cont)) THEN BEGIN
      ;; STOP
      Printtocon, " "+MyString(x)+" "+MyString(y)+" "+MyString(r)+" "+MyString(x*scale)+" "+MyString(y*scale)+" "+MyString(r*scale)
   END
   Widget_Control, base, /DESTROY
   IF (sticky EQ 0) THEN BEGIN
      WSET, (*datap).Window
      plots,xx,yy,/device,COLOR=plcolor
      plot_cross, x, y, COLOR=plcolor
      if ((keyword_set(acirc))) THEN BEGIN
         FOR i=0, N_ELEMENTS(acirc)-1 DO BEGIN
           StaticXCIRCLE, x, y, r*acirc[i],COLOR=plcolor
         ENDFOR
      END
   ENDIF
   device, set_graphics=gmode0  ; Restore mode.
   device, cursor_standard=2
   xi = xr & yi = yr & ri = rr
   ;; next line is for debugging only
   ;; Print, "XCircle: returning (", xi, ",", yi, ",", ri, ")"

   XConsole_PopState

   return, result
END

PRO StaticXCIRCLE, xi, yi, ri, COLOR=color
;; plots a circle on the screen in XOR-Mode
;; x,y = Zentrum des Kreises
;;   r = Radius des Kreises
;; binning nicht noetig, da mit reinen TV-Koordinaten aufgerufen
;;
   if KEYWORD_SET(color) THEN plcolor=color ELSE plcolor=0
   a = makex(0, 360, 4)/!radeg
   cosa = cos(a)
   sina = sin(a)
   xx = xi + ri*cosa
   yy = yi + ri*sina
   device, get_graphics=gmode0  ; Entry mode.
   device, set_graphics=oplotmode       ; XOR mode.
   plots,xx,yy,/device,COLOR=plcolor
   device, set_graphics=gmode0  ; Restore mode.

END













