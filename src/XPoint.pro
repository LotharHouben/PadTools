PRO plot_cross, x1, y1, COLOR=color, SZ=sz
   plcolor=25600L
   IF KEYWORD_SET(color) THEN plcolor=color
   IF NOT(KEYWORD_SET(sz)) THEN sz=10
   plots, [x1-sz,x1-2], [y1,y1],/device,COLOR=plcolor
   plots, [x1+2,x1+sz], [y1,y1],/device,COLOR=plcolor
   plots, [x1,x1], [y1-sz,y1-2],/device,COLOR=plcolor
   plots, [x1,x1], [y1+2,y1+sz],/device,COLOR=plcolor
  return
END


FUNCTION XPoint, xi1, yi1, bin=binning, P=p, TITLE=title
COMMON MOUSEBINDINGS, mm_move, mm_resize, mm_break
XConsole_PushState
XConsole_WaitingForInput
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    PrintToCon, "% XPoint:    Fatal error "
    PrintToCon, "%   Error status  - " + STRING(error_status)
    PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
    CATCH, /Cancel
    XConsole_PopState
    return, 0
END

   ;;
   result=0   ; return false by default
   oplotmode=9
   plcolor=100L
   ;;
   ;; is there a window?
   ;;
   IF (!D.WINDOW LT 0) THEN BEGIN
       printToCon, "% XPoint: invalid window id"
       XConsole_PopState
       return, result
   END
   ;;
   ;; read the window dimensions
   ;;
   wsizex=!D.X_VSIZE & wsizey=!D.Y_VSIZE
   ;;
   if (not (keyword_set(binning))) THEN binning=1
   if (not (keyword_set(title))) THEN title="Point Dialog"
   x1 = xi1/binning
   y1 = yi1/binning
   ;;
   base = Widget_Base(TITLE=title, /COLUMN)
   ;;
   row1=WIDGET_BASE(base, /COLUMN)
   row1a = WIDGET_BASE(row1, /ROW)
   x_text=WIDGET_Label(row1a, VALUE="x, y = ")
   x_label = WIDGET_Label(row1a, /Dynamic_Resize, Value=MySTRING(x1*binning)+', '+MySTRING(y1*binning))
   IF KEYWORD_SET(p) THEN BEGIN
        row2a = WIDGET_BASE(row1, /ROW)
        p_text=WIDGET_Label(row2a, VALUE="data = ")
        p_label = WIDGET_Label(row2a, /Dynamic_Resize, Value=MySTRING((*(*p).im)[xi1,yi1]))
   END
   ;;
   ; create another row for the sticky flag
   row2 =  WIDGET_BASE(base, /ROW, XPAD=10)
   sticktext=Widget_Label(row2, VALUE='Keep Marker')
   togglebase = WIDGET_BASE(row2, /ROW, /EXCLUSIVE)
   stick_y = WIDGET_BUTTON(togglebase, VALUE='yes', UVALUE='stick_y', /NO_RELEASE)
   stick_n = WIDGET_BUTTON(togglebase, VALUE='no', UVALUE='stick_n', /NO_RELEASE)
   ;; and a row for the single step movement buttons
   mvbuttons = WIDGET_BASE(base, /ROW)
   mvb = CW_BGroup(mvbuttons, /ROW , ['  ^  ','  v  ','  <  ','  >  '] , Button_UVALUE=['up', 'down','left','right'])
   ;;
   row3 =  WIDGET_BASE(base, /ROW)
   b = CW_BGroup(row3, /ROW , ['   Accept   ','   Cancel   '] , Button_UVALUE=['close','cancel'])
widpos=PlaceWidget(base, POSKEY="cr")
 WIDGET_CONTROL, base, XOFFSET=widpos[0], YOFFSET=widpos[1], /REALIZE

   device, get_graphics=gmode0  ; current graphics mode.
   device, set_graphics=oplotmode       ; XOR mode.
   device, cursor_standard=45
   plot_cross, x1, y1, COLOR=plcolor
   do_it_again = 0 & sticky=1 & WIDGET_CONTROL, stick_y, /SET_BUTTON

   ;; empty buffered mouse button clicks
   ClearMouseButton, mm_break


   ;; bring current window to top
   WSHOW, !D.WINDOW

   REPEAT BEGIN
      cursor, ix, iy, /DEVICE, /NOWAIT
      ;;
      ;; check whether cursor position is inside the window frame
      ;;
      IF (((ix GE 0) OR (iy GE 0)) AND (ix LT wsizex) AND (iy LT wsizey)) THEN BEGIN
          IF (!mouse.button eq mm_move) THEN BEGIN
              IF ((ix NE x1) OR (iy NE y1)) THEN BEGIN
                  plot_cross, x1, y1, COLOR=plcolor  ;; overplot old plot crosshair cursor
                  TVCRS, ix, iy       ;; set new cursor position
                  x1=ix & y1=iy
                  plot_cross, x1, y1, COLOR=plcolor  ;; plot new crosshair cursor and display coordinates
                  WIDGET_Control, x_label, SET_VALUE=MySTRING(x1*binning)+','+MySTRING(y1*binning)
                  IF KEYWORD_SET(p) THEN BEGIN
                      xi1=x1*binning & yi1=y1*binning
                      WIDGET_Control, p_label, SET_VALUE=MySTRING((*(*p).im)[xi1,yi1])
                  END
              ENDIF
          ENDIF
          IF (!mouse.button EQ mm_break) THEN BEGIN
              result=1 & do_it_again = NOT(do_it_again)
          END
      ENDIF

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

      IF ((ev.id EQ b) OR (ev.id EQ mvb)) THEN BEGIN                ;; Button-Menu
         CASE ev.value OF
            'close': BEGIN
               do_it_again = NOT(do_it_again)
               result=1;
            END
            'cancel': BEGIN
                ;; ignore the sticky flag on cancel
                if (sticky EQ 1) THEN BEGIN
                    ;; remove cross from screen
                    plot_cross, x1, y1
                END
                do_it_again = NOT(do_it_again)
           result=0;
           END
           ELSE:BEGIN
               ;; create new coordinates
               dxr=0 & dyr=0
               CASE ev.value OF
                   'up': dyr=1
                   'down': dyr=-1
                   'left': dxr=-1
                   'right': dxr=1
                   ELSE:
               ENDCASE
               plot_cross, x1, y1
               x1=x1+dxr & y1=y1+dyr
               IF (x1 LT 0) THEN x1=0 ELSE IF (x1 GE wsizex*binning) THEN x1=wsizex*binning-1
               IF (y1 LT 0) THEN y1=0 ELSE IF (y1 GE wsizey*binning) THEN y1=wsizey*binning-1
               plot_cross, x1, y1
               WIDGET_Control, x_label, SET_VALUE=STRING(x1*binning)+','+STRING(y1*binning)
               IF KEYWORD_SET(p) THEN BEGIN
                   xi1=x1*binning & yi1=y1*binning
                   WIDGET_Control, p_label, SET_VALUE=MySTRING((*(*p).im)[xi1,yi1])
               END
           END
         endcase
      ENDIF
      MyWait, GetWaitTime()
   ENDREP UNTIL do_it_again

   ;; empty buffered mouse button clicks
   ClearMouseButton, mm_break

   Widget_Control, base, /DESTROY

   if (sticky EQ 0) THEN BEGIN
   ;; remove cross from screen
      plot_cross, x1, y1
   END

   device, set_graphics=gmode0  ; Restore mode.
   device, cursor_standard=2

   xi1 = x1*binning
   yi1 = y1*binning
   
   XConsole_PopState

   return, result
END










