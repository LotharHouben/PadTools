
FUNCTION XActionLINE, viewerp, xi1, yi1, xi2, yi2, BIN=binning, ACTIONFUNC=actionfunc,FUNCARGS=funcargs, LOG=log
COMMON MOUSEINTERACTION, SlowComputer
COMMON PLOTOBJECTS, ByDefaultSticky
;;
   SlowComputer=0
   XConsole_PushState
   XConsole_WaitingForInput
IF NOT(GetDebugFlag()) THEN BEGIN 
   CATCH, Error_status
   IF (Error_status NE 0) THEN BEGIN
       PrintToCon, "% XActionLine:    Fatal error "
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
result=0
oplotmode=9
plcolor=100L
WSET, (*datap).Window
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
   x1 = xi1/binning & y1 = yi1/binning & x2 = xi2/binning & y2 = yi2/binning 
   ;; w= SQRT(wx^2+wy^2)/binning ;; width
   w=width
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
   b = CW_BGroup(row4, /ROW , ['  Store  ','  Update Preview  ','   Help   ','  Cancel  '] , Button_UVALUE=['accept','preview','help','cancel'])
   ;; widpos=PlaceWidget(base, POSKEY=WidgetPos("ActiveDialogWin"))    
   ;; Widget_Control, base, XOFFSET=widpos[0], YOffset=widpos[1], /REALIZE
   Widget_Control, base, /REALIZE
   
   device, get_graphics_function=gmode0  ; store the current graphics mode.
  
   ;; now define the sprites for the endpoints
   pixsize=6
   p1=CreateSprite(pixsize, COLOR=255, CHANNEL=2) ;; green - start
   p2=CreateSprite(pixsize, COLOR=255, CHANNEL=1) ;; red   - end
   ;;
   device, set_graphics_function=oplotmode       ; XOR mode.

   draw_line, x1, x2, y1, y2, p1, p2, W=w, COLOR=plcolor, NOSPRITE=nosprite
   device, set_graphics_function=gmode0       ; XOR mode.
   ;;
   sticky=1
   If (sticky EQ 1) THEN WIDGET_CONTROL, stick_y, /SET_BUTTON $
   ELSE WIDGET_CONTROL, stick_n, /SET_BUTTON
   ;;
   do_it_again = 0
   liveupdate=1
   ;;
   ;; bring current window to top
   IF (keyword_set(windownr)) THEN BEGIN
      WSHOW, windownr
   END ELSE BEGIN
      WSHOW, !D.WINDOW
   END
   ;;
   REPEAT BEGIN
      WAIT, 0.04
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
                   device, set_graphics_function=oplotmode       ; XOR mode.
                   remove_line, p1, p2,  W=w, COLOR=plcolor, NOSPRITE=nosprite
                   x1=ix & y1=iy
                   draw_line, x1, x2, y1, y2, p1, p2, W=w, COLOR=plcolor, NOSPRITE=nosprite
                   device, set_graphics_function=gmode0
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
                   device, set_graphics_function=oplotmode       ; XOR mode.
                   remove_line, p1, p2, W=w, COLOR=plcolor, NOSPRITE=nosprite
                   x2=ix & y2=iy
                   draw_line, x1, x2, y1, y2, p1, p2 , W=w, COLOR=plcolor, NOSPRITE=nosprite
                   device, set_graphics_function=gmode0
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
                  device, set_graphics_function=oplotmode 
                   remove_line, p1, p2, W=w, COLOR=plcolor, NOSPRITE=nosprite
                   x1=tx1 & x2=tx2 & y1=ty1 & y2=ty2
                   draw_line, x1, x2, y1, y2, p1, p2, W=w, COLOR=plcolor, NOSPRITE=nosprite
                   device, set_graphics_function=gmode0
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
               'accept': BEGIN

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
                   if (w_label GT 0) THEN BEGIN
                      TMP='' & WIDGET_Control, w_label, GET_VALUE=TMP
                      TMP=TMP(0)
                      IF (TMP NE '') THEN tw=((ABS(ROUND(FLOAT(TMP)))))
                   END
                   IF CheckLineCoordinates(tx1, ty1, tx2, ty2, tw, maxx, maxy) THEN BEGIN
                      device, set_graphics_function=oplotmode 
                      remove_line, p1, p2,  W=w, COLOR=plcolor, NOSPRITE=nosprite
                      x1=FIX(tx1/binning) & x2=FIX(tx2/binning) & y1=FIX(ty1/binning) & y2=FIX(ty2/binning) & w=FIX(tw/binning)
                      draw_line, x1, x2, y1, y2, p1, p2, W=w, COLOR=plcolor, NOSPRITE=nosprite
                      device, set_graphics_function=gmode0
                   END ELSE printToCon, "% XLine: coordinates out of bounds!"                   
                   Update_LineInfo, x1_label, tx1, x2_label, tx2, y1_label, ty1, y2_label, ty2, w_label, tw, dist_label, BIN=1
                   ;; now call the action function!
                    ;; the action function may have redefined the
                    ;; current image pointer, so we have to reset the
                    ;; focus window
                    ;;device, set_graphics_function=gmode0  ; Restore mode.
                    ;;device, cursor_standard=2
                    f=call_function(actionfunc, FUNCARGS=funcargs, ROI=[tx1,ty1,tx2,ty2], LOG=log, SCALEBAR=scalebar, ROTEMPAD=rotempad)
                    ;; leave the result result in the Image List
                    ;; create a new container in the image list for the next line profile
                    self=funcargs["EMPADObj"]
                    o=funcargs['outputp']
                    Sz=Size(*(*(*o).datap).data)
                    s=["Profile diffraction", "Parent data: "+self.GetSetName(), "ROI: "+MyString(tx1)+":"+MyString(ty1)+","+MyString(tx2)+":"+MyString(ty2),"Number of frames: "+MyString(Sz[3]), "Mode: "+"Series"]
                    ThrowStack, PTR_NEW(FLTARR(Sz[1],Sz[2],2)), "ProfileStack("+self.GetSetName()+")", BIN=1, TITLE="ROIDiff", SAMP=[self->GetDetectorSampling(/X),self->GetDetectorSampling(/Y),self->GetScanSampling(/X)], UNIT=['1/nm','1/nm','nm']
                    o=GetCurrentP()
                    ;; define contrastmode
                    (*(*o).datap).contrastmode="auto"
                    (*(*o).datap).contrastsubmode="minmax" 
                    (*(*o).datap).contrastroi="diff"
                    ;; create a list with function arguments
                    ;;
                    funcargs['outputp']=o
                    ;; STOP
                    WSET, (*datap).Window
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
                                      ;; Delete the last dummy container in the Image List
                   res=DataList_DeleteCurrentElement(GetRootP())
                   Update_XTabControl
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
                      device, set_graphics_function=oplotmode 
                       remove_line, p1, p2,  W=w, COLOR=plcolor, NOSPRITE=nosprite
                       x1=FIX(tx1/binning) & x2=FIX(tx2/binning) & y1=FIX(ty1/binning) & y2=FIX(ty2/binning) & w=FIX(tw/binning)
                       draw_line, x1, x2, y1, y2, p1, p2, W=w, COLOR=plcolor, NOSPRITE=nosprite
                       device, set_graphics_function=gmode0 
                       do_it_again = NOT(do_it_again)
                       result=1
                   END ELSE printToCon, "% XLine: coordinates out of bounds!"                   
                   
                   IF KEYWORD_SET(sequence) THEN BEGIN
                       TMP='' & WIDGET_Control, seqavg_label, GET_VALUE=TMP
                       TMP=TMP(0) & IF (TMP NE '') THEN seqavg=FIX(FLOAT(TMP))
                       TMP='' & WIDGET_Control, seqstep_label, GET_VALUE=TMP
                       TMP=TMP(0) & IF (TMP NE '') THEN seqstep=FIX(FLOAT(TMP))
                   END 
                   ;; Delete the last dummy container in the Image List
                   res=DataList_DeleteCurrentElement(GetRootP())
                   Update_XTabControl
                   
                END
           endcase
       ENDIF 
   ENDREP  UNTIL do_it_again

   if (sticky EQ 0) THEN BEGIN
      ;; remove line from screen
      device, set_graphics_function=oplotmode 
      remove_line, p1, p2, W=w, COLOR=plcolor, NOSPRITE=nosprite
   END    

   device, set_graphics_function=gmode0  ; Restore mode.

   device, cursor_standard=2
   Widget_Control, base, /DESTROY
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
;;       printTocon, "%                 (w, wx, wy) = ("+ MyString(w)+", "+MyString(wx)+", "+MyString(wy)+")"
       printTocon, "%                    (l, angle) = ("+MyString(dist)+", "+MyString(angle)+")"
;;       IF KEYWORD_SET(sequence) THEN BEGIN
;;       printToCon, "%          (seq.-avg, step) = ("+MyString(dist)+", "+MyString(angle)+")"
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
 
