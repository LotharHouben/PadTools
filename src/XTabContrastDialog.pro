;; MODIFICATONS
;; Contrast handling from version 5.5 on:
;; 
;;
;; Obtaining contrast settings and displaying (in display.pro)
;; (1) Read contrast setting from data stack descriptor 
;; (2) display image
;;
;; Setting contrast settings for a stack (in ContrastInspector)
;; (1) Use the ContrastInspector to get parameters
;; (2) Use procedure SetStackContrastMode to set Mode  parameters for a stack
;;     Syntax: PRO SetStackContrastMode, AUTO=auto, MANUAL=manual, SDEV=sdev,
;;     MDEV=mdev, MINMAX=minmax, QUARTER=quarter, FULL=full, SRANGE=srange
;; (3) Use SetContrastValues to set hi and lo cut offs
;;
;; TODO: 03.11.2013 - Update XTabContrastDialog by using
;;                    GetStackContrastMode()
;;                  - modify event handler to modify stack mode via SetStackContrastMode




pro SetContrastInspectorPrefs
;; This procedure is called only once during startup
;; rx0, rx1, ry0, ry1 are not used for now
COMMON CONTRASTPREFS, sd, minprc, maxprc
COMMON XCD_REGION, rflag, rx0, rx1, ry0,ry1
;; rflag=0 ;; full frame auto contrast
;; rflag=1 ;; region auto contrast
rflag=2 ;; quarter frame auto contrast
sd=3 & minprc=10 & maxprc=90
SetContrastMode, 'auto'
SetAutoContrastValues, [0.,1.]
return
end

Pro Update_XTabContrastDialog
COMMON PLOTCOLORS, backgr, foregr
COMMON XCD_WIDGETS, min, max, hdraw, aclabel, minprclabel, maxprclabel, base, ffbutton, qfbutton, spbutton, auto, manual, picktable, clin, clog, cexp, gammalabel, gammaval
COMMON XCD_DATA, cmin, cmax
COMMON XCD_REGION, rflag, rx0, rx1, ry0,ry1
;; update the contrast dialog window 
;; update histogram 
;; update min and max values 
;; this routine does not call the TVDisplay routine!
IF NOT(GetDebugFlag()) THEN BEGIN
   CATCH, Error_status
   IF (Error_status NE 0) THEN BEGIN
      PrintToCon, "% Update_XTabContrastDialog: Fatal error "
      PrintToCon, "%            Error status  - " + STRING(error_status)
      PrintToCon, "%            Error message - " + !ERROR_STATE.MSG
      CATCH, /Cancel
      return
   END
END
;; update the information in the minimum and maximum field and the histogram 
IF (XREGISTERED('XTabContrastDialog')) THEN BEGIN

    ;; get the currently focussed image
    ptr=GetRootP()
    IF (NOT(PTR_VALID(ptr))) THEN BEGIN
       print, "% Update_XTabContrastDialog: Root pointer is invalid"
       return
    END
    p=(*ptr).current

        IF (NOT(PTR_VALID(p))) THEN BEGIN
           print, "% Update_XTabContrastDialog: Current pointer is invalid"
           return
        END
        e=(*p).datap
   
    IF PTR_VALID(e) THEN BEGIN
       ;; get contrast mode 
       ;; if auto then get min and max from TV Display routine
;;;    ########## TV Display update
       cmin=(*e).contrast[0] & cmax=(*e).contrast[1]
;;        
;;       mode=GetContrastMode()
;;       IF (mode EQ 'auto') THEN BEGIN
;;          ;; TVDisplay
;;          v=GetAutoContrastValues()
;;          cmin=v[0] & cmax=v[1]
;;       END ELSE BEGIN
          ;; print, "calling TVdisplay - contrast="+MyString((*e).contrast[0])+":"+ MyString((*e).contrast[1])
          ;; TVDisplay, CONTRAST=(*e).contrast
          ;; set cmin, cmax to contrast data in data stack structure
;;          cmin=(*e).contrast[0] & cmax=(*e).contrast[1]
;;       END       
       ;; 
       ;; ++++++ update mn, max data fields in widget
       ;;
;;       WIDGET_Control, min, SET_VALUE=STRING(cmin)
;;       WIDGET_Control, max, SET_VALUE=STRING(cmax)
       ;;
              ;;; ++++++++++ Histogram Display ++++++++++++++
       ;; region of interest for histogram calculation:
       ;; frm=[X0,X1,Y0,Y1] 2D image coordinates
       ;; default= full frame
       ;; rflag=2: quarter frame
       submode='undef'
       frameroi='undef'
       sdevrange=2.
       hilo=[0.,1.]
       mode=GetStackContrastMode(SUBMODE=submode, ROI=frameroi, SRANGE=srange) ;;
       rflag=2
       if (submode EQ 'quarter') THEN rflag=2
       CASE (*e).zcoord OF
          3: BEGIN
             frm=[0,(*e).SzX-1,0,(*e).SzY-1] ;; full frame
             IF (rflag EQ 2) THEN frm=[FLOOR((*e).SzX*0.25),FLOOR((*e).SzX*0.75),FLOOR((*e).SzY*0.25),FLOOR((*e).SzY*0.75)]
          END
          2:BEGIN                 
             frm=[0,(*e).SzX-1,0,(*e).SzZ-1]
             IF (rflag EQ 2) THEN frm=[FLOOR((*e).SzX*0.25),FLOOR((*e).SzX*0.75),FLOOR((*e).SzZ*0.25),FLOOR((*e).SzZ*0.75)]
          END
          1: BEGIN
             frm=[0,(*e).SzY-1,0,(*e).SzZ-1]
             IF (rflag EQ 2) THEN frm=[FLOOR((*e).SzY*0.25),FLOOR((*e).SzY*0.75),FLOOR((*e).SzZ*0.25),FLOOR((*e).SzZ*0.75)]
          END 
          ELSE:
       ENDCASE
    END 
    ;; get window nr of histogram draw area 
    WIDGET_Control, hdraw, GET_VALUE=winindex
    WSET, winindex
    
    ;; get drawable size, this will be the number of bins in the histogram
    
    Xsize=!D.X_VSIZE
    ;; minimum and maximum are equal to the values in the input boxes
    ;; in the dialog = cmin, cmax
    histcmax=FLOAT(cmax) & histcmin=FLOAT(cmin)

    Print, "% Update_XTabContrastDialog: Histogram range = ", [cmin,cmax]
        CASE (*e).zcoord OF
           1: BEGIN             
              h=HISTOGRAM(FLOAT((*(*e).data)[(*e).slice,frm[0]:frm[1],frm[2]:frm[3]]), NBins=XSize, MIN=histcmin, MAX=histcmax)
           END
           2: BEGIN             
              h=HISTOGRAM(FLOAT((*(*e).data)[frm[0]:frm[1],(*e).slice,frm[2]:frm[3]]), NBins=XSize, MIN=histcmin, MAX=histcmax)
           END
           3: BEGIN             
              h=HISTOGRAM(FLOAT((*(*e).data)[frm[0]:frm[1],frm[2]:frm[3],(*e).slice]), NBins=XSize, MIN=histcmin, MAX=histcmax)
           END
           ELSE: 
        END
        ;; 
        IF (!VERSION.OS EQ 'darwin') THEN BEGIN
		;; hack for the weird Mac OS X bug
		plot, h, XMARGIN=[1,1], YMARGIN=[1,1],  XSTYLE=4, YSTYLE=4,  BACKGROUND=backgr, Color=foregr, /DEVICE
	
	;;/DATA, POS=[0.,200.,0.,100.],
             END ELSE BEGIN
	        ;; at least the Linux platform works 
                plot, h, XMARGIN=[1,1], YMARGIN=[1,1], XSTYLE=4, YSTYLE=4, BACKGROUND=backgr, Color=foregr
             END
             ;; STOP
;;        (*imp).CMin=cmin & (*imp).CMax=cmax
;;        (*altp).CMin=cmin & (*altp).CMax=cmax
;;        IF (((*p).id) EQ 'group') THEN TVDisplay, imp, GRPP=altp
;;        ELSE TVDisplay, imp
          END 

END   
 


Pro hdrawctxtmenu, ev  
COMMON XCD_WIDGETS, min, max, hdraw, aclabel, minprclabel, maxprclabel, base, ffbutton, qfbutton, spbutton, auto, manual, picktable, clin, clog, cexp, gammalabel, gammaval
COMMON XCD_DATA, cmin, cmax
COMMON XCD_REGION, rflag, rx0, rx1, ry0,ry1
;;
;; context menu of the histogram area
;; - changes contrast setting
;;  - as a consequence the histogram needs to be redisplayed
;;
sdevrange=2.
submode='undef'
roi='quarter'
hilo=[0.,1.]
;; IF NOT(GetStackContrastMode(SUBMODE=submode, ROI=roi, SRANGE=srange, HILO=hilo ) EQ 'auto') THEN BEGIN
 ;; we are not in auto mode
;;   ErrMsg, ["The currently focussed data stack is configured for manual contrast settings.", $
;;            "Please activate the automatic contrast mode in the Preferences tabulator in ", $
;;            "order to use the automatic contrast scaling feature."]            
;;END
ptr=GetRootP()
IF (NOT(PTR_VALID(ptr))) THEN BEGIN
   print, "% HDrawCtxtMenu: Root pointer is invalid"
   return
END
p=(*ptr).current
IF (NOT(PTR_VALID(p))) THEN BEGIN
   print, "% HDrawCtxtMenu: Current pointer is invalid."
   return
END
;;
e=(*p).datap
IF PTR_VALID(e) THEN BEGIN
   submode='undef'
   frameroi='undef'
   sdevrange=2.
   hilo=[0.,1.]
   mode=GetStackContrastMode(SUBMODE=submode, ROI=frameroi, SRANGE=srange) ;;
   ;; region of interest for histogram calculation:
   ;; frm=[X0,X1,Y0,Y1] 2D image coordinates
   ;; default= full frame
   ;; rflag=2: quarter frame
   
   CASE (*e).zcoord OF
      3: BEGIN
         frm=[0,(*e).SzX-1,0,(*e).SzY-1] ;; full frame
         IF (frameroi EQ 'quarter') THEN frm=[FLOOR((*e).SzX*0.25),FLOOR((*e).SzX*0.75),FLOOR((*e).SzY*0.25),FLOOR((*e).SzY*0.75)]
      END
      2:BEGIN                 
         frm=[0,(*e).SzX-1,0,(*e).SzZ-1]
         IF (frameroi EQ 'quarter') THEN frm=[FLOOR((*e).SzX*0.25),FLOOR((*e).SzX*0.75),FLOOR((*e).SzZ*0.25),FLOOR((*e).SzZ*0.75)]
      END
      1: BEGIN
         frm=[0,(*e).SzY-1,0,(*e).SzZ-1]
         IF (frameroi EQ 'quarter') THEN frm=[FLOOR((*e).SzY*0.25),FLOOR((*e).SzY*0.75),FLOOR((*e).SzZ*0.25),FLOOR((*e).SzZ*0.75)]
      END 
      ELSE:
   ENDCASE
END 
;;
WIDGET_CONTROL, ev.id, GET_UVALUE = uv
;;print, uv
;; one of the butons was pressed
CASE uv OF
   'sdev': BEGIN
      ;; sdev mode
      SetStackContrastMode, /SDEV ;; default is sdev
      ;; auto contrast, mode sdev
   END  
   'minmax':  BEGIN ;; returns min and max of image 
      ;; sdev mode
      SetStackContrastMode, /MINMAX ;; default is sdev
      ;; auto contrast, mode sdev
      ;; SetContrast, e, FRAME=frm, MODE='minmax'

   END   
   'mdev':  BEGIN ;; returns min and max of image 
      ;; sdev mode
      SetStackContrastMode, /MDEV ;; default is sdev
      ;; auto contrast, mode sdev
      ;; SetContrast, e, FRAME=frm, MODE='mdev'
   END   
   'diff':  BEGIN                        ;; returns min and max of image 
      ;; sdev mode
      SetStackContrastMode, /DIFF ;; default is sdev
      ;; auto contrast, mode sdev
      ;; SetContrast, e, FRAME=frm, MODE='diff'
   END
   'histeq': SetStackContrastMode, /HISTEQUAL ;; default is sdev
   'clip':  BEGIN ;; returns min and max of image 
         XConsole_PushState
         XConsole_Busy
         TypeCastVol, 'byte'
         SetStackContrastMode, /AUTO, /MINMAX ;; default is min max
         XConsole_PopState
      END
    'repstat':  BEGIN ;; returns min and max of image 
       ReportStatistics
       return
      END    
   ELSE:
 END
TVDisplay, /ALLOWMANUALOVERWRITE ;; This routine will set the contrast cut offs in auto mode but not in manual mode.
;; ALLOW once here to do the same in manual mode as in auto mode, since the user asked for it
print, "% Hdrawctxtmenu: contrast="+MyString((*e).contrast[0])+":"+ MyString((*e).contrast[1])
Update_XTabContrastDialog
END   
     

Pro hdraw_event, event
COMMON XCD_WIDGETS, min, max, hdraw, aclabel, minprclabel, maxprclabel, base, ffbutton, qfbutton, spbutton, auto, manual, picktable, clin, clog, cexp, gammalabel,gammaval
COMMON XCD_DRAW, lower_x, upper_x
COMMON XCD_DATA, cmin, cmax
IF (TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_DRAW') THEN BEGIN
   IF (event.release EQ 4) THEN BEGIN
      WIDGET_DISPLAYCONTEXTMENU, event.id, event.x,event.y, WIDGET_INFO(event.id, FIND_BY_UNAME='hdrawctxtmenu')
   END
   IF (event.PRESS AND NOT(event.RELEASE)) THEN lower_x=event.X
   IF (NOT(event.PRESS) AND (event.RELEASE)) THEN BEGIN 
      upper_x=event.X
      ;;       print, lower_x, " ", upper_x
      if (upper_x LT lower_x) THEN BEGIN
         tmp=lower_x & lower_x=upper_x & upper_x=tmp
      END
      WIDGET_Control, hdraw, GET_VALUE=winindex
      WSET, winindex
      ;; get drawable size
      Xsize=!D.X_VSIZE
      cmin=FLOAT(lower_x)/FLOAT(XSize)*(cmax-cmin)+cmin
      cmax=FLOAT(upper_x)/FLOAT(XSize)*(cmax-cmin)+cmin
      ;; Values will only be applied in manual mode!
      mode=GetStackContrastMode()
      IF (mode NE 'auto') THEN BEGIN
         ptr=GetRootP()
         IF (PTR_VALID(ptr)) THEN BEGIN
            p=(*ptr).current
            IF ((PTR_VALID(p))) THEN BEGIN
               e=(*p).datap
;;
               IF PTR_VALID(e) THEN BEGIN
                  print, "% HDraw_Event: Setting stack contrast to ", [cmin,cmax]
                  (*e).contrast[0]=cmin & (*e).contrast[1]=cmax                
                  Update_XTabContrastDialog
                  TVDisplay
               END 
            END  
         END  
      END ELSE BEGIN
         printtocon, "% hdraw_event: Contast Mode is not Manual: "+MyString(mode)
      END
   END 
END   
return
END  


Pro Close_XTabContrastDialog
COMMON XCD_WIDGETS, min, max, hdraw, aclabel, minprclabel, maxprclabel, base, ffbutton, qfbutton, spbutton, auto, manual, picktable, clin, clog, cexp, gammalabel,gammaval
    WIDGET_CONTROL, base, /DESTROY
    return
END

PRO XTabContrastDialog_event, ev
COMMON XCD_WIDGETS, min, max, hdraw, aclabel, minprclabel, maxprclabel, base, ffbutton, qfbutton, spbutton, auto, manual, picktable, clin, clog, cexp, gammalabel,gammaval
COMMON CONTRASTPREFS, sd, minprc, maxprc
COMMON XCD_DATA, cmin, cmax
COMMON XCD_REGION, rflag, rx0, rx1, ry0,ry1
;; print, "uv=", uv
IF (TAG_NAMES(ev, /STRUCTURE_NAME) EQ 'WIDGET_TAB') THEN BEGIN
   ;; print, "Tab event"
   Update_XTabContrastDialog
   ;; histogram tab was touched
   ;;
END ELSE IF (ev.id EQ gammaval) THEN BEGIN
     WIDGET_Control, gammaval, GET_VALUE=x
     x=FLOAT(x(0))
     IF (x GT 0) THEN BEGIN
        gamma=x 
     END ELSE BEGIN
        gamma=1.
        WIDGET_Control, gammaval, SET_VALUE=STRING(gamma)
        printtocon, "Gamma value has to be > 0. Setting value to  "+MyString(gamma)+"."
     END 
     SetContrastScaling, GetContrastScaling(), GAMMA=gamma
  END  ELSE BEGIN
   WIDGET_CONTROL, ev.id, GET_UVALUE = uv
 ;;  Widget_Control, ev.top, Get_UValue=info, /No_Copy
 ;;  STOP

CASE uv OF
   'auto-mode': BEGIN
      ;; automatic mode: the interactive functions are deactivated,
      ;; auto contrast is active, holds in general for every stack!
      automode=1 & manualmode=0
   END
   'manual-mode': BEGIN
      ;; automatic mode: the interactive functions are deactivated,
      ;; auto contrast is active, holds in general for every stack!
      SetContrastMode, 'manual'
      print, "Setting manual contrast mode"
      automode=0 & manualmode=1
   END
    'acsdev':BEGIN
       sdevrange=0
        x=''
        WIDGET_Control, aclabel, GET_VALUE=x
        x=FLOAT(x(0))
        IF (x GT 0) THEN BEGIN
            sd=x 
         END
        sdevrange=x
    END
  'apply':  BEGIN
                                ; Read new data from Widgets
      TMP=0.0 & xmin=0
      WIDGET_Control, min, GET_VALUE=TMP
      xmin=float(TMP[0])
      TMP=0.0 & xmax=0
      WIDGET_Control, max, GET_VALUE=TMP
      xmax=FLOAT(TMP[0])
      IF (xmax LT xmin) THEN BEGIN
          TMP=xmax
          xmax=xmin
          xmin=TMP
      ENDIF 
      ;; printtocon, "xmin="+MyString(xmin)
      ;; printtocon, "xmax="+MyString(xmax)
      hilo=[xmin,xmax]
      automode=0 & manualmode=1
      SetStackContrastMode, /MANUAL, HILO=hilo
      WIDGET_CONTROL, manual, /SET_BUTTON
      ;; STOP
      
   END     
  'fullframe': BEGIN
      rflag=0
      sparse=0 & full=1 & quarter=0 & diff=0
   END
  'diff': BEGIN
      rflag=3
      full=0 & sparse=0 & diff=1 & quarter=0
  END
  'quarterframe': BEGIN
      rflag=2
      sparse=0 & quarter=1 & full=0 & diff=0
   END
  'sparse': BEGIN
     rflag=1
     sparse=1 & quarter=0 & full=0 & diff=0
  END
  'help': BEGIN
     help_text = [" Use this dialog to set the pixel data values "," which correspond to the smallest and the "," largest color index. "," Pressing on 'apply' will redisplay the image. "," Use 'update' when the image focus has changed. "]   
     msg = dialog_info(help_text, /Info)
     return
  END
  'colortable':BEGIN
     ;;
      ctable='no'
       DEVICE, Get_Visual_Depth=vdepth
       if (vdepth EQ 8) then ctable='yes'
       if (vdepth GT 8) THEN BEGIN
           DEVICE, Get_Decomposed=decomp
           if (decomp GT 0) THEN BEGIN 
              DEVICE, Decomposed=0
              ctable='yes'
           END
        END ELSE ctable='yes'
       IF (ctable EQ 'yes') THEN BEGIN
          ;; make pick color table active
          WIDGET_CONTROL, picktable, SENSITIVE=1
          ;; load color table
          SET_DEFAULT_COLORTABLE
          set_default_colormode, 0
       END
       
    END  
  
  'decomposed':BEGIN
     ;;
     DEVICE, Decomposed=1
     set_default_colormode, 1
     ;; make pick color table button inactive
     WIDGET_CONTROL, picktable, SENSITIVE=0
  END
  'clin': SetContrastScaling, 'lin' ;; default is 'lin'
  'clog': SetContrastScaling, 'log' ;; 
  'cexp': SetContrastScaling, 'exp' ;;
  'cgamma': BEGIN
     WIDGET_Control, gammaval, GET_VALUE=x
     x=FLOAT(x(0))
     IF (x GT 0) THEN BEGIN
        gamma=x 
     END ELSE BEGIN
        gamma=1.
        WIDGET_Control, gammaval, SET_VALUE=STRING(gamma)
     END
     SetContrastScaling, 'gamma', GAMMA=gamma
  END
  'picktable': XColors
  'close': BEGIN
     Widget_Control, ev.top, /DESTROY 
     return
  END
  ELSE: BEGIN
     printtocon, "% XTabContrastDialog_event: UValue "+uv+" not implemented yet."
  END
ENDCASE
IF PTR_VALID(GetCurrentP()) THEN BEGIN
   SetStackContrastMode, AUTO=automode, MANUAL=manualmode, QUARTER=quarter, FULL=full, SRANGE=srange
   TVDisplay
END
Update_XTabContrastDialog
Update_XTabControl
END    
;; Update DataStackInfo and display image
END  

FUNCTION XTabContrastDialogUpdateContrastRange, hilo
  COMMON XCD_WIDGETS, min, max, hdraw, aclabel, minprclabel, maxprclabel, base, ffbutton, qfbutton, spbutton, auto, manual, picktable, clin, clog, cexp, gammalabel,gammaval
  WIDGET_Control, min, SET_VALUE=FLoat(hilo[0])
  WIDGET_Control, max, SET_VALUE=FLoat(hilo[1])
  ;; Update_XTabContrastDialog
  ;; Update_XTabControl
  return, 1
END

FUNCTION XTabContrastDialogGetContrastRange
  COMMON XCD_WIDGETS, min, max, hdraw, aclabel, minprclabel, maxprclabel, base, ffbutton, qfbutton, spbutton, auto, manual, picktable, clin, clog, cexp, gammalabel,gammaval
  ;; Update_XTabContrastDialog
  ;; Update_XTabControl
  lo=0. & hi=1.
  WIDGET_Control, min, GET_VALUE=lo
  WIDGET_Control, max, GET_VALUE=hi
  return, FLOAT([lo,hi])
END


Pro XTabContrastDialog
COMMON XCD_WIDGETS, min, max, hdraw, aclabel, minprclabel, maxprclabel, base, ffbutton, qfbutton, spbutton, auto, manual, picktable, clin, clog, cexp, gammalabel, gammaval
COMMON XCD_DATA, cmin, cmax
COMMON XCD_REGION, rflag, rx0, rx1, ry0,ry1
COMMON CONTRASTPREFS, sd, minprc, maxprc
COMMON DISPLAYSCOPE, offsetx, offsety, sizex, sizey, MonNum, MonNames, MonRects, MonPrim
   submode='minmax' & roi='quarter' & sdevrange=2. & hilo=[0.,1.]
   mode=GetStackContrastMode(SUBMODE=submode, ROI=roi, SRANGE=srange, HILO=hilo) 
   quit = 0
   base = Widget_BASE(TITLE='Contrast Inspector', /COLUMN,GROUP_LEADER=WidgetID(0,/GETGROUPLEADERTOP))
   wTab = WIDGET_TAB(base, LOCATION=0)
   ScreenSizeFactor=1
   XW=FIX(280.*ScreenSizeFactor)
   XH=FIX(180.*ScreenSizeFactor)
   ;;
   ;; Tab: histogram
   ;;   

   Tab2 = WIDGET_BASE(wTab, Title='  Histogram  ', /COLUMN)  

   hdraw = WIDGET_DRAW(Tab2, /Button_Events, EVENT_PRO='hdraw_event', RENDERER=1, Graphics_Level=0, SCR_XSIZE=XW, SCR_YSIZE=XH, UNITS=0, SCROLL=0)
   hcontextbase=WIDGET_BASE(hdraw, /CONTEXT_MENU, uname = 'hdrawctxtmenu')
   hdrawb1=WIDGET_BUTTON(hcontextbase, VALUE = 'Standard deviation', UVALUE = 'sdev', EVENT_PRO='HDrawCtxtMenu')
   hdrawb1=WIDGET_BUTTON(hcontextbase, VALUE = 'Median deviation', UVALUE = 'mdev', EVENT_PRO='HDrawCtxtMenu')
   hdrawb2=WIDGET_BUTTON(hcontextbase, VALUE = 'Min to Max', UVALUE = 'minmax', EVENT_PRO='HDrawCtxtMenu')
   hdrawb2=WIDGET_BUTTON(hcontextbase, VALUE = 'Diff', UVALUE = 'diff', EVENT_PRO='HDrawCtxtMenu')
   hdrawb2=WIDGET_BUTTON(hcontextbase, VALUE = 'HistEqual', UVALUE = 'histeq', EVENT_PRO='HDrawCtxtMenu')
   hdrawb2=WIDGET_BUTTON(hcontextbase, VALUE = 'Report Statistics', UVALUE = 'repstat', EVENT_PRO='HDrawCtxtMenu',  /SEPARATOR)

  
   hdrawb2=WIDGET_BUTTON(hcontextbase, VALUE = 'Create Stack with Clipped Contrast', UVALUE = 'clip', EVENT_PRO='HDrawCtxtMenu')

   Tab0 = WIDGET_BASE(wTab, Title=' Preferences ', /COLUMN)

   row0 = WIDGET_BASE(Tab0, /ROW)
   
   text=Widget_Label(Tab0, VALUE="")
   input2 =  WIDGET_BASE(Tab0, /ROW, XPAD=0)
   text=Widget_Label(input2, VALUE="Auto Contrast Range (SDEV): ", /ALIGN_LEFT)
   aclabel = CW_Field(input2, TITLE="", XSize=4, Value=MySTRING(sdevrange))
   ;; acsdev =  WIDGET_BUTTON(input2b, VALUE="  Apply  ", UVALUE='acsdev') 
   ;;
   cbuttons = WIDGET_BASE(Tab0,/COLUMN)
   text=Widget_Label(cbuttons, VALUE="", /ALIGN_LEFT)
   text=Widget_Label(cbuttons, VALUE="Frame for Moment Calculation:", /ALIGN_LEFT)
   togglebase2 = WIDGET_BASE(cbuttons, /COLUMN ,/EXCLUSIVE)
   ffbutton = WIDGET_BUTTON(togglebase2, VALUE='Full Frame', UVALUE='fullframe', /NO_RELEASE)	
   qfbutton = WIDGET_BUTTON(togglebase2, VALUE='Quarter Frame', UVALUE='quarterframe', /NO_RELEASE)	
   spbutton = WIDGET_BUTTON(togglebase2, VALUE='Sparse Grid', UVALUE='sparse', /NO_RELEASE)	
   diffbutton = WIDGET_BUTTON(togglebase2, VALUE='Diffraction', UVALUE='diff', /NO_RELEASE)

   Tab01 = WIDGET_BASE(wTab, Title=' Scaling ', /COLUMN)

  
 
   cbuttons = WIDGET_BASE(Tab01,/COLUMN)
   text=Widget_Label(cbuttons, VALUE="", /ALIGN_LEFT)
   togglebase01 = WIDGET_BASE(cbuttons, /COLUMN ,/EXCLUSIVE)
   clin = WIDGET_BUTTON(togglebase01, VALUE='Linear', UVALUE='clin', /NO_RELEASE)	
   clog = WIDGET_BUTTON(togglebase01, VALUE='Logarithmic', UVALUE='clog', /NO_RELEASE)	
   cexp = WIDGET_BUTTON(togglebase01, VALUE='Exponential', UVALUE='cexp', /NO_RELEASE)	
   gammalabel = WIDGET_BUTTON(togglebase01, VALUE='Gamma correction', UVALUE='cgamma', /NO_RELEASE)
   gammavalue=1.
   m=GetContrastScaling(GAMMA=gammavalue)
   CASE m OF
      'gamma:': WIDGET_CONTROL, gamma, /SET_BUTTON 
      'exp:': WIDGET_CONTROL, cexp, /SET_BUTTON 
      'log': WIDGET_CONTROL, clog, /SET_BUTTON 
      ELSE: WIDGET_CONTROL, clin, /SET_BUTTON 
   END
   row01 = WIDGET_BASE(Tab01, /ROW)
   text=Widget_Label(row01, VALUE="    ")
   gammaval = CW_Field(row01, TITLE="Gamma", XSize=4, Value=MySTRING(gammavalue),/RETURN_EVENTS)  
   
Tab1 = WIDGET_BASE(wTab, Title=' Colors ', /COLUMN)

   cbuttons11 = WIDGET_BASE(Tab1,/COLUMN)
   text11=Widget_Label(cbuttons11, VALUE="   ", /ALIGN_LEFT)
   text11=Widget_Label(cbuttons11, VALUE="Color Model", /ALIGN_LEFT)
   togglebase11 = WIDGET_BASE(cbuttons11, /COLUMN ,/EXCLUSIVE)
   decbutton = WIDGET_BUTTON(togglebase11, VALUE='True Color', UVALUE='decomposed', /NO_RELEASE)	
   cmbutton = WIDGET_BUTTON(togglebase11, VALUE='Pseudocolor', UVALUE='colortable', /NO_RELEASE)
   ;;
   decomposed=1
   DEVICE, Get_Visual_Depth=vdepth
   if (vdepth EQ 8) then decomposed=0
   if (vdepth GT 8) THEN BEGIN
      DEVICE, Get_Decomposed=decomposed
   END
   ;;
   IF (decomposed EQ 0) THEN WIDGET_CONTROL, cmbutton, /SET_BUTTON ELSE  WIDGET_CONTROL, decbutton, /SET_BUTTON
   ;;
   sensitive=1
   IF (decomposed EQ 1) THEN sensitive=0
   cbuttons12 = WIDGET_BASE(Tab1,/ROW)
   text11=Widget_Label(cbuttons12, VALUE="  ", /ALIGN_LEFT)
   picktable = Widget_Button(cbuttons12, Value=' Pick color table ', UVALUE='picktable')
   WIDGET_CONTROL, picktable, SENSITIVE=sensitive
   ;;
   input =  WIDGET_BASE(Tab2, /ROW, XPAD=0)
   togglebase = WIDGET_BASE(input, /ROW ,/EXCLUSIVE)
   auto=WIDGET_BUTTON(togglebase, VALUE="Auto", UVALUE="auto-mode", /NO_RELEASE)
   manual=WIDGET_BUTTON(togglebase, VALUE="Manual", UVALUE="manual-mode", /NO_RELEASE)
   IF (mode EQ 'auto') THEN WIDGET_CONTROL, auto, /SET_BUTTON ELSE  WIDGET_CONTROL, manual, /SET_BUTTON
   
   min = CW_FIELD(input, TITLE='', XSIZE=8, VALUE=STRING(hilo[0]), /ROW)
   max = CW_FIELD(input, TITLE=':', XSIZE=8, VALUE=STRING(hilo[1]), /ROW)

   inputb =  WIDGET_BASE(base, /ROW, XPAD=0)
   apply =  WIDGET_BUTTON(inputb, VALUE=' Apply ', UVALUE='apply') 
   close =  WIDGET_BUTTON(inputb, VALUE='Close', UVALUE='close')
  
   CASE roi OF
      'quarter': WIDGET_CONTROL, qfbutton, /SET_BUTTON 
      'sparse': WIDGET_CONTROL, spbutton, /SET_BUTTON
      'diff': WIDGET_CONTROL, diffbutton, /SET_BUTTON 
      ELSE: WIDGET_CONTROL, ffbutton, /SET_BUTTON
   END

;;    info = {  AutoContrast:auto, $ ; The WID of the auto contrast radiobox.
 ;;             ManualContrast:auto, $    ; The WID of the manual contrast radiobox.
;;              PickCT:picktable}            ; The widget ID of the picktable button
   
   Widget_Control, base, XOFFSET=offsetx, YOFFSET=630+offsety, /REALIZE
;;    Widget_Control, base, Set_UValue=info, /No_Copy
   ;; Update_XTabContrastDialog
   cmin=hilo[0] & cmax=hilo[1]
   XManager, 'XTabContrastDialog', base, /NO_BLOCK  
END












