FUNCTION InverseUnit, unit
  unittemplaterev=['1/km','1/m','1/mm','1/um','1/nm','1/pm','1/fm']
  unittemplate=Reverse(["fm","pm","nm","um","mm","m","km"])
IF (STRMID(unit, 0, 2) EQ '1/') THEN BEGIN
    ;; it is a reciproval space unit
   unitind=WHERE(unittemplaterev EQ unit)
   unitind=unitind[0]
   return, unittemplate[unitind]
 END ELSE BEGIN
    unitind=WHERE(unittemplate EQ unit)
    unitind=unitind[0]
   return, unittemplaterev[unitind]
 END
 ;;
END



FUNCTION FromUnitToAngstrom, value, unit
res=value
  unittemplate=["fm","pm","nm","um","mm","m","km"]
  unitind=WHERE(unittemplate EQ unit)
  unitind=unitind[0]
  IF (unitind GE 0) THEN BEGIN
     nmind=WHERE(unittemplate EQ "nm")
     diff=(unitind-nmind[0])
     res=value*10.^(diff*3)
     res=res*10
  END
  return, res
END

FUNCTION FromUnitToNm, value, unit
  res=value
  unittemplate=["fm","pm","nm","um","mm","m","km"]
  unitind=WHERE(unittemplate EQ unit)
  unitind=unitind[0]
  IF (unitind GE 0) THEN BEGIN
     nmind=WHERE(unittemplate EQ "nm")
     diff=(unitind-nmind[0])
     res=value*10.^(diff*3)
  END
  return, res
END

FUNCTION GetScaleBar, npix, samp, UNIT=unit, INVERT=invert, COLOR=color
 IF NOT(keyword_set(unit)) THEN unit='nm'
 fracsize=0.2  ;; fracsize is the target size for the scale bar as a fraction of the frame size
 ;; calculate scalebar dimension and size
 fullsize=ULONG(npix)*samp
  
 ;; STOP
 approxsize=fracsize*fullsize  ;; approxsize is the approximate size of the scale bar in real units
 IF (approxsize LE 0) THEN BEGIN
    printtocon, "% GetScaleBar: Illegal sampling data. Returning"
    return,  {label: 'uncalibrated', size: 0, color:!D.N_COLORS-1}
 END
 ;; iterate until the approximate size (mantissa) is between 1 and 10 
 digits=0
 While (approxsize LT 1.) DO BEGIN
    digits -= 1
    approxsize = approxsize*10
 END
 While (approxsize GT 10.) DO BEGIN
    digits += 1
    approxsize = approxsize/10
 END
 ;; now we have a mantissa and an exponent
 ;; the mantissa is between 1 and 10. 
 label=[1,2,3,5,10]
 dist=ABS(approxsize-label) ;; recalculate the pixel size of the scale bar
 lab=label(WHERE (dist EQ (MIN(dist)))) ;; which of the labels comes closest?
 lab=lab[0] 
 scalebarlen=CEIL(FLOAT(lab)/approxsize*fracsize*npix)
 IF (lab EQ 10) THEN BEGIN  
    lab=1
    digits +=1
 END
 unitinc=digits/3
 unitincmod=digits MOD 3
 CASE unitincmod OF
      -2:BEGIN
         unitinc -= 1
         digits = 1
      END
 ;;   2: BEGIN
 ;;      unitinc += 1
 ;;      digits= -1
 ;;   END
    ELSE: digits=unitincmod
 END
 IF (STRMID(unit, 0, 2) EQ '1/') THEN BEGIN
    ;; it is a reciproval space unit
    unittemplate=['1/km','1/m','1/mm','1/um','1/nm','1/pm','1/fm']
 END ELSE BEGIN
    unittemplate=["fm","pm","nm","um","mm","m","km"]
 END
 ;;
 unitind=WHERE(unittemplate EQ unit)
 unitind=unitind[0]
 IF (unitinc NE 0) THEN BEGIN
    unitind=unitind+unitinc
    WHILE (unitind LT 0) DO BEGIN
       digits+=-3
       unitind += 1
    END
    WHILE (unitind GE N_Elements(unittemplate)) DO BEGIN
       digits+=3
       unitind -= 1
    END
 END 
 IF (digits EQ 0) THEN BEGIN
    labelstring=STRCOMPRESS(lab)
 END ELSE BEGIN
    IF (digits LT 0) THEN BEGIN
       labelstring='0.'
       WHILE (digits LT -1) DO BEGIN
          labelstring=labelstring+'0'
          digits += 1
       END
       labelstring=labelstring + STRCOMPRESS(lab, /REMOVE_ALL)
    END ELSE BEGIN 
       IF (digits GE 0) THEN BEGIN
          labelstring=STRCOMPRESS(lab, /REMOVE_ALL)
          WHILE (digits GT 0) DO BEGIN
             labelstring=labelstring+'0'
             digits -= 1
          END
       END
    END
 END
 unitlab=unittemplate(unitind)
 IF (STRMID(unit, 0, 2) EQ '1/') THEN BEGIN
    ;; strip off 1/ and replace by ^-1
    unitlab=STRMID(unitlab,2,STRLEN(unitlab)-2)
    unitlab=unitlab+'!U-1'
 END
 IF keyword_set(invert) THEN col=0 ELSE col=!D.N_COLORS-1
 IF keyword_set(color) THEN col=color
 return, {label:STRTRIM(labelstring, 2)+" "+unitlab, size:scalebarlen, color:col}
END


FUNCTION ScaleMarkPos, dimx, dimy, binning, scalebarlen, textwidth, charsize, POS=pos, VERTGAP=vertgap
  ;; dimx, dimy = image dimensions
  ;; binning = binning factor
  ;; scalebarlen = size of scalebar (original image dimensions, not
  ;; binned)
  ;; textwidth = textwidth in pix
  ;; charsize = character size in pix (height)
  ;; pos = 'll' for lower left
  res={xbar:0, ybar:0, xlab:0, ylab:0}
  if not(keyword_set(pos)) THEN pos='ll'
  if not(keyword_set(vertgap)) THEN vertgap=1.0
  CASE pos OF
   'll': BEGIN
        ;; scale bar in the lower left
        ;; align scalebar relative to the label
        sw=ROUND(scalebarlen/binning)
        xind=ROUND((textwidth-sw)/2)
        IF (xind LT 0) THEN BEGIN
           ;; scalebar is longer than text
           res.xbar=dimx/20
           res.xlab=res.xbar-xind
           res.ylab=dimy/20
           res.ybar=res.ylab+ROUND(vertgap*charsize)
        END
        IF (xind GE 0) THEN BEGIN
           ;; scalebar is shorter than text
           res.xlab=dimx/20
           res.xbar=res.xlab+xind
           res.ylab=dimy/20
           res.ybar=res.ylab+ROUND(vertgap*charsize)
        END
     END
     'lr': BEGIN
        END
     ELSE:
  END
  return, res
END


PRO DrawScaleBar, s, POS=pos, WIDTH=width, WDOW=wdow, BINNING=binning, CHARSIZE=charsize
  currwinid=!d.window
  IF (keyword_set(wdow)) THEN winid=wdow ELSE winid=currwinid
  IF NOT(keyword_set(charsize)) THEN charsize=3
  ;; Get Text properties, Use buffer device
  mydevice = !D.NAME  
  SET_PLOT, 'Z'
  ;; DEVICE, SET_FONT='Helvetica Bold', /TT_FONT 
  ERASE
  XYOUTS, 0, 0, '!4'+s.label, WIDTH=thiswidth, FONT=1, CHARSIZE=charsize, /DEVICE
  thisheight=charsize*!D.Y_CH_SIZE
  SET_PLOT, mydevice  
  devicex= !d.x_size
  devicey= !d.y_size
  if not(keyword_set(pos)) THEN pos='ll'
  smpos=scalemarkpos(devicex, devicey, binning, s.size, thiswidth*devicex, thisheight)
  ;; DEVICE, SET_FONT='Helvetica Bold', /TT_FONT 
  XYOUTS, smpos.xlab, smpos.ylab, '!4'+s.label, FONT=1, COLOR = s.color, CHARSIZE=charsize, /DEVICE
  PLOTS, [smpos.xbar,smpos.xbar+s.size/binning],[smpos.ybar,smpos.ybar], COLOR = s.color , /DEVICE ,thick=charsize
  WSET, currwinid
END

FUNCTION ScaleBar::Init
;; barid: array of widget_draw ids
;; minv:  array of starting values 
;; maxv:  array of end values
 
 return, 1
END   

FUNCTION ScaleBar::Get, npix, samp, UNIT=unit
  self.npix=npix
 self.samp=samp
 IF keyword_set(unit) THEN self.unit=unit 
 fracsize=0.2
 ;; calculate scalebar dimension and size
 fullsize=ULONG(npix)*samp
 approxsize=fracsize*fullsize
 ;; iterate until the approximate size is between 1 and 10
 digits=0
 While (approxsize LT 1.) DO BEGIN
    digits -= 1
    approxsize = approxsize*10
 END
 While (approxsize GT 10.) DO BEGIN
    digits += 1
    approxsize = approxsize/10
 END
 ;; now we have a mantissa and an exponent
 label=[1,2,3,5,10]
 dist=ABS(approxsize-label) 
 lab=label(WHERE (dist EQ (MIN(dist))))
 self.scalebarlen=CEIL(FLOAT(lab)/approxsize*fracsize*npix)
 IF (lab EQ 10) THEN BEGIN
    lab=1
    digits +=1
 END
 unitinc=digits/3
 unitincmod=digits MOD 3
 CASE unitincmod OF
    -2:BEGIN
       unitinc -= 1
       digits = 1
    END
    2: BEGIN
       unitinc += 1
       digits= -1
    END
    ELSE: digits=unitincmod
 END
 
 unittemplate=["fm","pm","nm","um","mm","m","km"]
 ;;
 unitind=WHERE(unittemplate EQ self.unit)
 IF (unitinc NE 0) THEN BEGIN
    unitind=unitind+unitinc
    WHILE (unitind LT 0) DO BEGIN
       digits+=-3
       unitind += 1
    END
    WHILE (unitind GE N_Elements(unittemplate)) DO BEGIN
       digits+=3
       unitind -= 1
    END
 END 
 IF (digits EQ 0) THEN BEGIN
    labelstring=STRCOMPRESS(lab)
 END ELSE BEGIN
    IF (digits LT 0) THEN BEGIN
       labelstring='0.'
       WHILE (digits LT -1) DO BEGIN
          labelstring=labelstring+'0'
          digits += 1
       END
       labelstring=labelstring + STRCOMPRESS(lab, /REMOVE_ALL)
    END ELSE BEGIN 
       IF (digits GE 0) THEN BEGIN
          labelstring=STRCOMPRESS(lab, /REMOVE_ALL)
          WHILE (digits GT 0) DO BEGIN
             labelstring=labelstring+'0'
             digits -= 1
          END
       END
    END
 END
 self.label=labelstring+" "+unittemplate(unitind)
  return, {label:self.label, size:self.scalebarlen}
END

PRO ScaleBar::Draw, POS=pos, WIDTH=width, WDOW=wdow, BINNING=binning, CHARSIZE=charsize
  currwinid=!d.window
  IF (keyword_set(wdow)) THEN winid=wdow ELSE winid=currwinid
  IF NOT(keyword_set(charsize)) THEN charsize=3
  ;; Get Text properties, Use buffer device
  mydevice = !D.NAME  
  SET_PLOT, 'Z'
  ;; DEVICE, SET_FONT='Helvetica Bold', /TT_FONT 
  ERASE
  XYOUTS, 0, 0, '!4'+self.label, WIDTH=thiswidth, FONT=1, CHARSIZE=charsize, /DEVICE
  thisheight=charsize*!D.Y_CH_SIZE
  SET_PLOT, mydevice  
  devicex= !d.x_size
  devicey= !d.y_size
  if not(keyword_set(pos)) THEN pos='ll'
  CASE pos OF
     'll': BEGIN
        ;; scale bar in the lower left
        ;; align scalebar relative to the label
        sw=ROUND(self.scalebarlen/binning)
        xind=ROUND((thiswidth*devicex-sw)/2)
        IF (xind LT 0) THEN BEGIN
           ;; scalebar is longer than text
           xbar=devicex/20
           xlab=xbar-xind
           ylab=devicey/20
           ybar=ROUND(1.4*thisheight)
        END
        IF (xind GE 0) THEN BEGIN
           ;; scalebar is shorter than text
           xlab=devicex/20
           xbar=xlab+xind
           ylab=devicey/20
           ybar=ROUND(1.4*thisheight)
        END
     END
     'lr': BEGIN
        END
     ELSE:
  END
  ;; DEVICE, SET_FONT='Helvetica Bold', /TT_FONT 
  XYOUTS, xlab, ylab, '!4'+self.label, FONT=1, CHARSIZE=charsize, /DEVICE
  PLOTS, [xbar,xbar+sw],[ybar,ybar], COLOR = !D.N_COLORS-1 , /DEVICE ,thick=charsize
  WSET, currwinid
END

PRO ScaleBar::Cleanup
  ;; CATCH, Error_status
  ;; IF (Error_status NE 0) THEN BEGIN
  ;;    Print, "% ProgressBar::Cleanup: Fatal error message"
  ;;    Print, "%            " + !ERR_STRING
  ;;   CATCH, /Cancel‚
  ;;   return
  ;; END
  Print, "% ScaleBar::Cleanup: Cleaning up object information."
  obj_destroy, self
  return
END

pro ScaleBar__define
 void={ScaleBar,name:'ScaleBar', $
       npix:1024, $
       samp: 1., $
       unit: "nm", $
       maxprim: 5, $
       scalebarlen: 0, $
       label: "", $
       backgr: !P.BACKGROUND, $
       foregr: !P.COLOR $
       }
 return
end


 PRO TestScaleBar
   myscalebar=obj_new('ScaleBar')
   WSET, 1
   print, myscalebar->Get(512, 200, UNIT="nm")
   ERASE
   myscalebar->Draw, BINNING=1
   print, myscalebar->Get(512, 20, UNIT="nm")
   ERASE
   myscalebar->Draw, BINNING=1
   print, myscalebar->Get(512, 2, UNIT="nm")
   ERASE
   myscalebar->Draw, BINNING=1
   print, myscalebar->Get(512, 0.2, UNIT="nm")
   ERASE
   myscalebar->Draw, BINNING=1
   print, myscalebar->Get(512, 0.02, UNIT="nm")
   ERASE
   myscalebar->Draw, BINNING=1
   print, myscalebar->Get(512, 0.002, UNIT="nm")
   ERASE
   myscalebar->Draw, BINNING=1
   print, myscalebar->Get(512, 0.0002, UNIT="nm")
   ERASE
   myscalebar->Draw, BINNING=1
   print, myscalebar->Get(256, 0.02, UNIT="nm")
   ERASE
   myscalebar->Draw, BINNING=0.5
   print, myscalebar->Get(512, 0.02, UNIT="nm")
   ERASE
   myscalebar->Draw, BINNING=1
   print, myscalebar->Get(768, 0.02, UNIT="nm")
   ERASE
   myscalebar->Draw, BINNING=1.5
   print, myscalebar->Get(1024, 0.02, UNIT="nm")
   ERASE
   myscalebar->Draw, BINNING=2
   obj_destroy, myscalebar
END
