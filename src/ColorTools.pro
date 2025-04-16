PRO Polar24bitImage, arg, mag, FROMCT=fromct, LIGHT=light, SDEV=sdev, SAVE=save, HSV=hsv, YUV=yuv, MULTIPLICITY=multiplicity, IDLIIM=idliim, HLS=hls, YPbPr=ypbpr
;; arg is polar angle map, from -pi to +pi
;; mag is the magnitude, will be  normalized to 0 ... 1 between min
;; and max by default od +-2 times standard deviation if keyword sdev
;; is set
;; light is an optional saturation map, from 0 to 1
;; save: fileneame, if set then the file save will be written,
;; otherwise the routine is interactive
;; the routine translates these images to a decomposed 24 bit rgb
;; image based on the HSV color system
;; if multiplicity is larger than 1 then the rotation is modulo PI for multiplicity 2, modulo PI/2 for multiplicity 4 etc.
;;
;; TODO: take current color map and map it onto the angular map. Use the value to scale the map 
;;
  hue=(arg+!PI)*180./!Pi ;; rad to deg from 0 to 360
  val=FLoat(mag)
  title=""
  IF keyword_set(multiplicity) THEN hue=FIX(multiplicity)*(hue MOD FIX(360/multiplicity))
  if keyword_set(SDEV) THEN BEGIN
     m=Robust_Mean(val, 3,  dev, num) 
     mult=2.
     minval=Max([min(val),m-mult*dev])
     maxval=Min([max(val),m+mult*dev])
     val=(val-minval)/(maxval-minval)
     b=WHERE(val LT 0, count)
     if (count GT 0) THEN val[B]=0
     b=WHERE(val GT 1, count)
     if (count GT 0) THEN val[B]=1
  END ELSE  val=(val-Min(val))/(Max(val)-Min(val)) ;; val is scaling from 0 to 1
  N=Size(arg)
  IF not(Keyword_set(light)) THEN BEGIN
     light=FLTARR(N[1],N[2],/NOZERO)
     light[*,*]=1.
  END
  IF keyword_set(yuv) THEN BEGIN
     ;; transform arg into yv plane
     y=0.5*(1.+Sin(hue/180.*!PI))
     v=0.615*Cos(hue/180.*!PI)
     COLOR_CONVERT, y*val, val, v*val, r, g, b, /YUV_RGB
     title="YUV image"
  END 
  IF keyword_set(hls) THEN BEGIN
     COLOR_CONVERT, hue, light, val, r, g, b, /HLS_RGB
     ;; COLOR_CONVERT, hue, val, light, r, g, b, /HSV_RGB
     title="HLS image"
  END
  IF keyword_set(fromct) THEN BEGIN
     ;; take from current color table
     tvlct,ctr,ctg,ctb,/get
     Nel=SIZE(ctr, /N_Elements)
     ;; now we have the r g b vectors with NEl elements that we'll map the hue to
     ;; now let's construct the r g b channel data for the images
     ;; angle to index
     ;; val is saturation as a multiplier
     ;; hue ranges from 0 to 360
     indexmap=Fix(hue/360.*(Nel-1))
     r=val*ctr[indexmap]
     g=val*ctg[indexmap]
     b=val*ctb[indexmap]
     title="ct"
  END
  IF keyword_set(ypbpr) THEN BEGIN
     ;; Luma ChromaBlue ChromaRed (analog). Y is a floating-point value between 0 and 1, Pb and Pr are floating-point values between -0.5 to 0.5.
     ;; value or luma is the greyscale
     ;; pb is the blue to yellow component -0.5 = blue 0.5 = yellow
     ;; pr is the cyan to pink component for chroma=1
     ;; kr, kg, kb are the r gb coefficients fot the greysvcale conversion
     ;; the following relationship will help to connect value and greyscale:
     ;; white is Y=1, Pb=0, Pr=0
     ;; black is Y=0, Pb=0, Pr=0
     ;; R' = Y' + 2(1 − Kr) Pr
     ;; or: Y'=R'-2(1-kr)Pr where R' is kr*val
     kr=0.299 & kg=0.587 & kb=0.114
     ;; all values were given for equal rgb, so we have to recalculate
     ;; cb=hue/360.-0.5
     cb=0.5*((cos(hue/180.*!Pi)))
     cb=2*cb*val*(-0.5*kr/(1-kb)-0.5*(kg/(1-kb)))
     ;; cr=hue/360.-0.5
     cr=0.5*((sin(hue/180.*!Pi)))
     cr=2*cr*val*(0.5*kr-0.5*kg/(1-kr)-0.5*kb/(1-kr))
     cr=cr/(Max(cr)-Min(cr))
     ;; cr=cb*0
     ;; calculate chroma from cb and cr
     val=val ;; -2*(1-kr)*cr
     ;; STOP
     COLOR_CONVERT, val, cb , cr, r, g, b, /YPBPR_RGB
     ;; COLOR_CONVERT, hue, val, light, r, g, b, /HSV_RGB
     title="YPbPr image"
  END
  IF (title EQ "") THEN BEGIN ;; default is HSV
  ;; IF keyword_set(hsv) THEN BEGIN
     COLOR_CONVERT, hue, light, val, r, g, b, /HSV_RGB
     ;; COLOR_CONVERT, hue, val, light, r, g, b, /HSV_RGB
     title="HSV image"
  END
  im=BYTARR(3,N[1],N[2])
  im[0,*,*]=r &  im[1,*,*]=g &   im[2,*,*]=b
  Device, Get_Decomposed=currentState
  Device, Decomposed=1
  ;; display image in 24 bit
  Window, XSIZE=N[1], YSIZE=N[2], TITLE=title
  TV, im, True=1
  ;;; STOP
  if NOT(keyword_set(save)) THEN BEGIN
     swapend=0
     crgrpd=1
     type=0
     filter="*"
     ftypes=PTR_NEW(["PNG"])
     files = rmd_pickfile(  $
;;  group_leader = event.top,     $
             filter_in = filter,              $
             path=GetWorkingDir(), $
             get_path = out_path,          $
             cancelled = cancelled,        $
             swapen = swapend,             $
             type = type,             $
             ftypes = ftypes,     $
             crgrp = crgrpd,             $
             /multiple, $
             /save)
     if NOT(cancelled EQ 1) then begin
        fname=files[0]
        type=(*ftypes)[type]
        Write_PNG, fname, im, r, g, b, _Extra=extra
     END
  END ELSE BEGIN
     Write_PNG, save, im, r, g, b, _Extra=extra
  END
  if keyword_set(idliim) THEN iImage, im
  Device, Decomposed=currentState
END


PRO OrientationMapColoring
 p1=GetCurrentP()
 p2=GetCurrentP()
 title='Select magnitude and orientation image'
 llabel='magnitude'
 rlabel='orientation'
 help=["Polar Image Coloring",$
          "", $
          "Select the polar data arrays for the magnitude and the azimuth data.", $
          "The azimuth data will be mapped to color and the magnitude data to mix the grey level. ", $
          "The azimuth data is required to scale between -Pi and +Pi. Both arrays need to have ", $ 
          "the same size.", $
         ""]
 IF (XSelectImages(p1,p2, TITLE=title, LLABEL=llabel, RLABEL=rlabel, HELP=help) GT 0) THEN BEGIN
    arg=(*p2).datap
    mag=(*p1).datap
    sdev=1
    s=list()
    s.Add, {value:2,label:"Multiplicity (1,2,3,..6,..)",newrow:0B}
    c=list()
    c.Add, {value:0B,label:"Use Current Colortable",newrow:1B}
    c.Add, {value:1B,label:"Apply Chroma from Contrast Cut-Offs",newrow:1B}
    help=["Polar Image Coloring",$
          "", $
          "The default color scheme for polar images uses the HSV color cone and a saturation S  set to 1.", $
          "The angle is mapped to H: hue=color, and the magnitude to V: value=chroma. ", $
          "The rainbow map of the HSV color table can be replaced by the colors of the current color table, ", $
          "in which case a cyclic anc perceptive color map can be selected. ", $
          "", $
          "Parameters:", $
               "", $
          "Multiplicity: Symmetry of the map, multiplicity 2 means Pi-periodic, 4 means Pi/2 periodic etc.", $
          "Use Current Colortable: Use the indexed colors of the currently selected colortable for coloring.", $
          "              Select a cyclic color table such as those provided in the color table file cyclic.tbl.", $
          "Apply Chroma from Contrast Cut-Offs: Use the current setting for the contrast cut-offs to clip the chroma.", $
          "              If not chosen then the scaling is based on the standard deviation in the magnitude image supplied.", $
         ""]
    IF (XMDataChoiceField(s, c, TITLE="Polar RGB Coloring", help=help) EQ 1) THEN BEGIN
       hsv=(c[0].value EQ 0B)
       yuv=0
       fromct=(c[0].value EQ 1B)
       multiplicity=s[0].value
       sdev=1
       IF (c[1].value EQ 1B) THEN BEGIN
          hilo=GetStackContrastValues(STACKP=p1) ;; Take the data from the ContrastInspector values.
          magim=BYTSCL(*((*mag).data), MAX=hilo[1], Min=hilo[0], TOP=254)+1
          sdev=0
       END
       ;; check the scaling of the agular map
       argmin=Min(*((*arg).data),Max=argmax)
       IF ((argmin LT -!DPI) OR (argmax GT !DPI)) THEN BEGIN
          void=dialog_message("Angular arguments array value range exceeds [-PI,PI]" , /ERROR)
       END ELSE BEGIN
          Polar24bitImage, (*((*arg).data)), magim, FROMCT=fromct, MULTIPLICITY=multiplicity, LIGHT=light, SDEV=sdev, SAVE=0, HSV=hsv, YUV=yuv, YPbPr=ypbpr
       END
    END 
 END
END 

PRO RGBColorCircle, MULTIPLICITY=multiplicity, Mask=mask, HSV=hsv, YPbPr=ypbpr, FROMCT=fromct, NOCHROMA=nochroma
  r=256
  polarmap=AngleIm(r)
  if keyword_set(mask) THEN BEGIN
     rim=RadiusIm(r)
     maskim=rim *0 +1
     B=WHERE(rim GT r)
     if keyword_set(nochroma) THEN rim=rim*0.+1.
     maskim[B]=0
     ;; polarmap(B)=0.
     rim=rim*maskim
  END 
  t='ColorCircle('
  IF NOT(keyword_set(multiplicity)) THEN multiplicity=1
  t=t+"multipicity="+MyString(multiplicity)+")"
  hsv=1
  ct=0
  ypbpr=0
  if keyword_set(ypbpr) THEN BEGIN
     hsv=0 & ypbpr=1 & ct=0
  END
  if keyword_set(fromct) THEN BEGIN
     hsv=0 & ypbpr=0 & ct=1
  END
  Polar24bitImage, polarmap, rim, MULTIPLICITY=multiplicity, LIGHT=0, SDEV=0, SAVE=0, HSV=hsv, YUV=0, IDLIIM=0, HLS=0, YPbPr=ypbpr, FROMCT=fromct
END


PRO ColorCircle, MULTIPLICITY=multiplicity, Mask=mask
  r=256
  polarmap=AngleIm(r,MULTIPLICITY=multiplicity)
  if keyword_set(mask) THEN BEGIN
     rim=RadiusIm(r)
     maskim=rim *0 +1
     B=WHERE(rim GT r)
     maskim[B]=0
     ;; polarmap(B)=0.
     polarmap=polarmap*maskim
  END 
  t='ColorCircle('
  IF NOT(keyword_set(multiplicity)) THEN multiplicity=1
  t=t+"multipicity="+MyString(multiplicity)+")"
  ThrowImage, polarmap, TITLE=t
  Printtocon, "% ColorCircle: Hint - Use HSV colortable from matplotlibcolors to colorize." 
END
     
