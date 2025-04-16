PRO Profile2DFindCentre, im, x, y, Kernel, INFO=info, PARINFO=parinfo, FITMAX=fitmax, FITMIN=fitmin, ERRIM=errim, FITDATA=fitdata, PARERR=parerr, FIXPRESET=fixpreset, ERRSTATE=errstate, PROFILE=profile, ALLOWTILT=allowtilt, QUIET=quiet, NITER=niter
;; COMMON Profile2DFitPrefs, err_est, sigma_arr, allowtilt, createcoordlist, profile, createpeakim, featuremap
;;
;; fit a Gaussian to the image im around x,y 
;; Kernel is an array marking the rectangular fit area and 
;; providing a mask by which im is multiplied
;;
IF NOT(GetDebugFlag()) THEN BEGIN 
   CATCH, Error_status
   IF (Error_status NE 0) THEN BEGIN
      PrintToCon, "% Profile2DFindCentre:  Fatal error "
      PrintToCon, "%   Error status  - " + STRING(error_status)
      PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
      XConsole_PopState
      CATCH, /Cancel
      return
   END
END

IF NOT(keyword_set(profile)) THEN profile='gaussian'
IF NOT(keyword_set(allowtilt)) THEN allowtilt=0

  Nim=SIZE(im)
  N=SIZE(Kernel)
  dx=N(1) & dy=N(2)
  x1=x-dx/2 & x2=x1+dx-1 
  y1=y-dy/2 & y2=y1+dy-1 
  ;; check box coordinates
  IF (x1 LT 0) THEN BEGIN
      x1=0 & x2=dx-1
  END ELSE BEGIN
      IF (x2 GE NIm[1]) THEN BEGIN
          x2=Nim[1]-1 & x1=x2-dx+1
      END
  END
  IF (y1 LT 0) THEN BEGIN
      y1=0 & y2=dy-1
  END ELSE BEGIN
      IF (y2 GE NIm[2]) THEN BEGIN
          y2=NIm[2]-1 & y1=y2-dy+1
      END
  END
  ;;  -----------------------------------------------------------
  ;; readjust centre position of the box to the Max or Min position in the box
  ;; print, "centering box around extremum"
  ;; and autodetect the extremum type
  ;;  -----------------------------------------------------------
  IF NOT(keyword_set(fixpreset)) THEN BEGIN
     IF NOT(keyword_set(fitmin) OR keyword_set(fitmax)) THEN BEGIN
        ;; autodetect extremum type
        BoundaryVal= 0.25*(im[x1,y1] + im[x1,y2] + im[x2,y1] + im[x2,y2])
        CentralVal=im[x,y]
        IF (BoundaryVal LT CentralVal) THEN BEGIN
           BoxMin =  MIN(im[x1:x2,y1:y2])
           BoxMax =  MAX(im[x1:x2,y1:y2], I)
           Amp=BoxMax & Base=BoxMin
           exttype="maximum"
        END ELSE BEGIN
           BoxMax =  MAX(im[x1:x2,y1:y2])
           BoxMin =  MIN(im[x1:x2,y1:y2], I)
           Amp=BoxMin & Base=BoxMax
           exttype="minimum"
        END
     END ELSE BEGIN
        IF keyword_set(fitmin) THEN BEGIN
           BoxMax =  MAX(im[x1:x2,y1:y2])
           BoxMin =  MIN(im[x1:x2,y1:y2], I)
           Amp=BoxMin & Base=BoxMax
           exttype="minimum"
        END
        IF keyword_set(fitmax) THEN BEGIN
           BoxMin =  MIN(im[x1:x2,y1:y2])
           BoxMax =  MAX(im[x1:x2,y1:y2], I)
           Amp=BoxMax & Base=BoxMin
           exttype="maximum"
        END
     END
     
     IF NOT(keyword_set(quiet)) THEN printtocon, "% Profile2DFindCentre: recentering box around "+exttype 
     cx = (I MOD dx) & cy = (I/dx)  ;; relative to the box
     cx = cx+x1 & cy=cy+y1          ;; in absolute coordinates
     IF KEYWORD_SET(recentre) THEN BEGIN
        x1=x-dx/2 & x2=x1+dx-1 
        y1=y-dy/2 & y2=y1+dy-1 
        IF (x1 LT 0) THEN BEGIN
           x1=0 & x2=dx-1
        END ELSE BEGIN
           IF (x2 GE (*p).SzX) THEN BEGIN
              x2=(*p).SzX-1 & x1=x2-dx+1
           END
        END
        IF (y1 LT 0) THEN BEGIN
           y1=0 & y2=dy-1
        END ELSE BEGIN
           IF (y2 GE (*p).SzY) THEN BEGIN
              y2=(*p).SzY-1 & y1=y2-dy+1
           END
        END 
     END
     IF NOT(keyword_set(quiet)) THEN PRINTToCon, "% Profile2DFindCentre: "+ exttype + " value "+MyString(Amp)+" at ("+MyString(cx)+ ","+ MyString(cy)+ ")"  
  END ELSE BEGIN
     cx=FIX((x2+x1)/2) & cy=FIX((y2+y1)/2)
     Amp=Mean(im[(cx-1):(cx+1),(cy-1):(cy+1)])
     Base=0.25*(Mean(im[x1,*])+Mean(im[x2,*])+Mean(im[*,y1])+Mean(im[*,y2]))
     IF NOT(keyword_set(quiet)) THEN PRINTToCon, "% Profile2DFindCentre: Fitting at preset position ("+MyString(cx)+ ","+ MyString(cy)+ ")"
  END
     ;; STOP

;; 


  ;; -----------------------------------------------------------
  ;;  set the x and y vectors XVec and YVec
  ;;  declare the z-value array tmp
  ;; declare the preset values for the parameters
  ;;  -----------------------------------------------------------
  XVec=FLTARR(dx) & YVec=FLTARR(dy)
  XVec(*)=x1+FLOAT(INDGEN(dx)) & YVec(*)=y1+FLOAT(INDGEN(dy))
  

  tmp=im[x1:x2,y1:y2]
  type=SIZE(im, /TYPE)
  CASE 1 OF
      IN(type,[1,2,3,4,5,12,13]): BEGIN
      END
      IN(type,[6,9]): BEGIN
          tmp=ABS(tmp)
          IF NOT(keyword_set(quiet)) THEN PrintToCon, "% Profile2DFindCentre: Complex-valued data, fitting magnitude!"
      END
      ELSE: BEGIN
          IF NOT(keyword_set(quiet)) THEN printtocon, "% Profile2DFindCentre: illegal data type, aborting fit"
          return
      END
  END
  IF (keyword_set(errim)) THEN BEGIN
     ;; 1-sigma error data is set
     IF PTR_VALID(errim) THEN BEGIN 
        errdata=(*errim)[x1:x2,y1:y2]
     END ELSE BEGIN
        errdata=SQRT(ABS(tmp))
     END  
  END 
  ;;
  ;; apply the mask
  ;;
  tmp=tmp*kernel
 
  NPix=(x2-x1+1)*(y2-y1+1)
  lorentzian=0
  moffat=0
;      Par(0)   Constant baseline level
;      Par(1)   Peak value
;      Par(2)   Peak half-width (x) -- gaussian sigma or half-width at half-max
;      Par(3)   Peak half-width (y) -- gaussian sigma or half-width at half-max
;      Par(4)   Peak centroid (x)
;      Par(5)   Peak centroid (y)
;      Par(6)   Rotation angle (radians) if TILT keyword set
;      Par(7)   Moffat power law index if MOFFAT keyword set
  NPar=7
  Par=[FLOAT(Base), FLOAT(Amp-Base), 1., 1., FLOAT(cx), FLOAT(cy), 0.]
  CASE profile OF
      'Lorentzian': Begin
          lorentzian=1
          gaussian=0
          moffat=0
      END
      'MLorentzian': Begin
          lorentzian=0
          gaussian=0
          moffat=1
          ;; overwrite Par because of the additional parameter for the variable power law
          NPar=8
          Par=[FLOAT(Base), FLOAT(Amp-Base), 1., 1., FLOAT(cx), FLOAT(cy), 0.,1.]
       END
      ELSE: gaussian=1
  END
  IF (keyword_set(parinfo)) THEN BEGIN
      ;; parameter info is set 
      ;; preset the initial values
      parinfo[*].VALUE=Par
  END
  ;; print, parinfo
 
 
  parerr=Par
  
 
  IF (keyword_set(parinfo)) THEN BEGIN
     ;; backup parinfo
      IF (keyword_set(errim)) THEN BEGIN
          wts=1./(errdata^2)
          zFit=mpfit2dpeak(tmp, Par, XVec, YVec, PARINFO=parinfo, WEIGHTS=wts, PERROR=parerr, TILT=allowtilt, GAUSSIAN=gaussian, LORENTZIAN=lorentzian, MOFFAT=moffat, CHISQ=chisq, QUIET=quiet, NITER=niter)
       END ELSE BEGIN
          zFit=mpfit2dpeak(tmp, Par, XVec, YVec, PARINFO=parinfo, TILT=allowtilt, GAUSSIAN=gaussian, LORENTZIAN=lorentzian, MOFFAT=moffat, CHISQ=chisq, QUIET=quiet, NITER=niter)
      END
   END ELSE BEGIN
      IF (keyword_set(errim)) THEN BEGIN
         wts=1./(errdata^2)
         zFit=mpfit2dpeak(tmp, Par, XVec, YVec, WEIGHTS=wts, PERROR=parerr, TILT=allowtilt, GAUSSIAN=gaussian, LORENTZIAN=lorentzian, MOFFAT=moffat, CHISQ=chisq, QUIET=quiet, NITER=niter)
      END ELSE BEGIN
         zFit=mpfit2dpeak(tmp, Par, XVec, YVec, TILT=allowtilt, GAUSSIAN=gaussian, LORENTZIAN=lorentzian, MOFFAT=moffat, CHISQ=chisq, QUIET=quiet, NITER=niter)
      END
  END
  
  if keyword_set(fitdata) then begin
      ;; return fit parameters
      fitdata=Par
  end

  IF (allowtilt NE 0) THEN BEGIN
      ;; the tilt angle theta=Par(6) is measured with respect to the first width parameter
      ;; theta is the angle which brings the axis Par(2) of the ellipses to the x-axis 
      ;; by clockwise rotation!
      ;; change the meaning: Par(6) will be the angle which brings the longer axis to 
      ;; the x-axis
      IF (Par(2) LT Par(3)) THEN Par(6)=Par(6)+!DPI/2.
      ;; Make sure that Par(6) is in the range [0,pi]
      Par(6) = ((Par(6) MOD !dpi) + 2*!dpi) MOD !dpi
      ;; change the meaning: Par(6) will be the angle between the x-axis and the long axis!
      Par(6)=!dpi-Par(6)
END
  
  x=Par(4) & y=Par(5)

  
  ;; report results
CASE 1 OF
   (profile EQ 'MLorentzian'): s=[profile+" Regression:", $
                                  "Const.    = " +MySTRING(Par(0)),$
                                  "Amp.      = " + MySTRING(Par(1)), $
                                  "Sigma (X,Y) = ("+MySTRING(Par(2))+","+MySTRING(Par(3))+")", $
                                  "Centre (X,Y) = ("+MySTRING(x) +","+MySTRING(y)+")", $
                                  "Tilt (rad) = "+MySTRING(Par(6)), $
                                  "Power law exponent = " +MySTRING(Par(7)),$
                                  "ChiSq/pix  = " +MySTRING(chisq/NPix)] 
   ELSE: s=[profile+" Regression:", $
                                  "Const.    = " +MySTRING(Par(0)),$
                                  "Amp.      = " + MySTRING(Par(1)), $
                                  "Sigma (X,Y) = ("+MySTRING(Par(2))+","+MySTRING(Par(3))+")", $
                                  "Centre (X,Y) = ("+MySTRING(x) +","+MySTRING(y)+")", $
                                  "Tilt (rad) = "+MySTRING(Par(6)), $
                                  "ChiSq/pix  = " +MySTRING(chisq/NPix)] 
END

    ;; printtocon, s

    ;; output of formatted string
    ;; format 
    ;;
    peakmax=Par(0)+Par(1)
    
    IF keyword_set(errim) THEN BEGIN
        CASE 1 OF
             (profile EQ 'MLorentzian'): BEGIN
                 fmt='(18E13.4)'
                 fs=STRING(FORMAT=fmt, x, y, Par(2), Par(3), Par(0), Par(1), Par(6), Par(7), ParErr(4), ParErr(5), $
                          ParErr(2), ParErr(3), ParErr(0), ParErr(1), ParErr(6), ParErr(7), peakmax, chisq)
             END
            ELSE: BEGIN 
                fmt='(16E13.4)'
                fs=STRING(FORMAT=fmt, x, y, Par(2), Par(3), Par(0), Par(1), Par(6), ParErr(4), ParErr(5), $
                          ParErr(2), ParErr(3), ParErr(0), ParErr(1), ParErr(6), peakmax, chisq)
            END
        END
    END ELSE BEGIN
       CASE 1 OF
             (profile EQ 'MLorentzian'): BEGIN
                 fmt='(10E13.4)'
                 fs=STRING(FORMAT=fmt, x, y, Par(2), Par(3), Par(0), Par(1), Par(6), Par(7), peakmax, chisq)
             END
             ELSE: BEGIN 
                 fmt='(9E13.4)'
                 fs=STRING(FORMAT=fmt, x, y, Par(2), Par(3), Par(0), Par(1), Par(6), peakmax, chisq)
             END
         END 
    END
 
    IF NOT(Keyword_Set(info)) THEN BEGIN
        IF NOT(keyword_set(quiet)) THEN PrintToCon, fs 
    END ELSE BEGIN
        IF NOT(keyword_set(quiet)) THEN PrintToCon, s 
    END
    
    IF (keyword_set(parinfo) AND  NOT(keyword_set(quiet))) THEN BEGIN
        ;; check whether parameter limits are reached
        FOR i=0,(N_ELEMENTS(parinfo)-1) DO BEGIN
           IF (parinfo[i].LIMITED[0] EQ 1) THEN BEGIN
              IF (Par[i] LE parinfo[i].LIMITS[0]) THEN BEGIN
                 serr="Parameter "+MyString(i)+" reached lower limit."
                 printtocon, "% WARNING: "+ serr
                 ;; The fit failed, what do we return as fitdata?
                 IF keyword_set(errstate) THEN errstate=serr
              END
           END
           IF (parinfo[i].LIMITED[1] EQ 1) THEN BEGIN
              IF (Par[i] GE parinfo[i].LIMITS[1]) THEN BEGIN
                 serr="Parameter "+MyString(i)+" reached upper limit."
                 printtocon, "% WARNING: "+ serr
                 ;; The fit failed, what do we return as fitdata?
                 IF keyword_set(errstate) THEN errstate=serr
              END
           END 
       END 
    END
 
END   



PRO Parabola2DFindCentre, p, x, y, Kernel, FITMAX=fitmax, FITMIN=fitmin, FITDATA=fitdata, PARINFO=parinfo, ERRIM=errim, RECENTRE=recentre
;;
;; fit a parabola to the image p around x,y 
;; Kernel is an array marking the rectangular fit area and 
;; providing a mask by which p is multiplied
;;
;; EXAMPLE
;;   rc=4
;;   kernel=FLTARR(2*rc+1, 2*rc+1)
;;   kernel[*,*]=1.
;;   x=XC & y=Yc
;;   Parabola2DFindCentre, im, x, y, kernel
;;
 IF (GetDebugFlag()) THEN BEGIN
  END ELSE BEGIN
     CATCH, Error_status
     IF (Error_status NE 0) THEN BEGIN
        PrintToCon, "% Parabola2DFindCentre:  Fatal error "
        Print, "%   Error status  - " + STRING(error_status)
        Print, "%   Error message - " + !ERROR_STATE.MSG
        ErrMsg, !ERROR_STATE.MSG
        CATCH, /Cancel
        return
     END
  END


N=SIZE(P)
  PSzX=N(1) & PSzY=N(2)
  Ptype=N(3)
  N=SIZE(Kernel)
  dx=N(1) & dy=N(2)
  x1=x-dx/2 & x2=x1+dx-1 
  y1=y-dy/2 & y2=y1+dy-1 
  ;; check box coordinates
  IF (x1 LT 0) THEN BEGIN
      x1=0 & x2=dx-1
  END
  IF (x2 GE PSzX) THEN BEGIN
     x2=PSzX-1 & x1=x2-dx+1
     IF (x1 LT 0) THEN BEGIN
        x1=0 & dx=PSzX
     END
  END
  
  IF (y1 LT 0) THEN BEGIN
      y1=0 & y2=dy-1
  END 
  IF (y2 GE PSzY) THEN BEGIN
     y2=PSzY-1 & y1=y2-dy+1
     IF (y1 LT 0) THEN BEGIN
        y1=0 & dy=PSzY
     END
  END
  ;;  -----------------------------------------------------------
  ;; readjust centre position of the box to the Max or Min position in the box
  ;; print, "centering box around extremum"
  ;; and autodetect the extremum type
  ;;  -----------------------------------------------------------
  IF NOT(keyword_set(fitmin) OR keyword_set(fitmax)) THEN BEGIN
      ;; autodetect extremum type
      BoundaryVal= 0.25*(P[x1,y1] + P[x1,y2] + P[x2,y1] + P[x2,y2])
      CentralVal=P[x,y]
      IF (BoundaryVal LT CentralVal) THEN BEGIN
          BoxMin =  MIN(P[x1:x2,y1:y2])
          BoxMax =  MAX(P[x1:x2,y1:y2], I)
          Amp=BoxMax & Base=BoxMin
          exttype="maximum"
      END ELSE BEGIN
          BoxMax =  MAX(P[x1:x2,y1:y2])
          BoxMin =  MIN(P[x1:x2,y1:y2], I)
          Amp=BoxMin & Base=BoxMax
          exttype="minimum"
      END
  END ELSE BEGIN
      IF keyword_set(fitmin) THEN BEGIN
          BoxMax =  MAX(P[x1:x2,y1:y2])
          BoxMin =  MIN(P[x1:x2,y1:y2], I)
          Amp=BoxMin & Base=BoxMax
          exttype="minimum"
      END
      IF keyword_set(fitmax) THEN BEGIN
          BoxMin =  MIN(P[x1:x2,y1:y2])
          BoxMax =  MAX(P[x1:x2,y1:y2], I)
          Amp=BoxMax & Base=BoxMin
          exttype="maximum"
      END
   END
  print, "% Parabola2DFindCentre: recentering box around "+exttype 
  cx = (I MOD dx) & cy = (I/dx)  ;; relative to the box
  cx = cx+x1 & cy=cy+y1  ;; in absolute coordinates
  x1=x-dx/2 & x2=x1+dx-1 
  y1=y-dy/2 & y2=y1+dy-1 
  IF (x1 LT 0) THEN BEGIN
      x1=0 & x2=dx-1
  END ELSE BEGIN
      IF (x2 GE PSzX) THEN BEGIN
          x2=PSzX-1 & x1=x2-dx+1
      END
  END
  IF (y1 LT 0) THEN BEGIN
      y1=0 & y2=dy-1
  END ELSE BEGIN
      IF (y2 GE PSzY) THEN BEGIN
          y2=PSzY-1 & y1=y2-dy+1
      END
  END
  ;; STOP
  PRINT, "% Parabola2DFindCentre: "+ exttype + " value "+MyString(Amp)+" at ("+MyString(cx)+ ","+ MyString(cy)+ ")"  
;; 


  ;; -----------------------------------------------------------
  ;;  set the x and y vectors XVec and YVec
  ;;  declare the z-value array tmp
  ;; declare the preset values for the parameters
  ;;  -----------------------------------------------------------
  XVec=FLTARR(dx) & YVec=FLTARR(dy)
  XVec(*)=x1+FLOAT(INDGEN(dx)) & YVec(*)=y1+FLOAT(INDGEN(dy))
  

  tmp=ABS(P[x1:x2,y1:y2])

  CASE 1 OF
      IN(Ptype,[1,2,3,4,5,12,13]): BEGIN
      END
      IN(Ptype,[6,9]): BEGIN
          tmp=ABS(tmp)
          Print, "% Parabola2DFindCentre: Complex-valued data, fitting magnitude!"
      END
      ELSE: BEGIN
          print, "% Parabola2DFindCentre: illegal data type, aborting fit"
          ErrMsg, "Illegal data type, aborting fit", /ERROR
          return
      END
  END

  ;;
  ;; apply the mask
  ;;
  tmp=tmp*kernel
  ;; parabola = a + b*(x-x0)^2 + c*(y-y0)^2
;      Par(0)   Constant baseline level
;      Par(1)   coefficient for the curvature in x
;      Par(2)   coefficient for the curvature in y
;      Par(3)   x0
;      Par(4)   y0
  NPar=5
  radius=0.5*FLOAT(x2-x1+1)
  coeffpreset=-Base/Amp/radius/radius
  Par=FltArr(5)
  Par=[FLOAT(Amp),coeffpreset,coeffpreset,cx,cy]
 
  IF (keyword_set(parinfo)) THEN BEGIN
      ;; parameter info is set 
      ;; preset the initial values
      parinfo[*].VALUE=Par
  END

  IF (keyword_set(errim)) THEN BEGIN
      ;; 1-sigma error data is set 
      errdata=errim[x1:x2,y1:y2]
  END 
  
  ParErr=Par
  err=1
  XR=INDGEN(x2-x1+1)+x1
  YC=INDGEN(y2-y1+1)+y1
  XVec=XR#(YC*0+1) &  YVec=(XR*0+1)#YC
   IF (keyword_set(parinfo)) THEN BEGIN
      IF (keyword_set(errim)) THEN BEGIN
          wts=1./(errdata^2)
         
          zFit= mpfit2dfun('parabola2D', XVec, YVec, tmp, err, Par, $
                               parinfo=parinfo, WEIGHTS=wts, PERROR=ParErr)
           
      END ELSE BEGIN
          zFit=mpfit2dfun('parabola2D', XVec, YVec, tmp, err, Par, $
                               parinfo=parinfo)
      END
  END ELSE BEGIN
      zFit= mpfit2dfun('parabola2D', XVec, YVec, tmp, err, Par)
  END
  
  ;; STOP
  if keyword_set(fitdata) then begin
      ;; return fit parameters
      fitdata=Par
  end

  x=zFit(3) & y=zFit(4)

  s=["% Regression f(x)=a+b(x-x0)^2+c(y-y0)^2:","%   a=" +MySTRING(zFit(0)), "%   b=" + MySTRING(zFit(1)), "%   c="+MySTRING(zFit(2)), "%   x0="+MySTRING(x), "%   y0="+MySTRING(y)] 

  for j=1,N_ELEMENTS(s) DO print, s[j-1]


    IF keyword_set(parinfo) THEN BEGIN
        ;; check whether parameter limits are reached
        FOR i=0,(N_ELEMENTS(parinfo)-1) DO BEGIN
           IF (parinfo[i].LIMITED[0] EQ 1) THEN BEGIN
               IF (Par[i] LE parinfo[i].LIMITS[0]) THEN print, "% WARNING: parameter "+MyString(i)+" reached lower limit!"  
           END
           IF (parinfo[i].LIMITED[1] EQ 1) THEN BEGIN
               IF (Par[i] GE parinfo[i].LIMITS[1]) THEN print, "% WARNING: parameter "+MyString(i)+" reached upper limit!"  
           END 
       END 
    END
 

END 
