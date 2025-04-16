Function VerticalDistance, im, missing=missing, X0=x0, Y0=y0, LOG=log, EXPONENT=exponent, DISTANCE=distance
;;
nparms=N_Params()
if (nparms lt 1) then begin
   Message, 'Usage:',/info
   Message, 'VerticalDistance, im, missing=missing, X0=x0, Y0=y0', /info
   Message, 'Purpose:  Interpolate an image to a vertical distance map', /info
   Message, '          its values given in Cartesian coordinates', /info
   Message, 'Input:    im = 2d array', /info
   Message, '          x0,y0 = origin of the coordinate system', /info
   Message, '                 x0,y0 must be inside the cartesian image', /info
   Message, '                 (in pixel units)', /info
   Message, '          missing = how to handle missing information', /info
   Message, '          log = logarithmic scaling of distances', /info
   Message, '          exponent = power law scaling of radii', /info
   Message, 'Returns:  array of values as function of the distance to the vertical center y0', /info
   Message, 'Requires: The IDL interpolate function', /info
   Message, 'Written:  L.H. Forschungszentrum Juelich 23/10/2011', /info
   return, 0
endif
if(keyword_set(missing)) then begin
   fmiss=missing
endif else begin
   fmiss=0.
endelse
;
; - - get sizes and ranges of radius (r) and angle (theta) arrays:
;
Nim=Size(im)
if(Nim(0) ne 2) then begin
  printtocon,"% VerticalDistance: Error - image is not a 2D array"
  return, 0
endif
cols=Nim(1) ;; number of columns, will be preserved in the output
rows=Nim(2) ;; number of vertical lines, image size will be reduced to half
dmax=LONG(rows/2)
yd=findgen(dmax)
x=findgen(cols)
;;
;; define origin within (!) the original cartesian mesh
;;
IF NOT(Keyword_set(x0)) THEN x0=FIX(Nim(1)/2)
IF NOT(Keyword_set(y0)) THEN y0=FIX(Nim(2)/2)
;; 
;; transform r coordinate to exponential
;;
if keyword_set(log) then BEGIN
;; yd=yd/float(dmax-1) ;; normalized distances
;; yd=yd*ALog(dmax-1)
;; yd=Exp(yd)
;; alternative: determine base
b=(2*dmax)^DOUBLE(1./dmax)
log=b ;; store base in log parameter
yd=findgen(dmax)+1
yd=FIX(b^DOUBLE(yd))
END
;; exponent is set
if keyword_set(exponent) then BEGIN
d0=DOUBLE(dmax-1)
b=d0^(1.-exponent)
help=b*DOUBLE(yd)^exponent
yd=FLOAT(help)
END
;;
IF Keyword_set(distance) then distance=yd
;; new mesh coordinates
;;

eins=fltarr(cols)
eins(*)=1.
xnew=x#eins[0:(dmax-1)]
;;ynew=y0+yd#eins
ynew=y0+TRANSPOSE(yd#eins)
;;
;; interpolate 
;; xpolar, ypolar do not form a regular xy grid, don't use /grid keyword
;;
frp=interpolate(im,xnew,ynew,cubic=-0.5,missing=fmiss)
;
return, frp
end



Function CartToPolar, im, missing=missing, X0=x0, Y0=y0, LOG=log, EXPONENT=exponent, PHI=phi, RHO=rho
;;
nparms=N_Params()
if (nparms lt 1) then begin
   Message, 'Usage:',/info
   Message, 'CartToPolar, im, missing=missing, X0=x0, Y0=y0', /info
   Message, 'Purpose:  Interpolate an image to polar coordinates from ', /info
   Message, '          its values given in Cartesian coordinates', /info
   Message, 'Input:    im = 2d array', /info
   Message, '          x0,y0 = origin of the coordinate system', /info
   Message, '                 x0,y0 must be inside the cartesian image', /info
   Message, '                 (in pixel units)', /info
   Message, '          missing = how to handle missing information', /info
   Message, '          log = logarithmic scaling of radii', /info
   Message, '          exponent = exponential scaling of radii', /info
   Message, 'Returns:  array of values as function of r, theta', /info
   Message, 'Requires: The IDL interpolate function', /info
   Message, 'Written:  L.H. Forschungszentrum Juelich 23/10/2011', /info
   return, 0
endif
if(keyword_set(missing)) then begin
   fmiss=missing
endif else begin
   fmiss=0.
endelse
   print,"% CartToPolar: Filling missing information with value "+MyString(fmiss)
;
; - - get sizes and ranges of radius (r) and angle (theta) arrays:
;
Nim=Size(im)
if(Nim(0) ne 2) then begin
  printtocon,"% CartToPolar: Error - image is not a 2D array"
  return, 0
endif
cols=Nim(1)
rows=Nim(2)
IF (cols GT rows) THEN N=cols ELSE N=rows
;; if N  is odd then N=2*(N DIV 2)+1
;; if N  is even then N=2*(N DIV 2) 
;; in any case: 2*(N DIV 2) + 1 >= N 
;; get the exponential radius and the angle image with size 2*(N DIV 2) +1   
rmax=LONG(N/2)
nr=long(rmax)
r=findgen(nr)
nt=long(2*rmax)
theta=2.*!pi*findgen(nt)/float(nt-1)
;;
;; define origin within (!) the original cartesian mesh
;;
IF NOT(Keyword_set(x0)) THEN x0=FIX(Nim(1)/2)
IF NOT(Keyword_set(y0)) THEN y0=FIX(Nim(2)/2)
;; 
;; transform r coordinate to exponential
;;
if keyword_set(log) then BEGIN
r=r/float(nr-1) ;; normalized radii
r=r*ALog(nr-1)
r=Exp(r)
END
;; exponent is set
if keyword_set(exponent) then BEGIN
r0=DOUBLE(nr-1)
b=r0^(1.-exponent)
help=b*DOUBLE(r)^exponent
r=FLOAT(help)
END
;;
IF Keyword_set(rho) then rho=r
IF Keyword_set(phi) then phi=theta
;; new mesh coordinates
;;
xpolar=x0+r#cos(theta)
ypolar=y0+r#sin(theta)
;;
;; interpolate 
;; xpolar, ypolar do not form a regular xy grid, don't use /grid keyword
;;
;; frp=interpolate(im,xpolar,ypolar,cubic=-0.5,missing=fmiss)
frp=interpolate(im,xpolar,ypolar,cubic=-0.5,missing=fmiss)
;
return, frp
end


PRO CartToPolarVolume, LOG=log, EXPONENT=exponent
XConsole_PushState
XConsole_Busy
;;CATCH, Error_status
;;IF NOT(GetDebugFlag()) THEN BEGIN
;;   IF (Error_status NE 0) THEN BEGIN
;;      Printtocon, "% CartToPolarVolume:    Fatal error "
;;      Printtocon, "%   Error status  - " + STRING(error_status)
;;      Printtocon, "%   Error message - " + !ERROR_STATE.MSG
;;      ErrMsg, !ERROR_STATE.MSG
;;      CATCH, /Cancel
;;      XConsole_PopState
;;      return
;;   END
;;END 
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     printtocon, "% CartToPolarVolume: Root pointer is invalid" 
      XConsole_PopState
     return
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% CartToPolarVolume: current stack pointer is invalid" 
      XConsole_PopState
     return
  END
  e=(*c).datap
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% CartToPolarVolume: current data pointer is invalid" 
      XConsole_PopState
     return
  END
  ;; determine size of polar image
 CASE (*e).zcoord OF
     1: BEGIN 
        ;; 2d image is yz slice
        cols=(*e).SzY & rows=(*e).SzZ
     END
     2: BEGIN 
        ;; 2d image is xz slice
        cols=(*e).SzX & rows=(*e).SzZ
     END
     3: BEGIN 
        ;; 2d image is xy slice
        cols=(*e).SzX & rows=(*e).SzY
     END
     ELSE: 
  END
  IF (cols GT rows) THEN dim=cols ELSE dim=rows
  dim=FIX(dim/2)
  name="CartToPolar("+(*c).name+")"
  printtocon, "% CartToPolarVolume: creating " + name 
  
  ;; STOP
  pp=DataList_CreateElement(ptr, name)
  (*pp).type=(*e).type
  ;; predefine new stack size
  CASE (*e).zcoord OF
     1: BEGIN 
        ;; 2d image is yz slice
        (*pp).SzX=(*e).SzX &  (*pp).SzY=dim & (*pp).SzZ=dim*2
     END
     2: BEGIN 
        ;; 2d image is xz slice
        (*pp).SzY=(*e).SzY &  (*pp).SzX=dim & (*pp).SzZ=dim*2
     END
     3: BEGIN 
        ;; 2d image is xy slice
        (*pp).SzZ=(*e).SzZ &  (*pp).SzX=dim & (*pp).SzY=dim*2
     END
     ELSE: 
  END
  (*pp).xsamp=(*e).xsamp
  (*pp).ysamp=(*e).ysamp
  (*pp).zsamp=(*e).zsamp
  (*pp).slice=(*e).slice
  (*pp).zcoord=(*e).zcoord
  (*pp).contrastmode="auto"
  ;; 
  ppd=(Data_GetEmptyArrayP(pp))
  IF NOT(PTR_VALID(ppd)) THEN BEGIN
     printtocon, "% CartToPolarVolume: Failed to create data array." 
     XConsole_PopState
     return
  END
  (*pp).data=ppd
  InitProgressBar, MINV=[0], MAXV=[], TEXT=text
  phi=1 & rho=1
  CASE (*e).zcoord OF
     1: BEGIN 
        InitProgressBar, MINV=[0], MAXV=[((*e).SzX-1)], TEXT='Processing image nr.' 
        For i=0,((*e).SzX-1) DO BEGIN
           ProgressBarSet, 0, i, TEXT=STRING(i)
           (*ppd)[i,*,*]=CartToPolar(REFORM((*(*e).data)[i,*,*]), LOG=log, EXPONENT=exponent, PHI=phi, RHO=rho)
        END
        DestroyProgressBar
     END
     2: BEGIN 
        InitProgressBar, MINV=[0], MAXV=[((*e).SzY-1)], TEXT='Processing image nr.' 
        For i=0,((*e).SzY-1) DO BEGIN
           ProgressBarSet, 0, i, TEXT=STRING(i)
           (*ppd)[*,i,*]=CartToPolar(REFORM((*(*e).data)[*,i,*]), LOG=log, EXPONENT=exponent, PHI=phi, RHO=rho)
        END
        DestroyProgressBar
     END
     3: BEGIN 
        InitProgressBar, MINV=[0], MAXV=[((*e).SzZ-1)], TEXT='Processing image nr.' 
        For i=0,((*e).SzZ-1) DO BEGIN
           ProgressBarSet, 0, i, TEXT=STRING(i)
           (*ppd)[*,*,i]=CartToPolar(REFORM((*(*e).data)[*,*,i]), LOG=log, EXPONENT=exponent, PHI=phi, RHO=rho)
        END
        DestroyProgressBar
     END
     ELSE: 
  END
  IF (N_ELEMENTS(phi) GT 1) THEN BEGIN
     printtocon, "% CartToPolarVolume: theta angles as a function of the pixel coordinate"
     printtocon, "%   pix theta"
     FOR i=0,(N_ELEMENTS(phi)-1) DO BEGIN
        printtocon, " "+MyString(i)+ " "+MyString(phi(i))
     END
     printtocon, "%"
  END
  IF (N_ELEMENTS(rho) GT 1) THEN BEGIN
     printtocon, "% CartToPolarVolume: radii as a function of the pixel coordinate"
     printtocon, "%   pix radius"
     FOR i=0,(N_ELEMENTS(rho)-1) DO BEGIN
        printtocon, " "+MyString(i)+ " "+MyString(rho(i))
     END
     printtocon, "%"
  END
  current=(*ptr).current
  (*current).name=name
  CreateWindow
  TVDisplay
  Update_XTabControl
  XConsole_PopState
END 


PRO VerticalDistanceVolume, LOG=log, EXPONENT=exponent
XConsole_PushState
XConsole_Busy
;;CATCH, Error_status
;;IF NOT(GetDebugFlag()) THEN BEGIN
;;   IF (Error_status NE 0) THEN BEGIN
;;      Printtocon, "% CartToPolarVolume:    Fatal error "
;;      Printtocon, "%   Error status  - " + STRING(error_status)
;;      Printtocon, "%   Error message - " + !ERROR_STATE.MSG
;;      ErrMsg, !ERROR_STATE.MSG
;;      CATCH, /Cancel
;;      XConsole_PopState
;;      return
;;   END
;;END 
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     printtocon, "% VerticalDistanceVolume: Root pointer is invalid" 
      XConsole_PopState
     return
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% VerticalDistanceVolume: current stack pointer is invalid" 
      XConsole_PopState
     return
  END
  e=(*c).datap
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% VerticalDistanceVolume: current data pointer is invalid" 
      XConsole_PopState
     return
  END
  ;; determine size of polar image
 CASE (*e).zcoord OF
     1: BEGIN 
        ;; 2d image is yz slice
        cols=(*e).SzY & rows=(*e).SzZ
     END
     2: BEGIN 
        ;; 2d image is xz slice
        cols=(*e).SzX & rows=(*e).SzZ
     END
     3: BEGIN 
        ;; 2d image is xy slice
        cols=(*e).SzX & rows=(*e).SzY
     END
     ELSE: 
  END
  dim=rows
  dim=FIX(dim/2)
  name="VerticalDistancePolar("+(*c).name+")"
  printtocon, "% VerticalDistanceVolume: creating " + name 
  
  ;; STOP
  pp=DataList_CreateElement(ptr, name)
  (*pp).type=(*e).type
  ;; predefine new stack size
  CASE (*e).zcoord OF
     1: BEGIN 
        ;; 2d image is yz slice
        (*pp).SzX=(*e).SzX &  (*pp).SzY=dim*2 & (*pp).SzZ=dim
     END
     2: BEGIN 
        ;; 2d image is xz slice
        (*pp).SzY=(*e).SzY &  (*pp).SzX=dim*2 & (*pp).SzZ=dim
     END
     3: BEGIN 
        ;; 2d image is xy slice
        (*pp).SzZ=(*e).SzZ &  (*pp).SzX=dim*2 & (*pp).SzY=dim
     END
     ELSE: 
  END
  (*pp).xsamp=(*e).xsamp
  (*pp).ysamp=(*e).ysamp
  (*pp).zsamp=(*e).zsamp
  (*pp).slice=(*e).slice
  (*pp).zcoord=(*e).zcoord
  (*pp).contrastmode="auto"
  ;; 
  ppd=(Data_GetEmptyArrayP(pp))
  IF NOT(PTR_VALID(ppd)) THEN BEGIN
     printtocon, "% VerticalDistanceVolume: Failed to create data array." 
     XConsole_PopState
     return
  END
  (*pp).data=ppd
  InitProgressBar, MINV=[0], MAXV=[], TEXT=text
  distance=1
  CASE (*e).zcoord OF
     1: BEGIN 
        InitProgressBar, MINV=[0], MAXV=[((*e).SzX-1)], TEXT='Processing image nr.' 
        For i=0,((*e).SzX-1) DO BEGIN
           ProgressBarSet, 0, i, TEXT=STRING(i)
           (*ppd)[i,*,*]=VerticalDistance(REFORM((*(*e).data)[i,*,*]), LOG=log, EXPONENT=exponent,DISTANCE=distance)
        END
        DestroyProgressBar
     END
     2: BEGIN 
        InitProgressBar, MINV=[0], MAXV=[((*e).SzY-1)], TEXT='Processing image nr.' 
        For i=0,((*e).SzY-1) DO BEGIN
           ProgressBarSet, 0, i, TEXT=STRING(i)
           (*ppd)[*,i,*]=VerticalDistance(REFORM((*(*e).data)[*,i,*]), LOG=log, EXPONENT=exponent, DISTANCE=distance)
        END
        DestroyProgressBar
     END
     3: BEGIN 
        InitProgressBar, MINV=[0], MAXV=[((*e).SzZ-1)], TEXT='Processing image nr.' 
        For i=0,((*e).SzZ-1) DO BEGIN
           ProgressBarSet, 0, i, TEXT=STRING(i)
           (*ppd)[*,*,i]=VerticalDistance(REFORM((*(*e).data)[*,*,i]), LOG=log, EXPONENT=exponent, DISTANCE=distance)
        END
        DestroyProgressBar
     END
     ELSE: 
  END
  IF (N_ELEMENTS(distance) GT 1) THEN BEGIN
     printtocon, "% VerticalDistanceVolume: distance as a function of the pixel coordinate"
     printtocon, "%   pix distance"
     FOR i=0,(N_ELEMENTS(distance)-1) DO BEGIN
        printtocon, " "+MyString(i)+ " "+MyString(distance(i))
     END
     printtocon, "%"
  END
  if (keyword_set(log)) THEN print, "% VerticalDistanceVolume: exponential base="+MyString(log)
  current=(*ptr).current
  (*current).name=name
  CreateWindow
  TVDisplay
  Update_XTabControl
  XConsole_PopState
END 
