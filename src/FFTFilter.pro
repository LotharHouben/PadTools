; Compute the "u" value = (x/a)^2 + (y/b)^2 with optional rotation
function computepeak_u, x, y, widx, widy, tilt=tilt, symmetric=sym
      return, (x/widx)^2 + (y/widy)^2
end

FUNCTION GetBWNQFilter, dimx, dimy, BWLO=bwlo, BWHI=bwhi, MEANVALUE=mv
  ;; Butterworth Nyquist filter
  ;; first create the integer arrays kx_ij=i ky_ij=j
  ;; cutoff frequencies are in units of Nyquist
  kx = LINDGEN(dimx)
  EinsX= INTARR(dimx)+1
  ky = LINDGEN(dimy)
  Einsy = INTARR(dimY)+1
  kx = kx-dimx/2
  maxx=MAX(kx)  
  ky = ky-dimy/2
  maxy=MAX(ky)
  kx = EinsY##kx
  ky = ky##EinsX
  if (keyword_set(bwlo)) then BEGIN
     if (bwlo[0] GT (1./Min([maxx,maxy]))) THEN BEGIN
        cutoffx=FLOAT(bwlo[0]*maxx)
        cutoffy=FLOAT(bwlo[0]*maxy)
        Printtocon, "% GetFFTFilter:   Butterworth hi pass filter"
        Printtocon, "     low frequency cutoff at ("+MyString(cutoffx)+", "+MyString(cutoffx)+") pixels"
        Printtocon, "     exponent  "+MyString(bwexp)
        u=computepeak_u(kx, ky, cutoffx, cutoffy)
        hpf=1.-1./Sqrt(1+u^bwlo[1])
     END ELSE hpf=EinsY##EinsX
  END  
  if (keyword_set(bwhi)) then BEGIN
     cutoffx=FLOAT(bwhi[0]*maxx)
     cutoffy=FLOAT(bwhi[0]*maxy)
     if ((cutoffx LT maxx) OR (cutoffy LT maxy)) THEN BEGIN
        Printtocon, "% GetFFTFilter:   Butterworth low pass filter"
        Printtocon, "     high frequency cutoff at ("+MyString(cutoffx)+", "+MyString(cutoffx)+") pixels"
        Printtocon, "     exponent  "+MyString(bwexp)
        u=computepeak_u(kx, ky, cutoffx, cutoffy)
        lpf=1./Sqrt(1+u^bwhi[1])
     END ELSE lpf=EinsX##EinsY
  END
  res=SHIFT(hpf*lpf,dimx/2,dimy/2)
  IF keyword_set(mv) THEN BEGIN
     res[0,0]=1.
     ;; print, "mv="+MyString(mv)
  END
  return, res
END 


PRO PrepareRecSampGrid, dimx, dimy, pkx, pky, pu, DX=dx, DY=dy, ANISO=aniso
  ;; PURPOSE: calculate a distance map, distance from center
  ;; RETURNS: pointer to floating point arrays of dimensions dimx*dimy
  ;; in pkx, pky, pu
  ;; dimx, dimy : dimension of image
  ;; pkx, pky: pointer to a gradient map along x with dimensions dimx, dimy
  ;; pu: pointer to distance map
  ;; dx, dy: center shift with respect to 0.5*dimx, 0.5*dimy (float is
  ;; possible)
  ;;
  ;; ANISO: Anisotropy in qx, qy
  ;;        qx is treated as 1, qy scales as aniso=qy/qx, => always calulate the radius in pix according to qx!
  ;;        if qy > qx then the effective radius of a ring is less than in qx direction, ky > kx per pixel, aniso > 1
  ;;        if qy < qx then the effective radius of a ring is larger than in qx direction, ky > kx per pixel, aniso > 1 
  if NOT(keyword_set(dx)) THEN dx=0
  if NOT(keyword_set(dy)) THEN dy=0
  IF PTR_VALID(pkx) THEN PTR_FREE, pkx
  IF PTR_VALID(pky) THEN PTR_FREE, pky
  IF PTR_VALID(pu) THEN PTR_FREE, pu
  kx = LINDGEN(dimx)
  EinsX= INTARR(dimx)+1
  ky= LINDGEN(dimy)
  Einsy = INTARR(dimY)+1
  kx = kx-(dimx/2+dx)
  ky = ky-(dimy/2+dy)
  IF keyword_set(aniso) THEN BEGIN
     ky=ky*aniso
  END
  pkx = PTR_NEW(EinsY##kx)
  pky = PTR_NEW(ky##EinsX)
  pu = PTR_NEW(Sqrt(((*pkx)^2+(*pky)^2)))
END


FUNCTION GetBPNQFilterPreCalcK, pkx, pky, pu, bwlo, bwhi, bwexp, GAUSSIAN=gaussian, TOPHAT=tophat,BUTTERWORTH=butterworth, MEANVALUE=mv, NOFFTSHIFT=nofftshift
  ;; Butterworth Nyquist filter
  ;; arrays kx_ij=i ky_ij=j
  ;; cutoff frequencies are in units of Nyquist
  maxx=MAX((*pkx))
  maxy=MAX((*pky))
  ;; Printtocon, "% GetFFTFilter:   Butterworth high-pass filter"
  ;; hi pass
  if (bwlo GT (1./Min([maxx,maxy]))) THEN BEGIN
     cutoffx=FLOAT(bwlo*maxx)
     cutoffy=FLOAT(bwlo*maxy)
     ;; Printtocon, "     low frequency cutoff at ("+MyString(cutoffx)+", "+MyString(cutoffy)+") pixels"
     ;; Printtocon, "     exponent  "+MyString(bwexp)
     IF keyword_set(gaussian) THEN BEGIN
        ;; u=computepeak_u((*pkx), (*pky), cutoffx, cutoffy)
        ;; hack: pkx and pky contain the anisotropy information,
        ;; using cutoffx and cutoffy here would compensate the effect of anisotropy
        u=computepeak_u((*pkx), (*pky), cutoffx, cutoffx)
        hpf=1.-exp(-0.5*SQRT(u)^bwexp)
     END
     IF keyword_set(tophat) THEN BEGIN
        ;; tophat filter, 1 outside of cutoffx, cutoffy
        hpf=(*pu GT cutoffx)
     END
     IF keyword_set(butterworth) THEN BEGIN
        ;; u=computepeak_u((*pkx), (*pky), cutoffx, cutoffy)
        u=computepeak_u((*pkx), (*pky), cutoffx, cutoffx)
        hpf=1.-1./Sqrt(1+(u)^bwexp)
     END
  END ELSE hpf=(*pkx)*0.+1.
  ;; low pass 
  if (bwhi LT 1) THEN BEGIN
     cutoffx=FLOAT(bwhi*maxx)
     cutoffy=FLOAT(bwhi*maxy)
     ;; Printtocon, "% GetFFTFilter:   Butterworth low pass filter"
     ;; Printtocon, "     high frequency cutoff at ("+MyString(cutoffx)+", "+MyString(cutoffy)+") pixels"
     ;; Printtocon, "     exponent  "+MyString(bwexp)
     IF keyword_set(gaussian) THEN BEGIN
        u=computepeak_u((*pkx), (*pky), cutoffx, cutoffx)
        lpf=exp(-0.5*SQRT(u)^bwexp)
     END
     IF keyword_set(butterworth) THEN BEGIN
        u=computepeak_u((*pkx), (*pky), cutoffx, cutoffx)
        lpf=1./Sqrt(1+(u)^bwexp)
     END
     IF keyword_set(tophat) THEN BEGIN
        ;; u=computepeak_u((*pkx), (*pky), cutoffx, cutoffx)
        lpf=(*pu LT cutoffx)
     END
  END ELSE lpf=(*pkx)*0.+1
  IF keyword_set(nofftshift) THEN BEGIN
     res=hpf*lpf
     IF keyword_set(mv) THEN BEGIN
        minind=WHERE((*pu) EQ 0, count)
        IF (count GE 1) THEN res[minind]=1.
        ;; print, "mv="+MyString(mv)
     END
  END ELSE BEGIN
     res=SHIFT(hpf*lpf,maxx,maxy)
     IF keyword_set(mv) THEN BEGIN
        res[0,0]=1.
        ;; print, "mv="+MyString(mv)
     END
  END
  return, res
END 



FUNCTION GetFFTFilter, dimx, dimy, RAMP=ramp, BWLO=bwlo, BWHI=bwhi
  ;; first create the integer arrays kx_ij=i ky_ij=j
  kx = fltarr(dimx,dimy)
  ky = fltarr(dimx,dimy)
  for xx=0,dimx-1 do begin
     kxakt = xx-dimx/2
     for yy=0,dimy-1 do begin
        kyakt = yy-dimy/2
        kx(xx,yy) = kxakt
        ky(xx,yy) = kyakt
     endfor
  endfor
  kx = shift(kx,-dimx/2,-dimy/2)
  ky = shift(ky,-dimx/2,-dimy/2)
  maxx=MAX(kx)
  maxy=MAX(ky)
  maxxy=Max([maxx,maxy])
  filter=FLTARR(dimx,dimy)
  tmpk=Sqrt((kx^2+ky^2))
  if (keyword_set(ramp)) then BEGIN
     Printtocon, "% GetFFTFilter:   Ramp filter, cutoff at "+MyString(maxxy)+" pixels"
        filter = tmpk le maxxy   ;; ramp times step function
        filter=filter*tmpk
  END 
  if (keyword_set(bwlo)) then BEGIN
        filter = tmpk le maxxy   ;; ramp times step function
        cutoff=bwlo[0]
        bwexp=2*bwlo[1]
        Printtocon, "% GetFFTFilter:   Butterworth low pass filter"
        Printtocon, "     cutoff at "+MyString(cutoff)+" pixels"
        Printtocon, "     high frequency cutoff at "+MyString(maxxy)+" pixels"
        Printtocon, "     exponent  "+MyString(bwexp)
        filter=filter*1./Sqrt(1+(tmpk/cutoff)^bwexp)
  END 
  if (keyword_set(bwhi)) then BEGIN
     filter = tmpk le maxxy   ;; ramp times step function
     cutoff=bwhi[0]
     bwexp=2*bwhi[1]
        Printtocon, "% GetFFTFilter:   Butterworth high pass filter"
        Printtocon, "     cutoff at "+MyString(cutoff)+" pixels"
        Printtocon, "     high frequency cutoff at "+MyString(maxxy)+" pixels"
        Printtocon, "     exponent  "+MyString(bwexp)
     filter=filter*(1.-1./Sqrt(1+(tmpk/cutoff)^bwexp))
  END
  return, filter
END

FUNCTION ApplyFilter, F, A
;; returns a filtered image
  return, FFT(FFT(A, -1) * F, 1)
END



PRO FFTFilterStack, RAMP=ramp, BWLO=bwlo, BWHI=bwhi
;; apply a FFT filter 
;; type= Rampfilter OR 
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    Printtocon, "% FFTFilterStack:    Fatal error "
    Printtocon, "%   Error status  - " + STRING(error_status)
    Printtocon, "%   Error message - " + !ERROR_STATE.MSG
    ErrMsg, !ERROR_STATE.MSG
    CATCH, /Cancel
    XConsole_PopState
    return
 END
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     printtocon, "% FFTFilterStack: Root pointer is invalid" 
     return
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% FFTFilterStack: current stack pointer is invalid" 
     return
  END
  e=(*c).datap
  IF (NOT(PTR_VALID(e))) THEN BEGIN
     printtocon, "% FFTFilterStack: current data pointer is invalid" 
     return
  END
  IF keyword_set(ramp) then name="Rampfilter"
  IF keyword_set(bwhi) then name="ButterworthHiPass"
  IF keyword_set(bwlo) then name="ButterworthLoPass"
  name=name+"("+(*c).name+")"
  ;; make sure we will not get an overflow when adding data 
  (*c).name=name
  F=GetFFTFilter((*e).SzY,(*e).SzZ, RAMP=ramp, BWLO=bwlo, BWHI=bwhi)
  ;; filter images
    ;; initialize progress bar
  pminv=[1] & pmaxv=[(*e).SzX] & ptext=["Filtering Image Nr."]
  InitProgressBar, MINV=pminv, MAXV=pmaxv, TEXT=ptext
  For i=0,((*e).SzX-1) DO BEGIN
     ProgressBarSet, 0, i, TEXT=MyString(i)
     (*(*e).data)[i,*,*]=ApplyFilter(F,REFORM(((*(*e).data)[i,*,*])))
  END
  DestroyProgressBar
END


PRO RampFilterStack
  ;; for compatibility with older program parts
 FFTFilterStack, /RAMP
END

FUNCTION GetRampFilter, dimx, dimy
;; for compatibility with older program parts
  return, GetFFTFilter(dimx, dimy, /RAMP)
END


PRO ButterworthFilter, HIGHPASS=highpass, LOWPASS=lowpass
 ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     printtocon, "% FFTFilterStack: Root pointer is invalid" 
     return
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% FFTFilterStack: current stack pointer is invalid" 
     return
  END
  e=(*c).datap
  IF (NOT(PTR_VALID(e))) THEN BEGIN
     printtocon, "% FFTFilterStack: current data pointer is invalid" 
     return
  END
cutoff=0.5
expon=1.
mydialog=obj_new("ListDialog",TITLE="Butterworth High Pass Filter")
mydialog->AddOption,"Cutoff Frequency (0-1) ", cutoff
mydialog->AddOption,"Exponent/2               ", expon
IF (mydialog->UserDialog() GT 0) THEN BEGIN
   IF (mydialog->Value("Cutoff Frequency (pixel) ",x)) THEN cutoff=x*(*e).SzX*0.5
   IF (mydialog->Value("Exponent/2               ",x)) THEN expon=x
   bwpar=[cutoff,expon]
   if keyword_set(highpass) THEN FFTFilterStack, BWHI=bwpar
   if keyword_set(lowpass) THEN FFTFilterStack, BWLO=bwpar
END
obj_destroy, mydialog
END


PRO ButterworthBandpassFilter
 ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     printtocon, "% ButterworthBandpassFilter: Root pointer is invalid" 
     return
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% ButterworthBandpassFilter: current stack pointer is invalid" 
     return
  END
  e=(*c).datap
  IF (NOT(PTR_VALID(e))) THEN BEGIN
     printtocon, "% ButterworthBandpassFilter: current data pointer is invalid" 
     return
  END
locutoff=0.25
hicutoff=0.5
expon=1.
mydialog=obj_new("ListDialog",TITLE="Butterworth Bandpass Filter")
mydialog->AddOption,"Low Frequency Cutoff  (0-1) ", locutoff
mydialog->AddOption,"High Frequency Cutoff (0-1) ", hicutoff
mydialog->AddOption,"Exponent                    ", expon
IF (mydialog->UserDialog() GT 0) THEN BEGIN
   IF (mydialog->Value("Low Frequency Cutoff  (0-1) ",x)) THEN locutoff=x
   IF (mydialog->Value("High Frequency Cutoff (0-1) ",x)) THEN hicutoff=x
   IF (mydialog->Value("Exponent                    ",x)) THEN expon=x
   name="ButterworthBandpass("+(*c).name+")"
   pminv=[1] & pmaxv=[(*e).SzX] & ptext=["Filtering Image Nr."]
   InitProgressBar, MINV=pminv, MAXV=pmaxv, TEXT=ptext
   For i=0,((*e).SzX-1) DO BEGIN
      ProgressBarSet, 0, i, TEXT=MyString(i)
      (*(*e).data)[i,*,*]=BANDPASS_FILTER(REFORM(((*(*e).data)[i,*,*])), locutoff, hicutoff, BUTTERWORTH=expon)
   END
   DestroyProgressBar
END
obj_destroy, mydialog
END


PRO BandpassFilter, GAUSSIAN=gaussian, IDEAL=ideal
 ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     printtocon, "% BandpassFilter: Root pointer is invalid" 
     return
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% BandpassFilter: current stack pointer is invalid" 
     return
  END
  e=(*c).datap
  IF (NOT(PTR_VALID(e))) THEN BEGIN
     printtocon, "% BandpassFilter: current data pointer is invalid" 
     return
  END
locutoff=0.25
hicutoff=0.5
mydialog=obj_new("ListDialog",TITLE="Bandpass Filter")
mydialog->AddOption,"Low Frequency Cutoff  (0-1) ", locutoff
mydialog->AddOption,"High Frequency Cutoff (0-1) ", hicutoff
IF (mydialog->UserDialog() GT 0) THEN BEGIN
   IF (mydialog->Value("Low Frequency Cutoff  (0-1) ",x)) THEN locutoff=x
   IF (mydialog->Value("High Frequency Cutoff (0-1) ",x)) THEN hicutoff=x
   if keyword_set(gaussian) then name='Gaussian'
   if keyword_set(ideal) then name='Ideal'
   name=name+"Bandpass("+(*c).name+")"
   pminv=[1] & pmaxv=[(*e).SzX] & ptext=["Filtering Image Nr."]
   InitProgressBar, MINV=pminv, MAXV=pmaxv, TEXT=ptext
   For i=0,((*e).SzX-1) DO BEGIN
      ProgressBarSet, 0, i, TEXT=MyString(i)
      (*(*e).data)[i,*,*]=BANDPASS_FILTER(REFORM(((*(*e).data)[i,*,*])), locutoff, hicutoff, GAUSSIAN=gaussian, IDEAL=ideal)
   END
   DestroyProgressBar
END
obj_destroy, mydialog
END


