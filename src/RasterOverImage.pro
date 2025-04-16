FUNCTION RasterOverImage, pim, size, step
  ;; pim=image pointer
  ;; size: side length of square box to scan
  ;; step: steps size for raster grid in pixels
  N=SIZE(*pim, /DIMENSIONS)
  sdimx=N[0] & sdimy=N[1] ;; source dimensions
  nx=sdimx/step & ny=sdimy/step
  NImages=LONG(nx)*LONG(ny)
  dtype=SIZE(*pim,/TYPE)
  case dtype of
	1 : BEGIN
             APtr=PTR_NEW(BYTARR(size,size,NImages))
            END
	2 : BEGIN
             APtr=PTR_NEW(INTARR(size,size,NImages))	
            END
	3 : BEGIN
             APtr=PTR_NEW(LONARR(size,size,NImages))
            END
	4 : BEGIN
             APtr=PTR_NEW(FLTARR(size,size,NImages))
            END
	5 : BEGIN
             APtr=PTR_NEW(DBLARR(size,size,NImages))
            END
	6 : BEGIN
             APtr=PTR_NEW(COMPLEXARR(size,size,NImages))
            END
	9 : BEGIN
             APtr=PTR_NEW(DCOMPLEXARR(size,size,NImages))
            END
	12 : BEGIN
             APtr=PTR_NEW(UINTARR(size,size,NImages))
          END
        13 : BEGIN
             APtr=PTR_NEW(ULONARR(size,size,NImages))
            END
	else: BEGIN
                print, "error: invalid data type " + MyString(dtype)
                return, PTR_NEW() 
              END
     endcase 
  count=0
  printtocon, ["% RasterOverImage: Creating Raster Data", "    nx x ny = "+String(nx)+" x "+String(ny)]
  For j=0,ny-1 DO BEGIN
     im=SHIFT((*pim),[0,-(j*step)])
     col=im[*,0:size]
     For i=0,nx-1 DO BEGIN
        im2=SHIFT(im,[-(i*step),0])
        (*APtr)[*,*,count] = im2[0:size-1,0:size-1]
        count+=1
     END
  END
  return, APtr
END 


Pro SlidingFFT, frmsize, frmstep, POW=pow, HANNING=hanning, PADD=padd
XConsole_PushState
XConsole_Busy
CATCH, Error_status
IF NOT(GetDebugFlag()) THEN BEGIN
   IF (Error_status NE 0) THEN BEGIN
      Printtocon, "% SlidingFFT:    Fatal error "
      Printtocon, "%   Error status  - " + STRING(error_status)
      Printtocon, "%   Error message - " + !ERROR_STATE.MSG
      ErrMsg, !ERROR_STATE.MSG
      CATCH, /Cancel
      XConsole_PopState
      return
   END
END 
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     printtocon, "% SlidingFFT: Root pointer is invalid" 
      XConsole_PopState
     return
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% SlidingFFT: current stack pointer is invalid" 
      XConsole_PopState
     return
  END
  imagee=(*c).datap
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% SlidingFFT: current data pointer is invalid" 
      XConsole_PopState
     return
  END
  ;;
  name="SlidingFFT("+(*c).name+")"
  printtocon, "% SlidingFFT: creating " + name 
  hlffrmsize=FIX(frmsize/2)
  if keyword_set(padd) THEN BEGIN
     if (padd GT frmsize) THEN BEGIN
        X0=FIX((padd-frmsize)/2) & Y0=FIX((padd-frmsize)/2)
        X1=X0+frmsize-1 & Y1=Y0+frmsize-1
        printtocon, "% SlidingFFT: Padding frame size from " + MyString(frmsize) + " to "+ MyString(padd)+" pixels."
        hlffrmsize=FIX((padd)/2)
        paddw=FLTARR(padd,padd)
     END ELSE printtocon, "% SlidingFFT: Padd size " + MyString(padd) + " smaller or equal to frame size "+ MyString(frmsize)+". Ignoring padding request. "     
  END


  CASE (*imagee).zcoord OF
     1: im=Float(REFORM((*(*imagee).data)[(*imagee).slice,*,*]))
     2: im=Float(REFORM((*(*imagee).data)[*,(*imagee).slice,*]))
     3: im=Float(REFORM((*(*imagee).data)[*,*,(*imagee).slice]))
     ELSE: BEGIN
        printtocon, "% SlidingFFT: Image slice coordinate conflict, offending value is "+MyString((*e).zcoord)  
        XConsole_PopState
        return       
     END 
  END 
 

  
  ;; calculate nx and ny
  ;; first index is x0=CEIL(size/2)
  ;; last index is x1=FLOOR(SzX-ssize/2)
  ;; number of steps is FLOOR((x1-x0+1)/step)
  xx0=CEIL(frmsize/2) & xx1=FLOOR((*imagee).SzX-frmsize/2) & nx=FLOOR((xx1-xx0+1)/frmstep)
  yy0=CEIL(frmsize/2) & yy1=FLOOR((*imagee).SzY-frmsize/2) & ny=FLOOR((yy1-yy0+1)/frmstep)

  printtocon, "% SlidingFFT: Sampling image with  " + MyString(nx) +" x " +MyString(ny) +" frames"

  ;;(*pp).SzZ=Long(nx)*Long(ny)
  ;;xsamp=(*imagee).xsamp & ysamp=(*imagee).ysamp
  ;;(*pp).xsamp=1./((*pp).SzX*xsamp) ;; will be FFT, 
  ;;(*pp).ysamp=1./((*pp).SzY*ysamp)
  ;;(*pp).zsamp=(*imagee).zsamp
  ;;(*pp).slice=FLOOR((*pp).SzZ/2)
  ;;(*pp).zcoord=(*imagee).zcoord
  ;;(*pp).contrastmode="auto"
  ;;
  IF keyword_set(padd) THEN ppd=PTR_NEW(FLTARR(padd,padd,LONG(nx)*LONG(ny))) ELSE ppd=PTR_NEW(FLTARR(frmsize, frmsize,LONG(nx)*LONG(ny)))
  IF NOT(PTR_VALID(ppd)) THEN BEGIN
     printtocon, "% SlidingFFT: Failed to create data array." 
     XConsole_PopState
     return
  END

  count=0

   minv=[0,0]
   maxv=[ny-1,nx-1]
   mybar=obj_new('ProgressBar', MINV=minv,MAXV=maxv,TEXT=["row","column"], STATUS="Extracting frames ...", FRAMESTYLE=2)
 
  if keyword_set(hanning) THEN BEGIN
     hwindow = HANNING(frmsize, frmsize)
     printtocon, "% SlidingFFT: Created Hanning window."
  END
   For j=0,ny-1 DO BEGIN
     ;; mybar->ReInit, 1, minv[1], maxv[1], Text="row"
     mybar->Set, 0, j, TEXT=STRING(j)
     tmpim=SHIFT(im,[0,-(j*frmstep)])
     col=tmpim[*,0:(frmsize-1)]
     For i=0,nx-1 DO BEGIN
        mybar->Set, 1, i, TEXT=STRING(i)
        tmpim2=SHIFT(col,[-(i*frmstep),0])
        cutframe=tmpim2[0:frmsize-1,0:frmsize-1]
        ;; apply hanning window
        IF keyword_set(hanning) THEN cutframe *= hwindow
        IF keyword_set(padd) THEN BEGIN
           paddw[X0:X1,Y0:Y1]=cutframe
           (*ppd)[*,*,count] = SHIFT(ABS(FFT(paddw)),-hlffrmsize,-hlffrmsize)
        END ELSE (*ppd)[*,*,count] = SHIFT(ABS(FFT(cutframe)),-hlffrmsize,-hlffrmsize)
        IF keyword_Set(pow) THEN (*ppd)[*,*,count] *= (*ppd)[*,*,count]
        count+=1
     END
  END
  
  obj_destroy, mybar

  N=Size(*ppd)
  empaddata=obj_new('EMPADObj',name,FRAMEX=N[1],FRAMEY=N[2])
  empaddata.SetScanSize, nx, ny
  xsamp=1./((*imagee).xsamp*N[1]) & ysamp=1./((*imagee).ysamp*N[2]) & zsamp=1.
  empaddata.SetScanSampling, (*imagee).xsamp*frmstep, (*imagee).ysamp*frmstep
  empaddata.SetDetectorSampling,   xsamp, ysamp
  empaddata.SetDataPointer, ppd
  ThrowStack, ppd, name, Extra=empaddata, TITLE=name, BIN=1, SAMP=[xsamp,ysamp,zsamp], UNIT=['1/nm','1/nm','1/nm']
  
 
 ;;  Update_XTabControl
END 
