

FUNCTION EMPADObj::Init, name, SCANX=scanx, SCANY=scany, SUBSET=subset, FRAMEX=framex, FRAMEY=framey
 self.setname=name
 IF keyword_set(framex) THEN self.framex=framex ELSE self.framex=128
 IF keyword_set(framey) THEN self.framey=framey ELSE self.framey=130
 self.scanx=256L
 self.scany=256L
 self.centerx=self.framex/2
 self.centery=self.framex/2
 self.setlength=65535L
 IF keyword_set(scanx) THEN self.scanx=LONG(scanx)
 IF keyword_set(scany) THEN self.scany=LONG(scany)
 self.samp=[1.0,1.0]
 self.qsamp=[1.0,1.0]
 IF keyword_set(subset) THEN BEGIN
    self.issubset=1B
    self.psubset=PTR_NEW(subset)
    self.setlength=Size(subset,/N_Elements)
 END
 return, 1
END

FUNCTION EMPADObj::GetName
 return, self.name
END

PRO EMPADObj::SetFrameSize, framex, framey
 self.framex=framex
 self.framey=framey
 self.centerx=self.framex/2
 self.centery=self.framex/2
END

Function EMPADObj::GetFrameSize, X=x, Y=Y
  IF keyword_set(x) THEN return, self.framex
  IF keyword_set(y) THEN return, self.framey
END

PRO EMPADObj::SetFrameCenter, framecenterx, framecentery
 self.centerx=framecenterx
 self.centery=framecentery
END

PRO EMPADObj::FlipScanXY
 tmp=self.scanx
 self.scanx=self.scany
 self.scany=tmp
END

Function EMPADObj::GetFrameCenter, X=x, Y=Y
  IF keyword_set(x) THEN return, self.framecenterx
  IF keyword_set(y) THEN return, self.framecentery
END

Function EMPADObj::GetFrameSize, X=x, Y=Y
  IF keyword_set(x) THEN return, self.framex
  IF keyword_set(y) THEN return, self.framey
END

PRO EMPADObj::SetScanSize, scanx, scany
 self.scanx=Long(scanx)
 self.scany=Long(scany)
 self.setlength=scanx*scany
END

PRO EMPADObj::ReshapeScanSize
  s=list()
  sx=self.scanx
  sy=self.scany
  s.Add, {value:sx,label:"Scan size x, y",newrow:1B}
  s.Add, {value:sy,label:"",newrow:0B}
  c=list()
  finished=0
  WHILE (NOT(finished)) DO BEGIN 
     IF NOT(XMDataChoiceField(s, c, TITLE="Edit Properties") EQ 1) THEN BEGIN
        printtocon, "% EMPADObj::ReshapeScanSize: Aborting."
        return
     END
     sx=s[0].value
     sy=s[1].value
     IF ((sx*sy) EQ self.setlength) THEN finished=1
  END
  self.scanx=Long(sx)
  self.scany=Long(sy)
  self.setlength=scanx*scany
END

PRO EMPADObj::SetScanSampling, sampx, sampy
 self.samp=[sampx,sampy]
END

PRO EMPADObj::SetDetectorSampling, qsampx, qsampy
 self.qsamp=[qsampx,qsampy]
END

Function EMPADObj::GetScanSampling, X=x, Y=Y
  IF keyword_set(x) THEN return, self.samp[0]
  IF keyword_set(y) THEN return, self.samp[1]
END

Function EMPADObj::GetDetectorSampling, X=x, Y=Y
  IF keyword_set(x) THEN return, self.qsamp[0]
  IF keyword_set(y) THEN return, self.qsamp[1]
END

Function EMPADObj::GetScanSize, X=x, Y=Y
  IF keyword_set(x) THEN return, self.scanx
  IF keyword_set(y) THEN return, self.scany
END

Function EMPADObj::GetSetLength
 return, self.setlength
END

Function EMPADObj::GetSetName
 return, self.setname
END


PRO EMPADObj::Read, filename, SWAPENDIAN=swapendian
  self.setname=filename
 self.p=ReadEMPADFile(self.setname, self.scanx, self.scany) ;; the pointer to the data array
END 

PRO EMPADObj::ReadMRC, filename, SWAPENDIAN=swapendian
  self.setname=filename
  self.p=ReadEMPADDataFromMRC(fname, self.scanx, self.scany, /VERBOSE, SWAPENDIAN=swapendian)
END 

Function EMPADObj::GetDataPointer
 return, self.p
END

PRO EMPADObj::SetDataPointer, p
 self.p=p
END

PRO EMPADObj::SetSubset, subset
  self.issubset=1
  IF PTR_VALID(self.psubset) THEN PTR_FREE, self.psubset
  self.psubset=PTR_NEW(subset)
  self.setlength=SIZE(subset, /N_ELEMENTS)
 return
END

FUNCTION EMPADObj::GetSubset, SUBSET=subset
  IF (self.issubset EQ 1) THEN BEGIN
     IF PTR_VALID(self.psubset) THEN BEGIN
        IF keyword_set(subset) THEN subset=*(self.psubset)
        return, 1
     END
  END 
 return, 0
END

PRO EMPADObj::Center_uDiff, SUBPIX=subpix, COMRADIUS=comradius, MAXONLY=maxonly, FITORDER=fitorder, PIXWISE=pixwise, GRAPHS=graphs
  ;; p pointer to the current data stack
  IF NOT(keyword_set(fitorder)) THEN fitorder=0
  fitorder=FIX(fitorder)
  IF (fitorder GT 3) THEN BEGIN
     fitorder=3
     printtocon, "% EMPADObj::Center_uDiff - Maximum fit order is 3. Setting fit order to 3."
  END
  IF (fitorder LT 0) THEN BEGIN
     fitorder=0
     printtocon, "% EMPADObj::Center_uDiff - Minimum fit order is 0. Setting fit order to 0."
  END
  refradius=5
  datap=self.p
  IF keyword_set(comradius) THEN refradius=comradius
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPADObj::Center_uDiff: Current stack pointer is invalid." 
     return
  END
  N=Size(*datap)
  DimX=N[1] & DimY=N[2] & DimZ=N[3]
  sx=FLTARR(DimZ)
  sy=FLTARR(DimZ)
  detectordim=self.framex
  support=1
  mask=GetMask(dimx,dimy, comradius,0,-1, SUPPORT=support, /CUTEMPAD)
  gradx=GradIm(dimx,dimy,0,0, /X,/CUTEMPAD)
  grady=GradIm(dimx,dimy,0,0, /Y,/CUTEMPAD)
  ;; 
  detectordim=self.framex
  For i=0L,DimZ-1 Do BEGIN
     ;; iterate centre of mass
     tmp=(*datap)[*,*,i]*mask
     sx[i]=Total(tmp*gradx)/Total(tmp)
     sy[i]=Total(tmp*grady)/Total(tmp)
     ;; print, i, sx[i], sy [i]
  END
  
  if keyword_set(graphs) THEN ThrowGraph, list(sx,sy), TITLE="Centre Displacements", XTITLE="index", YTITLE="dx, dy (pix)"
  Printtocon, "  Displacement in X: Min="+MyString(Min(sx))+"' Max="+MyString(Max(sx))
  Printtocon, "  Displacement in Y: Min="+MyString(Min(sy))+"' Max="+MyString(Max(sy)) 
  IF (self.issubset) THEN BEGIN
     im=FLTARR(self.scanx*self.scany)
     im[*(self.psubset)]=sx
     im=REFORM(im,self.scanx,self.scany)
  END ELSE im=REFORM(sx,self.scanx,self.scany)
  ;; take care of NANs
  B=WHERE(Finite(im, /NAN), count)
  IF (count GT 0) THEN im[B]=0.
  fit=SFIT(im,fitorder)
  If keyword_Set(pixwise) THEN dx=im ELSE dx=fit
  name="DX("+self.setname+")"
  ThrowImage, dx, TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
  ;;
  IF (self.issubset) THEN BEGIN
     im=FLTARR(self.scanx*self.scany)
     im[*(self.psubset)]=sy
     im=REFORM(im,self.scanx,self.scany)
  END ELSE im=REFORM(sy,self.scanx,self.scany)
  ;; take care of NANs
  B=WHERE(Finite(im, /NAN), count)
  IF (count GT 0) THEN im[B]=0.
  fit=SFIT(im,fitorder)
  If keyword_Set(pixwise) THEN dy=im ELSE dy=fit
  name="DY("+self.setname+")"
  ThrowImage, dy, TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
  ;; correct stack
  sx=REFORM(dx,self.scanx*self.scany)
  sy=REFORM(dy,self.scanx*self.scany)
  For i=0L,DimZ-1 Do BEGIN
     IF keyword_set(subpix) THEN BEGIN
        (*datap)[*,0:(detectordim-1),i] = FLOAT(subshift(REFORM((*datap)[*,0:(detectordim-1),i]),-sx[i],-sy[i]))
     END ELSE BEGIN
        dx=ROUND(sx[i])
        dy=ROUND(sy[i])
        (*datap)[*,0:(detectordim-1),i]=SHIFT((*datap)[*,0:(detectordim-1),i],-dx,-dy)
     END
  END
END


PRO EMPADObj::FrameShift, DX=dx, DY=dy, SUBPIX=subpix
  ;; p pointer to the current data stack
  IF NOT(keyword_set(dx)) THEN dx=0
  IF NOT(keyword_set(dy)) THEN dy=0
  IF ((dx EQ 0) AND (dy EQ 0)) THEN return
  datap=self.p
  N=Size(*datap)
  DimX=N[1] & DimY=N[2] & DimZ=N[3]
  detectordim=self.framex
  For i=0L,DimZ-1 Do BEGIN
     IF keyword_set(subpix) THEN BEGIN
        (*datap)[*,0:(detectordim-1),i] = FLOAT(subshift(REFORM((*datap)[*,0:(detectordim-1),i]),dx,dy))
     END ELSE BEGIN
        dx=ROUND(dx)
        dy=ROUND(dy)
        (*datap)[*,0:(detectordim-1),i]=SHIFT((*datap)[*,0:(detectordim-1),i],dx,dy)
     END
  END
END


PRO EMPADObj::SpatialAverage, halfwidth, RADIALWEIGHT=radialweight, SUMUP=sumup
  ;; Average over neighbouring real space pixels
  ;; neihbouring pixels to the left and right are considered in a
  ;; square-pattern:
  ;; halfwidth=1 means 9 pixels
  ;; halfwidth=2 means 25 pixels
  ;; p pointer to the current data stack
  ;; sumup means summing instead of averaging
  datap=self.p
  IF keyword_set(comradius) THEN refradius=comradius
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPADObj::SpatialAverage: Current stack pointer is invalid." 
     return
  END
  N=Size(*datap)
  DimX=LONG(self.scanx) & DimY=LONG(self.scany) & DimZ=LONG(N[3])
  detectordim=self.framex
  halfwidth=ABS(FIX(halfwidth))
  filter=SquareROIIndices(DimX,DimY,FIX(halfwidth))
  Norm=halfwidth*2+1
  Norm=Norm*Norm
  tmp=FLTARR(self.framex,self.framey)
  if keyword_set(sumup) THEN BEGIN
     printtocon, "% EMPADObj::SpatialAverage: Summing over "+MyString(Norm)+" raster positions."
     Norm=1.
  END  ELSE BEGIN
     printtocon, "% EMPADObj::SpatialAverage: Averaging over "+MyString(Norm)+" raster positions."
     Norm=1./Norm
  END
  ;; Well have to go row by row
  ;; in each row we'll start from index halfwidth
  ;; and proceed to index DimX-1-halfwidth
  ;; the firts row to process is halfwidth
  ;; the lastrow is DimY-1-halfwidth
  startcol=halfwidth
  endcol=DimX-1-halfwidth
  startrow=halfwidth
  endrow=DimY-1-halfwidth
  marginpix=LONG(halfwidth)*DimX+halfwidth
  maxind=DimY*DimX-1-marginpix
  minv=[startrow] ;; slow processing axis
  maxv=[endrow]
  mybar=obj_new('ProgressBar', MINV=minv,MAXV=maxv,TEXT=["row"], STATUS="Processing line ...", FRAMESTYLE=2)
  targetarray=(*datap)[*,*,*] ;;; needs lots of memory
  For j=startrow,endrow Do BEGIN ;; loop over all diffraction patterns, ignore margins
     mybar->Set, 0, j, TEXT=STRING(j) ;; update progress bar value
     f=filter+j*DIMX+startcol ;; advance rows
     For i=startcol,endcol DO BEGIN
        tmp[*,*]=0.
        if keyword_set(sumup) THEN BEGIN
           for k=0,N_Elements(f)-1 Do tmp += (*datap)[*,*,f[k]]
        END ELSE BEGIN
           for k=0,N_Elements(f)-1 Do tmp += Norm*(*datap)[*,*,f[k]]
        END
        ;; fill in avergaged data in the previous array
        targetarray[*,*,j*DimX+i]=tmp
        f=f+1 ;; advance one column
     END    
  END
  (*datap)=targetarray
  obj_destroy, mybar
END  


PRO EMPADObj::UpScale, factorx, factory, P=p, INTERP=interp
  ;;
  ;; Upscale diffraction frames
  ;;
  datap=self.p
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPADObj::UpScale: Current stack pointer is invalid." 
     return
  END
  N=Size(*datap)
  DimX=LONG(self.scanx) & DimY=LONG(self.scany) & DimZ=LONG(N[3])
  framex=FIX(self.framex*factorx) & framey=FIX(self.framey*factory)
  printtocon, "% EMPADObj::UpScale: Scaling in x, y by factor " + MyString(framex/self.framex)+", "+MyString(framey/self.framey)
  tmpdata=FLTARR(framex,framey,DimZ)
  minv=[0]
  maxv=[100]
  pbarperc=0
  pbarstep=5
  M=LONG(DimZ)
  rb=(((framex MOD self.framex) EQ 0) AND (framey MOD self.framey))
  if keyword_set(interp) THEN rb=0
  norm=1. ;; total needs to remain the same
  mybar=obj_new('ProgressBar', MINV=minv,MAXV=maxv,TEXT=["% done"], STATUS="Processing frames ...", FRAMESTYLE=2)
  For i=0,DimZ-1 Do BEGIN ;; loop over all diffraction patterns, ignore margins
     perc=LONG(i*100. / M)
     if (FIX(perc) GT pbarperc) THEN BEGIN
        pbarperc += pbarstep
        mybar->Set, 0, pbarperc, TEXT=STRING(pbarperc)
     END
     IF rb THEN tmpdata[*,*,i]=norm*Rebin((*datap)[*,*,i], framex, framey) ELSE tmpdata[*,*,i]=norm*CONGRID((*datap)[*,*,i], framex, framey, CUBIC=-0.5)
  END
  tmpp=datap
  self.p=PTR_NEW(tmpdata)
  self->SetDetectorSampling, self.qsamp[0]*self.framex/framex, self.qsamp[1]*self.framey/framey
  self->SetFrameSize, framex, framey
  PTR_FREE, tmpp
  obj_destroy, mybar
  IF keyword_set(p) THEN BEGIN 
     e=(*p).datap
     (*e).id="UpScale("+(*e).id+")"
     (*e).SzX=framex & (*e).SzY=framey
     (*e).xsamp=self.qsamp[0] & (*e).ysamp=self.qsamp[1]
     if ((*e).displayscalebar EQ 1) THEN  BEGIN
        ;; renew scalebar
        color=(*(*e).ScaleBar).color
        PTR_FREE, (*e).ScaleBar
        (*e).ScaleBar=PTR_NEW(GetScaleBar(((*e).SzX),((*e).xsamp), UNIT=((*e).xunit)))
        (*(*e).ScaleBar).color=color
     END
     (*e).data=self.p
  END
  

END  


PRO EMPADObj::CropFrames, X0, WX, Y0, HY, P=p
;;
  ;; Upscale diffraction frames
  ;;
  datap=self.p
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPADObj::CropFrames: Current stack pointer is invalid." 
     return
  END
  N=Size(*datap)
  DimX=LONG(self.scanx) & DimY=LONG(self.scany) & DimZ=LONG(N[3])
  framex=FIX(WX) & framey=FIX(HY)
  X1=X0+framex-1 & Y1=Y0+framey-1
  printtocon, "% EMPADObj::CropFrames: (x0:x1,y0:y1)=(" + MyString(X0)+":"+MyString(X1)+", "+ MyString(Y0)+":"+MyString(Y1)
  dtype=SIZE((*datap)[*,*,0],/TYPE)
  tmpdata=make_array(framex,framey,DimZ, TYPE=dtype)
  minv=[0]
  maxv=[100]
  pbarperc=0
  pbarstep=5
  mybar=obj_new('ProgressBar', MINV=minv,MAXV=maxv,TEXT=["% done"], STATUS="Processing frames ...", FRAMESTYLE=2)
  For i=0L,DimZ-1 Do BEGIN ;; loop over all diffraction patterns, ignore margins
     perc=i*100./DimZ
     if (FIX(perc) GT pbarperc) THEN BEGIN
        pbarperc += pbarstep
        mybar->Set, 0, pbarperc, TEXT=STRING(pbarperc)
     END
    tmpdata[*,*,i]=((*datap)[X0:X1,Y0:Y1,i])
  END
  tmpp=datap
  self.p=PTR_NEW(tmpdata)
  ;; 
  self->SetFrameSize, framex, framey
  PTR_FREE, tmpp
  obj_destroy, mybar
  IF keyword_set(p) THEN BEGIN 
     e=(*p).datap
     (*e).id="CroppedFrames("+(*e).id+")"
     (*e).SzX=framex & (*e).SzY=framey
     (*e).xsamp=self.qsamp[0] & (*e).ysamp=self.qsamp[1]
     if ((*e).displayscalebar EQ 1) THEN  BEGIN
        ;; renew scalebar
        color=(*(*e).ScaleBar).color
        PTR_FREE, (*e).ScaleBar
        (*e).ScaleBar=PTR_NEW(GetScaleBar(((*e).SzX),((*e).xsamp), UNIT=((*e).xunit)))
        (*(*e).ScaleBar).color=color
     END
     (*e).data=self.p
  END
  

END  

FUNCTION EMPADObj::SpatialBinning, bin
  ;; Bin over neighbouring real space pixels
  ;; neihbouring pixels to the left and right are considered in a
  ;; square-pattern:
  ;; p pointer to the current data stack
  ;;
  ;; BUG: There is still an issue with the last row
  datap=self.p
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPADObj::SpatialBinning: Current stack pointer is invalid." 
     return, PTR_NEW()
  END
  N=Size(*datap)
  DimX=LONG(self.scanx) & DimY=LONG(self.scany) & DimZ=LONG(N[3])
  detectordim=self.framex
  ;; BinDimX=FLOOR(DimX/bin) & BinDimY=FLOOR(DimY/bin) &
  ;; BinDimZ=BinDimX*BinDimY;; spatial binning, the remaining
  ;; dimensions in x,y,z
  BinDimX=FLOOR(DimX/bin) & BinDimY=FLOOR(DimY/bin) & BinDimZ=BinDimX*BinDimY;; spatial binning, the remaining dimensions in x,y,z
  filter=SquareROIIndices(DimX,DimY,bin,/NONCENTERED) ;; index square patch, the 0,0/0,1/1,0/1,1 pixel indices for bin 2 = 0,DimX,1,DimX+1
  tmp=FLTARR(self.framex,self.framey)
  Norm=1. ;; sum up the data, do not average
  minv=[0]
  maxv=[100]
  pbarstep=5
  pbarperc=0
  M=LONG(BinDimX*BinDimY)
  pbindata=PTR_NEW(FLTARR(self.framex,self.framey,BinDimZ))
  mybar=obj_new('ProgressBar', MINV=minv,MAXV=maxv,TEXT=["% done"], STATUS="Processing frames ...", FRAMESTYLE=2)
  ;;;; for debugging
  ;;origwdow=!D.WINDOW
  ;;a=BYTARR(DimX,DimY)
  ;;Window, XSIZE=DimX, YSIZE=DimY
  ;;wdow=!D.WINDOW
  ;; 
  ;; (BinDimZ-1)*bin*bin
  ;;
  max=(DimZ-max(filter)-1)/bin/bin
  maxloop=Min([max,BinDimZ-1])
  For i=0,maxloop Do BEGIN ;; loop over all diffraction patterns, ignore margins
     ;; fast direction is over columns (!), slow over rows
     ;; a row is rowunbin=i*bin/DimX
     ;; a column in the unbinned case is colunbin=(i*bin) MOD DimX
     ;; the last pixel has an issue
     perc=Fix(i*100. / M)
     if (FIX(perc) GE pbarperc) THEN BEGIN
        ;; increment progressbar in 5% steps
        pbarperc += pbarstep
        mybar->Set, 0, pbarperc, TEXT=STRING(pbarperc)
     END
     ;; advance filter position along columns
     ;; 
     rowunbin=i*bin*bin/DimX
     colunbin=(i*bin) MOD DimX 
     ;; advance filter along columns
     ;; rowunbin=(i*bin) MOD DimY
     ;; colunbin=i*bin/DimY
     f = filter + LONG(rowunbin*DimX)+colunbin
     ;; f = filter + LONG(rowunbin)+colunbin
;; f gets a very strange value when the program runs, why?
     tmp[*,*]=0. ;; initialize binned data
     ;;
     for j=0,N_Elements(f)-1 Do tmp += Norm*(*datap)[*,*,f[j]]
     (*pbindata)[*,*,i]=tmp
     ;;;; for debugging
     ;; WSET, wdow
     ;;a[*,*]=0 & a[f]=1 & TVSCL, a
     ;; WSET, origwdow
  END
  obj_destroy, mybar
  return, pbindata
END


FUNCTION EMPADObj::SpatialBinningTwoStage, bin
  ;; Bin over neighbouring real space pixels
  ;; neihbouring pixels to the left and right are considered in a
  ;; square-pattern:
  ;; p pointer to the current data stack
  ;;
  ;; BUG: There is still an issue with the last row
  datap=self.p
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPADObj::SpatialBinning: Current stack pointer is invalid." 
     return, PTR_NEW()
  END
  N=Size(*datap)
  DimX=LONG(self.scanx) & DimY=LONG(self.scany) & DimZ=LONG(N[3])
  detectordim=self.framex
  ;; BinDimX=FLOOR(DimX/bin) & BinDimY=FLOOR(DimY/bin) &
  ;; BinDimZ=BinDimX*BinDimY;; spatial binning, the remaining
  ;; dimensions in x,y,z
  BinDimX=FLOOR(DimX/bin) & BinDimY=FLOOR(DimY/bin) & BinDimZ=BinDimX*BinDimY;; spatial binning, the remaining dimensions in x,y,z
  Norm=1. ;; sum up the data, do not average
  minv=[0]
  maxv=[100]
  pbarstep=5
  pbarperc=0
  M=LONG(BinDimX*BinDimY)
  StageOneData=FLTARR(self.framex,self.framey,BinDimX*DimY)
  
  mybar=obj_new('ProgressBar', MINV=minv,MAXV=maxv,TEXT=["% done"], STATUS="Binning Stage 1 ...", FRAMESTYLE=2)
  ;;;; for debugging
  ;;origwdow=!D.WINDOW
  ;;a=BYTARR(DimX,DimY)
  ;;Window, XSIZE=DimX, YSIZE=DimY
  ;;wdow=!D.WINDOW
  ;; 
  ;; (BinDimZ-1)*bin*bin
  ;;
  ;;Stage1
  StageOneData=bin*Rebin((*datap), self.framex, self.framey,BinDimX*DimY,/SAMPLE)
  ncols=BinDimX & nrows=BinDimY
  mybar->ReInit, 0, 0, nrows
  mybar->SetStatus, "Binning stage 2 ..."
   ;; Stage 2
  pbindata=PTR_NEW(FLTARR(self.framex,self.framey,BinDimZ))
  For j0=0,nrows-1 DO BEGIN
     ;; loop over rows
     ;; we want to add sequential rows,
     ;; their indices in the one-dimensional scheme are
     ;; (j0*bin+0)*ncols:((j0*bin+1)*ncols-1)
     ;; (j0*bin+1)*ncols:((j0*bin+2)*ncols-1)
     ;; ....
     ;; (j0*bin+i)*ncols:((j0*bin+i+1)*ncols-1) where j0=row in binned frame, i=0 ..binfact
     mybar->Set, 0, j0+1, TEXT=STRING(j0)
     i0=j0*bin*ncols ;; starting row element for binning, not that the target row is j0 and the starting element is j0*ncols
        (*pbindata)[*,*,(j0*ncols):((j0+1)*ncols-1)] = StageOneData[*,*,i0:(i0+ncols-1)]
        printtocon, "% debug: -  row index "+ MyString(i0) + " -> "+  MyString((j0*ncols)) +" in binned scheme"
        For i=2,bin Do Begin
           printtocon, "%        adding frame index range "+ MyString((i0+(i-1)*ncols)) + " -> "+ MyString((i0+i*ncols-1)) +" to binned row"
           printtocon, "%                              at "+ MyString((j0*ncols)) + " -> "+ MyString(((j0+1)*ncols-1)) +" to binned row"
           (*pbindata)[*,*,(j0*ncols):((j0+1)*ncols-1)] += StageOneData[*,*,(i0+(i-1)*ncols):(i0+i*ncols-1)]
        END 
     END 
     ;;;; for debugging
     ;; WSET, wdow
     ;;a[*,*]=0 & a[f]=1 & TVSCL, a
     ;; WSET, origwdow
  obj_destroy, mybar
  return, pbindata
END   


PRO EMPADObj::Threshold, threshold, TOONE=toone
  ;; Set all pixel values below threshold to 0 or 1 (for log processing)
  datap=self.p
  val=0.
  IF keyword_set(toone) THEN val=1.
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPADObj::Threshold: Current stack pointer is invalid." 
     return
  END
  N=Size(*datap)
  DimX=LONG(self.scanx) & DimY=LONG(self.scany) & DimZ=LONG(N[3])
  minv=[0]
  maxv=[100]
  pbarperc=0
  pbarstep=5
  M=LONG(DimX*DimY)
  mybar=obj_new('ProgressBar', MINV=minv,MAXV=maxv,TEXT=["% done"], STATUS="Processing frames ...", FRAMESTYLE=2)
  For i=0,DimZ-1 Do BEGIN ;; loop over all diffraction patterns
     perc=LONG(i*100. / M)
     if (FIX(perc) GT pbarperc) THEN BEGIN
        pbarperc += pbarstep
        mybar->Set, 0, pbarperc, TEXT=STRING(pbarperc)
     END
     ;;; find all values lower than thresh and set them to 0 or 1
     tmp=(*datap)[*,*,i]
     B=WHERE(tmp LT threshold,count)
     if (count GT 0) THEN BEGIN
        tmp[B]=val
        (*datap)[*,*,i]=tmp
     END
  END
  obj_destroy, mybar
END

PRO EMPADObj::HalfRotAdd
  ;; Mirror frame at x and y and add the two frames to symmetrize the diffraction data
  datap=self.p
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPADObj::HalfRotAdd: Current stack pointer is invalid." 
     return
  END
  N=Size(*datap)
  DimX=LONG(self.scanx) & DimY=LONG(self.scany) & DimZ=LONG(N[3])
  minv=[0]
  maxv=[100]
  pbarperc=0
  pbarstep=5
  M=LONG(DimX*DimY)
  mybar=obj_new('ProgressBar', MINV=minv,MAXV=maxv,TEXT=["% done"], STATUS="Processing frames ...", FRAMESTYLE=2)
  For i=0,DimZ-1 Do BEGIN ;; loop over all diffraction patterns
     perc=LONG(i*100. / M)
     if (FIX(perc) GT pbarperc) THEN BEGIN
        pbarperc += pbarstep
        mybar->Set, 0, pbarperc, TEXT=STRING(pbarperc)
     END
     ;;; find all values lower than thresh and set them to 0 or 1
     tmp=ROTATE((*datap)[*,*,i],2) ;; Frame data rotated 180 deg counterclockwise
     (*datap)[*,*,i]+=tmp
  END
  obj_destroy, mybar
END

PRO EMPADObj::MaskCols, number, LEFT=left, right=right, TOONE=toone
  ;; Set all pixel values below threshold to 0 or 1 (for log processing)
  datap=self.p
  val=0.
  IF keyword_set(toone) THEN val=1.
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPADObj::MaskCols: Current stack pointer is invalid." 
     return
  END
  IF (number GE self.scany) THEN BEGIN
     printtocon, "% EMPADObj::MaskCols: Too many rows." 
     return
  END
  N=Size(*datap)
  DimX=LONG(self.scanx) & DimY=LONG(self.scany) & DimZ=LONG(N[3])
  l=list()
  For i=1,number DO BEGIN
     For j=1,DimY DO BEGIN
        IF keyword_set(left) THEN l.Add, (j-1)*DimX+i-1
        IF keyword_set(right) THEN l.Add, j*DimX-(i+1)
     END
  END
  val=0.
  IF keyword_set(toone) THEN val=1.0
  For i=1,l.Count() Do BEGIN ;; loop over all diffraction patterns
     ;;; find all values lower than thresh and set them to 0 or 1
     (*datap)[*,*,l[i-1]]=val
  END
END  

PRO EMPADObj::MaskRows, number, TOP=top, BOTTOM=bottom, TOONE=toone
  ;; Set all pixel values below threshold to 0 or 1 (for log processing)
  datap=self.p
  val=0.
  IF keyword_set(toone) THEN val=1.
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPADObj::MaskRows: Current stack pointer is invalid." 
     return
  END
  IF (number GE self.scany) THEN BEGIN
     printtocon, "% EMPADObj::MaskRows: Too many rows." 
     return
  END
  N=Size(*datap)
  DimX=LONG(self.scanx) & DimY=LONG(self.scany) & DimZ=LONG(N[3])
  l=list()
  For i=1,number DO BEGIN
     For j=1,DimX DO BEGIN
        IF keyword_set(bottom) THEN l.Add, (i-1)*DimX+j-1
        IF keyword_set(top) THEN l.Add, (DimY-1-(i-1))*DimX+j-1
     END
  END
  val=0.
  IF keyword_set(toone) THEN val=1.0
  For i=1,l.Count() Do BEGIN ;; loop over all diffraction patterns
     ;;; find all values lower than thresh and set them to 0 or 1
     (*datap)[*,*,l[i-1]]=val
  END
END

PRO EMPADObj::MaskFrameRows, number, TOP=top, BOTTOM=bottom, TOONE=toone
  ;; Set all pixel values below threshold to 0 or 1 (for log processing)
  datap=self.p
  val=0.
  IF keyword_set(toone) THEN val=1.
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPADObj::MaskRows: Current stack pointer is invalid." 
     return
  END
  IF (number GE self.framey) THEN BEGIN
     printtocon, "% EMPADObj::MaskFrameRows: Too many rows." 
     return
  END
  N=Size(*datap)
  For i=1,number DO BEGIN
     IF keyword_set(top) THEN BEGIN
        row=self.framey-i 
        (*datap)[*,row,*]=val
     END
     IF keyword_set(bottom) THEN BEGIN
        row=i-1
        (*datap)[*,row,*]=val
     END
  END
END  


PRO EMPADObj::SubtractFrame, Frame
  ;; Subtract a frame
  datap=self.p
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPADObj::MaskRows: Current stack pointer is invalid." 
     return
  END
  N=Size(*datap)
  DimX=LONG(self.scanx) & DimY=LONG(self.scany) & DimZ=LONG(N[3])
  For i=0,DimZ-1 DO BEGIN
      ;; loop over all diffraction patterns
     ;;; find all values lower than thresh and set them to 0 or 1
     (*datap)[*,*,i] -= frame
  END
END

PRO EMPADObj::SubtractAvg, CENTERMASKRADIUS=centermaskradius, DX=dx, DY=dy
  ;; Subtract average frame
  ;; if centermaskradius is set the the transmitted beam will be kept
  ;; in the center within the radius centermask radius
  datap=self.p
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPADObj::SubtractAvg: Current stack pointer is invalid." 
     return
  END
  N=Size(*datap)
  Dimx=N[1] & DimY=N[2] & DimZ=N[3]
  norm=1./DimZ
  avg=DBLARR(DimX,DimY)
  For i=0L,DimZ-1 Do BEGIN
     avg+=norm*(*datap)[*,*,i]
  END
  IF keyword_set(centermaskradius) THEN BEGIN
     if not(keyword_set(dx)) THEN dx=0
     if not(keyword_set(dy)) THEN dy=0
     mask=1.-psf_gaussian(NDIMENSION=2,NPIXEL=[self.framex,self.framey],FWHM=[centermaskradius, centermaskradius],CENTROID=[self.framex/2+dx,self.framex/2+dy],/NORMALIZEMAX)
     avg = avg*mask
  END
  For i=0,DimZ-1 DO BEGIN
      ;; loop over all diffraction patterns
     ;;; find all values lower than thresh and set them to 0 or 1
     (*datap)[*,*,i] -= avg
  END
END  


PRO EMPADObj::COM, COMRADIUS=comradius, FITORDER=fitorder
  ;; p pointer to the current data stack
  IF NOT(keyword_set(fitorder)) THEN fitorder=0
  fitorder=FIX(fitorder)
  IF (fitorder GT 3) THEN BEGIN
     fitorder=3
     printtocon, "% EMPADObj::COM - Maximum fit order is 3. Setting fit order to 3."
  END
  IF (fitorder LT 0) THEN BEGIN
     fitorder=0
     printtocon, "% EMPADObj::COM - Minimum fit order is 0. Setting fit order to 0."
  END
  refradius=5
  datap=self.p
  IF keyword_set(comradius) THEN refradius=comradius
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPADObj::Center_uDiff: Current stack pointer is invalid." 
     return
  END
  N=Size(*datap)
  DimX=N[1] & DimY=N[2] & DimZ=N[3]
  delta=2*refradius+1
  sx=FLTARR(DimZ)
  sy=FLTARR(DimZ)
  ;;
  support=1
  mask=GetMask(dimx,dimy, refradius,0,-1, SUPPORT=support, /CUTEMPAD)
  gradx=self.qsamp[0]*GradIm(dimx,dimy,0,0, /X,/CUTEMPAD)
  grady=self.qsamp[1]*GradIm(dimx,dimy,0,0, /Y,/CUTEMPAD)
  ;;
  
  detectordim=self.framex
  For i=0L,DimZ-1 Do BEGIN
     ;; iterate centre of mass
     tmp=(*datap)[*,*,i]*mask
     sx[i]=Total(tmp*gradx)/Total(tmp)
     sy[i]=Total(tmp*grady)/Total(tmp)
     ;; print, i, sx[i], sy [i]
  END 
  ;; ThrowGraph, list(sx,sy), TITLE="COM Displacements",
  ;; XTITLE="index", YTITLE="dx, dy (pix)"
  ;; The displacements are the individual image shift displacements
  IF (self.issubset) THEN BEGIN
     im=FLTARR(self.scanx*self.scany)
     im[*(self.psubset)]=sx
     im=REFORM(im,self.scanx,self.scany)
  END ELSE im=REFORM(sx,self.scanx,self.scany)
;;  name="DX("+self.setname+")"
;;  ThrowImage, im, TITLE=name, BIN=0.5, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
  name="COMX("+self.setname+")"
  ;; fit
  ;; take care of NANs
  B=WHERE(Finite(im, /NAN), count)
  IF (count GT 0) THEN im[B]=0.
  fit=SFIT(im,fitorder)
  comx=im-fit
  ThrowImage, comx, TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
  ;; Y component
  IF (self.issubset) THEN BEGIN
     im=FLTARR(self.scanx*self.scany)
     im[*(self.psubset)]=sy
     im=REFORM(im,self.scanx,self.scany)
  END ELSE im=REFORM(sy,self.scanx,self.scany)
;;  name="DY("+self.setname+")"
;;  ThrowImage, im, TITLE=name, BIN=0.5, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
  name="COMY("+self.setname+")"
  ;; take care of NANs
  B=WHERE(Finite(im, /NAN), count)
  IF (count GT 0) THEN im[B]=0.
  fit=SFIT(im,fitorder)
  comy=im-fit
  ThrowImage, comy, TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','1/nm']
  name="MagCOM("+self.setname+")"
  ThrowImage, Sqrt(comx*comx+comy*comy), TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','1/nm']
  name="ArgCOM("+self.setname+")"
  ThrowImage, Atan(comy,comx), TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','rad']
END



PRO EMPADObj::MapScalarFrameDataToImage,  data, LABEL=label
  ;; produce an output map for the scalar data to the real-space frame
  ;; The length of the 1D array data needs to match the number of slices, if the data set is a subset then the it needs to konform the subset length
  ;;
  ;; Use cases: Process the frames in the raw data stack for a scalar property, this can be done with a routine on a non-PAD object
  ;;            Then map the property back to real space
  IF NOT(GetDebugFlag()) THEN BEGIN
   CATCH, Error_status
   IF (Error_status NE 0) THEN BEGIN
      PrintToCon, "% EMPADObj::MapScalarFrameDataToImage:    Fatal error "
      PrintToCon, "%   Error status  - " + STRING(error_status)
      PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
      CATCH, /Cancel
      return
   END
END
  IF not(keyword_set(label)) THEN label="Map"
  datap=self.p
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPADObj::MapScalarFrameDataToImage: Current stack pointer is invalid." 
     return
  END
  N=Size(*datap)
  DimX=N[1] & DimY=N[2] & DimZ=N[3]
  ;; DimZ is the number of slices
  IF (Size(data,/N_ELEMENTS) NE DimZ) THEN BEGIN
     printtocon, "% EMPADObj::MapScalarFrameDataToImage: Number of frames do not match." 
     return
  END
  IF (self.issubset) THEN BEGIN
     im=FLTARR(self.scanx*self.scany)
     im[*(self.psubset)]=data
     im=REFORM(im,self.scanx,self.scany)
  END ELSE im=REFORM(data,self.scanx,self.scany)
  name=label+"("+self.setname+")"
  ThrowImage, im, TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
; 
END 


PRO EMPADObj::CenterCorrection, COMRADIUS=comradius, FITORDER=fitorder, SUBPIX=subpix, PIXWISE=pixwise, UPSCALING=upscaling, GRAPHS=graphs, REGION=region, ITERATE=iterate, Maps=maps
  ;; p pointer to the current data stack
  IF NOT(keyword_set(comradius)) THEN comradius=10
  IF NOT(keyword_set(fitorder)) THEN fitorder=2
  IF NOT(keyword_set(subpix)) THEN subpix=0
  IF NOT(keyword_set(pixwise)) THEN pixwise=0
  IF NOT(keyword_set(upscaling)) THEN upscaling=0
  IF NOT(keyword_set(iterate)) THEN iterate=0
  scalefact=4
  fitorder=FIX(fitorder)
  IF (fitorder GT 3) THEN BEGIN
     fitorder=3
     printtocon, "% EMPADObj::CenterCorrection - Maximum fit order is 3. Setting fit order to 3."
  END
  IF (fitorder LT 0) THEN BEGIN
     fitorder=0
     printtocon, "% EMPADObj::CenterCorrection - Minimum fit order is 0. Setting fit order to 0."
  END
  IF (subpix GT 0) THEN printtocon, "% EMPADObj::CenterCorrection - Subpixel shift."
  IF (subpix GT 0) THEN printtocon, "% EMPADObj::CenterCorrection - Upscaling data."
  refradius=5
  datap=self.p
  IF keyword_set(comradius) THEN refradius=comradius
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPADObj::Center_uDiff: Current stack pointer is invalid." 
     return
  END
  N=Size(*datap)
  DimX=N[1] & DimY=N[2] & DimZ=N[3]
  delta=2*refradius+1
  comx=INDGEN(delta)
  sx=FLTARR(DimZ)
  sy=FLTARR(DimZ)
  detectordim=self.framex
  IF GetDebugFlag() THEN BEGIN
     origwdow=!D.WINDOW
     preview=FLTARR(DimX, DimY)
     ThrowImage, preview, Title='Debug', BIN=1
     p=GetCurrentP()
     wdow=(*(*p).datap).window
     quiet=0
     WSET, origwdow
  END
  mask=BYTARR(self.framex,detectordim)
  mask[*,*]=1B
  IF keyword_set(region) THEN BEGIN
     ;; we have to create a mask, should be like a beam stop
     ;; STOP
     w=GetCurrentP()
     w=(*w).datap
     IF ((*w).window GT -1) THEN BEGIN
        WSET, (*w).window
        X1=self.framex-1 & binszx=self.framex
        Y1=self.framey-1 & binszy=self.framey
        cx=FIX(((*w).SzX/2)/(*w).binning) & x0=cx
        cy=FIX(((*w).SzY/2)/(*w).binning) & y0=cy
        r0=FIX(((*w).SzX/4)/(*w).binning)
        IF (XCircle(x0,y0,r0) EQ 1) THEN BEGIN
           dx=Round((x0-cx)*(*w).binning)
           dy=Round((y0-cy)*(*w).binning)
           radius=FLOOR(r0*1.0*(*w).binning)
        END ELSE return
        radius=10
        mask=self.AnnularMask(0,radius, EXPONENT=16)
        mask=SHIFT(mask, dx, dy)
        mask=mask[*,0:(detectordim-1)]
     END
  END
  For i=0L,DimZ-1 Do BEGIN
     im=((*datap)[*,0:(detectordim-1),i]*mask) ;; search in masked image
     index_array=where(im EQ Max(im), Count)
     IF (count GT 1) THEN index_array=index_array[0]
     ;; refine max to com
     x=index_array mod detectordim  ;; position of the maximum in x and y
     y=(index_array/detectordim)
     x0=x-refradius & x1=x+refradius  ;; serach range for refinement in absolute coordinates
     y0=y-refradius & y1=y+refradius
     IF ((x0 GE 0) AND (x1 LT DimX) AND (y0 GE 0) AND (y1 LT DimY)) THEN BEGIN
        patch=(*datap)[x0:x1,y0:y1,i]    
        xvec=total(patch,2)
        ddx=(Transpose(comx)#xvec)/Total(xvec)-refradius  ;; center of mass diplacement
        yvec=total(patch,1)
        ddy=(Transpose(comx)#yvec)/Total(yvec)-refradius
        sx[i]=x - self.centerx + ddx    ;; total displace ment to shift the pattern to the centre
        sy[i]=y - self.centery + ddy
        ;; print, i, sx[i], sy [i]
     END
     IF GetDebugFlag() THEN BEGIN
        origwdow=!D.WINDOW
        WSET, wdow
        TV, BYTSCL(ALOG((*datap)[*,*,i]),MIN=3, MAX=16)
        ;; mark peak locations
        oplotmode=3                                      ;; source copy plotting mode
        plcolor=1500L                                    ;; some plot color
        device, get_graphics_function=gmode              ; store the current graphics mode.                 
        device, set_graphics_function=oplotmode          ; XOR mode.
        ;; device coordinates
        plot_Box, x0, x1, y0, y1, COLOR=plcolor, /NOCROSS
        plot_cross, ROUND(x+ddx), Round(y+ddy), COLOR=plcolor, SZ=5
        WSET, origwdow
        device, set_graphics_function=gmode ; source copy mode.
        Wait, 0.1
     END
  END 
  IF keyword_set(graphs) THEN ThrowGraph, list(sx,sy), TITLE="COM Displacements", XTITLE="index", YTITLE="dx, dy (pix)"
  PrintToCon, "% EMPADObj::CenterCorrection, Displacements in X and Y."
  PrintToCon, "   X_Min X_Max Y_Min Y_Max"
  PrintToCon, "   initial: "+MyString(Min(sx))+" "+MyString(Max(sx)) + " "+MyString(Min(sy))+" "+MyString(Max(sy)) 
  IF (self.issubset) THEN BEGIN
     im=FLTARR(self.scanx*self.scany)
     im[*(self.psubset)]=sx
     im=REFORM(im,self.scanx,self.scany)
  END ELSE im=REFORM(sx,self.scanx,self.scany)

  IF (pixwise EQ 0) THEN BEGIN
     ;; correct descan, fit polynomial to the displacements 
     minv=[0] &  maxv=[100]
     mybar=obj_new('ProgressBar', MINV=minv,MAXV=maxv,TEXT=["% done"], STATUS="Fitting displacements ...", FRAMESTYLE=2)
     thresh=2
     IF (self.issubset) THEN BEGIN
        ;; the case of a subset
           ;; calculate grid points X and Y and create 3D array 
           ;; for the fit through Z
           ;; Data is a 3-by-n array containing the x, y, and z
        ;; locations of n points sampled on the surface.
        
        ;;PrintToCon, "% EMPADObj::CenterCorrection: Regression analysis"
        mybar->SetStatus, "Fitting x displacements."
        data=FLTARR(3,N_Elements(*(self.psubset)))
        xvals=FIX(*(self.psubset) MOD self.scanx)
        yvals=FIX(*(self.psubset)/self.scanx)
        data[0,*]=xvals
        data[1,*]=yvals
        data[2,*]=sx
        coeffs=[]
        comx=SFIT(data,fitorder, /IRREGULAR, KX=coeffs, /MAX_DEGREE) ;; fit displacements on the irregular grid
        resid=data[2,*]-comx
        rmean=robust_mean(resid,thresh,rsigma,rnum,GoodInd=GoodInd) ;; do statistics over the residuum
        BadInd=WHERE(resid  GT 1, count) ;; no more than 1 pix deviation is allowed
        IF (count GT 0) THEN BEGIN
           data[2,BadInd]=comx[BadInd]
           comx=SFIT(data,fitorder, /IRREGULAR, KX=coeffs, /MAX_DEGREE) ;; fit displacements on the irregular grid
        END
        ;; PrintToCon, "   Displacement in X (fitted): Min=" +MyString(Min(comx))+", Max=" + MyString(Max(comx))
        ;; PrintToCon, "                      Mean deviation from fit=" +MyString(rmean)
        ;;PrintToCon, "                      Number of skipped outliers=" +MyString(count)
        xrmean=rmean & xcount=count
        im=FLTARR(self.scanx,self.scany)
        im[*,*]=coeffs[0] & im[*(self.psubset)]=comx
        name="CentreCorrX("+self.setname+")"
        if keyword_set(maps) THEN ThrowImage, im, TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
        ;; sxfit=REFORM(comx[2,*])
        ;; mybar->SetStatus, "Interpolating displacements along x."
        ;; comx=cgKrig2D(sxfit,xvals,yvals,NX=self.scanx,NY=self.scany,EXPONENTIAL=[0.5,0]) ;; interpolate to the regular grid, a bit slow
        mybar->SetStatus, "Fitting y displacements."
        data[2,*]=sy
        comy=SFIT(data,fitorder, /IRREGULAR, KX=coeffs, /MAX_DEGREE) ;; fit displacements on the irregular grid
        resid=data[2,*]-comy
        rmean=robust_mean(resid,thresh,rsigma,rnum,GoodInd=GoodInd) ;; do statistics over the residuum
        BadInd=WHERE(resid  GT 1, count) ;; no more than 1 pix deviation is allowed
        IF (count GT 0) THEN data[2,BadInd]=comy[BadInd]
        comy=SFIT(data,fitorder, /IRREGULAR, KX=coeffs, /MAX_DEGREE) ;; fit displacements on the irregular grid
        PrintToCon, "   fit   : "+MyString(Min(comx))+" "+MyString(Max(comx)) + " "+MyString(Min(comy))+" "+MyString(Max(comy)) 
        ;; PrintToCon, "   Displacement in Y: Min=" +MyString(Min(comy))+", Max=" + MyString(Max(comy))
        ;;PrintToCon, "                      Mean deviation from fit=" +MyString(rmean)
        ;;PrintToCon, "                      Number of skipped outliers=" +MyString(count)
        PrintToCon, "   mean deviation from fit x y: "+MyString(xrmean) + " "+MyString(rmean)
        PrintToCon, "   number of skipped outliers x y: "+MyString(xcount) + " "+MyString(count)
        im=FLTARR(self.scanx,self.scany)
        im[*,*]=coeffs[0] & im[*(self.psubset)]=comy
        name="CentreCorrY("+self.setname+")"
        if keyword_set(maps) THEN ThrowImage, im, TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
        ;;
        mybar->SetStatus, "Shift correction."
        mybar->SetStatus, "Shifting frames."
        pbarperc=0
        M=LONG(DimZ)
        mybar->Set, 0, pbarperc, TEXT=STRING(pbarperc)
        For i=0L,DimZ-1 Do BEGIN
           perc=LONG(i)*100 / M
           if (FIX(perc) GT pbarperc) THEN BEGIN
              pbarperc=perc
              mybar->Set, 0, pbarperc, TEXT=STRING(pbarperc)
           END
  
           IF keyword_set(subpix) THEN BEGIN
              (*datap)[*,0:(detectordim-1),i] = FLOAT(subshift(REFORM((*datap)[*,0:(detectordim-1),i]),-comx[i],-comy[i]))
           END ELSE BEGIN
              dx=ROUND(comx[i])
              dy=ROUND(comy[i])
              (*datap)[*,0:(detectordim-1),i]=SHIFT((*datap)[*,0:(detectordim-1),i],-dx,-dy)
           END
        END   
     END ELSE BEGIN
        im=REFORM(sx,self.scanx,self.scany) ;; shift x map
        coeffs=[]
        comx=SFIT(im,fitorder, KX=coeffs, /MAX_DEGREE)              ;; fit displacements in a first round, the grid is regular
        ;; now lets kick out outliers and then do the fit again
        resid=ABS(im-comx)
        rmean=robust_mean(resid,thresh,rsigma,rnum,GoodInd=GoodInd) ;; do statistics over the residuum
        BadInd=WHERE(resid  GT 1, count) 
        IF (count GT 0) THEN BEGIN
           im[BadInd]=comx[BadInd]
           comx=SFIT(im,fitorder, KX=coeffs, /MAX_DEGREE) ;; fit displacements a second time, aftre kicking out the outliers
        END
        ;;PrintToCon, "   Displacement in X: Min=" +MyString(Min(comx))+", Max=" + MyString(Max(comx))
        ;;PrintToCon, "                      Mean deviation from fit=" +MyString(rmean)
        ;;PrintToCon, "                      Number of skipped outliers=" +MyString(count)
        xrmean=rmean & xcount=count
        ;; 
        im=REFORM(sy,self.scanx,self.scany)
        coeffs=[]
        comy=SFIT(im,fitorder, KX=coeffs, /MAX_DEGREE)              ;; fit displacements in a first round, the grid is regular
        ;; now lets kick out outliers and then do the fit again
        resid=ABS(im-comy)
        rmean=robust_mean(resid,thresh,rsigma,rnum,GoodInd=GoodInd)
        BadInd=WHERE(resid  GT 1, count) 
        IF (count GT 0) THEN BEGIN
           im[BadInd]=comy[BadInd]
           comy=SFIT(im,fitorder, KX=coeffs, /MAX_DEGREE) ;; fit displacements a second time, aftre kicking out the outliers
        END
        ;;PrintToCon, "   Displacement in Y: Min=" +MyString(Min(comy))+", Max=" + MyString(Max(comy))
        ;;PrintToCon, "                      Mean deviation from fit=" +MyString(rmean)
        ;;PrintToCon, "                      Number of skipped outliers=" +MyString(count)
;;
        PrintToCon, "   fit   : "+MyString(Min(comx))+" "+MyString(Max(comx)) + " "+MyString(Min(comy))+" "+MyString(Max(comy)) 
        PrintToCon, "   mean deviation from fit x y: "+MyString(xrmean) + " "+MyString(rmean)
        PrintToCon, "   number of skipped outliers x y: "+MyString(xcount) + " "+MyString(count)
     name="CentreCorrX("+self.setname+")"
     if keyword_set(maps) THEN ThrowImage, comx, TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
     name="CentreCorrY("+self.setname+")"
     if keyword_set(maps) THEN ThrowImage, comy, TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
     ;; now shift x and y
     comx=REFORM(comx,self.scanx*self.scany,1)
     comy=REFORM(comy,self.scanx*self.scany,1)
          ;; subpixel shift can be slow, add a progress bar
        
        mybar->SetStatus, "Shifting frames."
        pbarperc=0
        M=LONG(DimZ)
        mybar->Set, 0, pbarperc, TEXT=STRING(pbarperc)
        For i=0L,DimZ-1 Do BEGIN
           perc=LONG(i)*100 / M
        if (FIX(perc) GT pbarperc) THEN BEGIN
           pbarperc=perc
           mybar->Set, 0, pbarperc, TEXT=STRING(pbarperc)
        END
  
        IF keyword_set(subpix) THEN BEGIN
           (*datap)[*,0:(detectordim-1),i] = FLOAT(subshift(REFORM((*datap)[*,0:(detectordim-1),i]),-comx[i],-comy[i]))
        END ELSE BEGIN
           dx=ROUND(comx[i])
           dy=ROUND(comy[i])
           (*datap)[*,0:(detectordim-1),i]=SHIFT((*datap)[*,0:(detectordim-1),i],-dx,-dy)
        END
     END
     END

      obj_destroy, mybar
  END ELSE BEGIN
     ;; shift by the sx, sy ... not the fitted data
     IF (self.issubset) THEN BEGIN
        ;;; XXXXX have to correct the case of a subset
        im=FLTARR(self.scanx,self.scany)
        im[*(self.psubset)]=sx
        name="CentreCorrX("+self.setname+")"
        ThrowImage,im, TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
        im[*(self.psubset)]=sy
        name="CentreCorrY("+self.setname+")"
        ThrowImage, im, TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
     END ELSE BEGIN
        name="CentreCorrX("+self.setname+")"
        ThrowImage, REFORM(sx,self.scanx,self.scany), TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
        name="CentreCorrY("+self.setname+")"
        ThrowImage, REFORM(sy,self.scanx,self.scany), TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
        ;; subpixel shift can be slow, add a progress bar
     END
        pbarperc=0
        M=LONG(DimZ)
        minv=[0] &  maxv=[100]
        mybar=obj_new('ProgressBar', MINV=minv,MAXV=maxv,TEXT=["% done"], STATUS="Fitting displacements ...", FRAMESTYLE=2)
        mybar->Set, 0, pbarperc, TEXT=STRING(pbarperc)
        For i=0L,DimZ-1 Do BEGIN
           perc=LONG(i)*100 / M
           if (FIX(perc) GT pbarperc) THEN BEGIN
              pbarperc=perc
              mybar->Set, 0, pbarperc, TEXT=STRING(pbarperc)
           END
           IF keyword_set(subpix) THEN BEGIN
           IF upscaling THEN BEGIN
              tmp=REFORM((*datap)[*,0:(detectordim-1),i])
              ;; scale image using bilinear inetrpolation
              tmpscaled=REBIN(tmp,detectordim*scalefact,detectordim*scalefact)
              ;; shift image
              dx=ROUND(sx[i]*scalefact)
              dy=ROUND(sy[i]*scalefact)
              tmp=SHIFT(tmpscaled,-dx,-dy)
              ;; scale down
              (*datap)[*,0:(detectordim-1),i]=REBIN(tmp,detectordim,detectordim)
           END ELSE BEGIN
              (*datap)[*,0:(detectordim-1),i] = FLOAT(subshift(REFORM((*datap)[*,0:(detectordim-1),i]),-sx[i],-sy[i]))
           END
        END ELSE BEGIN
           dx=ROUND(sx[i])
           dy=ROUND(sy[i])
           (*datap)[*,0:(detectordim-1),i]=SHIFT((*datap)[*,0:(detectordim-1),i],-dx,-dy)
        END
     END ;; for
        obj_destroy, mybar
  END 
END    

PRO EMPADObj::BF, RADIUS=radius, DX=dx, DY=dy, TITLE=title
  ; p= ptr to the current data stack 
  refradius=8
  IF NOT(keyword_set(dx)) then dx=0
  IF NOT(keyword_set(dy)) then dy=0
  IF keyword_set(radius) then refradius=radius
  datap=self.p
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPAD_BF: Current stack pointer is invalid." 
     return
  END
  N=Size(*datap)
  DimX=N[1] & DimY=N[2] & DimZ=N[3]
  s=FLTARR(DimZ)
  For i=0L,DimZ-1 Do BEGIN
     patch=(*datap)[(self.centerx-dx-refradius):(self.centerx-dx+refradius),(self.centery-dy-refradius):(self.centery-dy+refradius),i]
     s[i]=total(patch,0)
  END
  IF (self.issubset) THEN BEGIN
     im=FLTARR(self.scanx*self.scany)
     im[*(self.psubset)]=s
     im=REFORM(im,self.scanx,self.scany)
  END ELSE im=REFORM(s,self.scanx,self.scany)
  IF keyword_set(title) THEN name=title ELSE name="BF"
  name=name+"("+self.setname+","+MyString(self.centerx-dx)+":"+MyString(self.centery-dy)+","+MyString(refradius)+")"
  ThrowImage, im, TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
END



PRO EMPADObj::Average
  ; p= ptr to the current data stack 
  datap=self.p
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPAD_Average: Current stack pointer is invalid." 
     return
  END
  N=Size(*datap)
  Dimx=N[1] & DimY=N[2] & DimZ=N[3]
  norm=1./DimZ
  avg=DBLARR(DimX,DimY)
  For i=0L,DimZ-1 Do BEGIN
     avg+=norm*(*datap)[*,*,i]
  END
  name="Average("+self.setname+")"
  ThrowImage, avg, TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['1/nm','1/nm','1/nm'] ;; can only be 1/nm ... EMPADObject
End

Function EMPADObj::AnnularMask, inner, outer, EXPONENT=exponent, NEGATIVE=negative, TOPHAT=tophat
  IF NOT(keyword_set(exponent)) THEN exponent=20
  detx=self.framex & dety=self.framey & dy=0
  IF ((self.framey EQ 130) AND (self.framex EQ 128)) THEN dy=-2
  aniso=0
  IF (self.qsamp[0] NE self.qsamp[1]) THEN aniso=self.qsamp[1]/self.qsamp[0]
  PrepareRecSampGrid, detx, dety, pkx, pky, pu, DY=dy, ANISO=aniso
  mask=GetBPNQFilterPreCalcK(pkx, pky, pu, (2.*inner)/detx, (2.*outer)/detx, exponent, /GAUSSIAN, /NOFFTSHIFT, MEANVALUE=centralpix, TOPHAT=tophat)
  If (keyword_set(negative)) THEN mask=1.-mask
  ;; cut the upper rows with scrambled information
  IF (dety GT detx) then mask[*,detx:(dety-1)]=0
  return, mask
END

Function EMPADObj::SegmentMask, inner, outer, DX=dx, DY=dy, EXPONENT=exponent, NEGATIVE=negative, SYMMETRY=symmetry, LARMOR=larmor, CLOCKWISE=clockwise, INDMASK=indmask, TOPHAT=tophat
  ;; prepare the radial segment masks, including edge smoothing
  ;; inner: inner segment radius in pixels
  ;; outer: outer segment radius in pixels
  ;; dx, dy: centre offset in pixels
  ;; exponent: smooting exponent in radial direction
  ;; clockwise: segments will rotate clockwise, default is the
  ;;            mathematical positive rotation, i.e. counterclockwise
  ;; indmask: If set then maskind will return a list of pointers to
  ;;          arrays that contain the indices of the mask-elements in the
  ;;          detector image array
  ;; symmetry: foldedness
  ;;           can be one
  ;; returns a 3D stack of 2D filters, for each symmetry one layer in the 3rd dimension
  IF NOT(keyword_set(exponent)) THEN exponent=20
  IF NOT(keyword_set(dx)) THEN dx=0
  IF NOT(keyword_set(dy)) THEN dy=0
  IF NOT(keyword_set(symmetry)) THEN symmetry=4
  IF NOT(keyword_set(larmor)) THEN larmor=0
  detx=self.framex & dety=self.framey 
  ;; IF ((self.framey EQ 130) AND (self.framex EQ 128)) THEN dy=-2
  aniso=0
  IF (self.qsamp[0] NE self.qsamp[1]) THEN aniso=self.qsamp[1]/self.qsamp[0]
  PrepareRecSampGrid, detx, dety, pkx, pky, pu, DY=dy, DX=dx, ANISO=aniso
  fmask=GetBPNQFilterPreCalcK(pkx, pky, pu, (2.*inner)/detx, (2.*outer)/detx, exponent, /GAUSSIAN, /NOFFTSHIFT, MEANVALUE=centralpix, TOPHAT=tophat)
  ;; cut the upper rows with scrambled information
  IF ((dety EQ 130) AND (detx EQ 128)) then BEGIN ;; is is EMPAD
     rmask=FLTARR(detx,dety)
     rmask[*,0:(dety-3)]=fmask[*,0:(dety-3)]
  END ELSE rmask=fmask
  If (keyword_set(negative)) THEN rmask=1.-rmask
  ;; now the angular mask
  IF keyword_set(aniso) THEN BEGIN
     ;; add dx, dy
     ;; distx=(INDGEN(detx) + dx -detx/2)#REPLICATE(1,dety)
     ;; disty=REPLICATE(1,detx)#((INDGEN(dety) + dy -dety/2)*aniso)
     distx=(INDGEN(detx) - dx -detx/2)#REPLICATE(1,dety)
     disty=REPLICATE(1,detx)#((INDGEN(dety) - dy -dety/2)*aniso)
  END ELSE BEGIN
     ;; distx=(INDGEN(detx) +dx -detx/2)#REPLICATE(1,dety)
     ;; disty=REPLICATE(1,detx)#((INDGEN(dety) + dy -dety/2))
     distx=(INDGEN(detx) -dx -detx/2)#REPLICATE(1,dety)
     disty=REPLICATE(1,detx)#((INDGEN(dety) - dy -dety/2))
  END
  ;; rotate the mask by the larmor offset
  IF keyword_set(clockwise) THEN BEGIN
     angmask=ATAN(-distx*sin(-larmor)+disty*cos(-larmor),-distx*cos(-larmor)-disty*sin(-larmor))
  END ELSE BEGIN
     angmask=ATAN(distx*sin(larmor)-disty*cos(larmor),-distx*cos(larmor)-disty*sin(larmor))
  END
  ;; note that the range of the angular mask is [-Pi,+Pi]
  ;; that means that the zero angle is not 0.
  ;; let's rotate by Pi
  ;;
  mx=Max(angmask) & mn=Min(angmask) ;; because of pixel rounding errors the span is not 2PI
  ;; angmask=angmask+!Pi
  angmask=angmask+(mx-mn)/2.
  ;;
  
  ;; span=(mx-mn)/symmetry
  span=2*!Pi/symmetry
  ;; segments=FINDGEN(symmetry)*2*!PI/symmetry
  segments=FINDGEN(symmetry)*span
  ;; segments span from 0 to 2Pi this way, the first segment
  ;; should start at 0. 
  mask=FLTARR(detx,dety,symmetry)
  if symmetry EQ 1 THEN mask=REFORM(mask,detx,dety,1)
  FOR i=0,symmetry-1 DO BEGIN
     ;; B=WHERE((angmask GE segments[i]) AND (angmask LT (segments[i]+span)),COMPLEMENT=C)
     B=WHERE((angmask GE segments[i]) AND (angmask LT (segments[i]+span)),COMPLEMENT=C)
     tmp=rmask
     tmp[C]=0.
     mask[*,*,i]=tmp
     if keyword_set(indmask) THEN Begin
        IF (i EQ 0) THEN indmask=list()
        indmask.add, PTR_NEW(B)
     END
  END
  return, mask
END

PRO EMPADObj::ADF, RADIUS=radius, DX=dx, DY=dy, Mask=mask
  ;; Radius= two-element vector with inner and outer radius in pix
  ;; dx, dy= optional shift of the detector
  ;; mask = if set then the filter mask will be copied to the image list
  IF NOT(keyword_set(radius)) then BEGIN
     ri=FIX(self.framex*0.1) & ro=ri+self.framex/4
  END ELSE BEGIN
     ri=radius[0]
     ro=radius[1]
  END
  IF NOT(keyword_set(dx)) then dx=0
  IF NOT(keyword_set(dy)) then dy=0
  filt=self.AnnularMask(ri,ro)
  IF ((dx NE 0) OR (dy NE 0)) THEN BEGIN
     filt=SHIFT(filt, dx, dy)
     if (dx GT 0) THEN IF (dx LT dimx) THEN filt[0:dx,*]=0.
     if (dy GT 0) THEN IF (dy LT dimy) THEN filt[*,0:dy]=0.
     if (dx LT 0) THEN IF ((-dx) LT dimX) THEN filt[(dimx+dx-1):(dimx-1),*]=0.
     if (dy LT 0) THEN IF ((-dy) LT dimy) THEN filt[*,(dimy+dy-1):(dimy-1)]=0.
  END
  datap=self.p
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPAD_ADF: Current stack pointer is invalid." 
     return
  END
  N=Size(*datap)
  DimX=N[1] & DimY=N[2] & DimZ=N[3]
  s=FLTARR(DimZ)
  For i=0L,DimZ-1 Do BEGIN
     patch=(*datap)[*,*,i]*filt
     s[i]=total(patch,0)
  END
  IF (self.issubset) THEN BEGIN
     im=FLTARR(self.scanx*self.scany)
     im[*(self.psubset)]=s
     im=REFORM(im,self.scanx,self.scany)
  END ELSE im=REFORM(s,self.scanx,self.scany)
  name="ADF("+self.setname+","+MyString(self.centerx-dx)+":"+MyString(self.centery-dy) $
  + ","+MyString(ri)+":"+MyString(ro) +")"
  ThrowImage, im,TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
  IF keyword_set(mask) THEN ThrowImage, filt,Title="Virtual ADF", SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
END 

PRO EMPADObj::BeamStop, Radius=radius, DX=dx, DY=dy, MASK=mask, GAUSSIAN=gaussian
  ;; gaussian beam stop
  ;; FWHM= two-element vector with horizontal and vertical radius in pix
  ;; dx, dy= optional shift of the beam stop
  ;; mask: IF set then the beamstop mask will be added to the image list
  ;; 
  IF NOT(keyword_set(radius)) then BEGIN
     radius=5
  END 
  IF NOT(keyword_set(dx)) then dx=0
  IF NOT(keyword_set(dy)) then dy=0
  IF keyword_set(gaussian) THEN BEGIN
     mask=1.-psf_gaussian(NDIMENSION=2,NPIXEL=[self.framex,self.framey],FWHM=[radius, radius],CENTROID=[self.framex/2+dx,self.framex/2+dy],/NORMALIZEMAX)
  END ELSE BEGIN
     mask=1.-self.AnnularMask(0,radius, EXPONENT=16)
     IF ((dx NE 0) OR (dy NE 0)) THEN filt=SHIFT(filt, dx, dy)
     IF ((dx NE 0) OR (dy NE 0)) THEN BEGIN
     if (dx GT 0) THEN IF (dx LT dimx) THEN filt[0:dx,*]=0.
     if (dy GT 0) THEN IF (dy LT dimy) THEN filt[*,0:dy]=0.
     if (dx LT 0) THEN IF ((-dx) LT dimX) THEN filt[(dimx+dx-1):(dimx-1),*]=0.
     if (dy LT 0) THEN IF ((-dy) LT dimy) THEN filt[*,(dimy+dy-1):(dimy-1)]=0.
  END
  END
  datap=self.p
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPAD_ADF: Current stack pointer is invalid." 
     return
  END
  N=Size(*datap)
  DimX=N[1] & DimY=N[2] & DimZ=N[3]
  s=FLTARR(DimZ)
  For i=0L,DimZ-1 Do BEGIN
     (*datap)[*,*,i]=(*datap)[*,*,i]*mask
  END
  IF keyword_set(mask) THEN ThrowImage, mask,Title="BeamStop", SAMP=[self.qsamp[0],self.qsamp[1],1.], UNIT=['1/nm','1/nm','nm']
END 

PRO EMPADObj::DiscCentre, SEARCHRADII=searchradii, SPARSE=sparse, DX=dx, DY=dy, CORRECT=correct, RADIUS=radius, METHOD=method, DISPLAY=display
  ;; Find diffraction disc shift and radii 
  ;; FWHM= two-element vector with horizontal and vertical radius in
  ;; pix
  ;; RADIUS: Search range for the radius of the Disc
  ;; method: "filtereddiff" - good for low pixel counts
  ;;
  IF NOT(keyword_set(method)) THEN method="standard"
  IF NOT(keyword_set(searchradii)) then BEGIN
     searchradii=[5,self.framex/2-1]
  END 
  datap=self.p
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPAD_DiscCentre: Current stack pointer is invalid." 
     return
  END
  N=Size(*datap)
  DimX=N[1] & DimY=N[2] & DimZ=N[3]
  s=FLTARR(DimZ)
  thresh=100
  r = 1
  disc = SHIFT(DIST(2*r+1), r, r) LE r
  radrange=INDGEN(searchradii[1]-searchradii[0]+1)+searchradii[0]
  IF NOT(keyword_set(sparse)) THEN BEGIN
     cx=FLTARR(DimZ)
     cy=FLTARR(DimZ)
     cr=FLTARR(DimZ)
     minv=[0]
     maxv=[100]
     mybar=obj_new('ProgressBar', MINV=minv,MAXV=maxv,TEXT=["% done"], STATUS="Processing frames ...", FRAMESTYLE=2)
     pbarperc=0
     M=LONG(DimZ)
     mybar->Set, 0, pbarperc, TEXT=STRING(pbarperc)
     For i=0L,DimZ-1 Do BEGIN
        perc=LONG(i)*100 / M
        if (FIX(perc) GT pbarperc) THEN BEGIN
           pbarperc=perc
           mybar->Set, 0, pbarperc, TEXT=STRING(pbarperc)
        END
        im=(*datap)[*,*,i]
        ;; Create circle gradient image
        Case method OF
           "filtereddiff": BEGIN
              im=(*datap)[*,*,i]
              im2= flattenImage(im,COFF=0.1)
              bdisc=BYTSCL(im2-im)
              thresh=200
           END
           else: BEGIN
              im_byt=BYTSCL((*datap)[*,*,i],Min=0,Max=MEDIAN((*datap)[*,*,i]),/NAN)
              bdisc = MORPH_GRADIENT(im_byt, disc)
           END
        END
        kpts=WHERE(bdisc GT thresh)
        xpts=kpts MOD self.framex
        ypts=kpts/self.framex
        circles=circleHoughLink(xpts,ypts,RADIUS=radrange,THRESHOLD=thresh,NCIRCLES=1,XBINS=200,YBINS=200,XMAX=self.framex/2,YMAX=self.framey/2,Hough_Matrix=A)
        ;; TVSCL, bdisc
        cx[i]=circles.cx
        cy[i]=circles.cy
        cr[i]=circles.R
     END
     obj_destroy, mybar
     name="CentreX("+self.setname+")"
     ThrowImage, REFORM(cx,self.scanx,self.scany),TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
     name="CentreY("+self.setname+")"
     ThrowImage, REFORM(cy,self.scanx,self.scany),TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
     name="Radius("+self.setname+")"
     ThrowImage, REFORM(cr,self.scanx,self.scany),TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
  END ELSE BEGIN
     ;; sparse
     printtocon, "% EMPADObj::CentreDisc"
     printtocon, "   choosing 100 random frames"
     npoints=100
     IF (npoints GT (DimZ/6)) THEN s=DimZ/4 ELSE BEGIN
        s=5
        npoints=s*s
     END
     cx=FLTARR(npoints)
     cy=FLTARR(npoints)
     cr=FLTARR(npoints)
     minv=[0]
     maxv=[npoints]
     ;;mybar=obj_new('ProgressBar', MINV=minv,MAXV=maxv,TEXT=["pattern number"], STATUS="Processing frames ...", FRAMESTYLE=2)
     printtocon, "% EMPADObj::CentreDisc"
     printtocon, "   locx  locy   cx     cy     r"
     For i=0L,npoints-1 Do BEGIN
        ;;mybar->Set, 0, i, TEXT=STRING(i)
        ;; get a random pattern smewhere from the central quarter 
        ind=FIX(randomu(seed)*self.scanx/2+self.scanx/4)+FIX(randomu(seed)*self.scany/2+self.scany/4)*self.scanx
        ;; Create circle gradient image
        IF keyword_set(display) THEN BEGIN
           
        END
        Case method OF
           "filtereddiff": BEGIN
              im=(*datap)[*,*,ind]
              im2= flattenImage(im,COFF=0.1)
              bdisc=BYTSCL(im2-im)
              thresh=200
           END
           else: BEGIN
              im_byt=BYTSCL((*datap)[*,*,ind],Min=0,Max=MEDIAN((*datap)[*,*,ind]),/NAN)
              bdisc = MORPH_GRADIENT(im_byt, disc)
           END
        END
        kpts=WHERE(bdisc GT thresh)
        xpts=kpts MOD self.framex
        ypts=kpts/self.framex
        circles=circleHoughLink(xpts,ypts,RADIUS=radrange,THRESHOLD=thresh,NCIRCLES=ncircles,XBINS=2*self.framex,YBINS=2*self.framex,XMAX=self.framex,YMAX=self.framey,Hough_Matrix=A)
        ;; TVSCL, bdisc
        cx[i]=circles.cx
        cy[i]=circles.cy
        cr[i]=circles.R
        printtocon, "    "+MyString(ind MOD self.framex)+" "+MyString((ind/self.framex))+" "+MyString(cx[i])+" "+MyString(cy[i])+" "+MyString(cr[i])
        IF keyword_set(display) THEN BEGIN
           w=GetCurrentP()
           w=(*w).datap
           IF ((*w).window GT -1) THEN BEGIN
              WSET, (*w).window
              X1=self.framex-1 & binszx=self.framex
              Y1=self.framey-1 & binszy=self.framey
              IF ((*w).binning NE 1) THEN BEGIN
                 binSzX=FLOOR(((*w).SzX)/(*w).binning)
                 binSzY=FLOOR(((*w).SzY)/(*w).binning)
                 X1=((*w).binning*binSzX)-1
                 Y1=((*w).binning*binSzY)-1
              END
              mask=BYTARR(self.framex,self.framey)
              mask[kpts]=1
              TVSCL,REBIN(mask[0:X1,0:Y1], binSzX,binSzY)
              StaticXCIRCLE, circles.cx*(*w).binning, circles.cy*(*w).binning, circles.R*(*w).binning, COLOR=1000L
           END 
        END   
     END 
     ;;obj_destroy, mybar
END 
  dx=robust_mean(cx,3)-(self.framex-1)/2
  dy=robust_mean(cy,3)-(self.framex-1)/2 ;; we center with respect to the smaller frame x dimension ...
  ;; if framex x is 128 then the centre pixel is at 63
  radius=robust_mean(cr,3)
  printtocon, "% EMPADObj::CentreDisc"
  printtocon, "   Mean displacement: "+MyString(dx)+" "+MyString(dy)
  printtocon, "   Mean radius      : "+MyString(radius)
  IF keyword_set(correct) THEN BEGIN
     pixdx=Round(dx) &  pixdy=Round(dy)
     ;; shift array
     (*datap)=SHIFT((*datap),[-pixdx,-pixdy,0])
     printtocon, "% EMPADObj::CentreDisc"
     printtocon, "   Array Shift (pix)   : "+MyString(-pixdx)+" "+MyString(-pixdy)
     printtocon, "   Residual Error (pix): "+MyString(dx-pixdx)+" "+MyString(dy-pixdy)
  END
END   


PRO EMPADObj::FixHotPix, Thresh=tresh, Radius=radius, ITERATE=iterate, FLATTEN=flatten
  ;; Fix hot pixels
  ;; 
  IF NOT(keyword_set(radius)) then radius=2 
  IF NOT(keyword_set(thresh)) then thresh=200
 
  datap=self.p
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% PADObj::FixHotPix: Current stack pointer is invalid." 
     return
  END
  N=Size(*datap)
  DimX=N[1] & DimY=N[2] & DimZ=N[3]
  minv=[0]
  maxv=[20]
  mybar=obj_new('ProgressBar', MINV=minv,MAXV=maxv,TEXT=["% done"], STATUS="Processing frames ...", FRAMESTYLE=2)
  pbarlev=DimZ/20. ;; 5% steps
  pbarind=0
  mybar->Set, 0, 1, TEXT=STRING(0)
  For i=0L,DimZ-1 Do BEGIN
     im=(*datap)[*,*,i]
     if keyword_set(flatten) THEN BEGIN
        RmHotPix, im, THRESHOLD=thresh, MEDRADIUS=radius, ITERATE=iterate, /FILTER, COFF=0.1
     END ELSE BEGIN
        RmHotPix, im, THRESHOLD=thresh, MEDRADIUS=radius, ITERATE=iterate
     END
     (*datap)[*,*,i]=im
     if (i GT pbarlev) THEN BEGIN
        pbarlev=pbarlev+DimZ/20. ;; next level
        pbarind=pbarind+1  
        mybar->Set, 0, pbarind, TEXT=STRING(pbarind*5)
     END
  END
  obj_destroy, mybar
END



PRO EMPADObj::MulipleDetectorSignal, RADIUS1=radius1, RADIUS2=radius2, RADIUS3=radius3, DX1=dx1, DY1=dy1, DX2=dx2, DY2=dy2, DX3=dx3, DY3=dy3, Mask=mask
  ;; Radius= two-element vector with inner and outer radius in pix
  ;; dx, dy= optional shift of the detector
  ;; mask = if set then the filter mask will be copied to the image list
  IF NOT(keyword_set(radius1)) then BEGIN
     ri1=FIX(self.framex*0.1) & ro1=ri1+self.framex/8
  END ELSE BEGIN
     ri1=radius1[0]
     ro1=radius1[1]
  END
  IF NOT(keyword_set(radius2)) then BEGIN
     ri2=FIX(self.framex*0.25) & ro2=ri2+self.framex/4
  END ELSE BEGIN
     ri2=radius2[0]
     ro2=radius2[1]
  END
  IF keyword_set(radius3) then BEGIN   ;; three dimensions! 
     ri3=radius3[0]
     ro3=radius3[1]
  END
  IF NOT(keyword_set(dx1)) then dx1=0
  IF NOT(keyword_set(dy1)) then dy1=0
  IF NOT(keyword_set(dx2)) then dx2=0
  IF NOT(keyword_set(dy2)) then dy2=0
  IF NOT(keyword_set(dx3)) then dx3=0
  IF NOT(keyword_set(dy3)) then dy3=0
  filt1=self.AnnularMask(ri1,ro1)
  filt2=self.AnnularMask(ri2,ro2) 
  IF keyword_set(radius3) then filt3=self.AnnularMask(ri3,ro3)
  IF ((dx1 NE 0) OR (dy1 NE 0)) THEN filt1=SHIFT(filt1, dx1, dy1)
  IF ((dx2 NE 0) OR (dy2 NE 0)) THEN filt2=SHIFT(filt2, dx2, dy2)
  IF keyword_set(radius3) then BEGIN
     IF ((dx3 NE 0) OR (dy3 NE 0)) THEN  filt3=SHIFT(filt3, dx3, dy3)
  END
  datap=self.p
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPAD_ADF: Current stack pointer is invalid." 
     return
  END
  N=Size(*datap)
  DimX=N[1] & DimY=N[2] & DimZ=N[3]
  s1=FLTARR(DimZ)
  s2=FLTARR(DimZ)
  IF keyword_set(radius3) then s3=FLTARR(DimZ)
  For i=0L,DimZ-1 Do BEGIN
     patch=(*datap)[*,*,i]*filt1
     s1[i]=total(patch,0)
     patch=(*datap)[*,*,i]*filt2
     s2[i]=total(patch,0)
     IF keyword_set(radius3) then BEGIN
        patch=(*datap)[*,*,i]*filt3
        s3[i]=total(patch,0)
     END
  END
  IF (self.issubset) THEN BEGIN
     im1=FLTARR(self.scanx*self.scany)
     im1[*(self.psubset)]=s1
     im1=REFORM(im1,self.scanx,self.scany)
     im2=FLTARR(self.scanx*self.scany)
     im2[*(self.psubset)]=s2
     im2=REFORM(im2,self.scanx,self.scany)
     IF keyword_set(radius3) then BEGIN
        im3=FLTARR(self.scanx*self.scany)
        im3[*(self.psubset)]=s3
        im3=REFORM(im3,self.scanx,self.scany)
     END
  END ELSE BEGIN
     im1=REFORM(s1,self.scanx,self.scany)
     im2=REFORM(s2,self.scanx,self.scany)
     IF keyword_set(radius3) then im3=REFORM(s3,self.scanx,self.scany)
  END
  name="Det1("+self.setname+","+MyString(self.centerx-dx1)+":"+MyString(self.centery-dy1) $
  + ","+MyString(ri1)+":"+MyString(ro1) +")"
  ThrowImage, im1,TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
  IF keyword_set(mask) THEN ThrowImage, filt1,Title="Virtual Detector 1", SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
  name="Det2("+self.setname+","+MyString(self.centerx-dx2)+":"+MyString(self.centery-dy2) $
  + ","+MyString(ri2)+":"+MyString(ro2) +")"
  ThrowImage, im2,TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
  IF keyword_set(mask) THEN ThrowImage, filt2,Title="Virtual Detector 2", SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
  IF keyword_set(radius3) THEN BEGIN
     name="Det3("+self.setname+","+MyString(self.centerx-dx3)+":"+MyString(self.centery-dy3) $
     + ","+MyString(ri3)+":"+MyString(ro3) +")"
     ThrowImage, im3,TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
     IF keyword_set(mask) THEN ThrowImage, filt3,Title="Virtual Detector 3", SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
  END
  IF keyword_set(radius3) THEN BEGIN
     g1=ScatterPlot3D(s1,s2,s3,SYMBOL='circle', /SYM_FILLED, SYM_SIZE=0.5,RGB_TABLE=7, XTITLE='Detector#1', YTITLE='Detector#2', ZTITLE='Detector#3')
  END ELSE BEGIN     
     g1=ScatterPlot(s1,s2,SYMBOL='circle', /SYM_FILLED, SYM_SIZE=0.5,RGB_TABLE=1, XTITLE='Detector#1', YTITLE='Detector#2')
  END
  ;; get ranges
  s=list()
  s.Add, {value:1E6,label:"Detector#1 - Lower, upper bound",newrow:0B}
  s.Add, {value:2E6,label:",",newrow:0B}
  s.Add, {value:1E6,label:"Detector#2 - Lower, upper bound",newrow:1B}
  s.Add, {value:2E6,label:",",newrow:0B}
  IF keyword_set(radius3) THEN BEGIN
     s.Add, {value:1E6,label:"Detector#3 - Lower, upper bound",newrow:1B}
     s.Add, {value:2E6,label:",",newrow:0B}
  END
  c=list()
  While (XMDataChoiceField(s, c, TITLE="Signal Threshold Parameters") EQ 1) DO BEGIN
     range1=[s[0].value,s[1].value]
     range2=[s[2].value,s[3].value]
     IF keyword_set(radius3) THEN BEGIN
        range3=[s[4].value,s[5].value]
        ind=Where((s1 GT range1[0]) AND (s1 LT range1[1]) AND (s2 GT range2[0]) AND (s2 LT range2[1]) AND (s3 GT range3[0]) AND (s3 LT range3[1]), count)
     END ELSE BEGIN
        ind=Where((s1 GT range1[0]) AND (s1 LT range1[1]) AND (s2 GT range2[0]) AND (s2 LT range2[1]), count)
     END
     ;; g2=ScatterPlot(s1[ind],s2[ind],SYMBOL='circle', /SYM_FILLED, SYM_SIZE=0.5,RGB_TABLE=7)
     IF (count GT 0) THEN BEGIN
        self.Subset, ind, /PREVIEW
     END ELSE  BEGIN
        printtocon, "% EMPADObj::MultipleDetectorSignal: No match at given ranges."
     END
  END
END 


PRO EMPADObj::SegmentDet, RADIUS=radius, DX=dx, DY=dy, Mask=mask, SYMMETRY=symmetry, LARMOR=larmor, CLOCKWISE=clockwise, TOPHAT=tophat, NAN=nan
  ;; Radius= two-element vector with inner and outer radius in pix
  ;; dx, dy= optional shift of the detector
  ;; mask = if set then the filter mask will be copied to the image
  ;; list
  ;; segment 1 starts at the larmor offset angle and spans to +2Pi/symmetry
  datap=self.p
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPAD_SegmentDet: Current stack pointer is invalid." 
     return
  END
  ;; prepare the Segment masks
  IF NOT(keyword_set(radius)) then BEGIN
     ri=FIX(self.framex*0.1) & ro=ri+self.framex/4
  END ELSE BEGIN
     ri=radius[0]
     ro=radius[1]
  END
  IF NOT(keyword_set(dx)) then dx=0
  IF NOT(keyword_set(dy)) then dy=0
  filt=self.SegmentMask(ri,ro, DX=dx, DY=dy, SYMMETRY=symmetry, LARMOR=larmor, CLOCKWISE=clockwise,TOPHAT=tophat) ;;  a set of n filters, n is the symmetry
  ;; apply the Segment masks
  N=Size(*datap)
  DimX=N[1] & DimY=N[2] & DimZ=N[3]
  s=FLTARR(DimZ,symmetry)
  For i=0L,DimZ-1 Do BEGIN
     For j=0,symmetry-1 DO BEGIN
        patch=(*datap)[*,*,i]*filt[*,*,j]
        s[i,j]=total(patch,0)
     END
  END
  IF keyword_set(mask) THEN BEGIN
     IF (symmetry GE 2) THEN BEGIN
     ThrowStack, PTR_NEW(filt), "Segment Detector, Symmetry="+MyString(symmetry), Title="Segment Detector, Symmetry="+MyString(symmetry), SAMP=[self.qsamp[0],self.qsamp[1],1.], UNIT=['1/nm','1/nm','1/nm']
     END ELSE IF (symmetry EQ 1) THEN BEGIN
        ThrowImage, filt, Title="Segment Detector, Symmetry="+MyString(symmetry), SAMP=[self.qsamp[0],self.qsamp[1],1.], UNIT=['1/nm','1/nm','1/nm']
     END 
  END 
  IF (self.issubset) THEN BEGIN
     im=FLTARR(self.scanx*self.scany,symmetry)
     For j=0,symmetry-1 DO im[*,*,j]=REFORM(s[*(self.psubset),j],self.scanx,self.scany)
  END ELSE BEGIN
     im=FLTARR(self.scanx,self.scany,symmetry)
     For j=0,symmetry-1 DO im[*,*,j]=REFORM(s[*,j],self.scanx,self.scany)
  END
  ;; check NAN
  IF keyword_set(nan) THEN BEGIN
     For j=0,symmetry-1 DO im[*,*,j]=FilterNAN(im[*,*,j])
  END
  name="SegmentImage("+self.setname+","+MyString(self.centerx-dx)+":"+MyString(self.centery-dy) $
       + ","+MyString(ri)+":"+MyString(ro) +","+MyString(symmetry)+":"+MyString(larmor)+")"
  If (symmetry GE 2) THEN BEGIN
     ThrowStack, PTR_NEW(im), name, TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
  END ELSE If (symmetry EQ 1) THEN BEGIN
     ThrowImage, im, TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
  END
END 

PRO EMPADObj::FilterSegment, RADIUS=radius, DX=dx, DY=dy, Mask=mask, SYMMETRY=symmetry, LARMOR=larmor, CLOCKWISE=clockwise, TOPHAT=tophat, NAN=nan, BF=bf
  ;; Radius= two-element vector with inner and outer radius in pix
  ;; dx, dy= optional shift of the detector
  ;; mask = if set then the filter mask will be copied to the image
  ;; list
  ;; segment 1 starts at the larmor offset angle and spans to +2Pi/symmetry
  ;; only the first segment is used!
  datap=self.p
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% PAD_FilterSegment: Current stack pointer is invalid." 
     return
  END
  ;; prepare the Segment masks
  IF NOT(keyword_set(radius)) then BEGIN
     ri=FIX(self.framex*0.1) & ro=ri+self.framex/4
  END ELSE BEGIN
     ri=radius[0]
     ro=radius[1]
  END
  IF NOT(keyword_set(dx)) then dx=0
  IF NOT(keyword_set(dy)) then dy=0
  filt=self.SegmentMask(ri,ro, DX=dx, DY=dy, SYMMETRY=symmetry, LARMOR=larmor, CLOCKWISE=clockwise,TOPHAT=tophat) ;;  a set of n filters, n is the symmetry
  ;; apply the Segment masks
  N=Size(*datap)
  DimX=N[1] & DimY=N[2] & DimZ=N[3]
  s=FLTARR(DimZ,symmetry)
  segind=0
  ;; Keep BF?
  IF keyword_set(bf) THEN BEGIN
     maxbf=self.centerx-ABS(dx)
     IF (bf GT maxbf) THEN bf=maxbf
     maxbf=self.centery-ABS(dy)
     IF (bf GT maxbf) THEN bf=maxbf
     For j=0,symmetry-1 DO filt[(self.centerx-dx-bf):(self.centerx-dx+bf),(self.centery-dy-bf):(self.centery-dy+bf),j]=1
  END
  For i=0L,DimZ-1 Do BEGIN ;; only the first segment is used!
        (*datap)[*,*,i]=(*datap)[*,*,i]*filt[*,*,segind]
        s[i,segind]=total((*datap)[*,*,i],0)
   END
  IF keyword_set(mask) THEN BEGIN
     If (symmetry GE 2) THEN BEGIN
        ThrowStack, PTR_NEW(filt), "Segment Filter, Symmetry="+MyString(symmetry), Title="Segment Filter, Symmetry="+MyString(symmetry), SAMP=[self.qsamp[0],self.qsamp[1],1.], UNIT=['1/nm','1/nm','1/nm']
     END ELSE IF (symmetry EQ 1) THEN BEGIN
        ThrowImage, filt, Title="Segment Filter, Symmetry="+MyString(symmetry), SAMP=[self.qsamp[0],self.qsamp[1],1.], UNIT=['1/nm','1/nm','1/nm']
     END 
  END
  IF (self.issubset) THEN BEGIN
     im=FLTARR(self.scanx*self.scany,symmetry)
     For j=0,symmetry-1 DO im[*,*,j]=REFORM(s[*(self.psubset),j],self.scanx,self.scany)
  END ELSE BEGIN
     im=FLTARR(self.scanx,self.scany,symmetry)
     For j=0,symmetry-1 DO im[*,*,j]=REFORM(s[*,j],self.scanx,self.scany)
  END
  ;; check NAN
  IF keyword_set(nan) THEN BEGIN
     For j=0,symmetry-1 DO im[*,*,j]=FilterNAN(im[*,*,j])
  END
  name="SegmentImage("+self.setname+","+MyString(self.centerx-dx)+":"+MyString(self.centery-dy) $
       + ","+MyString(ri)+":"+MyString(ro) +","+MyString(symmetry)+":"+MyString(larmor)+")"
  If (symmetry GE 2) THEN BEGIN
     ThrowStack, PTR_NEW(im), name, TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
  END  ELSE IF (symmetry EQ 1) THEN BEGIN
     ThrowImage, im, TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
  END
END 


PRO EMPADObj::DPC, RADIUS=radius, DX=dx, DY=dy, Mask=mask, LARMOR=larmor, CLOCKWISE=clockwise, TYPE=type, RETP=retp, TOPHAT=tophat
  ;; Radius= two-element vector with inner and outer radius in pix
  ;; dx, dy= optional shift of the detector
  ;; mask = if set then the filter mask will be copied to the image
  ;; list
  ;; segment 1 starts at the larmor offset angle and spans to
  ;; +2Pi/symmetry
  ;; retp: if set then return an array with the pointers to the DPC images
  datap=self.p
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPAD_SegmentDet: Current stack pointer is invalid." 
     return
  END
  ;; prepare the Segment masks
  IF NOT(keyword_set(type)) THEN type=0
  CASE type OF
     1: BEGIN
        IF keyword_set(clockwise) THEN thelarmor=larmor+!Pi/4 ELSE  thelarmor=larmor-!Pi/4
        self.SegmentDet, RADIUS=radius, DX=dx, DY=dy, Mask=mask, SYMMETRY=4, LARMOR=thelarmor, CLOCKWISE=clockwise, TOPHAT=tophat
     END
     ELSE:  self.SegmentDet, RADIUS=radius, DX=dx, DY=dy, Mask=mask, SYMMETRY=4, LARMOR=larmor, CLOCKWISE=clockwise, TOPHAT=tophat
  END
  ;; Last stack thrown are the segment images
  p=GetCurrentP()
  ;; Detector layout for larmor=0 and counterclockwse notation (standard)
  ;;
  ;;   B A
  ;;   C D
  ;;
  ;; for clockwise notation:
  ;;
  ;;   C D
  ;;   B A
  ;;
  ;; Rose (type 0): DPC-y=(A+B)-(C+D), DPC-x=(A+D)-(C+B) ;; note that the sign
  ;; in y is reversed when clockwise notation is used
  ;;
  ;; FEI= (type 1) and counterclockwise notation with larmor = -45 deg:
  ;;
  ;;     B
  ;;   C   A
  ;;     D
  ;;
  ;; for counterclockwise notation with larmor = -45 deg
  ;;
  ;;     D
  ;;   C   A
  ;;     B
  ;; 
  ;;  DPC-x=A-C, DPC-y=B-D for counterclockwise notation, note that the sign
  ;; in y is reversed when clockwise notation is used
  ;;
  ;; Larmor: rotation of the segments in radians. Note that for the
  ;; clockwise rotation also the larmor rotation changes.
  ;; 
  pim=(*(*p).datap).data
  CASE type OF
     1: BEGIN
        ;; FEI scheme
        DPCx=(*pim)[*,*,0]-(*pim)[*,*,2]
        DPCy=(*pim)[*,*,1]-(*pim)[*,*,3]
        name="DPC(A-C) at "+ MyString(larmor*180./!Pi)+" deg,"+MyString(self.centerx-dx)+":"+MyString(self.centery-dy) + ","+MyString(ri)+":"+MyString(ro) +","+self.setname+")"
        
        ThrowImage, DPCx, TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
        name="DPC(B-D) at "+ MyString(larmor*180./!Pi)+" deg,"+MyString(self.centerx-dx)+":"+MyString(self.centery-dy) + ","+MyString(radius[0])+":"+MyString(radius[1])  +","+self.setname+")"
        p1=GetCurrentP()
        ThrowImage, DPCy, TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
        p2=GetCurrentP()
        ;;
     END
     ELSE: BEGIN
        ;; default is "Rose"
        DPCx=((*pim)[*,*,0]+(*pim)[*,*,3])-((*pim)[*,*,2]+(*pim)[*,*,1])
        DPCy=((*pim)[*,*,0]+(*pim)[*,*,1])-((*pim)[*,*,2]+(*pim)[*,*,3])
        name="DPC((A+D)-(C+B)) at "+ MyString(larmor*180./!Pi)+" deg,"+MyString(self.centerx-dx)+":"+MyString(self.centery-dy) + ","+MyString(ri)+":"+MyString(ro) +","+self.setname+")"
        
        ThrowImage, DPCx, TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
        name="DPC((A+B)-(C+D)) at "+ MyString(larmor*180./!Pi)+" deg,"+MyString(self.centerx-dx)+":"+MyString(self.centery-dy) + ","+MyString(radius[0])+":"+MyString(radius[1])  +","+self.setname+")"
        p1=GetCurrentP()
        ThrowImage, DPCy, TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
        p2=GetCurrentP()
     END 
     
  END
  if keyword_set(retp) THEN retp=[p1,p2]
END 

PRO EMPADObj::PickDiff, DX=dx, DY=dy, Mask=mask, BF=bf, CUMULATIVEMASK=cumulativemask, AUTOFILESAVE=autofilesave, SCALEBAR=scalebar, ROTEMPAD=rotempad, INNERR=innerr, OUTERR=outerr
  ;; Radius= two-element vector with inner and outer radius in pix
  ;; dx, dy= optional shift of the detector
  ;; mask = if set then the filter mask will be copied to the image list
  IF NOT(keyword_set(innerr)) THEN ri=0 ELSE ri=innerr
  IF NOT(keyword_set(outerr)) THEN ro=self.framex/4 ELSE ro=outerr
  IF (keyword_set(bf)) then BEGIN
     ri=0 & ro=self.framex/16
  END
  IF NOT(keyword_set(dx)) then dx=0
  IF NOT(keyword_set(dy)) then dy=0
  filt=self.AnnularMask(ri,ro)
  IF ((dx NE 0) OR (dy NE 0)) THEN BEGIN
     filt=SHIFT(filt, dx, dy)
     if (dx GT 0) THEN IF (dx LT dimx) THEN filt[0:dx,*]=0.
     if (dy GT 0) THEN IF (dy LT dimy) THEN filt[*,0:dy]=0.
     if (dx LT 0) THEN IF ((-dx) LT dimX) THEN filt[(dimx+dx-1):(dimx-1),*]=0.
     if (dy LT 0) THEN IF ((-dy) LT dimy) THEN filt[*,(dimy+dy-1):(dimy-1)]=0.
  END
  datap=self.p
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPAD_PickDiff: Current stack pointer is invalid." 
     return
  END
  N=Size(*datap)
  DimX=N[1] & DimY=N[2] & DimZ=N[3]
  s=FLTARR(DimZ)
  For i=0L,DimZ-1 Do BEGIN
     patch=(*datap)[*,*,i]*filt
     s[i]=total(patch,0)
  END
  IF (self.issubset) THEN BEGIN
     im=FLTARR(self.scanx*self.scany)
     im[*(self.psubset)]=s
     im=REFORM(im,self.scanx,self.scany)
  END ELSE im=REFORM(s,self.scanx,self.scany)
  name="Preview("+self.setname+","+MyString(self.centerx-dx)+":"+MyString(self.centery-dy) $
       + ","+MyString(ri)+":"+MyString(ro) +")"
  ;; STOP
  ;; Throw ADF image, make sure the TV binning is fine
  ;; Create an image for the diffraction output, BIN factor is inherited, tryto enlarge by factor 4
  imbin = TVBIN(self.scanx,self.scany, /MAXIMISE, maxfrac=0.25, maxzoom=8)
  ThrowImage, im,TITLE=name, BIN=imbin, SAMP=[self.qsamp[0],self.qsamp[1],1.], UNIT=['1/nm','1/nm','nm']
  p=GetCurrentP()
  FUNC="DisplayROIDiff"
  x1=FIX(self.framex*0.5)-8 & x2=x1+16
  y1=FIX(self.framey*0.5)-8 & y2=y1+16
  ;; Create an image for the cumulative mask
  if keyword_set(cumulativemask) THEN BEGIN
     ThrowImage, BYTARR(self.scanx,self.scany), TITLE="ROI Diff - Cumulative Mask", BIN=imbin, SAMP=[self.samp[0],self.samp[1],1.0], UNIT=['nm','nm','nm']
     m=GetCurrentP()
     ;; define contrastmode
     (*(*m).datap).contrastmode="auto"
     (*(*m).datap).contrastsubmode="sdev" 
     (*(*m).datap).contrastroi="full"
  END ELSE m=PTR_NEW()
  ;; Create an image for the diffraction output, BIN factor is inherited, tryto enlarge by factor 4
  roidiffbin = TVBIN(self.framex,self.framey,/MAXIMISE, maxfrac=0.4,maxzoom=4)
  ThrowImage, FLTARR(self.framex,self.framey), TITLE="ROI Diff - Preview", BIN=roidiffbin, SAMP=[self.qsamp[0],self.qsamp[1],1.0], UNIT=['1/nm','1/nm','1/nm']
  o=GetCurrentP()
  ;; define contrastmode
  SetStackContrastMode, /FULL ,/AUTO, /MINMAX
  ;;(*(*o).datap).contrastmode="auto"
  ;;(*(*o).datap).contrastsubmode="sdev" 
  ;;(*(*o).datap).contrastroi="diff"
  ;; Create a list with function arguments
  ;;
  funcargs=hash('EMPADObj',self,'outputp',o,'capturep',p,'cumulativemaskp',m)
  print, XActionBox(p, x1, y1, x2, y2,BIN=imbin,ACTIONFUNC=FUNC,FUNCARGS=funcargs,AUTOFILESAVE=autofilesave, /LOG, SCALEBAR=scalebar, ROTEMPAD=rotempad)
  ;; now we have an image in p that we can use for navigation and to
  ;; pick out the diffraction data
  ;; let's mark a rectangle in that image first
  
  
END


PRO EMPADObj::PickLineDiff, DX=dx, DY=dy, Mask=mask, BF=bf
  ;; Radius= two-element vector with inner and outer radius in pix
  ;; dx, dy= optional shift of the detector
  ;; mask = if set then the filter mask will be copied to the image list
  IF NOT(keyword_set(bf)) then BEGIN
     ri=FIX(self.framex*0.25) & ro=self.framex/2
  END ELSE BEGIN
     ri=0 & ro=self.framex/4
  END
  IF NOT(keyword_set(dx)) then dx=0
  IF NOT(keyword_set(dy)) then dy=0
  filt=self.AnnularMask(ri,ro)
  IF ((dx NE 0) OR (dy NE 0)) THEN BEGIN
     filt=SHIFT(filt, dx, dy)
     if (dx GT 0) THEN IF (dx LT dimx) THEN filt[0:dx,*]=0.
     if (dy GT 0) THEN IF (dy LT dimy) THEN filt[*,0:dy]=0.
     if (dx LT 0) THEN IF ((-dx) LT dimX) THEN filt[(dimx+dx-1):(dimx-1),*]=0.
     if (dy LT 0) THEN IF ((-dy) LT dimy) THEN filt[*,(dimy+dy-1):(dimy-1)]=0.
  END
  datap=self.p
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPAD_PickDiff: Current stack pointer is invalid." 
     return
  END
  N=Size(*datap)
  DimX=N[1] & DimY=N[2] & DimZ=N[3]
  s=FLTARR(DimZ)
  For i=0L,DimZ-1 Do BEGIN
     patch=(*datap)[*,*,i]*filt
     s[i]=total(patch,0)
  END
  IF (self.issubset) THEN BEGIN
     im=FLTARR(self.scanx*self.scany)
     im[*(self.psubset)]=s
     im=REFORM(im,self.scanx,self.scany)
  END ELSE im=REFORM(s,self.scanx,self.scany)
  name="ADF("+self.setname+","+MyString(self.centerx-dx)+":"+MyString(self.centery-dy) $
  + ","+MyString(ri)+":"+MyString(ro) +")"
  ThrowImage, im, TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
  p=GetCurrentP()
  FUNC="DisplayROILineDiff"
  
  x1=FIX(self.scanx/4) & x2=x1+FIX(self.scanx/2)
  y1=FIX(self.scany/4) & y2=y1+FIX(self.scany/2)
  ;; Create a stack for the output the data pointer in the object will
  ;; be changed when the Box dialog calls "DisplayROILineDiff"
  ThrowStack, PTR_NEW(FLTARR(self.framex,self.framey,2)), "ProfileStack("+self.setname+")", BIN=0.5, TITLE="ROIDiff", SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
  o=GetCurrentP()
  ;; define contrastmode
  (*(*o).datap).contrastmode="auto"
  (*(*o).datap).contrastsubmode="minmax" 
  (*(*o).datap).contrastroi="diff"
  ;; create a list with function arguments
  ;;
  funcargs=hash('EMPADObj',self,'outputp',o,'last',PTR_NEW())
  ;; last is a pointer to an array with the start coordinates
  print, XActionLINE(p, x1, y1, x2, y2,BIN=(*(*p).datap).binning,ACTIONFUNC=FUNC,FUNCARGS=funcargs,/LOG)
  ;; now we have an image in p that we can use for navigation and to
  ;; pick out the diffraction data
  ;; let's mark a rectangle in that image first
  
  
END 


PRO EMPADObj::FluctuationMap, RADIUS=radius, DX=dx, DY=dy, Mask=mask, TOPHAT=tophat, METHOD=method
  ;; radius= two-element vector with inner and outer radius in pix
  ;; dx, dy= optional shift of the detector
  ;; mask = if set then the filter mask will be copied to the image
  ;; list
  IF NOT(keyword_set(method)) then method="sdev"
  IF NOT(keyword_set(radius)) then BEGIN
     ri=FIX(self.framex*0.1) & ro=ri+self.framex/4
  END ELSE BEGIN
     ri=radius[0]
     ro=radius[1]
  END
  IF NOT(keyword_set(dx)) then dx=0
  IF NOT(keyword_set(dy)) then dy=0
  datap=self.p
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPAD::FluctuationMap: Current stack pointer is invalid." 
     return
  END
  N=Size(*datap)
  DimX=N[1] & DimY=N[2] & DimZ=N[3]
  filt=self.AnnularMask(ri,ro, EXPONENT=16, TOPHAT=tophat)
  IF ((dx NE 0) OR (dy NE 0)) THEN BEGIN
     filt=SHIFT(filt, dx, dy)
     if (dx GT 0) THEN IF (dx LT dimx) THEN filt[0:dx,*]=0.
     if (dy GT 0) THEN IF (dy LT dimy) THEN filt[*,0:dy]=0.
     if (dx LT 0) THEN IF ((-dx) LT dimX) THEN filt[(dimx+dx-1):(dimx-1),*]=0.
     if (dy LT 0) THEN IF ((-dy) LT dimy) THEN filt[*,(dimy+dy-1):(dimy-1)]=0.
  END
  s=FLTARR(DimZ)
  For i=0L,DimZ-1 Do BEGIN
     patch=(*datap)[*,*,i]*filt
     CASE method of
        "entropy": s[i]=ImageEntropy(patch)
        "sdev": s[i]=STDDEV(patch)
        ELSE: s[i]=Variance(patch)/Mean(patch)
    END
  END
  IF (self.issubset) THEN BEGIN
     im=FLTARR(self.scanx*self.scany)
     im[*((self).subset)]=s
     im=REFORM(im,self.scanx,self.scany)
  END ELSE im=REFORM(s,self.scanx,self.scany)
  CASE method of
     "entropy": BEGIN
        name="EntropyMap("+self.setname+","+MyString(self.centerx-dx)+":"+MyString(self.centery-dy) + ","+MyString(ri)+":"+MyString(ro) +")"
        printtocon, "% Entropy statistics"
     END
     ELSE:  BEGIN
        name="FluctuationMap("+self.setname+","+MyString(self.centerx-dx)+":"+MyString(self.centery-dy)   + ","+MyString(ri)+":"+MyString(ro) +")"
         printtocon, "% Fluctuation sytatistics (var/mean on annular patch)"
 
     END
  END
  m=Moment(s, mdev=mdev, sdev=sdev)
  printtocon, "    center x, y         = "+MyString(self.centerx-dx)+", "+MyString(self.centery-dy)
  printtocon, "    radius inner, outer = "+ MyString(ri)+", "+MyString(ro)
  printtocon, "    mean                = "+MyString(m[0])
  printtocon, "    variance            = "+MyString(m[1])
  printtocon, "    mean abs dev        = "+MyString(mdev)
  printtocon, "    min, max            = "+MyString(Min(s))+", "+MyString(Max(s))
  printtocon, "    skewness            = "+MyString(m[2])
  printtocon, "    kurtosis            = "+MyString(m[3])
  ThrowImage, im, TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
  IF keyword_set(mask) THEN ThrowImage, filt, Title="Virtual Fluctuation Map Mask", SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
  IF PTR_VALID(self.pfluctmap) THEN PTR_FREE, self.pfluctmap
  self.pfluctmap=PTR_NEW(im)
END

PRO EMPADObj::AzimuthCorr, Radius=radius, ANGWIDTH=angwidth, RECTARR=rectarr, MAPS=maps, ARG=arg, MEDIANWIDTH=medianwidth, THRESHOLD=threshold, MAXSYM=maxsym
;; PARAMETERS:
;; x, y      = centre 
;; ri, ro    = radius in pix, Type: Integer
;; angwidth  = angular step width in deg, Type: Float
;; maps      = returns the pointers to the maps with 2-fold, 4-fold and 6-fold symmtery
;; arg
  ;; 
;; RETURNS: rectarr = image with x=angle and y=index, an image of the polar
;; trace for each pattern in p
;;
;; Creates  
;; psymmetrycorr = pointer to an array holding the symmetrycorrelation, array of rotary symmetry
;; correlation with x=index, y=symmetry
;; e.g. symmetrycorr[ind,2] is the correlation of image ind with 2-fold symmetry 
;;      symmetrycorr[ind,3] is the correlation of image ind with 
;;      3-fold symmetry
;;      ...
;;      symmetrycorr[ind,2] is the correlation of image ind with 6-fold symmetry
  fill=1 ;; fill gap pixels with neighbour content
  IF Not(keyword_set(radius)) then BEGIN
     ri=FIX(self.framex*0.1) & ro=ri+self.framex/4
  END ELSE BEGIN
     ri=radius[0]
     ro=radius[1]
  END
  IF NOT(keyword_set(dx)) then dx=0
  IF NOT(keyword_set(dy)) then dy=0
  IF NOT(keyword_set(angwidth)) then angwidth=1.
  center=self.centerx
  detectordim=self.framex
  datap=self.p
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPAD::PolarToRect: Data is invalid." 
     return
  END
  ;; auto-determine stack size
  N=Size(*datap)
  DimX=N[1] & DimY=N[2] & DimZ=N[3]
  ;;
  ri=FLOOR(ri)
  r=FLOOR(ro) ;; should be integer
  x0=self.centerx-dx-r & x1=self.centerx-dx+r & y0=self.centery-dy-r & y1=self.centery-dy+r
  Radius=RadiusIm(r)
  Angle_ = AngleIm(r) ;; 2D-arrays
;;
;; new, faster code
;;
;; create array to hold the radial scan data
xdim=r & ydim=CEIL(360./angwidth)
ImNorm=INTARR(xdim,ydim)
Im=FLTARR(xdim,ydim,DimZ)
;; Window, XSIZE=xdim, YSize=ydim

minv=[0]
maxv=[10]
mybar=obj_new('ProgressBar', MINV=minv,MAXV=maxv,TEXT=["% done"], STATUS="Processing frames ...", FRAMESTYLE=2)
pbarlev=DimZ/10.
pbarind=0
mybar->Set, 0, pbarind, TEXT=MyString(pbarind)
For ind=0,DimZ-1 Do BEGIN 
scan_im=(*datap)[x0:x1,y0:y1,ind]
Im[*,*,ind]=0
ImNorm[*,*]=0
;; the number of pixel contributing to the bin between r' and r'+1
;; 
spix=0L
npix=0L
for i=0,(x1-x0) do begin
 for j=0,(y1-y0) do begin
        aind=ROUND(Angle_(i,j)/(!DPI)*180/angwidth)
        ;; the closest integer Radius
        rind= Round(Radius(i,j))
        ;; add image data
        IF ((aind LT ydim) And (rind LT xdim) And (rind GE ri)) THEN BEGIN
            Im[rind,aind,ind] = Im[rind,aind,ind] + scan_im[i,j]
            ImNorm[rind,aind]+=1
        END ELSE BEGIN
            spix+=1
        END 
        npix+=1
    endfor 
ENDFOR
FOR i=ri,(xdim-1) Do BEGIN
   FOR j=0,(ydim-1) Do BEGIN
      IF (ImNorm[i,j] GT 0) THEN Im[i,j,ind] = Im[i,j,ind]/ImNorm[i,j]
   END
END
;; find all non-zero data points
IF KEYWORD_SET(fill) THEN BEGIN
    FOR i=ri,(xdim-1) DO BEGIN
        pixdata=0
        FOR j=0,(ydim-1) DO BEGIN
            IF (Im[i,j,ind] NE 0.0) THEN pixdata=Im[i,j,ind] ELSE Im[i,j,ind]=pixdata
        END
    END
 END
;; TVSCL, Im[*,*,ind]
if (ind GT pbarlev) THEN BEGIN
   pbarlev=pbarlev+DimZ/10.
   pbarind=pbarind+1  
   mybar->Set, 0, pbarind, TEXT=STRING(pbarind*10)
END
END  ;; For ind loop
mybar->SetStatus, "Correlating azimuth profiles ..."
;; project Im[*,*,ind] along x-direction
Proj=Total(Im, 1)
if keyword_set(rectarr) THEN rectarr=Proj
IF NOT(keyword_set(maxsym)) THEN maxsym=6
acorr=FLTARR(DimZ,maxsym+1) ;; last dimension is symmetry
IF keyword_set(arg) THEN argcorr=BYTARR(DimZ,maxsym+1) ;; last dimension is symmetry
;; For i=0,DimZ-1 DO acorr2= A_CORRELATE(p[*,i], ydim/2)
angdeltapeak=FLTARR(ydim,maxsym+1) ;; number of angular sampling points
angdeltapeak[*,*]=0.
For i=2,maxsym Do Begin
   For j=0,i-1 Do BEGIN
      ind=(dimy-1)/maxsym*j
      angdeltapeak[ind,i]=1.
   END
END
For i=0,DimZ-1 DO BEGIN ;;; for all patterns
   tmp=proj[*,i] ;; the azimuth trace
   if (keyword_set(medianwidth) AND (medianwidth GT 1)) THEN tmp=Median(tmp,medianwidth) ;; filter to reduce noise
   For j=2,maxsym DO BEGIN   ;;; for all symmetries
      acorr[i,j] = A_CORRELATE(tmp, ydim/j) ;; ydim is the number of pixels over 0 - 2PI
      IF keyword_set(arg) THEN BEGIN
         maxindex=-1
         maxval=-1
         c=PeriodicVectorCorr(tmp,angdeltapeak[*,j],MAXINDEX=maxindex, MAXVAL=maxval)
         ;;  reduce symmetry
         ;; symmetry 2 means two-fold, value modulo 180 is relevant, i.e. modulo ydim/2  
         symredmaxind=maxindex MOD (ydim/j)
         argcorr[i,j]=FIX(symredmaxind*254./ydim+1) ;; color index from 1 to 255, 0 is reserved for transparency
         ;; 1 is 0 deg, 255 is 360 deg
         IF keyword_set(threshold) THEN BEGIN
            IF (acorr[i,j] LT threshold) THEN argcorr[i,j]=0
         END
      END
   END
END
IF PTR_VALID(self.psymmetrycorr) THEN PTR_FREE, self.psymmetrycorr
self.psymmetrycorr=PTR_NEW(acorr)
;; create images
;; the reform command works for with the * placeholder for a full data set
;; for a subset the indices of the correlation array are connected with
;; the subscripts of the subset
IF (self.issubset) THEN BEGIN
     IF  (maxsym GE 2) THEN BEGIN
        name="SymmetryCorr(twofold,"+self.setname+","+MyString(self.centerx-dx)+":"+MyString(self.centery-dy) $
          + ","+MyString(ri)+":"+MyString(ro) +")"
        im=FLTARR(self.scanx*self.scany)
        im[*(self.psubset)]=(*(self.psymmetrycorr))[*,2]
        im=REFORM(im,self.scanx,self.scany)
        ThrowImage, im,TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
        pp2=GetCurrentP()
     END
     IF  (maxsym GE 4) THEN BEGIN
        name="SymmetryCorr(Fourfold,"+self.setname+","+MyString(self.centerx-dx)+":"+MyString(self.centery-dy) $
             + ","+MyString(ri)+":"+MyString(ro) +")"
        im=FLTARR(self.scanx*self.scany)
        im[*(self.psubset)]=(*(self.psymmetrycorr))[*,4]
        im=REFORM(im,self.scanx,self.scany)
        ThrowImage, im,TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
        pp4=GetCurrentP()
     END
     IF  (maxsym GE 6) THEN BEGIN
        name="SymmetryCorr(sixfold,"+self.setname+","+MyString(self.centerx-dx)+":"+MyString(self.centery-dy) $
             + ","+MyString(ri)+":"+MyString(ro) +")"
        im=FLTARR(self.scanx*self.scany)
        im[*(self.psubset)]=(*(self.psymmetrycorr))[*,6]
        im=REFORM(im,self.scanx,self.scany)
        ThrowImage, im,TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
        pp6=GetCurrentP()
     END
     IF keyword_set(arg) THEN BEGIN
        IF  (maxsym GE 6) THEN BEGIN
           name="ArgSymmetryCorr(twofold,"+self.setname+","+MyString(self.centerx-dx)+":"+MyString(self.centery-dy) $
                + ","+MyString(ri)+":"+MyString(ro) +")"
           im=FLTARR(self.scanx*self.scany)
           im[*(self.psubset)]=argcorr[*,2]
           im=REFORM(im,self.scanx,self.scany)
           ThrowImage, im,TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
           argpp2=GetCurrentP()
        END
        IF  (maxsym GE 4) THEN BEGIN
           im=FLTARR(self.scanx*self.scany)
           im[*(self.psubset)]=argcorr[*,4]
           im=REFORM(im,self.scanx,self.scany)
           name="ArgSymmetryCorr(fourfold,"+self.setname+","+MyString(self.centerx-dx)+":"+MyString(self.centery-dy) $
                + ","+MyString(ri)+":"+MyString(ro) +")"
           ThrowImage, im,TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
           argpp4=GetCurrentP()
        END
        IF  (maxsym GE 6) THEN BEGIN
           im=FLTARR(self.scanx*self.scany)
           im[*(self.psubset)]=argcorr[*,6]
           im=REFORM(im,self.scanx,self.scany)
           name="ArgSymmetryCorr(sixfold,"+self.setname+","+MyString(self.centerx-dx)+":"+MyString(self.centery-dy) $
                + ","+MyString(ri)+":"+MyString(ro) +")"
           ThrowImage, im,TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
           argpp6=GetCurrentP()
        END
     END
  END ELSE BEGIN
     IF  (maxsym GE 2) THEN BEGIN
        name="SymmetryCorr(twofold,"+self.setname+","+MyString(self.centerx-dx)+":"+MyString(self.centery-dy) $
          + ","+MyString(ri)+":"+MyString(ro) +")"
        ThrowImage, REFORM((*(self.psymmetrycorr))[*,2],self.scanx,self.scany),TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
        pp2=GetCurrentP()
     END
     IF  (maxsym GE 4) THEN BEGIN
        name="SymmetryCorr(fourfold,"+self.setname+","+MyString(self.centerx-dx)+":"+MyString(self.centery-dy) $
             + ","+MyString(ri)+":"+MyString(ro) +")"
        ThrowImage, REFORM((*(self.psymmetrycorr))[*,4],self.scanx,self.scany), TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
        pp4=GetCurrentP()
     END
     IF  (maxsym GE 6) THEN BEGIN
        name="SymmetryCorr(sixfold,"+self.setname+","+MyString(self.centerx-dx)+":"+MyString(self.centery-dy) $
          + ","+MyString(ri)+":"+MyString(ro) +")"
        ThrowImage, REFORM((*(self.psymmetrycorr))[*,6],self.scanx,self.scany), TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
        pp6=GetCurrentP()
     END
     IF keyword_set(arg) THEN BEGIN
        IF  (maxsym GE 2) THEN BEGIN
           name="ArgSymmetryCorr(twofold,"+self.setname+","+MyString(self.centerx-dx)+":"+MyString(self.centery-dy) $
                + ","+MyString(ri)+":"+MyString(ro) +")"       
           ThrowImage, REFORM(argcorr[*,2],self.scanx,self.scany),TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
           argpp2=GetCurrentP()
        END
        IF  (maxsym GE 4) THEN BEGIN
           name="ArgSymmetryCorr(fourfold,"+self.setname+","+MyString(self.centerx-dx)+":"+MyString(self.centery-dy) $
                + ","+MyString(ri)+":"+MyString(ro) +")"
           ThrowImage, REFORM(argcorr[*,4],self.scanx,self.scany),TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
           argpp4=GetCurrentP()
        END
        IF  (maxsym GE 6) THEN BEGIN
           name="ArgSymmetryCorr(sixfold,"+self.setname+","+MyString(self.centerx-dx)+":"+MyString(self.centery-dy) $
                + ","+MyString(ri)+":"+MyString(ro) +")"
           ThrowImage, REFORM(argcorr[*,6],self.scanx,self.scany),TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
           argpp6=GetCurrentP()
        END
     END
  END
  if keyword_set(maps) THEN BEGIN
     CASE maxsym OF
        2: maps=[pp2]
        4: maps=[pp2,pp4]
        6: maps=[pp2,pp4,pp6]
        ELSE:
     END
  END
  if keyword_set(arg) THEN BEGIN
     CASE maxsym OF
        2: arg=[argpp2]
        2: arg=[argpp2,argpp4]
        2: arg=[argpp2,argpp4,argpp6]
        ELSE:
     END
  END
;; done  
obj_destroy, mybar
END


PRO EMPADObj::OrientationMap,  RADIUS=radius, DX=dx, DY=dy, Mask=mask, SAVE=save
  ;; is the centre of mass on a ADF detector
  ;; equivalent to a com analysis on a ADF mask
 
  ;; Radius= two-element vector with inner and outer radius in pix
  ;; dx, dy= optional shift of the detector
  ;; mask = if set then the filter mask will be copied to the image list
  IF NOT(keyword_set(radius)) then BEGIN
     ri=FIX(self.framex*0.1) & ro=ri+self.framex/4
  END ELSE BEGIN
     ri=radius[0]
     ro=radius[1]
  END
  IF NOT(keyword_set(dx)) then dx=0
  IF NOT(keyword_set(dy)) then dy=0
  
  ;;IF NOT(keyword_set(fitorder)) THEN fitorder=0
  ;; fitorder=FIX(fitorder)
  ;; IF (fitorder GT 3) THEN BEGIN
  ;;   fitorder=3
  ;;   printtocon, "% EMPADObj::OrientationMap - Maximum fit order is 3. Setting fit order to 3."
  ;; END
  ;; IF (fitorder LT 0) THEN BEGIN
  ;;   fitorder=0
  ;;   printtocon, "% EMPADObj::OrientationMap - Minimum fit order is 0. Setting fit order to 0."
  ;;END
  datap=self.p
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPADObj::OrientationMap: Current stack pointer is invalid." 
     return
  END
  N=Size(*datap)
  DimX=N[1] & DimY=N[2] & DimZ=N[3]
  filt=self.AnnularMask(ri,ro, EXPONENT=20)
  IF ((dx NE 0) OR (dy NE 0)) THEN BEGIN
     filt=SHIFT(filt, dx, dy)
     if (dx GT 0) THEN IF (dx LT dimx) THEN filt[0:dx,*]=0.
     if (dy GT 0) THEN IF (dy LT dimy) THEN filt[*,0:dy]=0.
     if (dx LT 0) THEN IF ((-dx) LT dimX) THEN filt[(dimx+dx-1):(dimx-1),*]=0.
     if (dy LT 0) THEN IF ((-dy) LT dimy) THEN filt[*,(dimy+dy-1):(dimy-1)]=0.
  END
  ;;
  gradx=GradIm(dimx,dimy,dx,dy, /X,/CUTEMPAD)
  grady=GradIm(dimx,dimy,dx,dy, /Y,/CUTEMPAD)
  sx=FLTARR(DimZ)
  sy=FLTARR(DimZ)
  sz=FLTARR(DimZ)
  ;;
  detectordim=self.framex
  For i=0L,DimZ-1 Do BEGIN
     ;; iterate centre of mass
     tmp=(*datap)[*,*,i]*filt
     sx[i]=Total(tmp*gradx)/Total(tmp)
     sy[i]=Total(tmp*grady)/Total(tmp)
     sz[i]=Total(tmp)
     ;; print, i, sx[i], sy [i]
  END 
  ;; ThrowGraph, list(sx,sy), TITLE="COM Displacements",
  ;; XTITLE="index", YTITLE="dx, dy (pix)"
  ;; The displacements are the individual image shift displacements
  IF (self.issubset) THEN BEGIN
     im=FLTARR(self.scanx*self.scany)
     im[*(self.psubset)]=sx
     im=REFORM(im,self.scanx,self.scany)
  END ELSE im=REFORM(sx,self.scanx,self.scany)
;;  name="DX("+self.setname+")"
;;  ThrowImage, im, TITLE=name, BIN=0.5, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
;  name="DiffCOMX("+self.setname+")"
  ;; fit
  ;; take care of NANs
  B=WHERE(Finite(im, /NAN), count)
  IF (count GT 0) THEN im[B]=0.
  ;;fit=SFIT(im,fitorder)
  ;; comx=im-fit
  comx=im
;  ThrowImage, comx, TITLE=name, BIN=0.5, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
  ;; Y component
  IF (self.issubset) THEN BEGIN
     im=FLTARR(self.scanx*self.scany)
     im[*(self.psubset)]=sy
     im=REFORM(im,self.scanx,self.scany)
  END ELSE im=REFORM(sy,self.scanx,self.scany)
;;  name="DY("+self.setname+")"
;;  ThrowImage, im, TITLE=name, BIN=0.5, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
;;  name="DiffCOMY("+self.setname+")"
  ;; take care of NANs
  B=WHERE(Finite(im, /NAN), count)
  IF (count GT 0) THEN im[B]=0.
  ;; fit=SFIT(im,fitorder)
  ;; comy=im-fit
  comy=im
;;  ThrowImage, comy, TITLE=name, BIN=0.5,
;;  SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']

  ;; Mag component
  IF (self.issubset) THEN BEGIN
     im=FLTARR(self.scanx*self.scany)
     im[*(self.psubset)]=sz
     im=REFORM(im,self.scanx,self.scany)
  END ELSE im=REFORM(sz,self.scanx,self.scany)
;;  name="DY("+self.setname+")"
;;  ThrowImage, im, TITLE=name, BIN=0.5, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
;;  name="DiffCOMY("+self.setname+")"
  ;; take care of NANs
  B=WHERE(Finite(im, /NAN), count)
  IF (count GT 0) THEN im[B]=0.
  ;; fit=SFIT(im,fitorder)
  ;; comy=im-fit
  mag=im
  name="DiffMag("+self.setname+")"
  ThrowImage, mag, TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
  name="DiffCOMArg("+self.setname+")"
  arg=Atan(comy,comx)
  ThrowImage, Atan(comy,comx), TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
  IF keyword_set(mask) THEN ThrowImage, filt*gradx,Title="WeightingMaskX", SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
  IF keyword_set(mask) THEN ThrowImage, filt*grady,Title="WeightingMaskY", SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
  Polar24bitImage, arg, mag, /SDEV
END 


PRO EMPADObj::OrientMapAzimuthCorr, Radius=radius, ANGWIDTH=angwidth, DX=dx, DY=dy, MEDIANWIDTH=medianwidth, METHOD=method
;; Orientation map by angular correlation
;;  
;; PARAMETERS:
;; x, y      = centre 
;; ri, ro    = radius in pix, Type: Integer
;; angwidth  = angular step width in deg, Type: Float
;; maps      = returns the pointers to the maps with correlation max
;; and correlation angle
;; arg
  ;; 
;; RETURNS: rectarr = image with x=angle and y=index, an image of the polar
;; trace for each pattern in p
;;
  IF Not(keyword_set(method)) then method='default'
  fill=1 ;; fill gap pixels with neighbour content
  IF Not(keyword_set(radius)) then BEGIN
     ri=FIX(self.framex*0.1) & ro=ri+self.framex/4
  END ELSE BEGIN
     ri=radius[0]
     ro=radius[1]
  END
  IF NOT(keyword_set(dx)) then dx=0
  IF NOT(keyword_set(dy)) then dy=0
  IF NOT(keyword_set(angwidth)) then angwidth=1.
  center=self.centerx
  detectordim=self.framex
  datap=self.p
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPAD::PolarToRect: Data is invalid." 
     return
  END
  ;; auto-determine stack size
  N=Size(*datap)
  DimX=N[1] & DimY=N[2] & DimZ=N[3]
  ;;
  ri=FLOOR(ri)
  r=FLOOR(ro) ;; should be integer
  x0=self.centerx-dx-r & x1=self.centerx-dx+r & y0=self.centery-dy-r & y1=self.centery-dy+r
  Radius=RadiusIm(r)
  Angle_ = AngleIm(r) ;; 2D-arrays
;;
;; new, faster code
;;
;; create array to hold the radial scan data
xdim=r & ydim=CEIL(360./angwidth)
ImNorm=INTARR(xdim,ydim)
Im=FLTARR(xdim,ydim,DimZ)
;; Window, XSIZE=xdim, YSize=ydim

minv=[0]
maxv=[10]
mybar=obj_new('ProgressBar', MINV=minv,MAXV=maxv,TEXT=["% done"], STATUS="Processing frames ...", FRAMESTYLE=2)
pbarlev=DimZ/10.
pbarind=0
mybar->Set, 0, pbarind, TEXT=MyString(pbarind)
For ind=0,DimZ-1 Do BEGIN 
scan_im=(*datap)[x0:x1,y0:y1,ind]
Im[*,*,ind]=0
ImNorm[*,*]=0
;; the number of pixel contributing to the bin between r' and r'+1
;; 
spix=0L
npix=0L
for i=0,(x1-x0) do begin
 for j=0,(y1-y0) do begin
        aind=ROUND(Angle_(i,j)/(!DPI)*180/angwidth)
        ;; the closest integer Radius
        rind= Round(Radius(i,j))
        ;; add image data
        IF ((aind LT ydim) And (rind LT xdim) And (rind GE ri)) THEN BEGIN
            Im[rind,aind,ind] = Im[rind,aind,ind] + scan_im[i,j]
            ImNorm[rind,aind]+=1
        END ELSE BEGIN
            spix+=1
        END 
        npix+=1
    endfor 
ENDFOR
FOR i=ri,(xdim-1) Do BEGIN
   FOR j=0,(ydim-1) Do BEGIN
      IF (ImNorm[i,j] GT 0) THEN Im[i,j,ind] = Im[i,j,ind]/ImNorm[i,j]
   END
END
;; find all non-zero data points
IF KEYWORD_SET(fill) THEN BEGIN
    FOR i=ri,(xdim-1) DO BEGIN
        pixdata=0
        FOR j=0,(ydim-1) DO BEGIN
            IF (Im[i,j,ind] NE 0.0) THEN pixdata=Im[i,j,ind] ELSE Im[i,j,ind]=pixdata
        END
    END
 END
;; TVSCL, Im[*,*,ind]
if (ind GT pbarlev) THEN BEGIN
   pbarlev=pbarlev+DimZ/10.
   pbarind=pbarind+1  
   mybar->Set, 0, pbarind, TEXT=STRING(pbarind*10)
END
END  ;; For ind loop
;; now we have the angular profiles in proj
mybar->SetStatus, "Correlating azimuth profiles ..."
;; project Im[*,*,ind] along x-direction
Proj=Total(Im, 1)
if keyword_set(rectarr) THEN rectarr=Proj
if (keyword_set(medianwidth) AND (medianwidth GT 1)) THEN BEGIN
   For i=0,DimZ-1 DO BEGIN ;;; for all patterns
      tmp=proj[*,i]        ;; the azimuth trace
      tmp=Median(tmp,medianwidth) ;; filter to reduce noise
      proj[*,i]=tmp               ;;
   END
END
acorr=FLTARR(DimZ,2) ;; acorr[*,0] = correlation value
                     ;; acorr[*,1] = correlation shift = angle
;; For i=0,DimZ-1 DO acorr2= A_CORRELATE(p[*,i], ydim/2)
Na=ydim
lags=INDGEN(Na) ;; lags for a 360 deg shift
;; add 'polarity'
;;proj[CEIL(ydim/2):ydim-1,*]=-proj[CEIL(ydim/2):ydim-1,*]
;;t=COMPLEXARR(Na)
For i=0,DimZ-1 DO BEGIN ;;; for all patterns
   ;; For j=0,Na-1 DO t[j]=proj[j,i]*COMPLEX(Cos(2*!DPI*j*1.0/Na),Sin(2*!DPI*j*1.0/Na)) ;; proj[*,i] is the azimuth trace
   ;; calculate correlation between azimuthal profile and the mirrored profile
   ;; ccorr=c_correlate(t,Conj(t),lags)
   ;; find max
   ;; maxccorrind=WHERE(REAL_PART(ccorr) EQ
   ;; MAX(REAL_PART(ccorr)),count)
   
   CASE method OF
      'peakfind': BEGIN
         m=Mean(proj[*,i]) & s=(proj[*,i]-m) ;; & plot,s, BACKGROUND=0
         B=WHERE(s LT 0) ;; cutoff everything below mean value
         s[B]=0.
         pf=PeakFinder(s,NPEAKS=npeaks,/OPTIMIZE,/SILENT)
         ind=WHERE(pf[4,*] EQ 1)
         if N_ELements(ind) GT 1 THEN ind=ind[0]
         ;; ccorr=c_correlate(s,REVERSE(s),lags)
         ;; maxccorrind=WHERE(ccorr EQ Max(ccorr))
         acorr[i,0] = pf[4,ind]
         acorr[i,1] = pf[1,ind]*!Pi/180.-!Pi ;;  output angles to -Pi ... Pi
      END
      ELSE: BEGIN
         ;; make two quadrants
         ;; determine centre of mass over 180 deg
         ;; 
         s=proj[0:179,i]
         Acorr[i,0] = STDDEV(s)
         s=(s-Mean(s)) ;; & plot,s, BACKGROUND=0
         B=WHERE(s LT 0) ;; cutoff everything below mean value
         s[B]=0.
         s=s/Total(s)
         acorr[i,1] = Total(s*FINDGEN(180))*!Pi/180.-!Pi ;;  output angles to 0-360 deg
      END
   END
END 
;; create images
;; the reform command works for with the * placeholder for a full data set
;; for a subset the indices of the correlation array are connected with
;; the subscripts of the subset
IF (self.issubset) THEN BEGIN
   name="CorrMax("+self.setname+","+MyString(self.centerx-dx)+":"+MyString(self.centery-dy) $
          + ","+MyString(ri)+":"+MyString(ro) +")"
   im=FLTARR(self.scanx*self.scany)
   im[*(self.psubset)]=acorr[*,0]
   im=REFORM(im,self.scanx,self.scany)
   ThrowImage, im,TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
   pp2=GetCurrentP()
   name="RotCorrMax("+self.setname+","+MyString(self.centerx-dx)+":"+MyString(self.centery-dy) $
          + ","+MyString(ri)+":"+MyString(ro) +")"
   im=FLTARR(self.scanx*self.scany)
   im[*(self.psubset)]=acorr[*,1]
   im=REFORM(im,self.scanx,self.scany)
   ThrowImage, im,TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
   pp2=GetCurrentP()
END ELSE BEGIN

   name="CorrMax("+self.setname+","+MyString(self.centerx-dx)+":"+MyString(self.centery-dy) $
          + ","+MyString(ri)+":"+MyString(ro) +")"
   im=FLTARR(self.scanx*self.scany)
   im=REFORM(acorr[*,0],self.scanx,self.scany)
   ThrowImage, im,TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
   pp2=GetCurrentP()
   name="RotCorrMax("+self.setname+","+MyString(self.centerx-dx)+":"+MyString(self.centery-dy) $
          + ","+MyString(ri)+":"+MyString(ro) +")"
   im=FLTARR(self.scanx*self.scany)
   im=REFORM(acorr[*,1],self.scanx,self.scany)
   ThrowImage, im,TITLE=name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
   pp2=GetCurrentP()
END
;; done  
obj_destroy, mybar
END  

PRO EMPADObj::RotationalBackgroundSubtraction, DELTA=delta, DX=dx, DY=dy, SIGMA=sigma
;;
;; subtract rotational average using the robust mean value
;; 
;;   
;; PARAMETERS:
;; x, y      = centre 
;; delta    = radius integrationof the averaging width in pix, Type: Integer
;;
;; RETURNS: 
;;
;; Creates  
;;      an image data stack that holds the fluctuation data vs radius
;;      for each pixel 
  fill=1 ;; fill gap pixels with neighbour content
  IF Not(keyword_set(delta)) then BEGIN
     delta=1
  END
  IF Not(keyword_set(sigma)) then BEGIN
     sigma=3
  END
  IF NOT(keyword_set(dx)) then dx=0
  IF NOT(keyword_set(dy)) then dy=0
  center=self.centerx
  detectordim=self.framex
  datap=self.p
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPAD::FluctuationImages: Data is invalid." 
     return
  END
  ;; auto-determine stack size odf the diffraction stack
  N=Size(*datap)
  DimX=N[1] & DimY=N[2] & DimZ=N[3]
  ;; determine the maximum radius
  ;; maxx=DimX-1
  r=detectordim/2-ABS(dx)-1-delta
  r=Min([r,detectordim/2-ABS(dy)-1-delta])
  r=FLOOR(r) ;; should be integer
  x0=detectordim/2-dx-r & x1=x0+2*r & y0=detectordim/2-dy-r & y1=y0+2*r
;;
;; new, faster code
;;
;; create array to hold the radial scan data
;;xdim=self.scanx & ydim=self.scany
;;Im=FLTARR(xdim,ydim,r+1)
minv=[0,0]
maxv=[r,100]
Support=FLTARR(self.framex,self.framey)
Support[*,*]=-100.
Support[x0:x1,y0:y1]=RadiusIm(r)
mybar=obj_new('ProgressBar', MINV=minv,MAXV=maxv,TEXT=["radius","% done"], STATUS="Processing ...", FRAMESTYLE=2)
pbarlev=DimZ/10.
;; The fluctuation signal S(Q), not spatially resolved (!)
;; for the spatially resolved S(Q,r) we need a region-of-interest
;; statistics tool
SFluctNorm=FLTARR(r+1)
SFluct=FLTARR(r+1)
For jj=0,r DO BEGIN
   w=Where(((Support GE jj) And (Support LT (jj+delta))),count)
   w1=Where(((Support GE jj) And (Support LT (jj+1))),count)
   IF (count GT 0) THEN BEGIN
      pbarind=0
      mybar->Set, 0, pbarind, TEXT=MyString(jj)
      mybar->Set, 1, pbarind, TEXT=MyString(pbarind)
      tmp=FLTARR(DimZ)
      For ind=0,DimZ-1 Do BEGIN 
         ;; loop over diffraction patterns
         if (ind GT pbarlev) THEN BEGIN
            pbarlev=pbarlev+DimZ/10.
            pbarind=pbarind+1  
            mybar->Set, 1, pbarind, TEXT=STRING(pbarind*10)
         END
         ;; set pixel value
         diffim=(*datap)[*,*,ind]
         ;; IF keyword_set(total) THEN tmp[ind]=Total(diffim[w]) ELSE tmp[ind]=stddev(diffim[w]) ;; signal over the annular ring
         ;;
         ;; take robust mean
         mean=robust_mean(diffim[w], sigma, meansig, num)
         diffim[w1] -= mean
         (*datap)[*,*,ind]=diffim
         tmp[ind]=mean
      END   ;; For ind loop
      ;; Im[*,*,jj]=REFORM(tmp,self.scanx,self.scany)
   END ELSE BEGIN
      ;;Im[*,*,jj]=0.
   END
   ;; record fluctuation value
   ;;SFluct[jj]=Variance(Im[*,*,jj])
   ;;flmean=Mean(Im[*,*,jj])
   ;;SFluctNorm[jj]=Variance(Im[*,*,jj])/flmean/flmean
END ;; For jj loop
;;IF keyword_set(total) THEN name="AnnularSegmentImages("+self.setname+")" ELSE name="FluctuationImages("+self.setname+")"
;;ThrowStack, PTR_NEW(Im), name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
obj_destroy, mybar
;;p=GetCurrentP()
;;IF keyword_set(total) THEN BEGIN
;;      s=["Annular Segment Images", "Parent data: "+self.GetSetName(), "Radii: "+MyString(0)+":"+MyString(r)+", Integration width (pix): "+MyString(delta), "Mode: "+"Sum of intensities", "Reciprocal Sampling: "+MyString(self.qsamp[0])+":"+MyString(self.qsamp[1])]
;;END ELSE BEGIN
;;   s=["Fluctuation Images", "Parent data: "+self.GetSetName(), "Radii: "+MyString(0)+":"+MyString(r)+", integration width: "+MyString(delta), "Mode: "+"Standard deviation", "Reciprocal Sampling: "+MyString(self.qsamp[0])+":"+MyString(self.qsamp[1])]
;;END
     ;;STOP
;;PrintToNote, (*p).datap, s, /TIMESTAMP
;; Create fluctuation graphs
;;ThrowGraph, list(SFluctNorm), XVALUES=FINDGEN(r+1)*self.qsamp[0], TITLE="Spatially Averaged Fluctuation Signal (normalized)", XTITLE="q (1/nm)", YTITLE="Normalized Fluctuation"
;;ThrowGraph, list(SFluct), XVALUES=FINDGEN(r+1)*self.qsamp[0], TITLE="Spatially Averaged Fluctuation Signal", XTITLE="q (1/nm)", YTITLE="Fluctuation"
END



PRO EMPADObj::FluctuationImages, DELTA=delta, DX=dx, DY=dy, TOTAL=total, TOPHAT=tophat
;; PARAMETERS:
;; x, y      = centre 
;; delta    = radius integration width in pix, Type: Integer
;;
;; RETURNS: 
;;
;; Creates  
;;      an image data stack that holds the fluctuation data vs radius
;;      for each pixel 
  fill=1 ;; fill gap pixels with neighbour content
  IF Not(keyword_set(delta)) then BEGIN
     delta=1
  END
  IF NOT(keyword_set(dx)) then dx=0
  IF NOT(keyword_set(dy)) then dy=0
  center=self.centerx
  detectordim=self.framex
  datap=self.p
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPAD::FluctuationImages: Data is invalid." 
     return
  END
  ;; auto-determine stack size odf the diffraction stack
  N=Size(*datap)
  DimX=N[1] & DimY=N[2] & DimZ=N[3]
  ;; determine the maximum radius
  ;; maxx=DimX-1
  r=detectordim/2-ABS(dx)-1-delta
  r=Min([r,detectordim/2-ABS(dy)-1-delta])
  r=FLOOR(r) ;; should be integer
  x0=detectordim/2-dx-r & x1=x0+2*r & y0=detectordim/2-dy-r & y1=y0+2*r
;;
;; new, faster code
;;
;; create array to hold the radial scan data
xdim=self.scanx & ydim=self.scany
Im=FLTARR(xdim,ydim,r+1)
minv=[0,0]
maxv=[r,100]
Support=FLTARR(self.framex,self.framey)
Support[*,*]=-100.
Support[x0:x1,y0:y1]=RadiusIm(r)
mybar=obj_new('ProgressBar', MINV=minv,MAXV=maxv,TEXT=["radius","% done"], STATUS="Processing ...", FRAMESTYLE=2)
pbarlev=DimZ/10.
;; The fluctuation signal S(Q), not spatially resolved (!)
;; for the spatially resolved S(Q,r) we need a region-of-interest
;; statistics tool
SFluctNorm=FLTARR(r+1)
SFluct=FLTARR(r+1)
For jj=0,r DO BEGIN
   w=Where(((Support GE jj) And (Support LT (jj+delta))),count)
   IF (count GT 0) THEN BEGIN
      pbarind=0
      mybar->Set, 0, pbarind, TEXT=MyString(jj)
      mybar->Set, 1, pbarind, TEXT=MyString(pbarind)
      tmp=FLTARR(DimZ)
      For ind=0,DimZ-1 Do BEGIN 
         ;; loop over diffraction patterns
         if (ind GT pbarlev) THEN BEGIN
            pbarlev=pbarlev+DimZ/10.
            pbarind=pbarind+1  
            mybar->Set, 1, pbarind, TEXT=STRING(pbarind*10)
         END
         ;; set pixel value
         diffim=(*datap)[*,*,ind]
         IF keyword_set(total) THEN tmp[ind]=Total(diffim[w]) ELSE tmp[ind]=stddev(diffim[w])
      END   ;; For ind loop
      Im[*,*,jj]=REFORM(tmp,self.scanx,self.scany)
   END ELSE BEGIN
      Im[*,*,jj]=0.
   END
   ;; record fluctuation value
   SFluct[jj]=Variance(Im[*,*,jj])
   flmean=Mean(Im[*,*,jj])
   SFluctNorm[jj]=Variance(Im[*,*,jj])/flmean/flmean
END ;; For jj loop
IF keyword_set(total) THEN name="AnnularSegmentImages("+self.setname+")" ELSE name="FluctuationImages("+self.setname+")"
ThrowStack, PTR_NEW(Im), name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
obj_destroy, mybar
p=GetCurrentP()
IF keyword_set(total) THEN BEGIN
      s=["Annular Segment Images", "Parent data: "+self.GetSetName(), "Radii: "+MyString(0)+":"+MyString(r)+", Integration width (pix): "+MyString(delta), "Mode: "+"Sum of intensities", "Reciprocal Sampling: "+MyString(self.qsamp[0])+":"+MyString(self.qsamp[1])]
END ELSE BEGIN
   s=["Fluctuation Images", "Parent data: "+self.GetSetName(), "Radii: "+MyString(0)+":"+MyString(r)+", integration width: "+MyString(delta), "Mode: "+"Standard deviation", "Reciprocal Sampling: "+MyString(self.qsamp[0])+":"+MyString(self.qsamp[1])]
END
     ;;STOP
PrintToNote, (*p).datap, s, /TIMESTAMP
;; Create fluctuation graphs
ThrowGraph, list(SFluctNorm), XVALUES=FINDGEN(r+1)*self.qsamp[0], TITLE="Spatially Averaged Fluctuation Signal (normalized)", XTITLE="q (1/nm)", YTITLE="Normalized Fluctuation"
ThrowGraph, list(SFluct), XVALUES=FINDGEN(r+1)*self.qsamp[0], TITLE="Spatially Averaged Fluctuation Signal", XTITLE="q (1/nm)", YTITLE="Fluctuation"
END

PRO EMPADObj::AngularFluctuationImages, DELTA=delta, DX=dx, DY=dy, Radius=radius
;; PARAMETERS:
;; x, y      = centre 
;; delta    = radius integration width in pix, Type: Integer
;;
;; RETURNS: 
;;
;; Creates  
;;      an image data stack that holds the fluctuation data vs
;;      diffraction angle
;;      for each pixel 
  fill=1 ;; fill gap pixels with neighbour content
  IF Not(keyword_set(delta)) then BEGIN
     delta=1
  END
  IF NOT(keyword_set(dx)) then dx=0
  IF NOT(keyword_set(dy)) then dy=0
  IF Not(keyword_set(radius)) then BEGIN
     ri=2 & ro=self.framex/2
  END ELSE BEGIN
     ri=radius[0]
     ro=radius[1]
  END
  center=self.centerx
  detectordim=self.framex
  datap=self.p
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPAD::FluctuationImages: Data is invalid." 
     return
  END
  ;; auto-determine stack size odf the diffraction stack
  N=Size(*datap)
  DimX=N[1] & DimY=N[2] & DimZ=N[3]
  ;; determine the maximum radius
  ;; maxx=DimX-1
  r=detectordim/2-ABS(dx)-1-delta
  r=Min([r,detectordim/2-ABS(dy)-1-delta])
  r=FLOOR(r) ;; should be integer
  x0=detectordim/2-dx-r & x1=x0+2*r & y0=detectordim/2-dy-r & y1=y0+2*r
;;
;; new, faster code
;;
;; create array to hold the radial scan data
NAng=CEIL(360./delta)
xdim=self.scanx & ydim=self.scany
Im=FLTARR(xdim,ydim,NAng)
minv=[1,0]
maxv=[NAng,100]
Support=FLTARR(self.framex,self.framey)
RSupport=FLTARR(self.framex,self.framey)
Support[*,*]=-100.
Support[x0:x1,y0:y1]= AngleIm(r)
RSupport[x0:x1,y0:y1]= RadiusIm(r)
mybar=obj_new('ProgressBar', MINV=minv,MAXV=maxv,TEXT=["angle increment","% done"], STATUS="Processing ...", FRAMESTYLE=2)
pbarlev=DimZ/10.
For jj=1,NAng DO BEGIN
   ang0=(jj-1)*delta/360.*2*!DPI
   ang1=jj*delta/360.*2*!DPI
   w=Where(((Support GE ang0) And (Support LE ang1)) AND ((RSupport GE ri) And (RSupport LE ro)),count)
   IF (count GT 0) THEN BEGIN
      pbarind=0
      mybar->Set, 0, pbarind, TEXT=MyString(jj)
      mybar->Set, 1, pbarind, TEXT=MyString(pbarind)
      tmp=FLTARR(DimZ)
      For ind=0,DimZ-1 Do BEGIN 
         ;; loop over diffraction patterns
         if (ind GT pbarlev) THEN BEGIN
            pbarlev=pbarlev+DimZ/10.
            pbarind=pbarind+1  
            mybar->Set, 1, pbarind, TEXT=STRING(pbarind*10)
         END
         ;; set pixel value
         diffim=(*datap)[*,*,ind]
         tmp[ind]=Total(diffim[w])
      END   ;; For ind loop
      Im[*,*,jj-1]=REFORM(tmp,self.scanx,self.scany)
   END ELSE BEGIN
      Im[*,*,jj-1]=0.
   END
END ;; For jj loop
name="AngularFluctuationImages("+self.setname+")"
ThrowStack, PTR_NEW(Im), name, SAMP=[self.samp[0],self.samp[1],1.], UNIT=['nm','nm','nm']
obj_destroy, mybar
p=GetCurrentP()
s=["Angular Fluctuation Images", "Parent data: "+self.GetSetName(), "Angles: "+MyString(0)+":"+MyString(2*!DPi)+", integration width: "+MyString(delta/360*!DPI), "Mode: "+"Sum", "Reciprocal Sampling: "+MyString(self.qsamp[0])+":"+MyString(self.qsamp[1])]
     ;;STOP
     PrintToNote, (*p).datap, s, /TIMESTAMP
END

PRO EMPADObj::Subset, ind, PREVIEW=preview, SUBSET=subset
  ;; subset returns the OBJ Pointer
  datap=self.p
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPAD::Subset: Data is invalid." 
     return
  END
  ;; auto-determine stack size
  DimZ=Size(ind, /N_ELEMENTS)
  IF (DIMZ GE 1) THEN BEGIN
     name="EMPAD_Subset("+self.setname+")"
     psubset=PTR_NEW((*datap)[*,*,ind])
     empaddata=obj_new('EMPADObj',name, ScanX=self.scanx, ScanY=self.scany, Subset=ind)
     empaddata.SetFrameSize, self.framex, self.framey
     empaddata.SetDataPointer, psubset
     ThrowStack, psubset, name, extra=empaddata, SAMP=[self.qsamp[0],self.qsamp[1],1.], UNIT=['1/nm','1/nm','1/nm']
     IF keyword_set(subset) THEN subset=empaddata
     ;; create preview
     IF keyword_set(preview) THEN BEGIN
        ;; create a simple BF image for preview
        empaddata->BF
     END
  END
END   


PRO EMPADObj::SubsetFromBinaryMask, maskp, PREVIEW=preview, SUBSET=subset
  ;; subset returns the OBJ Pointer
  datap=self.p
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPAD::Subset: Data is invalid." 
     return
  END
  ;; check sizes
  ;;
  IF NOT(PTR_VALID(maskp)) THEN  BEGIN
     printtocon, "% EMPADObj::SubsetFromBinaryMask: Mask pointer not set."
     return
  END ELSE mask=*maskp
  
  N=SIZE(mask)
  IF (N[0] NE 2) THEN BEGIN
     printtocon, "% EMPADObj::SubsetFromBinaryMask: Wrong mask dimensions. 2D array expected."
     return
  END
   IF ((N[1] NE self.scanx) OR (N[2] NE self.scany)) THEN BEGIN
     printtocon, "% EMPADObj::SubsetFromBinaryMask: Wrong mask size. Check number of pixels horizontally and vertically."
     return
  END
  ;; matching sizes, continue
   totmask=mask
   ind=WHERE(totmask EQ 1) ;; correct indices for the full data set
   IF self.issubset THEN BEGIN
      tmp=BYTARR(self.scanx,self.scany)
      tmp[self.ind]=1
      totmask=totmask*tmp
      ind=WHERE(totmask EQ 1) ;; correct indices for the full data set, wrong for the subset
      indinsubset=ind
      FOR j=0,N_ELEMENTS(ind)-1 DO indinsubset[j]=WHERE(self.ind EQ ind[j])
      ind=indinsubset
   END
  ;; auto-determine stack size
  DimZ=Size(ind, /N_ELEMENTS)
  IF (DIMZ GE 1) THEN BEGIN
     name="EMPAD_Subset("+self.setname+")"
     psubset=PTR_NEW((*datap)[*,*,ind])
     empaddata=obj_new('EMPADObj',name, ScanX=self.scanx, ScanY=self.scany, Subset=ind)
     empaddata.SetFrameSize, self.framex, self.framey
     empaddata.SetDataPointer, psubset
     ThrowStack, psubset, name, extra=empaddata, SAMP=[self.qsamp[0],self.qsamp[1],1.], UNIT=['1/nm','1/nm','1/nm']
     IF keyword_set(subset) THEN subset=empaddata
     ;; create preview
     IF keyword_set(preview) THEN BEGIN
        ;; create a simple BF image for preview
        empaddata->BF
     END
  END
END   

PRO EMPADObj::CropToSubset, roi
  datap=self.p
  If (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPAD::CropToSubset: Data is invalid." 
     return
  END
  ;; auto-determine stack size
  ;; Determine indices
  printtocon, "% EMPAD::CropToSubset: ROI=("+MyString(roi[0])+","+MyString(roi[1])+","+MyString(roi[2])+","+MyString(roi[3])+")"
  ;; prepare the filter mask
  help=BYTARR(self.scanx,self.scany)
  help[roi[0]:roi[1],roi[2]:roi[3]]=1
  ind=Where(help GT 0)
  ;; array indices from the filter mask
  DimZ=Size(ind, /N_ELEMENTS)
  IF (DIMZ GE 1) THEN BEGIN
     name="EMPAD_CroppedSet("+self.setname+")"
     psubset=PTR_NEW((*datap)[*,*,ind])
     empaddata=obj_new('EMPADObj',name, ScanX=roi[1]-roi[0]+1, Scany=roi[3]-roi[2]+1)
     empaddata.SetFrameSize, self.framex, self.framey
     empaddata.SetDataPointer, psubset
     ThrowStack, psubset, name, extra=empaddata, SAMP=[self.qsamp[0],self.qsamp[1],1.], UNIT=['1/nm','1/nm','1/nm']
  END
END   

PRO EMPADObj::CropSeries, range
  datap=self.p
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPAD::CropSeries: Data is invalid." 
     return
  END
  N=Size(*datap)
  DimX=N[1] & DimY=N[2] & DimZ=N[3]
  ;; auto-determine stack size
  ;; Determine indices
  IF (range[0] GT range[1]) THEN BEGIN
     tmp=range[0] & range[0]=range[1] & range[1]=tmp
  END
  if (range[0] LT 0) THEN range[0]=0
  if (range[1] GE DimZ) THEN range[1]=DimZ-1
  printtocon, "% EMPAD::CropSeries: Range=("+MyString(range[0])+","+MyString(range[1])+")"
  help=BYTARR(DimZ) ;; fake mask to determine index array
  help[range[0]:range[1]]=1
  ind=Where(help GT 0)
  DimZ=Size(ind, /N_ELEMENTS)
  IF (DIMZ GE 1) THEN BEGIN
     name="EMPAD_CroppedSeries("+self.setname+")"
     psubset=PTR_NEW((*datap)[*,*,ind])
     empaddata=obj_new('EMPADObj',name, ScanX=N_Elements(ind), ScanY=1)
     empaddata.SetFrameSize, self.framex, self.framey
     empaddata.SetDataPointer, psubset
     ThrowStack, psubset, name, extra=empaddata, SAMP=[self.qsamp[0],self.qsamp[1],1.], UNIT=['1/nm','1/nm','1/nm']
  END
END



PRO EMPADObj::GetAzimuthCorrSubset, symmetry, lowercorr
  IF NOT(PTR_VALID(self.psymmetrycorr)) THEN BEGIN
     printtocon, "% EMPADObj::GetAzimuthCorrSubset: Calculate 'Azimuthal Ring Intensity Correlation Map' first."
     return
  END
  datap=self.p
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPAD::GetAzimuthCorrSubset: Data is invalid." 
     return
  END
  symmetry=FIX(symmetry)
  IF ((symmetry GE 2) and (symmetry LE 6)) THEN BEGIN
     ind=WHERE((*(self.psymmetrycorr))[*,symmetry] GE lowercorr, count)
     IF (count GT 0) THEN BEGIN
        self.Subset, ind, /PREVIEW
     END ELSE  BEGIN
        printtocon, "% EMPADObj::GetAzimuthCorrSubset: No match at given lower bound."
        return
  END
  END ELSE  BEGIN
     printtocon, "% EMPADObj::GetAzimuthCorrSubset: Symmetry has to be between 2 and 6."
     return
  END
END

PRO EMPADObj::GetFluctuationMapSubset, lowercorr, SUBSET=subset
  IF NOT(PTR_VALID(self.pfluctmap)) THEN BEGIN
     printtocon, "% EMPADObj::GetFluctuationMapSubset: Calculate 'Fluctuation Map' first."
     return
  END
  datap=self.p
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPADObj::GetFluctuationMapSubset: Data is invalid." 
     return
  END
 
  ind=WHERE((*(self.pfluctmap)) GE lowercorr, count)
  IF (count GT 0) THEN BEGIN
     self.Subset, ind, /PREVIEW, SUBSET=subset
  END ELSE  BEGIN
     printtocon, "% EMPADObj::GetFluctuationMapSubsett: No match at given lower bound."
     return
  END
END 


PRO EMPADObj::PolarToRect, dx, dy, radius, delta, SUBSET=subset, FILL=fill, Proj=proj, INNER=inner
  ;; Transform to cartesian coordinates
  ;; radius= outer radius in pix
  ;; dx, dy= optional shift of the center
  ;; delta = azimuth step in degree
  ;; fill: if set then the missing information is filled in
  ;; proj: if set then a (1) 3D data set is created that contains the two
  ;; spatial coordinates and the diffraction radius as a third
  ;; coordinate; this dataset is equivalent to a sequential annular
  ;; detector with with one, and (2) a xy-graph is created that is the
  ;; radial plot of diffraction intensities
  maxradius=self.framex/2-1-max([abs(dx),abs(dy)])
  IF (radius GT maxradius) THEN radius=maxradius
  ;;; could add a filter later:
  ;; filt=self.AnnularMask(ri,ro, EXPONENT=16, TOPHAT=tophat)
  ;;IF ((dx NE 0) OR (dy NE 0)) THEN BEGIN
  ;;   filt=SHIFT(filt, dx, dy)
  ;;   if (dx GT 0) THEN IF (dx LT dimx) THEN filt[0:dx,*]=0.
  ;;   if (dy GT 0) THEN IF (dy LT dimy) THEN filt[*,0:dy]=0.
  ;;   if (dx LT 0) THEN IF ((-dx) LT dimX) THEN filt[(dimx+dx-1):(dimx-1),*]=0.
  ;;   if (dy LT 0) THEN IF ((-dy) LT dimy) THEN filt[*,(dimy+dy-1):(dimy-1)]=0.
  ;;END
  datap=self.p
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPAD::PolarToRect: Current stack pointer is invalid." 
     return
  END
  N=Size(*datap)
  DimX=N[1] & DimY=N[2] & DimZ=N[3]
  ;;
  ;; prepare output array
  ;;
  im=(*datap)[*,*,0]
  ;; im=(*datap)[*,*,i]*filt
  radius_=RadiusIm(radius)  ;; a
  angle_= AngleIm(radius)
  x0=self.frameX/2+dx
  y0=self.frameY/2+dy
  tmatrix=PTR_NEW()
  im=RotationalImageData(im, x0, y0, radius, delta, FILL=fill, RADIUS_=radius_, ANGLE_=angle_,TMATRIX=tmatrix)
  N=Size(im)
  s=FLTARR(N[1],N[2],DimZ)
  s[*,*,0]=im
  ;;
  ;; process
  ;;
  mybar=obj_new('ProgressBar', MINV=0,MAXV=DimZ-1,TEXT=["frame no"], STATUS="Transforming frames ...", FRAMESTYLE=2)
  For i=1L,DimZ-1 Do BEGIN
      mybar->Set, 0, i, TEXT=STRING(i)
     im=RotationalImageData((*datap)[*,*,i], x0, y0, radius, delta, FILL=fill, RADIUS_=radius_, ANGLE_=angle_,TMATRIX=tmatrix)
     s[*,*,i]=im
  END
  obj_destroy, mybar
  IF (keyword_set(inner) AND (inner LT radius)) THEN BEGIN
     s[0:inner,*,*]=0.
  END
 name="Rect("+self.setname+","+MyString(x0)+":"+MyString(y0) $
  + ","+MyString(radius)+":"+MyString(delta) +")"
 ThrowStack, PTR_NEW(s), name, TITLE=name, SAMP=[self.qsamp[0],delta,1.], UNIT=['1/nm','deg','']
 IF keyword_set(proj) THEN BEGIN
    sum=Total(Float(s), 2)
    ;; now the itegration is over the angular coordinate
    ;; the first dimension holds the radius
    ;; the second dimension the index position
    ;; how do we translate back to a 3D image?
    ;; basically like a multidimensional detector image
    ;; it is the same as an annular dark field image with
    N=SIZE(sum)
    Im=FLTARR(self.scanx,self.scany,N[1])
    IF self.issubset THEN BEGIN
       tmp=FLTARR(self.scanx,self.scany)
       For i=0,N[1]-1 DO BEGIN
          tmp[*(self.psubset)]=sum[i,*]
          Im[*,*,i]=tmp
       END
    END ELSE BEGIN
       For i=0,N[1]-1 DO BEGIN
          Im[*,*,i]=REFORM(sum[i,*],self.scanx,self.scany)
       END
    END
    name="ProjRect("+self.setname+","+MyString(x0)+":"+MyString(y0) $
  + ","+MyString(radius)+":"+MyString(delta) +")"
    Throwimage, im,TITLE=name, SAMP=[self.samp[0],self.samp[1],self.qsamp[0]], UNIT=['nm','nm','1/nm']
    ;; create an xy graph for the average radial plot
    sum=Total(sum,2)
    ;; create a graph object
    mygraph=obj_new('XYGraph', 'Radial Scattering Data', XTITLE='q (pix)', YTITLE='Intensity')
  ;; register graph in Inspector graph list!
  newgraph=GetNewXYGraphContainer("Radial Scattering Data")
  IF NOT(PTR_VALID(newgraph)) THEN BEGIN
  ;; an error occured, delete the object
     ErrMsg, "Could not create graph list item"
     mygraph->TidyUp
     obj_destroy, mygraph
     return
  END
;; store the object reference in the graph list item
  (*newgraph).pobj=mygraph
  ;; create a data set, X and Y are arrays
  mygraph->AddSet, 'Set 0', INDGEN(N[1]), sum, PSYM=4, SYMSIZE=1, /LINE, COLOR=RGBColor(200,0,0)
  mygraph->UpdateDisplay
  ;;
 END
 
 
 ;;  STOP
   ;; output
END 


FUNCTION EMPADObj::PropsToJSON
  ;; CATCH, Error_status
  ;; IF (Error_status NE 0) THEN BEGIN
  ;;    Print, "% EMPADObj::Cleanup: Fatal error message"
  ;;    Print, "%            " + !ERR_STRING
  ;;   CATCH, /Cancel‚
  ;;   return
  ;; END
  PrintToCon, "% EMPADObj::PropsToJSON: Creating JSON Descriptor."
  h=hash()
  h["setname"]=self.setname
  ;;    p: PTR_NEW(), $
  h["frame"]=[self.framex,self.framey]
  h["scan"]=[self.scanx,self.scany]
  h["center"]=[self.centerx,self.centery]
  h["samp"]=self.samp
  h["qsamp"]=self.qsamp
  return,JSON_SERIALIZE(h)
END

FUNCTION EMPADObj::JSONToProps, jsonstr, SETSHAPE=setshape
  PrintToCon, "% EMPADObj::JSONToProps: Reading JSON Descriptor."
  h=JSON_PARSE(jsonstr,  /FOLD_CASE)
  IF keyword_set(setshape) THEN BEGIN
     datap=self.p
     IF (NOT(PTR_VALID(datap))) THEN BEGIN
        printtocon, "% EMPADObj::JSONToProps: Current stack pointer is invalid."
        printtocon, "%    Shape remains unchanged." 
     END ELSE BEGIN
        N=Size(*datap)
        DimX=N[1] & DimY=N[2] & DimZ=N[3]
        frame=h["frame"]
        IF (frame[0] NE DimX) THEN printtocon, "%    Warning: Mismatch in frame size X, is - target = "+MyString(DimX)+" - "+MyString(frame[0])  

        IF (frame[1] NE DimY) THEN printtocon, "%    Warning: Mismatch in frame size Y, is - target = "+MyString(DimY)+" - "+MyString(frame[1])
        IF ((frame[0] EQ DimX) and  (frame[1] EQ DimY)) THEN BEGIN
           center=h["center"]
           self.centerx=center[0] & self.centery=center[0]
           printtocon, "%    Setting centerx, centery = "+MyString(self.centerx)+" - "+MyString(self.centery)
        END
        scan=h["scan"]
        IF ((scan[0]*scan[1]) EQ DimZ) THEN BEGIN
           self.scanx=scan[0] & self.scany=scan[1]
           printtocon, "%    Reshaping to scanx, scany = "+MyString(self.scanx)+", "+MyString(self.scany)           
        END ELSE printtocon, "%    Warning: Mismatch in scan size, is - target = "+MyString(DimZ)+" - "+MyString(scan[0]) +"x"+MyString(scan[1])
     END
  END
  ;; set sampling data
  samp=h["samp"]
  self.samp[0]=samp[0] & self.samp[1]=samp[1]
  printtocon, "%    Setting samplingx, samplingy = "+MyString(self.samp[0])+", "+MyString(self.samp[1])
  qsamp=h["qsamp"]
  self.qsamp[0]=qsamp[0] & self.qsamp[1]=qsamp[1]
  printtocon, "%    Setting qsamplingx, qsamplingy = "+MyString(self.qsamp[0])+", "+MyString(self.qsamp[1])
  return, 1
END


pro EMPADObj__define
 void={EMPADObj,name:'EMPADObj', $
       setname:"", $
       p: PTR_NEW(), $
       framex:128L, $
       framey:130L, $
       scanx:256L, $
       scany:256L, $
       centerx: 64, $
       centery: 64, $
       samp:[1.0,1.0], $
       qsamp:[1.0,1.0], $
       issubset: 0B, $
       psubset: PTR_NEW(), $
       psymmetrycorr: PTR_NEW(), $
       pfluctmap: PTR_NEW(), $
       setlength: 655535L, $
       break: 0}
 return
end

PRO EMPADObj::Cleanup
  ;; CATCH, Error_status
  ;; IF (Error_status NE 0) THEN BEGIN
  ;;    Print, "% EMPADObj::Cleanup: Fatal error message"
  ;;    Print, "%            " + !ERR_STRING
  ;;   CATCH, /Cancel‚
  ;;   return
  ;; END
  Print, "% EMPADObj::Cleanup: Cleaning up object."
  IF PTR_VALID(self.p) THEN BEGIN
     ;; delete
     PTR_FREE, self.p
  END
  IF PTR_VALID(self.psubset) THEN BEGIN
     ;; delete
     PTR_FREE, self.psubset
  END
  IF PTR_VALID(self.psymmetrycorr) THEN PTR_FREE, self.psymmetrycorr
  HELP, self
  obj_destroy, self
  HELP, self
  return
END



