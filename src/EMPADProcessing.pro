FUNCTION RadiusIm, r
;;
;; returns a quadratic image of dimensions (2r+1,2r+1)
;; containing in each pixel the distance
;; to the central pixel  
;; 

Radius = fltarr(2*r+1,2*r+1)
x=INDGEN(r+1, /LONG)    ;; x = 0 1 2 3 ... r
ID=LONARR(r+1)+1 ;; ID = 1 1 1 1 1 1 ...
tmp=(x*x)#ID     ;; tmp = 0 1 4 9 ...
                 ;;       repeated in r+1 rows
help=tmp + TRANSPOSE(tmp)  ;; help(i,j)=i^2+j^2, i=0..r, j=0..r
help=SQRT(FLOAT(help))  ;; help=euclidian distance to zero
i0=r & i1= 2*r
Radius(i0:i1,i0:i1) = help  ;; first quad
Radius(0:r,i0:i1) = REVERSE(help) ;; second quad is mirrored
Radius(0:i1,0:r) = REVERSE(Radius(0:i1,r:i1),2) ;; third and fourth quad
return, Radius
END

FUNCTION AngleIm, r,  MULTIPLICITY=multiplicity
;;
;; returns a quadratic image of dimensions (2r+1,2r+1) 
;; containing in each pixel the polar
;; angle with respect to the centre in radians  
;;
TheAngle = fltarr(2*r+1,2*r+1)
x=INDGEN(r, /LONG)+1    ;; x =  1 2 3 ... r, avoid 0 !!
ID=LONARR(r)+1 ;; ID = 1 1 1 1 1 1 ...
tmp=x#ID       ;; x repeated in r rows
help=ATAN(TRANSPOSE(tmp)/FLOAT(tmp))
i0=r+1
i1= 2*r
i2=r-1
TheAngle(i0:i1,i0:i1)=help  ;; first quad
TheAngle(0:i2,i0:i1) = !DPI-REVERSE(help) ;; second quad, mirrored + pi
TheAngle(0:i2,r)=!DPI
TheAngle(i0:i1,r)=0
TheAngle(r,i0:i1)=0.5*!DPI
TheAngle(0:i1,0:i2) = 2*!DPI-REVERSE(TheAngle(0:i1,i0:i1),2) ;; third and fourth quad
IF keyword_set(multiplicity) THEN BEGIN
   IF multiplicity GT 1 THEN BEGIN
      TheAngle =  TheAngle MOD ((2 * !PI)/multiplicity)
   END
END
return, TheAngle
END



PRO EMPAD_PolarToRect, p, ri, ro, angwidth, FILL=fill, RECTARR=rectarr, SYMMETRYCORR=symmetrycorr
;; PARAMETERS:
;; p         = set of EMPAD diffraction images
;; x, y      = centre 
;; ri, ro    = radius in pix, Type: Integer
;; angwidth = angular step width in deg, Type: Float 
;;
;; RETURNS: rectarr = image with x=angle and y=index, an image of the polar
;; trace for each pattern in p
;; symmetrycorr = symmetrycorrelation, array of rotary symmetry
;; correlation with x=index, y=symmetry
;; e.g. symmetrycorr[ind,2] is the correlation of image ind with 2-fold symmetry 
;;      symmetrycorr[ind,3] is the correlation of image ind with 
;;      3-fold symmetry
;;      ...
;;      symmetrycorr[ind,2] is the correlation of image ind with 6-fold symmetry
  
  IF (NOT(PTR_VALID(p))) THEN BEGIN
     printtocon, "% EMPAD_PolarToRect: Current stack pointer is invalid." 
     return
  END
  e=(*p).datap
  IF (NOT(PTR_VALID(e))) THEN BEGIN
     printtocon, "% EMPAD_PolarToRect: Current data pointer is invalid." 
     return
  END
  ;; auto-determine stack size
  datap=(*e).data
  N=Size(*datap)
  DimX=N[1] & DimY=N[2] & DimZ=N[3]
  detectordim=DimX
  center=Round(DimX/2)
  ;;
  ri=FLOOR(ri)
  r=FLOOR(ro) ;; should be integer
  x0=center-r & x1=center+r & y0=center-r & y1=center+r
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
END ;; For im loop
;; project Im[*,*,ind] along x-direction
P=Total(Im, 1)
if keyword_set(rectarr) THEN rectarr=P
if keyword_set(symmetrycorr) THEN BEGIN 
   acorr=FLTARR(DimZ,7) ;; last dimension is symmetry
;; For i=0,DimZ-1 DO acorr2= A_CORRELATE(p[*,i], ydim/2)
   For i=0,DimZ-1 DO BEGIN
      For j=2,6 DO BEGIN
         acorr[i,j] = A_CORRELATE(p[*,i], ydim/j)
      END
   END
symmetrycorr=acorr
;; done
END
END



PRO EMPAD_Center_uDiff, p, SUBPIX=subpix, COMRADIUS=comradius
  ;; p pointer to the current data stack
  refradius=5
  IF keyword_set(comradius) THEN refradius=comradius
  IF (NOT(PTR_VALID(p))) THEN BEGIN
     printtocon, "% EMPAD_Center_uDiff: Current stack pointer is invalid." 
     return
  END
  e=(*p).datap
  IF (NOT(PTR_VALID(e))) THEN BEGIN
     printtocon, "% EMPAD_Center_uDiff: Current data pointer is invalid." 
     return
  END
  ;; auto-determine stack size
  datap=(*e).data
  N=Size(*datap)
  DimX=N[1] & DimY=N[2] & DimZ=N[3]
  detectordim=DimX
  center=Round(DimX/2)
  delta=2*refradius+1
  comx=INDGEN(delta)
  sx=FLTARR(DimZ)
  sy=FLTARR(DimZ)  
  For i=0L,DimZ-1 Do BEGIN
     index_array=where((*datap)[*,0:(detectordim-1),i] EQ Max((*datap)[*,0:(detectordim-1),i]), Count)
     IF (count GT 1) THEN index_array=index_array[0]
     ;; refine max to com
     x=index_array mod detectordim
     y=(index_array/detectordim)
     x0=x-refradius & x1=x+refradius
     y0=y-refradius & y1=y+refradius
     IF ((x0 GE 0) AND (x1 LT DimX) AND (y0 GE 0) AND (y1 LT DimY)) THEN BEGIN
        patch=(*datap)[x0:x1,y0:y1,i]    
        xvec=total(patch,2)
        ddx=(Transpose(comx)#xvec)/Total(xvec)-refradius
        yvec=total(patch,1)
        ddy=(Transpose(comx)#yvec)/Total(yvec)-refradius
        sx[i]=x - center + ddx
        sy[i]=y - center + ddy
        
        IF keyword_set(subpix) THEN BEGIN
           (*datap)[*,0:(detectordim-1),i] = FLOAT(subshift(REFORM((*datap)[*,0:(detectordim-1),i]),-sx[i],-sy[i]))
        END ELSE BEGIN
           dx=ROUND(sx[i])
           dy=ROUND(sy[i])
           (*datap)[*,0:(detectordim-1),i]=SHIFT((*datap)[*,0:(detectordim-1),i],-dx,-dy)
        END
         ;; print, i, sx[i], sy[i]
     END  
  END
END 


PRO EMPAD_BF, p, sx, sy, RADIUS=radius
  ; p= ptr to the current data stack 
  refradius=5
  IF keyword_set(radius) then refradius=radius
  IF (NOT(PTR_VALID(p))) THEN BEGIN
     printtocon, "% EMPAD_BF: Current stack pointer is invalid." 
     return
  END
  e=(*p).datap
  IF (NOT(PTR_VALID(e))) THEN BEGIN
     printtocon, "% EMPAD_BF: Current data pointer is invalid." 
     return 
  END
  ;; auto-determine stack size
  datap=(*e).data
  N=Size(*datap)
  DimX=N[1] & DimY=N[2] & DimZ=N[3]
  detectordim=DimX
  center=Round(DimX/2)
  delta=11
  comx=INDGEN(delta)
  s=FLTARR(DimZ)
  For i=0L,DimZ-1 Do BEGIN
     patch=(*datap)[(center-refradius):(center+refradius),(center-refradius):(center+refradius),i]
     s[i]=total(patch,0)
  END

  name="BF("+(*p).name+","+MyString(center)+":"+MyString(refradius)+")"
  pp=DataList_CreateElement(GetRootP(), name)
  ;; make sure we will not get an overflow when adding data 
  (*pp).type=4
  ;;
  (*pp).SzX=sx
  (*pp).SzY=sy
  (*pp).SzZ=1
  (*pp).slice=0
  (*pp).zcoord=3
  (*pp).contrastmode="auto"
  ppd=(Data_GetEmptyArrayP(pp))
  (*pp).data=ppd
  IF (PTR_VALID(ppd)) THEN BEGIN
     (*ppd)[*,*,0]=REFORM(s,sx,sy)
  END
  CreateWindow
  Update_XTabControl
  TVDisplay
END 

FUNCTION EMPAD_roi_to_indices, sx, sy, roi
  arr=BYTARR(sx,sy)
  arr[roi[0]:roi[1],roi[2]:roi[3]]=1
  arr=REFORM(arr,Long(sx)*Long(sy),1)
  return, WHERE(arr EQ 1)
END

PRO EMPAD_extract_roi, p, ind
  ;; p pointer to the current data stack
  IF (NOT(PTR_VALID(p))) THEN BEGIN
     printtocon, "% EMPAD_extract_roi: Current stack pointer is invalid." 
     return
  END
  e=(*p).datap
  
  IF (NOT(PTR_VALID(e))) THEN BEGIN
     printtocon, "% EMPAD_extract_roi: Current data pointer is invalid."
     return 
  END
  N=Size(*datap)
  DimX=N[1] & DimY=N[2] & DimZ=N[3]
  DimZ=Size(ind, /N_ELEMENTS)
  IF (DIMZ GE 1) THEN BEGIN
     name="EMPAD_ROISlice("+(*p).name+")"
     pp=DataList_CreateElement(GetRootP(), name)
     ;; make sure we will not get an overflow when adding data 
     (*pp).type=(*e).type
     ;;
     (*pp).SzX=(*e).SzX
     (*pp).SzY=(*e).SzY
     (*pp).SzZ=DimZ
     (*pp).xsamp=(*e).xsamp
     (*pp).ysamp=(*e).ysamp
     (*pp).zsamp=(*e).zsamp
     (*pp).slice=0
     (*pp).zcoord=3
     (*pp).contrastmode="auto"
     ppd=(Data_GetEmptyArrayP(pp))
     (*pp).data=ppd
     IF (PTR_VALID(ppd)) THEN BEGIN
        (*ppd)=(*(*e).data)[*,*,ind]
        
     END 
     CreateWindow
     Update_XTabControl
     TVDisplay
  END
  
END   

FUNCTION EMPAD_bin_core, p, bin, sx, sy
  ;; Bin over neighbouring real space pixels
  ;; neihbouring pixels to the left and right are considered in a
  ;; square-pattern:
  ;; p pointer to the current data stack
  ;;
  ;; BUG: There is still an issue with the last row
  IF (NOT(PTR_VALID(p))) THEN BEGIN
     printtocon, "% EMPADObj::SpatialBinning: Current stack pointer is invalid." 
     return, PTR_NEW()
  END
  datap=(*p).datap
  IF (NOT(PTR_VALID(datap))) THEN BEGIN
     printtocon, "% EMPADObj::SpatialBinning: Current stack pointer data link is invalid." 
     return, PTR_NEW()
  END
  datap=(*datap).data
  N=Size(*datap)
  DimX=LONG(sx) & DimY=LONG(sy) & DimZ=LONG(N[3])
  detectordim=N[1]
  ;; BinDimX=FLOOR(DimX/bin) & BinDimY=FLOOR(DimY/bin) &
  ;; BinDimZ=BinDimX*BinDimY;; spatial binning, the remaining
  ;; dimensions in x,y,z
  BinDimX=FLOOR(DimX/bin) & BinDimY=FLOOR(DimY/bin) & BinDimZ=BinDimX*BinDimY;; spatial binning, the remaining dimensions in x,y,z
  filter=SquareROIIndices(DimX,DimY,bin,/NONCENTERED) ;; index square patch, the 0,0/0,1/1,0/1,1 pixel indices for bin 2 = 0,DimX,1,DimX+1
  tmp=FLTARR(N[1],N[2])
  Norm=1. ;; sum up the data, do not average
  minv=[0]
  maxv=[100]
  pbarstep=5
  pbarperc=0
  M=LONG(BinDimX*BinDimY)
  pbindata=PTR_NEW(FLTARR(N[1],N[2],BinDimZ))
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
  STOP
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
     ;;WSET, wdow
     ;;a[*,*]=0 & a[f]=1 & TVSCL, a
     ;; WSET, origwdow
  END
  obj_destroy, mybar
  return, pbindata
END  

PRO EMPAD_bin, p, bin, sx, sy
  ;; p pointer to the current data stack
  IF (NOT(PTR_VALID(p))) THEN BEGIN
     printtocon, "% EMPAD_bin: Current stack pointer is invalid." 
     return
  END
  e=(*p).datap
  IF (NOT(PTR_VALID(e))) THEN BEGIN
     printtocon, "% EMPAD_bin: Current data pointer is invalid." 
     return 
  END
  pbindata=EMPAD_bin_core(p, bin, sx, sy)  
  IF (PTR_VALID(pbindata)) THEN BEGIN
     N=SIZE(*pbindata)
     DimX=N[1] & DimY=N[2] & DimZ=N[3]
     name="PAD_Bin("+(*p).name+")"
     pp=DataList_CreateElement(GetRootP(), name)
     ;; make sure we will not get an overflow when adding data 
     (*pp).type=(*e).type
     ;;
     (*pp).SzX=DimX
     (*pp).SzY=DimY
     (*pp).SzZ=DimZ
     (*pp).xsamp=bin*(*e).xsamp
     (*pp).ysamp=bin*(*e).ysamp
     (*pp).zsamp=(*e).zsamp
     (*pp).slice=0
     (*pp).zcoord=(*e).contrastmode
     (*pp).contrastmode=(*e).contrastmode
     (*pp).data=pbindata
     CreateWindow
     Update_XTabControl
     TVDisplay
  END 
END    


FUNCTION EMPAD_annular_mask, inner, outer, exponent, detx, dety, CENTRALPIX=centralpix
;; prepare a mask
;; inner: inner radius in pix
;;  detx=128 & dety=130
  PrepareRecSampGrid, detx, dety, pkx, pky, pu, DY=-2
  mask=GetBPNQFilterPreCalcK(pkx, pky, pu, (2.*inner)/detx, (2.*outer)/detx, exponent, /GAUSSIAN, /NOFFTSHIFT, MEANVALUE=centralpix)
  return, mask
END
  

PRO EMPAD_apply_mask, p, mask, sx, sy, SDEV=sdev, THRESHSDEV_INDICES=threshsdev_indices
  ;; by default EMPAD_apply_mask returns the total intensity
  ;; if sdev is set then it will return the standard deviation (=
  ;; fluctuation EM)
  ; p= ptr to the current data stack 
  IF (NOT(PTR_VALID(p))) THEN BEGIN
     printtocon, "% EMPAD_BF: Current stack pointer is invalid." 
     return
  END
  e=(*p).datap
  IF (NOT(PTR_VALID(e))) THEN BEGIN
     printtocon, "% EMPAD_BF: Current data pointer is invalid." 
     return 
  END
  ;; auto-determine stack size
  datap=(*e).data
  N=Size(*datap)
  DimX=N[1] & DimY=N[2] & DimZ=N[3]
  delta=11
  comx=INDGEN(delta)
  s=FLTARR(DimZ)
  For i=0L,DimZ-1 Do BEGIN
     IF keyword_set(sdev) THEN s[i]=STDDEV((*datap)[*,*,i]*mask) ELSE s[i]=total((*datap)[*,*,i]*mask,0)
  END
  name="Mask("+(*p).name+")"
  pp=DataList_CreateElement(GetRootP(), name)
  ;; make sure we will not get an overflow when adding data 
  (*pp).type=4
  ;;
  (*pp).SzX=sx
  (*pp).SzY=sy
  (*pp).SzZ=1
  (*pp).slice=0
  (*pp).zcoord=3
  (*pp).contrastmode="auto"
  ppd=(Data_GetEmptyArrayP(pp))
  (*pp).data=ppd
  IF KEYWORD_SET(threshsdev_indices) THEN threshsdev_indices=WHERE(s GE threshsdev_indices)
  IF (PTR_VALID(ppd)) THEN BEGIN
     (*ppd)[*,*,0]=REFORM(s,sx,sy)
  END
  CreateWindow
  Update_XTabControl
  TVDisplay
END

PRO EMPAD_fluctuation_corr, p, mask, sx, sy, SDEV=sdev
  ;; EMPAD_fluctuation_corr_mask analysis the correlation on a
  ;; diffraction ring
  ;; mask should be a ring aperture
  ;; the mask will be projected along 
  IF (NOT(PTR_VALID(p))) THEN BEGIN
     printtocon, "% EMPAD_BF: Current stack pointer is invalid." 
     return
  END
  e=(*p).datap
  IF (NOT(PTR_VALID(e))) THEN BEGIN
     printtocon, "% EMPAD_fluctuation_corr: Current data pointer is invalid." 
     return 
  END
  ;; auto-determine stack size
  datap=(*e).data
  N=Size(*datap)
  DimX=N[1] & DimY=N[2] & DimZ=N[3]
  delta=11
  comx=INDGEN(delta)
  s=FLTARR(DimZ)
  For i=0L,DimZ-1 Do BEGIN
     ;; process here
     ;; data set is (*datap)[*,*,i]*mask
  END
  name="Fluctuation("+(*p).name+")"
  pp=DataList_CreateElement(GetRootP(), name)
  ;; make sure we will not get an overflow when adding data 
  (*pp).type=4
  ;;
  (*pp).SzX=sx
  (*pp).SzY=sy
  (*pp).SzZ=1
  (*pp).slice=0
  (*pp).zcoord=3
  (*pp).contrastmode="auto"
  ppd=(Data_GetEmptyArrayP(pp))
  (*pp).data=ppd
  IF (PTR_VALID(ppd)) THEN BEGIN
     (*ppd)[*,*,0]=REFORM(s,sx,sy)
  END
  CreateWindow
  Update_XTabControl
  TVDisplay
END




PRO EMPAD_transform_by_mask, p, mask
  ; p= ptr to the current data stack 
  IF (NOT(PTR_VALID(p))) THEN BEGIN
     printtocon, "% EMPAD_BF: Current stack pointer is invalid." 
     return
  END
  e=(*p).datap
  IF (NOT(PTR_VALID(e))) THEN BEGIN
     printtocon, "% EMPAD_BF: Current data pointer is invalid." 
     return 
  END
  ;; auto-determine stack size
  datap=(*e).data
  N=Size(*datap)
  DimX=N[1] & DimY=N[2] & DimZ=N[3]
  For i=0L,DimZ-1 Do BEGIN
     (*datap)[*,*,i]=(*datap)[*,*,i]*mask
  END
  name="Mask("+(*p).name+")"
  Update_XTabControl
  TVDisplay
END



;; Sequence
PRO EMPAD_test
;; parameters
roi=1
sx=256 & sy=256
peakradius=5
;; get current data stack pointer
p=GetCurrentP()
;; align diffraction pattern - use pixel accuracy to avoid artefacts
;; EMPAD_CENTER_UDIFF, p, /SUBPIX, COMRADIUS=peakradius
EMPAD_CENTER_UDIFF, p, COMRADIUS=peakradius
paligned=p
;; create a virtual bright-field image
EMPAD_BF, p, sx, sy, RADIUS=peakradius
pbf=GetCurrentP()
;; select a region of interest in the bright-field image
ROISelection, ROI=roi
;; convert to 2D ROI 1D-array
ind=EMPAD_roi_to_indices(sx, sy, LONG(roi))
;; slice EMPAD data stack, return stack with diffraction data for the ROI
p=paligned
EMPAD_extract_roi, p, ind
;; set up an annular filter from 10 to 20 pix radius, exponent 14
filt=EMPAD_annular_mask(10,20,14) 
EMPAD_apply_mask, p, filt, sx, sy ;; create virtual ADF image
EMPAD_apply_mask, p, filt, sx, sy, /SDEV ;; create fluctuation EM image
;; bin by a factor two
;; parameters: binning factor, scan points in x and y 
EMPAD_bin, paligned, 2, 256, 256
;; mask central beam
filt=EMPAD_annular_mask(11,200,10) ;; create an inverse filter 
EMPAD_transform_by_mask, GetCurrentP(), filt;;
;; create fluctuation image
;; - create a filter with an inner ring diameter of 14 pix and an
;;   outer diameter of 17 pix
filt=EMPAD_annular_mask(14,17,20)
;; - get the fluctuation mask
EMPAD_apply_mask, GetCurrentP(), filt, 256,256, /SDEV
;; (determine thresholds in the fluctuation mask)
;; - get locations with a fluctuation larger than a threshold
thresh_ind=125.
EMPAD_apply_mask, GetCurrentP(), filt, 256,256, /SDEV, ThreshSDEV_INDICES=thresh_ind
;;   (threshind now holds the one dimensional array of locations)
;;   (if you want to see the mask behind it: )
;;   (mask=BYTARR(Long(256)*Long(256)) & mask=reform(mask,sx,sy) &
;;   mask(threshind)=1B & TVSCL, mask)
;; - extract all diffraction patterns that match the criterion sdev >
;;   thresh
EMPAD_extract_roi, GetCurrentP(), thresh_ind
;; analze these for symmetry, just for the subset with large variance
;; on the diffraction ring
symmetrycorr=1
EMPAD_PolarToRect, GetCurrentP(), 14, 20, 1., /FILL, SYMMETRYCORR=symmetrycorr
;; plot 2 fold symmetry
sym2foldim=FLTARR(sx,sy)
sym2foldim(thresh_ind)=ABS(symmetrycorr(*,2))
Throwimage, sym2foldim GT 0.2, TITLE="Twofold symmetry correlation"
;; same for the whole data set
p=paligned
EMPAD_PolarToRect, GetCurrentP(), 14, 20, 1., /FILL, SYMMETRYCORR=symmetrycorr
ThrowImage, REFORM(symmetrycorr[*,2],sx,sy), TITLE="twofold"
;; take only those with the correlation larger than 0.3
ind=WHERE((symmetrycorr[*,2] GT 0.3))
p=paligned
EMPAD_extract_roi, p, ind
p=GetCurrentP()
EMPAD_CENTER_UDIFF, p, /SUBPIX, COMRADIUS=6
END



PRO SpatialBinning, o, bin
  ;; o=EMPADObject
pdata=o.SpatialBinning(bin)
            N=Size(*pdata)
            ;; creating EMPAD Object
            fname='Bin('+o.GetSetName()+')'
            empaddata=obj_new('EMPADObj',fname)
            empaddata.SetScanSize, FIX(o.GetScanSize(/X)/bin), FIX(o.GetScanSize(/Y)/bin) 
            empaddata.SetFrameSize, o.GetFrameSize(/X),o.GetFrameSize(/Y)
            empaddata.SetScanSampling, o.GetScanSampling(/X)*bin, o.GetScanSampling(/Y)*bin
            empaddata.SetDetectorSampling, o.GetDetectorSampling(/X), o.GetDetectorSampling(/Y)
            empaddata.SetDataPointer, pdata
            ;;empaddata.SetVoltage(o.GetVoltage())
              ;; link data to pointer list, so that it can be displayed
            ;;
            ptr=GetRootP() ;; datalistpointer
            names=DataList_GetNames(ptr)
            Selected=WHERE(names EQ fname)
            IF (Selected EQ -1) THEN BEGIN
               e=DataList_CreateElement(ptr, fname)
            END ELSE BEGIN
               i=2
               REPEAT BEGIN
                  altname=fname+ "" + MyString(i)
                  Selected=WHERE(names EQ altname)
                  i=i+1
               END UNTIL (Selected EQ -1)
               e=DataList_CreateElement(ptr, altname)
            END 
            IF PTR_VALID(e) THEN BEGIN
               (*e).data=empaddata.GetDataPointer()
               IF (PTR_VALID((*e).data)) THEN BEGIN
                  displaybin=0.5
                  (*e).id=empaddata.GetSetName()
                  (*e).SzX=empaddata.GetFrameSize(/X)
                  (*e).SzY=empaddata.GetFrameSize(/Y)
                  (*e).SzZ=empaddata.GetScanSize(/X)*empaddata.GetScanSize(/Y)
                  (*e).xsamp=empaddata.GetDetectorSampling(/X)
                  (*e).ysamp=empaddata.GetDetectorSampling(/Y)
                  (*e).xunit='1/nm'
                  (*e).yunit='1/nm'
                  ;; (*e).voltage=empaddata.GetVoltage()
                  (*e).id=fname
                  (*e).type=4
                  (*e).slice=FIX((*e).SzZ/2)
                  (*e).zcoord=3
                  (*e).extra=empaddata
                  (*e).contrastmode="auto"
                  (*e).contrastsubmode="minmax" 
                  (*e).contrastroi="diff"
                  bin=1
                  (*e).binning=displaybin
                  (*e).binningx=displaybin
                  (*e).binningy=displaybin
                  CreateWindow, BIN=displaybin
                  TVDisplay
                  Update_XTabControl
               End
            END  
END

PRO ToPADObject, NONINTERACTIVE=noninteractive
  p=GetCurrentP()
   IF (NOT(PTR_VALID(p))) THEN BEGIN
     printtocon, "% ToPADObject: Current stack pointer is invalid." 
     return
  END
  e=(*p).datap
  IF (NOT(PTR_VALID(e))) THEN BEGIN
     printtocon, "% ToPADObject: Current data pointer is invalid." 
     return 
  END
  ;; auto-determine stack size
  datap=(*e).data
  N=Size(*datap)
  
  qsamplingx=(*e).xsamp
  qsamplingy=(*e).ysamp
  sy=N[3] & sx=1
  samplingx=1. & samplingy=1.
 IF NOT(keyword_set(noninteractive)) THEN BEGIN
    s=list()
    s.Add, {value:sx,label:"Scan size x, y",newrow:1B}
    s.Add, {value:sy,label:"",newrow:0B}
    s.Add, {value:samplingx,label:"Scan sampling rate x, y",newrow:1B}
    s.Add, {value:samplingy,label:"",newrow:0B}
    s.Add, {value:qsamplingx,label:"Detector sampling rate qx, qy",newrow:1B}
    s.Add, {value:qsamplingy,label:"",newrow:0B}
    c=list()
    IF NOT(XMDataChoiceField(s, c, TITLE="Edit Properties") EQ 1) THEN BEGIN
       printtocon, "% ToPADObject: Aborting."
       return
    END
    sx=s[0].value
    sy=s[1].value
    samplingx=s[2].value
    samplingy=s[3].value
    qsamplingx=s[4].value
    qsamplingy=s[5].value
 END
 empaddata=obj_new('EMPADObj',(*p).name,FRAMEX=N[1],FRAMEY=N[2])
 empaddata.SetScanSize, sx, sy
 empaddata.SetScanSampling, samplingx, samplingy
 empaddata.SetDetectorSampling, qsamplingx, qsamplingy
 empaddata.SetDataPointer, datap
 (*e).extra=empaddata
END 



PRO EditPADObjectMetaData, padobj, NONINTERACTIVE=noninteractive
IF OBJ_VALID(padobj) THEN BEGIN
   xsamp=padobj->GetScansampling(/X)
   ysamp=padobj->GetScansampling(/Y) 
   qxsamp=padobj->GetDetectorSampling(/X)
   qysamp=padobj->GetDetectorSampling(/Y) 
   sx=padobj->GetScanSize(/X)
   sy=padobj->GetScanSize(/Y)
  
 IF NOT(keyword_set(noninteractive)) THEN BEGIN
    s=list()
    s.Add, {value:sx,label:"Scan size x, y",newrow:1B}
    s.Add, {value:sy,label:"",newrow:0B}
    s.Add, {value:xsamp,label:"Scan sampling rate x, y",newrow:1B}
    s.Add, {value:ysamp,label:"",newrow:0B}
;;     s.Add, {value:sampunit,label:"Unit",newrow:0B}
    s.Add, {value:qxsamp,label:"Detector sampling rate qx, qy",newrow:1B}
    s.Add, {value:qysamp,label:"",newrow:0B}
;;    s.Add, {value:qsampunit,label:"",newrow:0B}
    c=list()
    IF NOT(XMDataChoiceField(s, c, TITLE="Edit PAD Meta Data") EQ 1) THEN BEGIN
       printtocon, "% ToPADObject: Aborting."
       return
    END
    sx_new=s[0].value
    sy_new=s[1].value
    xsamp=s[2].value
    ysamp=s[3].value
    qxsamp=s[4].value
    qysamp=s[5].value
 END
 IF ((LONG(sx_new)*LONG(sy_new)) EQ (sx*sy)) THEN BEGIN
    padobj->SetScanSize, sx, sy
 END ELSE printtocon, "% EditPADObjectMetaData:  Shape change ignored, mismatch in total number of array elements."
 padobj->SetScanSampling, xsamp, ysamp
 padobj->SetDetectorSampling, qxsamp, qysamp
END 
END 

PRO RecenterDiffractionFrames, PICK=pick
  ptr=GetRootP()
  IF NOT(PTR_VALID(ptr)) THEN BEGIN
     printtocon, "% Menu: No 3D Data found."
     return
  END
  ptr=(*ptr).current
  IF NOT(PTR_VALID(ptr)) THEN BEGIN
     printtocon, "% Menu: Current 3D Data invalid."
     return
  END
  ptr=(*ptr).datap
  IF NOT(PTR_VALID(ptr)) THEN BEGIN
     printtocon, "%  Menu: Current 3D Data holds no data."
         return
      END
  IF keyword_set(pick) THEN BEGIN
  END ELSE BEGIN
     centerx=FIX((*ptr).SzX/2) & centery=FIX((*ptr).SzY/2)
     x=centerx & y=centery & r=FIX(centerx/2)
     IF XCircle(x,y,r, bin=(*ptr).binning) THEN BEGIN
        dx=Round(x-centerx) & dy=ROUND(y-centery)
        IF OBJ_VALID((*ptr).extra) THEN BEGIN
           o=(*ptr).extra
           o.FrameShift, DX=-dx, DY=-dy
        END ELSE BEGIN
           d=(*ptr).data
           IF PTR_VALID(d) THEN BEGIN
              N=SIZE(*d)
              DimX=N[1] & DimY=N[2] & DimZ=N[3]
              For i=0L,DimZ-1 Do BEGIN
                    (*d)[*,*,i]=SHIFT((*d)[*,*,i],dx,dy)
              END
           END
        END
        TVDISPLAY
     END
  END
END

PRO RecenterDiffractionFrames, PICK=pick
  ptr=GetRootP()
  IF NOT(PTR_VALID(ptr)) THEN BEGIN
     printtocon, "% Menu: No 3D Data found."
     return
  END
  ptr=(*ptr).current
  IF NOT(PTR_VALID(ptr)) THEN BEGIN
     printtocon, "% Menu: Current 3D Data invalid."
     return
  END
  ptr=(*ptr).datap
  IF NOT(PTR_VALID(ptr)) THEN BEGIN
     printtocon, "%  Menu: Current 3D Data holds no data."
         return
      END
  IF keyword_set(pick) THEN BEGIN
  END ELSE BEGIN
     centerx=FIX((*ptr).SzX/2) & centery=FIX((*ptr).SzY/2)
     x=centerx & y=centery & r=FIX(centerx/2)
     IF XCircle(x,y,r, bin=(*ptr).binning) THEN BEGIN
        dx=Round(x-centerx) & dy=ROUND(y-centery)
        IF OBJ_VALID((*ptr).extra) THEN BEGIN
           o=(*ptr).extra
           o.FrameShift, DX=-dx, DY=-dy
        END ELSE BEGIN
           d=(*ptr).data
           IF PTR_VALID(d) THEN BEGIN
              N=SIZE(*d)
              DimX=N[1] & DimY=N[2] & DimZ=N[3]
              For i=0L,DimZ-1 Do BEGIN
                    (*d)[*,*,i]=SHIFT((*d)[*,*,i],dx,dy)
              END
           END
        END
        TVDISPLAY
     END
  END
END

PRO SpatialBinningSimplified, o, bin
  ;; o=EMPADObject
pdata=o.SpatialBinningTwoStage(bin)
            N=Size(*pdata)
            ;; creating EMPAD Object
            fname='Bin('+o.GetSetName()+')'
            empaddata=obj_new('EMPADObj',fname)
            empaddata.SetScanSize, FIX(o.GetScanSize(/X)/bin), FIX(o.GetScanSize(/Y)/bin) 
            empaddata.SetFrameSize, o.GetFrameSize(/X),o.GetFrameSize(/Y)
            empaddata.SetScanSampling, o.GetScanSampling(/X)*bin, o.GetScanSampling(/Y)*bin
            empaddata.SetDetectorSampling, o.GetDetectorSampling(/X), o.GetDetectorSampling(/Y)
            empaddata.SetDataPointer, pdata
            ;;empaddata.SetVoltage(o.GetVoltage())
              ;; link data to pointer list, so that it can be displayed
            ;;
            ptr=GetRootP() ;; datalistpointer
            names=DataList_GetNames(ptr)
            Selected=WHERE(names EQ fname)
            IF (Selected EQ -1) THEN BEGIN
               e=DataList_CreateElement(ptr, fname)
            END ELSE BEGIN
               i=2
               REPEAT BEGIN
                  altname=fname+ "" + MyString(i)
                  Selected=WHERE(names EQ altname)
                  i=i+1
               END UNTIL (Selected EQ -1)
               e=DataList_CreateElement(ptr, altname)
            END 
            IF PTR_VALID(e) THEN BEGIN
               (*e).data=empaddata.GetDataPointer()
               IF (PTR_VALID((*e).data)) THEN BEGIN
                  displaybin=0.5
                  (*e).id=empaddata.GetSetName()
                  (*e).SzX=empaddata.GetFrameSize(/X)
                  (*e).SzY=empaddata.GetFrameSize(/Y)
                  (*e).SzZ=empaddata.GetScanSize(/X)*empaddata.GetScanSize(/Y)
                  (*e).xsamp=empaddata.GetDetectorSampling(/X)
                  (*e).ysamp=empaddata.GetDetectorSampling(/Y)
                  (*e).xunit='1/nm'
                  (*e).yunit='1/nm'
                  ;; (*e).voltage=empaddata.GetVoltage()
                  (*e).id=fname
                  (*e).type=4
                  (*e).slice=FIX((*e).SzZ/2)
                  (*e).zcoord=3
                  (*e).extra=empaddata
                  (*e).contrastmode="auto"
                  (*e).contrastsubmode="minmax" 
                  (*e).contrastroi="diff"
                  bin=1
                  (*e).binning=displaybin
                  (*e).binningx=displaybin
                  (*e).binningy=displaybin
                  CreateWindow, BIN=displaybin
                  TVDisplay
                  Update_XTabControl
               End
            END  
END


PRO FixEMPADDiffRot
  ptr=GetRootP()
  IF NOT(PTR_VALID(ptr)) THEN BEGIN
     printtocon, "% FixEMPADDiffRot: No 3D Data found."
     return
  END
  ptr=(*ptr).current
  IF NOT(PTR_VALID(ptr)) THEN BEGIN
     printtocon, "% FixEMPADDiffRot: Current 3D Data invalid."
     return
  END
  ptr=(*ptr).datap
  IF NOT(PTR_VALID(ptr)) THEN BEGIN
     printtocon, "%  FixEMPADDiffRot: Current 3D Data holds no data."
         return
      END
  IF OBJ_VALID((*ptr).extra) THEN BEGIN
     o=(*ptr).extra
     minv=[0]
     maxv=[2]
     mybar=obj_new('ProgressBar', MINV=minv,MAXV=maxv,TEXT=["Stage"], STATUS="EMPAD Rotation Compensation", FRAMESTYLE=2)
     mybar->Set, 0, 1, TEXT="1"
     mybar->SetStatus, "Mirroring along x ..."
     MirrorAxis, Axis="x", /INPLACE
     framedatap=o.GetDataPointer()
     N=Size(*framedatap)
     DimX=N[1] & DimY=N[2] & DimZ=N[3]
     mybar->Set, 0, 1, TEXT="2"
     mybar->SetStatus, "Rotating 90 deg cw ..."
     For i=0L,DimZ-1 Do BEGIN
        (*framedatap)[0:127,0:127,i]=ROTATE((*framedatap)[0:127,0:127,i],3)
     END
     obj_destroy, mybar
     Update_XTabControl
     TVDisplay
  END ELSE printtocon, "% FixEMPADDiffRot: Not a PAD object."
END 
