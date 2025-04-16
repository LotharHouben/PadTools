Function StrainMapping, ERR=err, UPSAMPLING=upsampling
;;  returns fitted peak locations of three diffracted beams, the centre beam, g1 and g2
;;  take three points to mark
;;  fit those three points across all DP's
IF NOT(GetDebugFlag()) THEN BEGIN
   CATCH, Error_status
   IF (Error_status NE 0) THEN BEGIN
      PrintToCon, "% StrainMapping:  Fatal error "
      PrintToCon, "%   Error status  - " + STRING(error_status)
      PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
      CATCH, /Cancel
      return, peakloc
   END
END
peakloc=-1
quiet=1
  ptr=GetRootP()
  IF NOT(PTR_VALID(ptr)) THEN BEGIN
     printtocon, "% StrainMapping: No 3D Data found."
      return, peakloc
  END
  ptr=(*ptr).current
  IF NOT(PTR_VALID(ptr)) THEN BEGIN
     printtocon, "% StrainMapping: Current 3D Data invalid."
      return, peakloc
  END
  e=(*ptr).datap
  IF NOT(PTR_VALID(ptr)) THEN BEGIN
     printtocon, "%  StrainMapping Current 3D Data holds no data."
      return, peakloc
   END
  ;; --------------- Do we need to compensate the rotation? ----------
  ;;
  IF XSimpleDialog("Fix EMPAD diffraction frame rotation before strain analysis?", YES="Yes", NO="No", Title="Fix Rotation") THEN FixEMPADDiffRot
  ;;
  ;;
  ;; auto-determine stack size
  ;; --------------- Peak Selection ----------
  ;; gcentre = undiffracted beam
  ;; g1      = first diffraction vector
  ;; g2      = second diffraction vector
  ;; ------------------------------------------
  ;; auto-determine stack size
  datap=(*e).data
  N=Size(*datap)
  NPar=7
  DimX=N[1] & DimY=N[2] & DimZ=N[3]
  IF NOT(keyword_set(upsampling)) THEN upsampling=1
  peakloc=FLTARR(DimZ, 3*NPar)
  peakerr=FLTARR(DimZ, 3*NPar)
  XC=DimX/2 & YC=DimY/2 & rc=2
  XG1=DimX/4 & YG1=DimY/2 & rg1=2
  XG2=DimX/2 & YG2=DimY/4 & rg2=2
  PrintToCon, "% > Mark the transmitted beam and the fitting region around it."
  IF (XCircle(XC, YC, rc, bin=(*e).binning, /STICKY, TITLE="Undiffracted Beam", HELP=["Mark the transmitted beam","and the fitting region around it."]) GT 0) THEN BEGIN
     kernelc=FLTARR(2*rc*upsampling+1, 2*rc*upsampling+1)
     kernelc[*,*]=1.
     PrintToCon, "% > Mark the first diffracted beam and the fitting region around it."
     IF (XCircle(XG1, YG1, rg1, bin=(*e).binning, /STICKY, TITLE="First Diffracted Beam", HELP=["Mark the first diffracted beam g1","and the fitting region around it."]) GT 0) THEN BEGIN
        kernelg1=FLTARR(2*rg1*upsampling+1, 2*rg1*upsampling+1)
        kernelg1[*,*]=1.
        PrintToCon, "% > Mark second non-colinear diffracted beam and the fitting region around it. "
        IF (XCircle(XG2, YG2, rg2, bin=(*e).binning, /STICKY, TITLE="Second Diffracted Beam", HELP=["Mark the second diffracted beam g2","and the fitting region around it."]) GT 0) THEN BEGIN
           kernelg2=FLTARR(2*rg2*upsampling+1, 2*rg2*upsampling+1)
           kernelg2[*,*]=1.
           minv=[0]
           maxv=[100]
           pbarperc=0
           pbarstep=5
           mybar=obj_new('ProgressBar', MINV=minv,MAXV=maxv,TEXT=["% done"], STATUS="Processing frames ...", FRAMESTYLE=2)
           mybar->SetStatus, "Fitting profiles ..."
           mybar->Set, 0, pbarperc, TEXT=STRING(pbarperc)
           IF GetDebugFlag() THEN BEGIN
              origwdow=!D.WINDOW
              preview=FLTARR(N[1]*upsampling, N[2]*upsampling)
              ThrowImage, preview, Title='Debug', BIN=1
              p=GetCurrentP()
              wdow=(*(*p).datap).window
              quiet=0
              WSET, origwdow
           END
           For ind=0L,DimZ-1 Do BEGIN
              perc=LONG(ind*100. / DimZ)
              if (FIX(perc) GT pbarperc) THEN BEGIN
                 pbarperc += pbarstep
                 mybar->Set, 0, pbarperc, TEXT=STRING(pbarperc)
              END
              im=REFORM((*(*e).data)[*,*,ind])
              IF (upsampling GT 1) THEN BEGIN
                im=REBIN(im, upsampling*DimX, upsampling*DimY) 
              END
              errim=PTR_NEW(SQRT(ABS(im))) 
              x=XC*upsampling & y=Yc*upsampling ;;; reset preset location
              fitdata=1 &  parerr=1
              Profile2DFindCentre, im, x, y, kernelc, FITDATA=fitdata, PARERR=parerr, /ALLOWTILT, ERRSTATE=errstate, /INFO, ERRIM=errim, QUIET=quiet
              fitdata[2:5] *= 1./upsampling
              if (((x-XC*upsampling)*(x-XC*upsampling)+(y-YC*upsampling)*(y-YC*upsampling)) > rc*upsampling) THEN fitdata[1]=0
              peakloc[ind,0:(NPar-1)]=fitdata[0:(NPar-1)]
              parerr[2:5] *= 1./upsampling
              peakerr[ind,0:(NPar-1)]=parerr[0:(NPar-1)]
              x=XG1*upsampling & y=YG1*upsampling ;;; reset preset location
              fitdata=1 &  parerr=1
              Profile2DFindCentre, im, x, y, kernelg1, FITDATA=fitdata, PARERR=parerr, /ALLOWTILT, ERRSTATE=errstate, /INFO, ERRIM=errim, QUIET=quiet
              fitdata[2:5] *= 1./upsampling
              if (((x-XG1*upsampling)*(x-XG1*upsampling)+(y-YG1*upsampling)*(y-YG1*upsampling)) > rg1*upsampling) THEN fitdata[1]=0
              peakloc[ind,NPar:(2*NPar-1)]=fitdata[0:(NPar-1)]
              parerr[2:5] *= 1./upsampling
              peakerr[ind,NPar:(2*NPar-1)]=parerr[0:(NPar-1)]
              x=XG2*upsampling & y=YG2*upsampling
              fitdata=1 &  parerr=1
              Profile2DFindCentre, im, x, y, kernelg2, FITDATA=fitdata, PARERR=parerr, /ALLOWTILT, ERRSTATE=errstate, /INFO, ERRIM=errim, QUIET=quiet
              fitdata[2:5] *= 1./upsampling
              if (((x-XG2*upsampling)*(x-XG2*upsampling)+(y-YG2*upsampling)*(y-YG2*upsampling)) > rg2*upsampling) THEN fitdata[1]=0
              peakloc[ind,(2*NPar):(3*NPar-1)]=fitdata[0:(NPar-1)]
              parerr[2:5] *= 1./upsampling
              peakerr[ind,NPar:(2*NPar-1)]=parerr[0:(NPar-1)]
              PTR_FREE, errim
              IF GetDebugFlag() THEN BEGIN
                 origwdow=!D.WINDOW
                 WSET, wdow
                 TV, BYTSCL(ALOG(im),MIN=0, MAX=16)
                 ;; mark peak locations
                 oplotmode=3                                ;; source copy plotting mode
                 plcolor=1500L                              ;; some plot color
                 device, get_graphics_function=gmode        ; store the current graphics mode.                 
                 device, set_graphics_function=oplotmode    ; XOR mode.
                 if (peakloc[ind,1] GT 0) THEN BEGIN
                    xdev0=FIX(peakloc[ind,4]*upsampling)           ;; device coordinates
                    ydev0=FIX(peakloc[ind,5]*upsampling)
                    plot_cross, xdev0, ydev0, COLOR=plcolor, SZ=5
                 END
                 if (peakloc[ind,1+NPar] GT 0) THEN BEGIN
                    xdev0=FIX(peakloc[ind,4+NPar]*upsampling)           ;; device coordinates
                    ydev0=FIX(peakloc[ind,5+NPar]*upsampling)
                    plot_cross, xdev0, ydev0, COLOR=plcolor, SZ=5
                 END
                 if (peakloc[ind,1+2*NPar] GT 0) THEN BEGIN
                    xdev0=FIX(peakloc[ind,4+2*NPar]*upsampling)           ;; device coordinates
                    ydev0=FIX(peakloc[ind,5+2*NPar]*upsampling)
                    plot_cross, xdev0, ydev0, COLOR=plcolor, SZ=5
                 END
                 WSET, origwdow
                 device, set_graphics_function=gmode ; source copy mode.
              END
           END
           obj_destroy, mybar
        END
     END
  END
  IF keyword_set(err) THEN err=peakerr
  return, peakloc
END


PRO StrainEvaluation, THRESHOLD=threshold, UPSAMPLING=upsampling
  origp=GetCurrentP()
  o=GetExtraObj()
  if not(keyword_set(threshold)) then treshold=0.04
  if not(keyword_set(upsampling)) then upsampling=4
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         sx=o.GetScanSIZE(/X) & sy=o.GetScanSIZE(/Y)
         err=1
         
         s=StrainMapping(UPSAMPLING=upsampling, Err=err)
         ;; s is a matrix [Nframes,3*7]
         ;; s[*, 0:6] are the peak parameters of the transmitted beam
         ;; const amp  dx dy  x y tilt
         NPar=7
         xind=4 & yind=5
         
         geomwidth=SQRT(s[*,2]^2+s[*,3]^2) ;; geometrical width of the transmitted beam
         g1x=(s[*,xind+NPar]-s[*,xind]) ;; distance between g1 and transmitted beam in x 
         g1y=(s[*,yind+NPar]-s[*,yind]) ;; distance between g1 and transmitted beam in y 
         g2x=(s[*,xind+2*NPar]-s[*,xind]) ;; distance between g2 and transmitted beam in x 
         g2y=(s[*,yind+2*NPar]-s[*,yind]) ;; distance between g2 and transmitted beam in y
         ;; mask values ... were is the data meaningful? Use the SBR,
         ;; it should be at least x % of the SBR in the transmitted beam
         peakSBRc=(s[*,1]-s[*,0])
         peakSBRg1=(s[*,1+NPar]-s[*,NPar])
         ratiog1=peakSBRg1/peakSBRc
         peakSBRg2=(s[*,1+2*NPar]-s[*,2*NPar])
         ratiog2=peakSBRg2/peakSBRc
         ;; create binary masks to exclude the locations with an SBR
         ;; below threshold
         valid=Where(ratiog1 GT threshold)
         maskg1=BYTARR(sx,sy)
         maskg1[valid]=1
         valid=Where(ratiog2 GT threshold)
         maskg2=BYTARR(sx,sy)
         maskg2[valid]=1
         mask=maskg2*maskg1
         ;;
         ThrowImage,REFORM(g1x,sx,sy)*maskg1, TITLE="g1 x-component", BIN=TVBin(sx,sy)
         ThrowImage,REFORM(g1y,sx,sy)*maskg1, TITLE="g1 y-component", BIN=TVBin(sx,sy)
         ThrowImage,REFORM(g2x,sx,sy)*maskg2, TITLE="g2 x-component", BIN=TVBin(sx,sy)
         ThrowImage,REFORM(g2y,sx,sy)*maskg2, TITLE="g2 y-compoent", BIN=TVBin(sx,sy)
         ThrowImage,REFORM(Sqrt(g1x*g1x+g1y*g1y),sx,sy)*maskg1, TITLE="g1 magnitude", BIN=TVBin(sx,sy)
         ThrowImage,REFORM(Sqrt(g2x*g2x+g2y*g2y),sx,sy)*maskg1, TITLE="g2 magnitude", BIN=TVBin(sx,sy)
         ;; average values in valid areas
         B=WHERE(REFORM(maskg1,sx*sy) EQ 1) & b1x=Robust_Mean(g1x[B],3) & b1y=Robust_Mean(g1y[B],3) ;; Three sigma mean
         ;;ThrowImage,REFORM((g1x-b1x)/Abs(b1x),sx,sy)*maskg1, TITLE="e1x"
         ;;ThrowImage,REFORM((g1y-b1y)/Abs(b1y),sx,sy)*maskg1, TITLE="e1y"
         Printtocon, "  Magnitude of g1 in high-SBR area on average: "+MyString(Sqrt(b1x*b1x+b1y*b1y)) 
         D=WHERE(REFORM(maskg2,sx*sy) EQ 1) & b2x=Robust_Mean(g2x[D],3) & b2y=Robust_Mean(g2y[D],3)
         Printtocon, "  Magnitude of g2 in high-SBR area on average: "+MyString(Sqrt(b2x*b2x+b1y*b1y)) 
         ;;
         ;; matrix elements of the strain tensor E
         
         ;; formalism: solve |gx|  |exx exy|   |bx|
         ;;                  |  | =|       | * |  |
         ;;                  |gy|  |eyx eyy|   |by|
         ;; exx > 1 means a stretch in diffraction space and a compression in real space
         ;; FIGURE OUT:
         ;; (1) Check exx! there was a typo before
         ;; (2) Make sure that exx > 1 is lattice compression in real space
         ;; (3) Rotation is in which direction?
         ;; (4) Give explanations before staring the routine
         
         invdet=1./(b1x*b2y-b2x*b1y)
         C=WHERE(REFORM(maskg1*maskg2,sx*sy) EQ 1)
         exx=FLTARR(sx*sy)
         exy=FLTARR(sx*sy)
         eyx=FLTARR(sx*sy)
         eyy=FLTARR(sx*sy)
         exx[C]=(g1x[C]*b2y-g2x[C]*b1y)*invdet
         exy[C]=(g2x[C]*b1x-g1x[C]*b2x)*invdet
         eyx[C]=(g1y[C]*b2y-g2y[C]*b1y)*invdet
         eyy[C]=(g2y[C]*b1x-g1y[C]*b2x)*invdet
         ThrowImage,REFORM(exx,sx,sy), TITLE="strain along x - exx", BIN=TVBin(sx,sy)
         ThrowImage,REFORM(eyy,sx,sy), TITLE="strain along y - eyy", BIN=TVBin(sx,sy)
         ThrowImage,REFORM(eyx,sx,sy), TITLE="rotation/shear component - eyx", BIN=TVBin(sx,sy)
         ThrowImage,REFORM(exy,sx,sy), TITLE="rotation/shear component - exy", BIN=TVBin(sx,sy)
         PrintToCon, "%"
         PrintToCon, "% The matrix elements of the strain tensor E are related to the reciprocal space vector transformation"
         PrintToCon, "%"
         PrintToCon, "% |gx|  |exx exy|   |bx|"
         PrintToCon, "% |  | =|       | * |  |"
         PrintToCon, "% |gy|  |eyx eyy|   |by|"
         PrintToCon, "%"
         PrintToCon, "% bx and by are the avergage components of the reciprocal lattice vector in x- and y-direction."
         PrintToCon, "% The average is taken over all data points with SBR above threshold."
         
         Printtocon, "% The strain matrix elements exx, eyy, exyand eyx describe the lattice distortion relative to the average parameter over"
         PrintToCon, "% the spatial locations with a significant signal-to-background ratio."
         PrintToCon, "%"
         PrintToCon, "% exx and eyy are the diagonal elements of the strain tensor. "
         PrintToCon, "% exx(eyy) > 1 means a stretch in diffraction space and a compression in real space"
         PrintToCon, "% exy and eyx are the off-diagonal elements of the strain tensor that describe shear and rotation. "
         PrintToCon, "% wxy=0.5(exy-eyx) is the pure rotation component. For sufficiently small exy and eyx the rotation is"
         PrintToCon, "% linear and in radians."
         ;; rotation component
         ;; 
         wxy=0.5*(exy-eyx)
         ThrowImage,REFORM(wxy,sx,sy), TITLE="rotation wxy", BIN=TVBin(sx,sy)
      END      
END
