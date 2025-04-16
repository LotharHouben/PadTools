PRO PolarRectCorrelate, p, pattern, shapex, shapey
  IF NOT(PTR_VALID(p)) THEN BEGIN
     printtocon, "% PolarRectCorrelate: Data invalid."
     return
  END
  IF NOT(PTR_VALID(pattern)) THEN BEGIN
     printtocon, "% PolarRectCorrelate: Pattern invalid."
     return
  END
  framedatap=p
  IF PTR_VALID(framedatap) THEN BEGIN
     N=Size(*framedatap)
     DimX=N[1] & DimY=N[2] & DimZ=N[3]
     arg=PTR_NEW(FLTARR(DimZ))
     stretch=PTR_NEW(FLTARR(DimZ))
     peak=PTR_NEW(FLTARR(DimZ))
     mybar=obj_new('ProgressBar', MINV=0,MAXV=DimZ-1,TEXT=["frame no"], STATUS="Correlating frames ...", FRAMESTYLE=2)
     For i=0L,DimZ-1 Do BEGIN
        mybar->Set, 0, i, TEXT=STRING(i)
        shvec=[1.,1.] & CMax=1.
        ic=imcorr((*framedatap)[*,*,i], *pattern, SHIFTVEC=shvec, CORRMAX=CMax, /SILENT)
        (*arg)[i]=shvec[1]*2*!DPI/DimY
        (*stretch)[i]=shvec[0]*2./DimX
        (*peak)[i]=CMax
     END
     obj_destroy, mybar
     ThrowImage, Reform(*arg, shapex, shapey) , title="Arg_PolarRectCorrelate"
     ThrowImage, Reform(*stretch, shapex, shapey), title="Scale_PolarRectCorrelate"
     ThrowImage, Reform(*peak, shapex, shapey), title="CorrMax_PolarRectCorrelate"
     Update_XTabControl
     TVDisplay
  END ELSE printtocon, "% FixEMPADDiffRot: Not a PAD object."
END 


PRO TemplateCorrelateDiff
;; get the two datasets: PAD set and the matching pattern
;; both will be transformed from polar to rectangular coordinates 
;; ideally the PAD set is pre-processed (beamstop to mask central beam etc)
  o=GetExtraObj()
  IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
     ;; choose mask image
     refp=XListSelector(TITLE="Choose Reference Pattern")
     IF PTR_VALID(refp) THEN refp=(*refp).datap ELSE return
     IF PTR_VALID(refp) THEN refp=(*refp).data ELSE return
     IF PTR_VALID(refp) THEN BEGIN
        ;; start to process
        ;;
        ;; parameters for processing
        ;;
        ;; get frame size
        fx=o->GetFrameSize(/X)
        inner=FIX(fx/10)
        outer=FIX(fx/2-1)
        delta=1.
        s=list()
        dx=0
        dy=0
        fill=1B
        s.Add, {value:inner,label:"Inner radius (pix)",newrow:1B}
        s.Add, {value:outer,label:"Outer radius (pix)",newrow:1B}
        s.Add, {value:delta,label:"Angular increment (degree)",newrow:1B}
        s.Add, {value:dx,label:"Off-center shift dx, dy (pix)",newrow:1B}
        s.Add, {value:dy,label:"",newrow:0B}
        c=list()
        c.Add, {value:fill,label:"Fill missing data",newrow:0B}
        IF (XMDataChoiceField(s, c, TITLE="Polar to rectangular coordinate transformation") EQ 1) THEN BEGIN
           dx=s[3].value
           dy=s[4].value
           outer=s[1].value
           delta=s[2].value
           fill=c[0].value
           inner=s[0].value
        END ELSE return
        ;; polar -> rect transform the reference pattern
        ;;
        refrectim=PTR_NEW(RotationalImageData(*refp, FIX(o->GetFrameSize(/X)/2), FIX(o->GetFrameSize(/Y)/2), outer, delta, FILL=fill, INNER=inner))
        ThrowImage, *refrectim, title="Pattern"
        ;;
        ;; polar -> rect transform the data
        ;;
        o.PolarToRect, dx, dy, outer, delta, FILL=fill, INNER=inner
        p=GetCurrentP()
        datap=(*(*p).datap).data
        ;; Fourier rotation and scale transform
        ;;
        PolarRectCorrelate, datap, refrectim , o->GetScanSize(/X), o->GetScanSize(/Y)
        ;;
        ;;
     END    
  END 
END 

