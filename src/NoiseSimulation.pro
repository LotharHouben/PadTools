PRO SparseEventModeDiffPattern, p, eperdiffframe, bsr, SEED=seed, falseposrate=falseposrate, falsenegrate=falsenegrate, DUPLICATE=duplicate
  ;; p: Pointer to the array of input fourier power, the 'pure signal' for one pixel in a scan image
  ;; the fourier power needs to be normalized 
  ;; eperdiffframe: expectation value for the number of electrons per diffraction frame 
  ;;            calculate this number from the number of electrons per
  ;;            pixel and second divided by the frames per second
  ;;            THIS number has to be less than 1 for this algorithm
  ;;            to make sense
  ;; falseposrate: false positive rate
  ;; falsenegrate: false negative rate
  ;; bsr: background to signal ratio, default: 0
  ;;
  ;; random seed has to be initialized
  ;;
  IF keyword_set(duplicate) THEN p=PTR_NEW(*p)
  IF keyword_set(falsenegrate) THEN falsenegrate=0
  IF keyword_set(falseposrate) THEN falseposrate=0
  IF not(keyword_set(seed)) THEN BEGIN
     seed = long(systime(/seconds) ) ;;  initializes the sequence with the system time
     PrintToCon, "% SparseEventModeImage: Seed =" +MyString(seed)
  END 
  IF NOT(keyword_set(bsr)) THEN bsr=0
  ;;(*p).comment="SparseEventModeDiffPattern("+(*p).comment+")"
  ;; PrintToCon, "     mean background signal (for normalized input data): "+MyString(scale*bsr/(1+bsr))
  ;; PrintToCon, "     mean signal (for normalized input data): "+MyString(scale/(1+bsr))
  dtype=SIZE(*p,/TYPE)
  CASE dtype OF
    6: BEGIN
        PRINTTOCON, "% SparseEventModeDiffPattern: complex values not supported yet"
       END
    9: BEGIN
        PRINTTOCON, "% SparseEventModeDiffPattern: complex values not supported yet"
       END
    ELSE: BEGIN
       N=SIZE(*p,/DIMENSIONS)
       DimX=N[0] & DimY=N[1] & DimZ=LONG(N[2])
       minv=[0]
       maxv=[100]
       pbarperc=0
       pbarstep=5
       mybar=obj_new('ProgressBar', MINV=minv,MAXV=maxv,TEXT=["% done"], STATUS="Processing frames ...", FRAMESTYLE=2)
       mybar->Set, 0, 0, TEXT=STRING(0) 
       For i=0L,DimZ-1 DO BEGIN
          perc=FIX(i*100. / DimZ)
          if (FIX(perc) GT pbarperc) THEN BEGIN
             pbarperc += pbarstep
             mybar->Set, 0, pbarperc, TEXT=STRING(pbarperc)
          END
          eperthisframe=poidev(eperdiffframe, SEED=seed) ;; number of electrons for this diffraction frame
          eperthisframe=eperthisframe[0]
          if (falsenegrate GT 0.) THEN BEGIN
          ;; a count that should be a count but isn't 
          ;; determine whether we have miss or not
             rand=randomu(seed,eperthisframe,/UNIFORM)
             dummy=WHERE(rand LT falsenegrate, count)
             IF (count GE 1) THEN BEGIN
                ; PRINTTOCON, "     False negative counts        : " + String(count)
                eperthisframe -= count
             END 
          END 
          ;; if GetDebugFlag() THEN PRINTTOCON, "     Electrons per this frame          : " + String(eperthisframe)
          ;; generate randomu array with the size of the image
          newim=((*p)[*,*,i]) ;; newim has original data
          newim[*,*]=0.
          if (falseposrate GT 0.) THEN BEGIN
             ;; a count that should not be a count but is 
             ;; determine how many with a poissonian distribution
             lamda=FIX(falseposrate*N_Elements(newim))
             Nfalse=poidev(lamda,SEED=seed)
             ;; PRINTTOCON, "     False positive counts        : " + String(NFalse)
             IF (NFalse GT 0) THEN BEGIN
                ;; determine a random uniform vector with Nfalse elements
                rand=randomu(seed, Nfalse,/UNIFORM)
                ;; STOP
                rand=rand*(N_Elements(newim)-1)
                rand=Round(rand)
                newim[rand]+=1
             END
          END 
          IF (eperthisframe LT 1) THEN BEGIN
             ;; no electron ... empty frame
             (*p)[*,*,i]=newim
          END ELSE BEGIN
             IF (bsr GT 0.0) THEN BEGIN
                bgim=((*p)[*,*,i])
                bgim[*,*]=0.
                ;; to be completed
             END
             ;; Algorithm:
             ;; prepare cumulative of probabilities
             ;; normalize that to 1
             ;; select N randomu values between ]0,1]
             ;; find index in cumulative where the random value falls into the range of values of the
             ;; step in the cumulative
             a=REFORM((*p)[*,*,i],DimX*DimY)
             For j=1,N_Elements(a)-1 DO a[j]+=a[j-1]
             a=a/Max(a)
             rand=randomu(seed, eperthisframe,/UNIFORM)
             newim=REFORM(newim,DimX*DimY)
             For jj=0,eperthisframe-1 DO BEGIN
                hit=WHERE(a GE rand[jj],count)
                IF (count GT 0) THEN newim[hit[0]]+=1
             END   
             newim=REFORM(newim,DimX,DimY)
             IF (bsr GT 0.0) THEN (*p)[*,*,i]=bgim+newim ELSE (*p)[*,*,i]=newim
             ;;POIDEV(ROUND(scale*((*(*p).im)/(1+bsr)))+ROUND(B), SEED=seed)
          END
       END  ;; for loop
       obj_destroy, mybar
    END 
  ENDCASE
END



PRO Wrapper_SparseEventModeDiffPattern
      origp=GetCurrentP()
       pd=(*origp).datap
       pdata=(*pd).data
       o=GetExtraObj()
       empadframe=0
       ;; check dimensions
       N=Size(*pdata)
      help=["Sparse Event Diffraction Modelling Parameters",$
               "", $ 
               "Parameters:", $
               "", $
               "Number of e per frame: Expectation value for the number of electrons per frame. ", $
            "False positive rate: Fraction of pixels that record a count that should not be.", $
            "                     (default: 0.002)", $
            "False negative rate: Fraction of counts that should be but are not.", $
            "                     (default: 0.003)", $
            "RHO: shear angle, zero means horizontal axis is sheared.", $
               "BINWIDTH: Sampling interval for radius dimension. ", $
               "Center Refinement: Peak refinement of the central spot, subpixel accuracy. Recommended.", $
            "Pixel Count Normalization: Normalize to pixel count. Recommended.", $
            "Return All Profiles: Return profiles for each diffraction frame in addition to the total.", $ 
         ""]
         s=list()
         s.Add, {value:10,label:"Number of e per frame:",newrow:0B}
         s.Add, {value:0.002,label:"False count rates, Positive:",newrow:1B}
         s.Add, {value:0.003,label:"Negative:",newrow:0B}
         c=list()
         ;;c.Add, {value:1B,label:"In place calculation",newrow:0B}
         IF (XMDataChoiceField(s, c, TITLE="Sparse Event Diffraction Modelling Parameter", HELP=help) EQ 1) THEN BEGIN
            IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
               ;; get stack pointer
               ;;
               empadframe=1
               SparseEventModeDiffPattern, pdata, s[0].value, 0.0, falseposrate=s[1].value, falsenegrate=s[1].value
            END ELSE BEGIN
               printtocon, '% Wrapper_SparseEventModeDiffPattern: Convert to PAD object first.'
            END
         END 
  
      END  


PRO DetectorNoiseSimulation
;;
;; Add detection noise to simulated data frames
;;
;; The algorithm follows the idea of "inverse" flatfields
;; - Input are a set of bright images (B) and a set of dark images (D)
;; - A gain reference image is calculated from B and D, G=B-D
;;   G represents the average response to a signal
;;   ideally G is an average over N bright and dark images: G_N
;;   (The variance of G_N for large N with respect to the shot noise tells about additional gain noise)
;;   (This gain noise is ignored here, so that G_N represents the gain)   
;; - The theoretical Signal ADU are obtained by the multiplication of the simulated shot-noise data with G_N
;;   and adding an individual noise set 
;;
;; STEP 1
;; - Load a set of dark images  
;; - Get pointers to the Bright and Dark Images
;;   
END
