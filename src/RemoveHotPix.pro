FUNCTION FilterNAN, im
  nonfin=Where(im NE im, count, COMPLEMENT=fin)

IF (count GT 0) THEN BEGIN
   threshold=5
   finmean=Robust_Mean(im[fin],threshold,sigmamean)

;; printtocon also accepts string arrays, each of the elements will be
;; printed on a single line
;;   s=["% FilterNAN: "+(*p).comment, $
;;     "   Found "+MyString(count)+" non-finite values.", $
;;     "   Replacing array elements with the mean value of the complement data =;; " + MyString(finmean)+"."]
;; print the string array to the console
;;   printtocon, s
   im[nonfin]=finmean
END ELSE BEGIN
;;   s=["% FilterNAN: "+(*p).comment, $
;;      "   Found no non-finite values."]
;;   printtocon, s
   
END
return, im
END  


FUNCTION MyExp2D, X, Y, P, EXPMULT=expmult
;;
;; 2D Gaussian with constant
;;
;; P(0)+P(1)*exp(-((X(i,j)-P(2))/P(4))^2)*exp(-((Y(i,j)-P(3))/P(5))^2)
Dim=SIZE(X)
EINSX=INTARR(Dim(1))
EINSX(*)=1
W1=TRANSPOSE(X##EINSX)
W2=Y##EINSX
IF Not(keyword_set(expmult)) THEN exponent=2 ELSE exponent=2*expmult
U=FLOAT(((W1-P(4))/P(2))^exponent + ((W2-P(5))/P(3))^exponent)
return, P(0)+P(1)*EXP(-U/2)
END

FUNCTION FlattenImage, A, COFF=coff, FILTER=filter, GETFILTER=getfilter
  ;; 
  ;; removes low frequency modulations
  ;; keywords: coff=cutoff relative to Nyquist
  ;; 
  ;; 2 modes of operation:
  ;; 
  ;; 1) prepare and apply a filter to avoid recalculation 
  ;;    f=FlattenImage(A,COFF=0.75, /GETFILTER)
  ;;    Afilt=FlattenImage(A, FILTER=f)
  ;; 2) one time application
  ;;    Afilt=FlattenImage(A,COFF=0.75)
  ;;
  IF NOT(keyword_set(filter)) THEN BEGIN
     IF NOT(keyword_set(coff)) THEN coff=0.5
     DimX=(Size(A))[1] & DimY=(Size(A))[2]
     X=INDGEN(DimX)
     Y=INDGEN(DimY)
     wx=FIX(DimX*coff/2.35) & wy=FIX(DimY*coff/2.35)
     p=[0,1.,wx,wy,DimX/2,DimY/2]
     filter = 1.-MyExp2D(X,Y,p,EXPMult=2)
     filter=SHIFT(filter,DimX/2,DimX/2)
     ;; 
     IF keyword_set(getfilter) THEN return, filter
  END
  return, ABS(FFT( FFT(A, -1) * filter, 1 ))
  ;; STOP
END


PRO RmHotPix, im, THRESHOLD=threshold, MEDRADIUS=medradius, FILTER=filter, COFF=coff, DARK=dark, ITERATE=iterate, ROBUSTMEAN=robustmean, DILATEMASK=dilatemask
  ;; get image statistics
  A=im
  seed=SYSTIME(/SECONDS)
  IF NOT(keyword_set(threshold)) THEN threshold=8
  IF NOT(keyword_set(medradius)) THEN medradius=2
  IF keyword_set(filter) THEN BEGIN
     A=FlattenImage(A, FILTER=filter)
  END ELSE BEGIN
     IF keyword_set(coff) THEN BEGIN
        A=FlattenImage(A, COFF=coff)
     END
  END
  stat=MOMENT(A)
  ;; meanv=stat[0] & stddev=sqrt(stat[1])
  rsdev=Robust_sigma(A)
  meanv=Robust_Mean(A,threshold,sigmamean)
  ;; PrintToCon, "% RmHotPixel: Robust mean, sigma: "+MyString(meanv)+","+MyString(rsdev)
 
     IF keyword_set(dark) THEN BEGIN
        ndx=where(A LT meanv-threshold*rsdev)
     END ELSE BEGIN
        ndx=where(A GT meanv+threshold*rsdev)
     END
     xsiz=(size(A))[1]
     ysiz=(size(A))[2]
     IF keyword_set(dilatemask) THEN BEGIN
        Mask=BYTARR(xsiz,ysiz)
        Mask[*,*]=0
        Mask[ndx]=1
        mask=(Dilate(mask,replicate(1,dilatemask,dilatemask)))
        ndx=WHERE(mask EQ 1) 
     END
    
     npix=n_elements(ndx)
      IF (keyword_set(dark)) THEN BEGIN
     ;; PrintToCon, "%   Number of dark pixels: "+MyString(npix)
  END ELSE BEGIN
     ;; PrintToCon, "%   Number of hot pixels "+MyString(npix)
  END
     xpos=ndx MOD xsiz 
     ypos=ndx/xsiz
     size=2*medradius+1 ;; muss ungerade sein!!
     ;; IF (npix GT 100) THEN BEGIN
     ;;   res=Dialog_Message("Number of hot pixels is larger than 100 ("+MyString(npix)+"). Do you want to proceed?", /QUESTION)
     ;;   
     ;; END
     FOR i=0,npix-1 DO BEGIN
        xind=xpos[i]-(size/2)+findgen(size) 
        yind=ypos[i]-(size/2)+findgen(size)
        nn=where(xind GE 0 AND xind LT xsiz AND yind GE 0 AND yind LT ysiz)
        ;; Create index array excluding ndx pixels
        fr=(N_Elements(nn))
        C=LONARR(fr*fr)
        for k=0,fr-1 DO for l=0,fr-1 Do C[(k*fr):((k+1)*fr-1)]=LONG(yind[k]*xsiz+xind[l])
        IF NOT(keyword_set(robustmean)) THEN BEGIN
           for m=0,npix-1 DO C=C(WHERE(C NE ndx[m]))
           ;; STOP
           IF (N_Elements(C) GT 0) THEN BEGIN
              im[xpos[i],ypos[i]]=median(A[C])
              A[xpos[i],ypos[i]]=median(A[C])
           END
        END ELSE BEGIN
           ;; re-evaluate meanv and dice the replacement value
           rsigma=Robust_sigma(A[C])
           rmean=Robust_Mean(A,threshold,sigmamean)
           newval=rmean
           REPEAT BEGIN
              newval=randomn(seed, /NORMAL)*rsigma+rmean
           END UNTIL (ABS(newval-rmean) LE (rmean*rsigma))
           
           im[xpos[i],ypos[i]]=newval
           ;; print, 'rstat - x,y,oldval,newval,loc mean, loc stddev=', MyString(xpos[i]), ',',  MyString(ypos[i]), ',',  MyString(A[xpos[i],ypos[i]]), ',', MyString(newval) , ' (', MyString(rmean), ',', MyString(rsigma)
           A[xpos[i],ypos[i]]=newval
        END
     ENDFOR
     
     
  END    
